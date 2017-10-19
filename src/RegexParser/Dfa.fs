namespace RegexParser

// See https://web.archive.org/web/20170129031713/http://www.cs.nuim.ie/~jpower/Courses/Previous/parsing/node9.html
// for a description of the algorithm used.
module Dfa =

    type private State = Set<Nfa.State>
    type StateTransitions<'state when 'state : comparison> = ('state * char * 'state) seq
    type Dfa<'state when 'state : comparison> = {
        initialState : 'state
        transitions : StateTransitions<'state>
        finalStates : 'state seq
    }

    let private getAllSymbols (nfa : Nfa.Nfa) =
        nfa
        |> Seq.collect (Seq.choose (fun transition -> transition.Key))
        |> Seq.distinct
        |> Seq.toList

    let getTransitionState (initialState : int) symbol dfa =
        dfa.transitions
        |> Seq.find (fun (tState, tSymbol, _) -> (tState = initialState) && (tSymbol = symbol))
        |> (fun (_, _, state) -> state)

    // Get the DFA state transition from `state` along `symbol`. This corresponds to the `move`
    // function in the implemented paper.
    let private getStateTransition (state : State) symbol (nfa : Nfa.Nfa) =
        let endState =
            state
            |> Seq.map (fun nfaState ->
                nfa
                |> Nfa.getReachableStates nfaState (Some symbol)
                |> Seq.map (fun state -> nfa |> Nfa.getClosure state)
                |> Set.unionMany)
            |> Set.unionMany
        (state, symbol, endState)

    let rec private buildInternal (alphabet : char seq) (unmarkedStates : State list) (markedStates : Set<State>) (stateTransitions : StateTransitions<State>) (nfa : Nfa.Nfa) : StateTransitions<State> =
        match unmarkedStates with
        | [] -> stateTransitions
        | head :: tail ->
            let stateTransitionsFromHead = alphabet |> Seq.map (fun symbol -> nfa |> getStateTransition head symbol)
            let stateTransitions = stateTransitions |> Seq.append stateTransitionsFromHead |> Seq.toList
            let markedStates = markedStates |> Set.add head
            let unmarkedStates =
                let unmarkedStatesInStateTransitionsFromHead =
                    stateTransitionsFromHead
                    |> Seq.map (fun (_, _, state) -> state)
                    |> Seq.filter (fun state -> markedStates |> Set.contains state |> not)
                tail
                |> Seq.append unmarkedStatesInStateTransitionsFromHead
                |> Seq.distinct
                |> Seq.toList
            buildInternal alphabet unmarkedStates markedStates stateTransitions nfa

    let private getFinalStates (nfa : Nfa.Nfa) (stateTransitions : StateTransitions<State>) =
        let dfaStates =
            stateTransitions
            |> Seq.collect (fun (initState, _, finalState) -> [initState; finalState])
            |> Seq.distinct
        let nfaFinalState = nfa |> Nfa.getFinalState

        dfaStates |> Seq.filter (fun state -> state |> Set.contains nfaFinalState) |> Seq.toList

    let private reduceDfa (dfa : Dfa<State>) : Dfa<int>=
        let states =
            dfa.transitions
            |> Seq.collect (fun (initState, _, finalState) -> [initState; finalState])
            |> Seq.distinct
        let stateMap =
            states
            |> Seq.mapi (fun i state -> (state, i))
            |> Map.ofSeq
        let reducedTransitions =
            dfa.transitions
            |> Seq.map (fun (initState, symbol, finalState) -> (stateMap |> Map.find initState, symbol, stateMap |> Map.find finalState))
            |> Seq.toList
        let reducedFinalStates =
            dfa.finalStates
            |> Seq.map (fun state -> stateMap |> Map.find state)
            |> Seq.toList
        let reducedInitialState = stateMap |> Map.find dfa.initialState

        {
            initialState = reducedInitialState
            transitions  = reducedTransitions
            finalStates  = reducedFinalStates
        }

    let build (nfa : Nfa.Nfa) : Dfa<int> =
        let alphabet = nfa |> getAllSymbols
        let initialState = nfa |> Nfa.getClosure 0

        let transitions = nfa |> buildInternal alphabet [ initialState ] Set.empty Seq.empty
        let finalStates = transitions |> getFinalStates nfa
        let dfa =
            {
                initialState = initialState
                transitions = transitions
                finalStates = finalStates
            }

        dfa |> reduceDfa

    let isMatch dfa input =
        let state = input |> Seq.fold (fun state symbol -> getTransitionState state symbol dfa) dfa.initialState
        dfa.finalStates |> Seq.contains state
