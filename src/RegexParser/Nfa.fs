namespace RegexParser

module Nfa =

    type State = int
    type StateTransitions = Map<char option, Set<State>>
    type Nfa = array<StateTransitions>

    let getReachableStates state symbol (nfa : Nfa) =
        nfa.[state] |> Map.tryFind symbol |> Option.defaultValue Set.empty

    let rec getClosure state (nfa : Nfa) =
        nfa
        |> getReachableStates state None
        |> Seq.map (fun state -> nfa |> getClosure state)
        |> Set.unionMany
        |> Set.add state

    // Internally, we're not adding a StateTransitions for the final state, so the
    // final state is the length of the NFA; however, when we've completed building
    // up the NFA, we append an empty StateTransition to the end of the NFA to simplify
    // getReachableStates to assume we can index the NFA with any valid state, thus the
    // final state is actually the length of the NFA - 1.
    let private getFinalStateInternal : Nfa -> State = Array.length
    let getFinalState = getFinalStateInternal >> (fun state -> state - 1)

    let private getSingleStateTransition char states =
        (char, states |> Set.ofSeq) |> Seq.singleton |> Map.ofSeq

    let private addOffsetToNfa =
        let addOffsetToStateTransitions offset stateTransitions =
            stateTransitions |> Map.map (fun _ states -> states |> Set.map (fun state -> state + offset))
        fun offset nfa ->
            nfa |> Array.map (addOffsetToStateTransitions offset)

    let rec private buildInternal : Regex -> Nfa = function
        | Regex.Char c -> handleChar c
        | Regex.Concat (a, b) -> handleConcat a b
        | Regex.Choice (a, b) -> handleChoice a b
        | Regex.Star regex -> handleStar regex

    and private handleChar c =
        [| getSingleStateTransition (Some c) [ 1 ] |]

    and private handleConcat a b =
        let aNfa = a |> buildInternal
        let bNfa = b |> buildInternal

        let aFinalState = aNfa |> getFinalStateInternal
        let bAdjustedNfa = bNfa |> addOffsetToNfa aFinalState

        bAdjustedNfa |> Array.append aNfa

    and private handleChoice a b =
        let aNfa = a |> buildInternal
        let bNfa = b |> buildInternal

        let aFinalState = aNfa |> getFinalStateInternal
        let bFinalState = bNfa |> getFinalStateInternal

        let aAdjustedNfa = aNfa |> addOffsetToNfa 1
        let bAdjustedNfa = bNfa |> addOffsetToNfa (aFinalState + 2)

        let finalState = (aFinalState) + (bFinalState) + 3

        [ getSingleStateTransition None [ finalState ] ]
        |> Seq.append bAdjustedNfa
        |> Seq.append [ getSingleStateTransition None [ finalState ] ]
        |> Seq.append aAdjustedNfa
        |> Seq.append [ getSingleStateTransition None [ 1; aFinalState + 2 ] ]
        |> Seq.toArray

    and private handleStar regex =
        let nfa = regex |> buildInternal |> addOffsetToNfa 1
        let finalState = nfa.Length + 2

        [ getSingleStateTransition None [ 1; finalState ] ]
        |> Seq.append nfa
        |> Seq.append [ getSingleStateTransition None [ 1; finalState ] ]
        |> Seq.toArray

    let build regex : Nfa =
        [| Map.empty |] |> Array.append (buildInternal regex)
