namespace RegexParser

module Nfa =

    type State = int
    type StateTransitions = Map<char option, Set<State>>
    type Nfa = array<StateTransitions>

    let private getFinalState : Nfa -> State = Array.length // >> ((-) 1)

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

        let aFinalState = aNfa |> getFinalState
        let bAdjustedNfa = bNfa |> addOffsetToNfa aFinalState

        bAdjustedNfa |> Array.append aNfa

    and private handleChoice a b =
        let aNfa = a |> buildInternal
        let bNfa = b |> buildInternal

        let aFinalState = aNfa |> getFinalState
        let bFinalState = bNfa |> getFinalState

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
