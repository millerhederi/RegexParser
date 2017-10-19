module ``DFA Tests``

open RegexParser
open NUnit.Framework

type Case = {
    regex : string
    isMatch : string -> bool
    input : string
    expectedIsMatch : bool
}
with
    override this.ToString () =
        sprintf "Regex: %s; Input: %s; Expected: %b" this.regex this.input this.expectedIsMatch

type CaseGroup = {
    regex : string
    expectTrue : string list
    expectFalse : string list
}
with
    static member toCases this =
        let dfa =
            match this.regex |> Pipeline.buildDfa with
            | Ok (Some dfa) -> dfa
            | Ok None -> failwith "Building the DFA was successful, but produced None."
            | Error error -> failwithf "Building the DFA was unsuccessful: '%s'." error
        let isMatch = Dfa.isMatch dfa

        let trueCases =
            this.expectTrue
            |> Seq.map (fun case ->
                {
                    regex = this.regex
                    isMatch = isMatch
                    input = case
                    expectedIsMatch = true
                }
            )
        let falseCases =
            this.expectFalse
            |> Seq.map (fun case ->
                {
                    regex = this.regex
                    isMatch = isMatch
                    input = case
                    expectedIsMatch = false
                }
            )
        trueCases |> Seq.append falseCases |> Seq.toList

let case regex =
    {
        regex = regex
        expectTrue = List.empty
        expectFalse = List.empty
    }

type ``Regex to AST conversion Tests`` () =
    static member cases =
        [
            { case "a|b" with
                expectTrue =
                    [
                        "a"
                        "b"
                    ]
                expectFalse =
                    [
                        "ab"
                        "aa"
                        ""
                    ]
            }
            { case "(ab)*a" with
                expectTrue =
                    [
                        "a"
                        "aba"
                        "ababa"
                        "ababababababa"
                    ]
                expectFalse =
                    [
                        ""
                        "b"
                        "ab"
                        "aa"
                    ]
            }
            { case "(a|b*)abb*" with
                expectTrue =
                    [
                        "aab"
                        "bbbbab"
                        "bbabbb"
                        "aabbbb"
                        "ab"
                        "abbbb"
                    ]
                expectFalse =
                    [
                        "ba"
                        "aa"
                        "babbbba"
                    ]
            }
        ] |> List.collect CaseGroup.toCases

    [<TestCaseSource("cases")>]
    member this.``When matching against a regex `` (case : Case) =
        Assert.AreEqual (case.expectedIsMatch, case.input |> case.isMatch)
