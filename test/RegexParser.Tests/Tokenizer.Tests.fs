module ``Tokenizer Tests``

open RegexParser
open NUnit.Framework

type Case = {
    input : string
    expected : seq<Token> option
}
with
    override this.ToString () = this.input

let expectOk input expected =
    {
        input = input
        expected = expected |> Some
    }

let expectError input =
    {
        input = input
        expected = None
    }

type ``Tokenizer Tests`` () =

    static member cases =
        [
            expectOk "a" [Token.Char 'a']
            expectOk "ab" [Token.Char 'a'; Token.Char 'b']
            expectOk "a|b" [Token.Char 'a'; Token.Or; Token.Char 'b']
            expectOk "(a|b)c" [Token.LParen; Token.Char 'a'; Token.Or; Token.Char 'b'; Token.RParen; Token.Char 'c']
            expectOk "(a|b)*" [Token.LParen; Token.Char 'a'; Token.Or; Token.Char 'b'; Token.RParen; Token.Star]
            expectOk "" []

            // Most of these are valid regex patterns, but we're only trying to handle a small subset of
            // actual regex patterns.
            expectError " "
            expectError "a b"
            expectError "1"
            expectError "+"
            expectError "."
            expectError "["
            expectError "]"
        ]

    [<TestCaseSource("cases")>]
    member this.``When tokenizing input `` (case : Case) =
        let actual = case.input |> Tokenizer.tokenize

        match case.expected, actual with
        | Some expected, Result.Ok actual -> Assert.AreEqual(expected, actual)
        | Some expected, Result.Error error -> Assert.Fail(sprintf "Expected '%A', but got Error '%s'." expected error)
        | None, Result.Ok actual -> Assert.Fail(sprintf "Expected Error, but got Ok '%A'." actual)
        | None, Result.Error _ -> ()
