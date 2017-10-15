module ``AST Tests``

open RegexParser
open NUnit.Framework

type Case = {
    input : string
    expected : Regex option option
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

type ``Regex to AST conversion Tests`` () =
    static member cases =
        [
            expectOk "a" (Some (Regex.Char 'a'))
            expectOk "a|b" (Some (Regex.Choice (Regex.Char 'a', Regex.Char 'b')))
            expectOk "ab" (Some (Regex.Concat (Regex.Char 'a', Regex.Char 'b')))
            expectOk "a|bc" (Some (Regex.Choice (Regex.Char 'a', Regex.Concat (Regex.Char 'b', Regex.Char 'c'))))
            expectOk "ab|c" (Some (Regex.Choice (Regex.Concat (Regex.Char 'a', Regex.Char 'b'), Regex.Char 'c')))
            expectOk "abc" (Some (Regex.Concat (Regex.Concat (Regex.Char 'a', Regex.Char 'b'), Regex.Char 'c')))
            expectOk "a|b|c" (Some (Regex.Choice (Regex.Char 'a', Regex.Choice (Regex.Char 'b', Regex.Char 'c'))))
            expectOk "(a)" (Some (Regex.Char 'a'))
            expectOk "a(b)" (Some (Regex.Concat (Regex.Char 'a', Regex.Char 'b')))
            expectOk "a(b|c)" (Some (Regex.Concat (Regex.Char 'a', Regex.Choice (Regex.Char 'b', Regex.Char 'c'))))
            expectOk "()a" (Some (Regex.Char 'a'))
            expectOk "a()" (Some (Regex.Char 'a'))
            expectOk "a()b" (Some (Regex.Concat (Regex.Char 'a', Regex.Char 'b')))
            expectOk "a*" (Some (Regex.Star (Regex.Char 'a')))
            expectOk "ab*" (Some (Regex.Concat (Regex.Char 'a', Regex.Star (Regex.Char 'b'))))
            expectOk "(ab)*" (Some (Regex.Star (Regex.Concat (Regex.Char 'a', Regex.Char 'b'))))
            expectOk "(a|b)*c" (Some (Regex.Concat (Regex.Star (Regex.Choice (Regex.Char 'a', Regex.Char 'b')), Regex.Char 'c')))
            expectOk "a*|b" (Some (Regex.Choice (Regex.Star (Regex.Char 'a'), Regex.Char 'b')))
            expectOk "a|b*" (Some (Regex.Choice (Regex.Char 'a', Regex.Star (Regex.Char 'b'))))
            expectOk "a|(bc)" (Some (Regex.Choice (Regex.Char 'a', Regex.Concat (Regex.Char 'b', Regex.Char 'c'))))
            expectOk "(a)(b)" (Some (Regex.Concat (Regex.Char 'a', Regex.Char 'b')))
            expectOk "(a)b|c" (Some (Regex.Choice (Regex.Concat (Regex.Char 'a', Regex.Char 'b'), Regex.Char 'c')))
            expectOk "(a|b)c" (Some (Regex.Concat (Regex.Choice (Regex.Char 'a', Regex.Char 'b'), Regex.Char 'c')))
            expectOk "a()" (Some (Regex.Char 'a'))
            expectOk "" None
            expectOk "()" None

            expectError "a("
            expectError "a|"
            expectError "|a"
            expectError "|"
            expectError "a)"
            expectError ")("
        ]

    [<TestCaseSource("cases")>]
    member this.``When building the AST for input `` (case : Case) =
        let actual = case.input |> Pipeline.buildAst

        match case.expected, actual with
        | Some expected, Result.Ok actual -> Assert.AreEqual(expected, actual)
        | Some expected, Result.Error error -> Assert.Fail(sprintf "Expected '%A', but got Error '%s'." expected error)
        | None, Result.Ok actual -> Assert.Fail(sprintf "Expected Error, but got Ok '%A'." actual)
        | None, Result.Error _ -> ()
