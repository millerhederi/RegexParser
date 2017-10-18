namespace RegexParser

[<RequireQualifiedAccess>]
module Result =

    let bind withOk = function
        | Ok result -> result |> withOk
        | Error error -> Error error

module Pipeline =

    let tokenize = Tokenizer.tokenize

    let buildAst =
        tokenize >> (Ast.build |> Result.bind)

    let buildNfa =
        buildAst >> (Nfa.build |> Option.map |> Result.map)

    let buildDfa =
        buildNfa >> (Dfa.build |> Option.map |> Result.map)

    let execute : seq<char> -> Result<Dfa.Dfa<int> option, string> =
        buildDfa
