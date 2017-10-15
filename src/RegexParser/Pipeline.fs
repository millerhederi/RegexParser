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

    let execute : seq<char> -> Result<Regex option, string> =
        buildAst
