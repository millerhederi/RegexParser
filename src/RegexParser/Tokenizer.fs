namespace RegexParser

[<RequireQualifiedAccess>]
type Token =
    | LParen
    | RParen
    | Char of char
    | Star
    | Or

module Tokenizer =

    let tokenize input =
        let rec exec input tokens =
            match input with
            | [] -> Result.Ok (tokens |> Seq.rev)
            | '(' :: tail -> exec tail (Token.LParen :: tokens)
            | ')' :: tail -> exec tail (Token.RParen :: tokens)
            | '*' :: tail -> exec tail (Token.Star :: tokens)
            | '|' :: tail -> exec tail (Token.Or :: tokens)
            | c :: tail when c |> System.Char.IsLetter -> exec tail (Token.Char c :: tokens)
            | c :: _ -> Result.Error (sprintf "Encountered an unexpected character '%c'." c)

        exec (input |> Seq.toList) List.empty
