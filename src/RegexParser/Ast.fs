namespace RegexParser

[<RequireQualifiedAccess>]
type Regex =
    | Char of char            // a
    | Choice of Regex * Regex // a|b
    | Concat of Regex * Regex // ab
    | Star of Regex           // a*

module Ast =

    type private GRegex =
        | Char of char
        | Choice of GRegex * GRegex
        | Concat of GRegex * GRegex
        | Star of GRegex
        | Group of Group
    and private Group =
        | Valid of GRegex
        | Invalid of GRegex option

    let rec private buildInternal (tokens : list<Token>) regex =
        match tokens with
        | [] -> Result.Ok (regex, [])
        | Token.Char c :: tail -> handleChar c tail regex
        | Token.Star :: tail -> handleStar tail regex
        | Token.Or :: tail -> handleOr tail regex
        | Token.RParen :: tail -> handleRParen tail regex
        | Token.LParen :: tail -> handleLParen tail regex

    and private handleChar char' tail regex =
        let regex =
            match regex with
            | None -> GRegex.Char char'
            | Some regex -> GRegex.Concat (regex, GRegex.Char char')
        buildInternal tail (regex |> Some)

    and private handleStar tail regex =
        match regex with
        | None -> Result.Error "Encountered '*' without a valid regex on the left."
        | Some regex ->
            let regex =
                match regex with
                | Char _ -> Star regex
                | Concat (a, b) -> Concat (a, Star b)
                | Choice (a, b) -> Choice (a, Star b)
                | Star a -> Star a
                | Group a -> Star (Group a)
            buildInternal tail (regex |> Some)

    and private handleOr tail regex =
        match regex with
        | None -> Result.Error "Attempting to '|' with an empty regex on the left."
        | Some regex ->
            match buildInternal tail None with
            | Result.Error _ as result -> result
            | Result.Ok (None, _)
            | Result.Ok (Some (Group (Invalid None)), _) ->
                Result.Error "Attempting to '|' with an empty regex on the right."
            | Result.Ok (Some (Group (Invalid (Some tailRegex))), tail) ->
                Result.Ok (Some (Group (Invalid (Some (Choice (regex, tailRegex))))), tail)
            | Result.Ok (Some tailRegex, _) ->
                Result.Ok (Some (Choice (regex, tailRegex)), [])

    and private handleRParen tail regex =
        Result.Ok (Some (GRegex.Group (Invalid regex)), tail)

    and private handleLParen tail regex =
        match buildInternal tail None, regex with
        | Result.Error _ as result, _ -> result
        | Result.Ok (Some (Group (Invalid None)), tail), _ ->
            buildInternal tail regex
        | Result.Ok (Some (Group (Invalid (Some groupRegex))), tail), Some regex ->
            buildInternal tail (Some (Concat (regex, Group (Valid groupRegex))))
        |Result.Ok (Some (Group (Invalid (Some groupRegex))), tail), None ->
            buildInternal tail (Some (Group (Valid groupRegex)))
        | Result.Ok _ as f, _ ->
            Result.Error "Encountered a '(' without a matching ')'."

    let rec private toRegex = function
        | Char a -> Regex.Char a
        | Choice (a, b) -> Regex.Choice (a |> toRegex, b |> toRegex)
        | Concat (a, b) -> Regex.Concat (a |> toRegex, b |> toRegex)
        | Star a -> Regex.Star (a |> toRegex)
        | Group (Valid a) -> a |> toRegex
        | Group (Invalid _) ->
            failwithf "Unreachable state Invalid regex to be converted."

    let build (tokens : seq<Token>) =
        match buildInternal (tokens |> Seq.toList) None with
        | Result.Error error -> Result.Error error
        | Result.Ok (None, []) -> Result.Ok None
        | Result.Ok (Some (Group (Invalid _)), _) -> Result.Error "Encountered a ')' without a matching '('."
        | Result.Ok (Some regex, []) -> Result.Ok (Some (regex |> toRegex))
        | Result.Ok (_, tail) -> Result.Error "The input could not be fully parsed."
