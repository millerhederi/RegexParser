#r @".\bin\Debug\netcoreapp2.0\RegexParser.dll"
open RegexParser

let tokens = Tokenizer.tokenize "(a|b*)ab"
let regex = tokens |> (Ast.build |> Result.bind)
let nfa = regex |> (Nfa.build |> Option.map |> Result.map)
let dfa = nfa |> (Dfa.build |> Option.map |> Result.map)

let isMatch =
    match dfa with
    | Ok (Some dfa) ->
        Dfa.isMatch dfa
    | Ok None
    | Error _ -> failwith "failed"

"aab" |> isMatch
