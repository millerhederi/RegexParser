#r @".\bin\Debug\netcoreapp2.0\RegexParser.dll"
open RegexParser

let tokens = Tokenizer.tokenize "(a|b*)ab"
let regex = tokens |> (Ast.build |> Result.bind)
let nfa = regex |> (Nfa.build |> Option.map |> Result.map)