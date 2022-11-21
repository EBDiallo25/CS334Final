open Combinator
open Parser
open Evaluator
open System

[<EntryPoint>]
let main (args: string[]): int =
    // let cs = [Line((0,0), (100,100), Red)]
    // let cs = [0..400] |> List.map (fun x -> Line((x,0), (400,x), Red))
    let path = args[0]
    let text = IO.File.ReadAllText path
    let input = prepare text
    match grammar input with
    | Success(ast,_) ->
        let svg = eval ast
        printfn "%s" svg
        0
    | Failure(pos,rule) ->
        printfn "Invalid expression."
        let msg = sprintf "Cannot parse input at position %d in rule '%s':" pos rule
        let diag = diagnosticMessage 20 pos text msg
        printf "%s" diag
        1