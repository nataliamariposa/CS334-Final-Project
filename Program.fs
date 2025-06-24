open Combinator
open System.IO
open AST
open Parser
open Evaluator

let usage() =
    printfn "Usage: dotnet run <input>"
    printfn "\twhere <input> is a valid card game definition, for example, 'player 1 {deck1 [heart, 1, 2, 3, 4]}'"
    exit 1

[<EntryPoint>]
let main args =
    if args.Length > 0 then
        let file = args[0]
        if not (System.IO.File.Exists file) then
         printfn $"ERROR: Cannot find file '{file}'."
         usage()
        let input = File.ReadAllText file

        match parse input with
        // | Some (DeckDef expr) ->
        //     let ourCards = expr.Cards
        //     Sum ourCards
            
        | Some expr ->
            let result, _ = eval expr Map.empty
            match result with
            |Num n -> printfn "Evaluation result: %d" n
            |Str n -> printfn "Evaluation result: %s" n
            |Bool n -> printfn "Evaluation result: %b" n
            |CardDef n -> printfn "Evaluation result: %A" n
            |DeckDef n -> printfn "Evaluation result: %A" n
            | _ -> printfn "Something"
            

        | None -> 
            printfn "Error: Unable to parse the input."
            usage()
    else
        usage()
    0 

//just checking