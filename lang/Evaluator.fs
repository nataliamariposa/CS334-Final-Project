module Evaluator

open System
open Combinator
open AST
open Parser

type Env = Map<string, Expr>


let rec eval (expr: Expr) (env: Env) : Expr * Env =
    match expr with
    | Num n -> expr, env
    | Str s -> expr, env
    | Bool b -> expr, env
    | CardDef _ -> expr, env
    | DeckDef d -> 
        printfn "Deck '%s' defined with %d cards." d.Name (List.length d.Cards)
        expr, env.Add(d.Name, DeckDef d) 
    | PlayerDef p ->
        printfn "Player %d made with deck '%s'." p.IdNum p.Deck.Name
        let idstr = string(p.IdNum)
        expr, env.Add(p.Deck.Name, DeckDef p.Deck)
    | Sum name ->
        match env.TryFind name with
        | Some expr ->
            match expr with
            | DeckDef d -> 
             let total = d.Cards |> List.sumBy (fun c -> c.Number)
             let namestr = string(name)
             printfn $"Sum of card values in '{name}' is {total}."
             Num(total), env
            | _ ->
             printfn "Only Sum on Player/Deck is allowed."
             exit 1
        | None ->
            printfn "Deck '%s' not found." name
            expr, env
    | Seq es ->
        match es with
        | [] ->
            printfn "ERROR: Empty sequence is not allowed."
            exit 1
        | [e] -> eval e env
        | e::es2 ->
            let _, env1 = eval e env
            let s = Seq es2
            eval s env1
    | Var v ->
        if env.ContainsKey v then
            let value = env[v]
            value, env
        else
            printfn $"ERROR: Undefined variable '{v}'."
            exit 1
    | Assign (lhs,rhs) ->
        match lhs with
        | Var v ->
            let rhsr, env1 = eval rhs env
            let env2 = env1.Add (v, rhsr)
            rhsr, env2
        | _ ->
            printfn "ERROR: The left side of an assignment must be a variable."
            exit 1
    | Equal (lhs,rhs) ->
        match lhs, rhs with
        | Num n1, Num n2 -> Bool (n1 = n2), env
        | Bool n1, Bool n2 -> Bool (n1 = n2), env
        | Str n1, Str n2 -> Bool (n1 = n2), env

        | Var v1, Var v2 -> Bool ((eval lhs env) = (eval rhs env)), env
        | Sum s1, Sum s2 -> Bool (s1 = s2), env
        | _ -> Bool(false), env
    | Greater (lhs,rhs) ->
        match lhs, rhs with
        | Num n1, Num n2 -> Bool (n1 > n2), env
        | Var v1, Var v2 -> Bool ((eval lhs env) > (eval rhs env)), env
        | Sum s1, Sum s2 -> Bool ((eval lhs env) > (eval rhs env)), env
        | _ -> Bool(false), env
    | Lesser (lhs,rhs) ->
        match lhs, rhs with
        | Num n1, Num n2 -> Bool (n1 < n2), env
        | Var v1, Var v2 -> Bool ((eval lhs env) < (eval rhs env)), env
        | Sum s1, Sum s2 -> Bool ((eval lhs env) < (eval rhs env)), env
        | _ -> Bool(false), env
    | Shuffle name ->
       match env.TryFind name with
       | Some (DeckDef deck) ->
        let rng = System.Random()
        let shuffledCards =
            deck.Cards
            |> List.map (fun card -> rng.Next(), card)
            |> List.sortBy fst
            |> List.map snd

        let shuffledDeck = { deck with Cards = shuffledCards }

        printfn "Deck '%s' shuffled." deck.Name
        DeckDef shuffledDeck, env.Add(name, DeckDef shuffledDeck)
       | Some _ ->
        printfn "ERROR: '%s' is not a deck and cannot be shuffled." name
        Nop, env
       | None ->
        printfn "ERROR: Deck '%s' not found." name
        Nop, env
    | Firstcard name ->
       match env.TryFind name with
       | Some (DeckDef deck) -> match deck.Cards with
                                | first :: _ -> CardDef(first), env
                                | [] -> Nop, env
       | Some _ ->
       printfn "ERROR: '%s' is not a deck" name
       Nop, env
       | None ->
        //printfn "ERROR: Deck '%s' not found." name
        Nop, env
    | Showcard (number, name) ->
       match env.TryFind name with
       | Some (DeckDef deck) when number >= 0 -> 
                                  match List.tryItem number deck.Cards with
                                  | Some card -> CardDef card, env
                                  | None -> 
                                       printfn "No card at index %d in deck '%s'." number name
                                       Nop, env
       | Some _ ->
       printfn "ERROR: '%s' is not a deck." name
       Nop, env
       | None ->
        //printfn "ERROR: Deck '%s' not found." name
        Nop, env
    | Takecard (giver, taker) ->
      match env.TryFind giver, env.TryFind taker with
      | Some (DeckDef giverDeck), Some (DeckDef takerDeck) ->
        match giverDeck.Cards with
        | [] ->
            printfn "Deck '%s' is empty, cannot take a card." giver
            Nop, env
        | card :: remaining ->
            let updatedGiver = { giverDeck with Cards = remaining }
            let updatedTaker = { takerDeck with Cards = card :: takerDeck.Cards }

            let newEnv =
                env
                |> Map.add giver (DeckDef updatedGiver)
                |> Map.add taker (DeckDef updatedTaker)

            printfn "Moved card %A from '%s' to '%s'" card giver taker
            Nop, newEnv

      | Some _, Some _ ->
        printfn "ERROR: One of '%s' or '%s' is not a DeckDef." giver taker
        Nop, env

      | _ ->
        printfn "ERROR: Deck(s) '%s' or '%s' not found." giver taker
        Nop, env
    | Hascards d ->
       match env.TryFind d with
       | Some (DeckDef deck) -> match deck.Cards with 
                                | first :: _ -> Bool(true), env
                                | [] -> Bool(false), env
       | Some _ -> 
       printfn "ERROR: '%s' is not a deck." d
       Nop, env
       | None ->
        //printfn "ERROR: Deck '%s' not found." name
        Nop, env
    | Winner d ->
       match env.TryFind d with
       | Some (DeckDef deck) -> printfn "'%s' is the winner!" d
                                DeckDef(deck), env
       | Some _ -> 
       printfn "ERROR: '%s' is not a deck." d
       Nop, env
       | None ->
        //printfn "ERROR: Deck '%s' not found." name
        Nop, env
    | While(condition, body) ->
      let rec loop (env: Env) : Expr * Env =
        let Resu, newEnv = eval condition env
        match Resu with
        | Bool true ->
            let _, envAfterBody = eval body newEnv
            loop envAfterBody
        | Bool false ->
            Nop, newEnv
        | _ ->
            printfn "ERROR: While condition must evaluate to a boolean."
            Nop, env
      loop env
    | If(condition, body) ->
      let rec loop (env: Env) : Expr * Env =
        let Resu, newEnv = eval condition env
        match Resu with
        | Bool true ->
            let resultExpr, envAfterBody = eval body newEnv
            resultExpr, envAfterBody
        | Bool false ->
            Nop, newEnv
        | _ ->
            printfn "ERROR: While condition must evaluate to a boolean."
            Nop, env
      loop env
    |Prompt s ->
       printfn "%s" s
       let answer = Console.ReadLine()
       match answer with
       | "1" -> Bool(true), env
       | "0" -> Bool(false), env
       | _ -> 
          printfn "ERROR: Answers besides 1 for True and 0 for False are not currently implemented."
          Nop, env
    | Nop -> 
        expr, env



