module CS334 
open AST
open Combinator
open System.IO


(*
 <polynomal> ::= <term>␣+␣<polynomial>
| <term>
<term> ::= <coeff>x^<exp>
<coeff> ::= -number | number
<exp> ::= -number | number
<number> ::= <digit>+
<digit> ::= 0 | ... | 9
 *)

let DEBUG = true


(* PARSER *)
let pad p = pbetween pws0 p pws0

let psymbol =
    pchar ',' <|>
    pchar '.' <|>
    pchar '~' <|>
    pchar '!' <|>
    pchar '?' <|>
    pchar '@' <|>
    pchar '#' <|>
    pchar '$' <|>
    pchar '%' <|>
    pchar '^' <|>
    pchar '&' <|>
    pchar '*' <|>
    pchar '(' <|>
    pchar ')' <|>
    pchar '-' <|>
    pchar '+' <|>
    pchar '_' <|>
    pchar '='

let Jletter = 
    pletter <|> pdigit <|> pchar ' '

let strnginsde = pmany0 Jletter |>> stringify <!> "strng inside"

let strng = pbetween (pchar '"') (strnginsde) (pchar '"') <!> "strng outside"


let posnumber =
    pmany1 pdigit
    |>> stringify
    |>> int
    <!> "num"

let pheart = pfresult(pstr("heart")) (Heart)
let pclub = pfresult(pstr("club")) (Club) 
let pdiamond = pfresult(pstr("diamond")) (Diamond)  
let pspade = pfresult(pstr("spade")) (Spade)

let suit = pheart <|> pclub <|> pspade <|> pdiamond


let negnumber =
    pseq(pmany0 (pchar('-')))(posnumber)(fun (x, y:int) -> -y)

let num = posnumber <|> negnumber 

let Cardinner = pleft (suit) (pstr(", "))
let pcard = pbetween(pchar('('))(pseq (Cardinner) (num) (fun (x, y) -> { Suit = x; Number = y }))(pchar(')')) <!> "term"

let morelist = pright (pchar(',')) pcard
let deckinner = pseq (pcard) (pmany0 morelist) (fun (x, y) -> (x:: y))
let pdeckcards = pbetween(pchar('['))(deckinner)(pchar(']'))

let realdeckinner = pleft (strng) (pstr(", "))
let pdeck =pbetween(pchar('{'))(pseq (realdeckinner) (pdeckcards) (fun (x, y) -> { Name = x; Cards = y }))(pchar('}')) <!> "deck"

let playerinner = pseq(pleft(num)(pchar' '))(pdeck) (fun (x, y) -> { IdNum = x; Deck = y }) <!> "innterplayer"
let pplayer = pright(pchar ("p "))(playerinner) <!> "player"

let grammar = pleft pplayer peof <!> "grammar"

// let evalTerm (term: Term) : Term = 
//    if term.exp = 0 then 
//       { coeff = 0; exp = 1 }
//    else
//       let x = (term.coeff)*((term.exp))
//       let y = (term.exp)-1
//       { coeff = x; exp = y }

// let evalPoly (poly: Polynomial) : Polynomial = 
//    poly |> List.map (fun x -> evalTerm x) |> List.sortByDescending (fun s -> s.exp) 

// let prettyPrint (poly: Polynomial) : string = 
//   let s = poly |> List.fold (fun acc x -> 
//      let first = x.coeff |> string 
//      let second = x.exp |> string
//      acc + first + "x^" + second + " + ") ""
//   s.Substring(0, s.Length - 3)


let parse(s: string) : Deck option = 
    let i = if DEBUG then
                debug s
            else
                prepare s
    match grammar i with
    | Success(ast,_) -> Some ast
    | Failure(_,_) -> None
