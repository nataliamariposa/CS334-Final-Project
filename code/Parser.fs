module Parser 

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

let emptyCardList : Card list = []
let reserved = [ "sum"; "Equal"; "shuffle"; ]

(* PARSER *)
let pexpr,pexprImpl = recparser()
let pad p = pbetween pws0 p pws0
let pparens = pbetween (pad (pchar '(')) pexpr (pad (pchar ')'))

//parses symbols 
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

//**
//parses strings
let Jletter = 
    pletter <|> pdigit <|> pchar ' ' <|> psymbol
let strnginsde = pmany0 Jletter |>> stringify |>> Str  <!> "strng inside"
//parses strings that are between ""
// let strng = pbetween (pchar '"') (pmany0(pad(strnginsde))) (pchar '"') <!> "strng outside"
//**
let pnotquot: Parser<char> = psat (fun c -> c <> '"') <!> "pnotquot"
let strng: Parser<Expr> = pbetween (pchar '"') (pmany1 pnotquot) (pchar '"')
                            |>> (fun cs ->
                                   let s = stringify cs
                                   let e = Str(s)
                                   e
                                ) <!> "pstring"


let pvarchar: Parser<char> = pletter <|> pdigit <!> "pvarchar"
let pvar: Parser<Expr> = pseq pletter (pmany0 pvarchar |>> stringify)
                           (fun (c: char, s: string) -> (string c) + s)
                           |>> (fun v ->
                                 if List.contains v reserved then
                                     failwith ("'" + v + "' is a reserved word.")
                                 else
                                     Var v
                               ) <!> "pvar"

let passign = pseq (pleft (pad pvar) (pad (pstr ":="))) (pad pexpr) Assign <!> "passign"

let pbooltrue = pfresult(pstr "true") (Bool(true))
let pboolfalse = pfresult(pad(pstr "false")) (Bool(false))
let pbooliscool = pbooltrue <|> pboolfalse <!> "pbool"

let pprompt = pright(pad(pstr "prompt")) (pad(strng)) |>> (fun s ->
        match s with
        | Str v -> Prompt v
        | _ -> failwith "Expected a string after 'prompt'.")<!> "pprompt"
//**
//parses positive numbers
let posnumber =
    pmany1 pdigit
    |>> stringify
    |>> int
    <!> "num"

//parses negative numbers with a '-'
let negnumber =
    pseq(pmany0 (pchar('-')))(posnumber)(fun (x, y:int) -> -y)

//parses both numbers
let num = posnumber <|> negnumber |>> Num
//
//**

let pshuffle =
    pright (pad (pstr "shuffle")) (pad strng)
    |>> (fun expr ->
        match expr with
        | Str s ->
            if List.contains s reserved then
                failwith $"'{s}' is a reserved word."
            else
                Shuffle s
        | _ -> failwith "Expected a string expression after 'shuffle'.")
    <!> "pshuffle"

let pfirstcard = 
    pright (pad (pstr "firstcard")) (pad strng)
    |>> (fun expr ->
        match expr with
        | Str s ->
            if List.contains s reserved then
                failwith $"'{s}' is a reserved word."
            else
                Firstcard s
        | _ -> failwith "Expected a string expression after 'firstcard'.")
    <!> "firstcard"
let pshowcard = 
    pseq(pright (pad (pstr "showcard")) (pad posnumber)) (pad strng) (fun (x,y) -> (x,y))
    |>> (fun (num, expr) ->
        match expr with
        | Str s ->
            if List.contains s reserved then
                failwith $"'{s}' is a reserved word."
            else
                Showcard (num, s)
        | _ -> failwith "Expected a string expression after 'showcard [Int]'.")
    <!> "showcard"


let ptakecardinner = pright (pad(pstr "takecard")) (pad strng)
let ptakecard = pseq (ptakecardinner) (pad strng) (fun (x,y) -> (x,y)) |>> (fun (name1, name2) ->
        match name1, name2 with
        | Str n1, Str n2 ->
            if (List.contains n1 reserved) || (List.contains n2 reserved) then
                failwith $"'{n1}' or '{n2}' is a reserved word."
            else
                Takecard(n1, n2)
        | _ -> failwith "Expected a string expression after 'showcard [Int]'.")
    //<!> "takecard"
let phascards = pright (pstr "hascards ") strng |>> (fun (name) -> 
                                      match name with
                                      |Str s -> Hascards s
                                      |_ -> Hascards "") <!> "hascards"

let pwinner = pright (pstr "winner ") strng |>> (fun (name) -> 
                                      match name with
                                      |Str s -> Winner s
                                      |_ -> Winner "") <!> "winner"

let pwhile =
    pright (pad (pstr "while")) (
        pseq (pbetween (pad(pchar '(')) (pad (pexpr)) (pad(pchar ')'))) pparens (fun (cond, body) -> While (cond, body))
    ) <!> "while"
let pifinner = pbetween (pad(pchar '(')) (pad (pexpr)) (pad(pchar ')')) <!> "pifinner"
let pif =
    pright (pad(pstr "if")) (
        pseq (pad(pifinner)) (pad(pparens)) (fun (cond, body) -> If (cond, body))
    ) <!> "if"
//**
//parses each suit 
let pheart = pfresult(pstr("heart")) (Heart)
let pclub = pfresult(pstr("club")) (Club) 
let pdiamond = pfresult(pstr("diamond")) (Diamond)  
let pspade = pfresult(pstr("spade")) (Spade)

//combines those suit parsers into one parser 
let suit = pheart <|> pclub <|> pspade <|> pdiamond
//
//**



//**
//parses a card and innercard, allowing for a card to have a suit and a num in the format suit,num
let Cardinner = pleft (suit) (pstr(", "))
let pcard = pbetween(pchar('('))(pseq (Cardinner) (num) (fun (x, y) -> 
                                      match y with
                                      |Num n -> { Suit = x; Number = n }
                                      |_ -> { Suit = x; Number = 0 }
                                      ))(pchar(')')) <!> "term"
//
//**



// let morelist = pright (pchar(',')) json
// let listinside = pseq (json) (pmany0 morelist) (fun (x, y) -> (x:: y))
// let list : Parser<JSON> = pbetween (pchar '[') (listinside) (pchar ']') |>> JList <!> "list outside"

//**
//parses a deck of cards
let morelist = pright (pchar(',')) pcard
let deckinner = pseq (pcard) (pmany0 morelist) (fun (x, y) -> (x:: y))
let empty = pseq (pad(pchar('['))) (pad(pchar(']'))) (fun (x, y) -> emptyCardList) <!> "empty"
let pdeckcard = pbetween(pchar('['))(deckinner)(pchar(']'))
let pdeckcards = pdeckcard <|> empty

let realdeckinner = pleft (strng) (pstr(", "))
let pdeck =pbetween(pchar('{'))(pseq (realdeckinner) (pdeckcards) (fun (x, y) -> 
                                      match x with
                                      |Str s -> { Name = s; Cards = y }
                                      |_ -> { Name = "p"; Cards = y }))(pchar('}')) <!> "deck"
//
//**

//**
//parses player that has a number and a deck
let playerinner = pseq(pleft(num)(pchar (' ')))(pdeck) (fun (x, y) -> 
                                      match x with
                                      |Num n -> { IdNum = n; Deck = y }
                                      |_ -> { IdNum = 0; Deck = y }) <!> "innterplayer"
let pplayer = pright(pstr ("player "))(playerinner) <!> "player"
//
//**

let acceptablecomparisons = psum <|> num <|> pvar <|> strng <!> "DO YoU"
let pequalinner = pleft (acceptablecomparisons) (pad(pchar '='))
let pequal = pseq (pequalinner) (acceptablecomparisons) (fun (x,y) -> (x,y)) |>> Equal <!> "Equal"

let pgreaterinner = pleft (pad(acceptablecomparisons)) (pad(pchar '>'))
let pgreater = pseq (pgreaterinner) (pad(acceptablecomparisons)) (fun (x,y) -> (x,y)) |>> Greater <!> "Greater"

let plesserinner = pleft (acceptablecomparisons) (pad(pchar '<'))
let plesser = pseq (plesserinner) (acceptablecomparisons) (fun (x,y) -> (x,y)) |>> Lesser <!> "Lesser"


let psum = 
    pright (pstr "sum ") strng |>> (fun (name) -> 
                                      match name with
                                      |Str s -> Sum s
                                      |_ -> Sum "") <!> "sum for real"



let pplayerExpr = pplayer |>> PlayerDef
let pdeckExpr = pdeck |>> DeckDef

pexprImpl := pplayerExpr <|> pdeckExpr <|> pwinner <|> pif <|> pprompt <|> phascards <|> pwhile <|> ptakecard <|> pshuffle <|> pbooliscool <|> passign <|> pequal <|> pgreater <|> plesser <|> psum  <|> pfirstcard <|> pshowcard <|> pvar <|> num <|> strng

let pexprs = pmany1 (pleft (pad pexpr) pws0) |>> Seq
//**
//grammar parser
let grammar = pleft pexprs peof <!> "grammar"

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


//**
//parse function
let parse(s: string) : Expr option = 
    let i = if DEBUG then
                debug s
            else
                prepare s
    match grammar i with
    | Success(ast,_) -> Some ast
    | Failure(_,_) -> None
//
//**
