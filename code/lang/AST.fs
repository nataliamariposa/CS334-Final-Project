module AST

type Suit = Heart | Diamond | Club | Spade

type Card = {
    Suit: Suit
    Number: int
}

type Deck = {
    Name: string
    Cards: Card list
}

type Player = {
    IdNum: int
    Deck: Deck
}

type Expr =
| Num of int
| Str of string
| Bool of bool
| CardDef of Card
| DeckDef of Deck
| PlayerDef of Player
| Seq of Expr List
| Nop //no operation
| Sum of string
| Var of string
| Equal of Expr * Expr
| Greater of Expr * Expr
| Lesser of Expr * Expr
| Shuffle of string
| Assign of Expr * Expr
| Firstcard of string
| Showcard of int * string
| Takecard of string * string
| Hascards of string
| Winner of string
| While of Expr * Expr
| If of Expr * Expr
| Prompt of string
