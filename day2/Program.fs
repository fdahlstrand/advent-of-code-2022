open System.IO

type Shape =
    | Rock
    | Paper
    | Scissor

type Outcome =
    | Win
    | Lose
    | Draw

type Move = Shape
type Response = Shape
type Round = Move * Response
type Game = Shape * Outcome

let outcome (round: Round) =
    match round with
    | (a, b) when a = b -> Draw
    | (Rock, Paper) -> Win
    | (Paper, Scissor) -> Win
    | (Scissor, Rock) -> Win
    | _ -> Lose

let shapeScore shape =
    match shape with
    | Rock -> 1
    | Paper -> 2
    | Scissor -> 3

let gameScore outcome =
    match outcome with
    | Win -> 6
    | Draw -> 3
    | Lose -> 0

let score (shape, outcome) =
    (gameScore outcome) + (shapeScore shape)

let play (round: Round) : Game =
    let (_, shape) = round

    (shape, outcome round)

let charToShape ch =
    match ch with
    | 'A'
    | 'X' -> Rock
    | 'B'
    | 'Y' -> Paper
    | 'C'
    | 'Z' -> Scissor
    | _ -> failwith (sprintf "Unsupported move '%c'" ch)

let stringToRound (s: string) : Round = (charToShape s[0], charToShape s[2])

File.ReadLines("./day2/input.txt")
|> Seq.map stringToRound
|> Seq.map play
|> Seq.map score
|> Seq.sum
|> printfn "%A"