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
type Strategem = Move * Outcome

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

let determineRound ((move, outcome): Strategem) : Round =
    let response =
        match outcome with
        | Draw -> move
        | Win ->
            match move with
            | Rock -> Paper
            | Paper -> Scissor
            | Scissor -> Rock
        | Lose ->
            match move with
            | Rock -> Scissor
            | Paper -> Rock
            | Scissor -> Paper

    (move, response)

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

let charToExpectedOutcome ch =
    match ch with
    | 'X' -> Lose
    | 'Y' -> Draw
    | 'Z' -> Win
    | _ -> failwith (sprintf "Unsupported outcome '%c'" ch)


let stringToRound (s: string) : Round = (charToShape s[0], charToShape s[2])

let stringToStrategem (s: string) : Strategem =
    (charToShape s[0], charToExpectedOutcome s[2])


File.ReadLines("./day2/input.txt")
|> Seq.map stringToRound
|> Seq.map play
|> Seq.map score
|> Seq.sum
|> printfn "Total score, misunderstood strategy: %d"

File.ReadLines("./day2/input.txt")
|> Seq.map stringToStrategem
|> Seq.map determineRound
|> Seq.map play
|> Seq.map score
|> Seq.sum
|> printfn "Total score, correct strategy: %d"
