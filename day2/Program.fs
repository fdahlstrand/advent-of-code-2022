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

let score (round: Round) =
    let (_, shape) = round
    (outcome >> gameScore) round + shapeScore shape

let firstPolicy (instruction: string) : Round =
    let charToShape ch =
        match ch with
        | 'A'
        | 'X' -> Rock
        | 'B'
        | 'Y' -> Paper
        | 'C'
        | 'Z' -> Scissor
        | _ -> failwith (sprintf "Unsupported shape '%c'" ch)

    (charToShape instruction[0], charToShape instruction[2])

let secondPolicy (instruction: string) : Round =
    let move =
        match instruction[0] with
        | 'A' -> Rock
        | 'B' -> Paper
        | 'C' -> Scissor
        | _ -> failwith (sprintf "Unsupported shape '%c'" instruction[0])

    let outcome =
        match instruction[2] with
        | 'X' -> Lose
        | 'Y' -> Draw
        | 'Z' -> Win
        | _ -> failwith (sprintf "Unsupported outcome '%c'" instruction[2])

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

let instantiatePlaybook (policy: string -> Round) (instructions: string seq) = instructions |> Seq.map policy

let calculateScore (playbook: Round seq) : int = playbook |> Seq.map score |> Seq.sum


File.ReadLines("./day2/input.txt")
|> instantiatePlaybook firstPolicy
|> calculateScore
|> printfn "Total score, misunderstood strategy: %d"

File.ReadLines("./day2/input.txt")
|> instantiatePlaybook secondPolicy
|> calculateScore
|> printfn "Total score, correct strategy: %d"
