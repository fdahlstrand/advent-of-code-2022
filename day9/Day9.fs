open System.Text.RegularExpressions

type Position = { X: int; Y: int }

type Direction =
    | Up
    | Down
    | Left
    | Right

let diff pos1 pos2 = (pos1.X - pos2.X, pos1.Y - pos2.Y)

let moveHeadStep dir pos =
    match dir with
    | Up -> { pos with Y = pos.Y + 1 }
    | Down -> { pos with Y = pos.Y - 1 }
    | Left -> { pos with X = pos.X - 1 }
    | Right -> { pos with X = pos.X + 1 }

let moveTailStep headPos tailPos =
    let (dx, dy) = diff tailPos headPos

    match (dx, dy) with
    | (-1, -1) -> tailPos
    | (0, -1) -> tailPos
    | (1, -1) -> tailPos
    | (-1, 0) -> tailPos
    | (0, 0) -> tailPos
    | (1, 0) -> tailPos
    | (-1, 1) -> tailPos
    | (0, 1) -> tailPos
    | (1, 1) -> tailPos
    | (0, -2) -> { tailPos with Y = tailPos.Y + 1 }
    | (0, 2) -> { tailPos with Y = tailPos.Y - 1 }
    | (-2, 0) -> { tailPos with X = tailPos.X + 1 }
    | (2, 0) -> { tailPos with X = tailPos.X - 1 }
    | (dx, dy) ->
        match (dx, dy) with
        | (-2, -1) -> { X = tailPos.X + 1; Y = tailPos.Y + 1 }
        | (-1, -2) -> { X = tailPos.X + 1; Y = tailPos.Y + 1 }
        | (2, -1) -> { X = tailPos.X - 1; Y = tailPos.Y + 1 }
        | (1, -2) -> { X = tailPos.X - 1; Y = tailPos.Y + 1 }
        | (2, 1) -> { X = tailPos.X - 1; Y = tailPos.Y - 1 }
        | (1, 2) -> { X = tailPos.X - 1; Y = tailPos.Y - 1 }
        | (-2, 1) -> { X = tailPos.X + 1; Y = tailPos.Y - 1 }
        | (-1, 2) -> { X = tailPos.X + 1; Y = tailPos.Y - 1 }
        | (-2, -2) -> { X = tailPos.X + 1; Y = tailPos.Y + 1 }
        | (-2, 2) -> { X = tailPos.X + 1; Y = tailPos.Y - 1 }
        | (2, -2) -> { X = tailPos.X - 1; Y = tailPos.Y + 1 }
        | (2, 2) -> { X = tailPos.X - 1; Y = tailPos.Y - 1 }
        | _ -> failwith $"No rule for H:%A{headPos}, T:%A{tailPos}, d:%A{(dx, dy)}"


type State =
    { HeadPos: Position
      Tails: Position list
      TailPositions: Set<Position> }

let moveLink headPos tailPos =
    let newTailPos = moveTailStep headPos tailPos

    newTailPos, newTailPos

let move state dir =
    let headPos = moveHeadStep dir state.HeadPos
    let tails, lastTail = (headPos, state.Tails) ||> List.mapFold moveLink

    { HeadPos = headPos
      Tails = tails
      TailPositions = Set.add lastTail state.TailPositions }

let moveHead state (dir, count) =
    (state,
     seq {
         for i in 1..count do
             yield dir
     })
    ||> Seq.fold move



let parseInstruction str =
    let m = Regex.Match(str, @"^(?<dir>[RULD])\s(?<cnt>[1-9][0-9]*)$")

    if m.Success then
        let dir =
            match m.Groups["dir"].Value with
            | "R" -> Right
            | "U" -> Up
            | "L" -> Left
            | "D" -> Down
            | _ -> failwith $"Failed to parse direction %s{str}"

        dir, System.Int32.Parse(m.Groups["cnt"].Value)
    else
        failwith $"Invalid instruction %s{str}"

let instructionsFromStrings: string seq -> (Direction * int) seq =
    Seq.map parseInstruction

let instructionsFromFile =
    System.IO.File.ReadAllLines
    >> instructionsFromStrings

let inputSample1 =
    seq {
        "R 4"
        "U 4"
        "L 3"
        "D 1"
        "R 4"
        "D 1"
        "L 5"
        "R 2"
    }
    |> instructionsFromStrings

let inputSample2 =
    seq {
        "R 5"
        "U 8"
        "L 8"
        "D 3"
        "R 17"
        "D 10"
        "L 25"
        "U 20"
    }
    |> instructionsFromStrings

let inputFile = instructionsFromFile "./day9/input.txt"

let createState linkLength =
    { HeadPos = { X = 0; Y = 0 }
      Tails = List.init linkLength (fun _ -> { X = 0; Y = 0 })
      TailPositions = Set.empty }

let countTailVisits input linkLength =
    (createState linkLength, input)
    ||> Seq.fold moveHead
    |> (fun s -> s.TailPositions)
    |> Set.count

countTailVisits inputFile 1
|> printfn "For a chain with 2 links, the tail visits %d positions."

countTailVisits inputFile 9
|> printfn "For a chain with 10 links, the tail visits %d positions."
