open System.Text.RegularExpressions

type Position = { X: int; Y: int }

type Path = Position list

type ScanData = Path list

type ScanCell =
    | Empty
    | Block
    | Sand

type Scan = ScanCell[,]

type MoveStatus =
    | Move of Position
    | Rest of Position
    | FreeFall
    | Full

type SimulationMode = | Abyss | Floor

type SimulationState =
    { Scan: Scan
      Height: int
      Pos: Position
      Floor: int option }

let position x y = { X = x; Y = y }

let blockCell (scan: Scan) x y = scan[x, y] <- Block

let horizontalLine scan p1 p2 =
    let y = p1.Y
    let sx = min p1.X p2.X
    let ex = max p1.X p2.X

    for x in sx..ex do
        blockCell scan x y

let verticalLine scan p1 p2 =
    let x = p1.X
    let sy = min p1.Y p2.Y
    let ey = max p1.Y p2.Y

    for y in sy..ey do
        blockCell scan x y

let line scan p1 p2 =
    match p1, p2 with
    | a, b when a.X = b.X -> verticalLine scan a b
    | a, b when a.Y = b.Y -> horizontalLine scan a b
    | _ -> failwith "Not a horizontal or vertical line"

let printScan scan startx =
    for y in 0 .. Array2D.length2 scan - 1 do
        for x in startx .. Array2D.length1 scan - 1 do
            let ch =
                match scan[x, y] with
                | Empty -> '.'
                | Block -> '#'
                | Sand -> 'o'

            printf $"%c{ch}"

        printfn ""

module ScanData =
    let (|Path|_|) input =
        let m =
            Regex.Match(input, @"^((?<pos>(?<x>[1-9][0-9]*),(?<y>[1-9][0-9]*))(\s->\s)?)*$")

        let toInt = System.Int32.Parse

        if (m.Success) then
            Some
                [ for i in 0 .. m.Groups["pos"].Captures.Count - 1 do
                      let x = m.Groups["x"].Captures[i].Value |> toInt
                      let y = m.Groups["y"].Captures[i].Value |> toInt

                      yield position x y ]
        else
            None

    let parse str =
        match str with
        | Path p -> p
        | _ -> failwith "Not a valid path"

    let fromFile: string -> ScanData =
        System.IO.File.ReadAllLines >> Seq.map parse >> Seq.toList

let step (scan: Scan) height floor pos =
    let move =
        match (pos.X - 1, pos.Y + 1), (pos.X, pos.Y + 1), (pos.X + 1, pos.Y + 1) with
        | _, (x, y), _ when scan[x, y] = Empty -> Move(position x y)
        | (x, y), _, _ when scan[x, y] = Empty -> Move(position x y)
        | _, _, (x, y) when scan[x, y] = Empty -> Move(position x y)
        | _ -> Rest pos

    match floor with
    | None ->
        match move with
        | Move p when p.Y >= height -> FreeFall
        | _ -> move
    | Some h ->
        match move with
        | Move p when p.Y > h -> Rest p
        | Rest p when p.X = 500 && p.Y = 0 -> Full
        | _ -> move
        

let rec move state =
    match step state.Scan state.Height state.Floor state.Pos with
    | Move pos -> move { state with Pos = pos }
    | Rest pos ->
        state.Scan[pos.X, pos.Y] <- Sand
        Rest pos
    | s -> s

let updateScan scan path =
    List.pairwise path |> List.iter (fun (p1, p2) -> line scan p1 p2)

let runSimulation data mode =
    let height =
        data |> List.map (List.maxBy (fun p -> p.Y) >> (fun p -> p.Y)) |> List.max

    let scan = Array2D.create 1000 (height + 2) Empty
    let sandStart = { X = 500; Y = 0 }

    List.iter (updateScan scan) data

    let simulationState =
        { Scan = scan
          Pos = sandStart
          Height = height
          Floor = match mode with
                  | Abyss -> None
                  | Floor -> Some height }

    match mode with
    | Abyss ->
        let notFreeFall = (<>) FreeFall
        Seq.initInfinite (fun _ -> move simulationState)
        |> Seq.takeWhile notFreeFall
        |> Seq.length
    | Floor ->
        let notFull = (<>) Full
        Seq.initInfinite (fun _ -> move simulationState)
        |> Seq.takeWhile notFull
        |> Seq.length |> (+) 1

let sample = ScanData.fromFile "./day14/sample.txt"
let input = ScanData.fromFile "./day14/input.txt"

runSimulation input Abyss |> printfn "%d units of sand come to rest"
runSimulation input Floor |> printfn "%d units of sand come to rest"