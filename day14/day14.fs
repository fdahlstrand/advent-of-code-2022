type Position = { X: int; Y: int }

type Path = Position list

type ScanData = Path list

type ScanCell =
    | Empty
    | Block
    | Sand

type Scan = ScanCell[,]

let position x y = { X = x; Y = y }

let blockCell (scan: Scan) x y = scan[x, y] <- Block

let horizontalLine scan p1 p2 =
    let y = p1.Y
    let sx = min p1.X p2.X
    let ex = max p1.X p2.X

    for x in sx..ex do
        blockCell scan x y

    scan

let verticalLine scan p1 p2 =
    let x = p1.X
    let sy = min p1.Y p2.Y
    let ey = max p1.Y p2.Y

    for y in sy..ey do
        blockCell scan x y

    scan


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

open System.Text.RegularExpressions

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

let sample = fromFile "./day14/sample.txt"
let input = fromFile "./day14/input.txt"

type ScanBoundary = { Min: Position; Max: Position }

let startState =
    { Min = position System.Int32.MaxValue System.Int32.MaxValue
      Max = position 0 0 }

let findBoundary state pos =
    { Min = position (min state.Min.X pos.X) (min state.Min.Y pos.Y)
      Max = position (max state.Max.X pos.X) (max state.Max.Y pos.Y) }

let mergeBoundary state boundary =
    { Min = position (min state.Min.X boundary.Min.X) (min state.Min.Y boundary.Min.Y)
      Max = position (max state.Max.X boundary.Max.X) (max state.Max.Y boundary.Max.Y) }

let getBoundary: ScanData -> ScanBoundary =
    List.map (List.fold findBoundary startState)
    >> List.fold mergeBoundary startState

let boundary = input |> getBoundary

boundary |> printfn "%A"

let scan = Array2D.create (boundary.Max.X + 2) (boundary.Max.Y + 2) Empty

input
|> List.map (fun s -> List.pairwise s |> List.map (fun (p1, p2) -> line scan p1 p2) |> ignore)
|> ignore

printScan scan (boundary.Min.X - 1)

type MoveStatus =
    | Move of Position
    | Blocked of Position
    | FreeFall

let step (scan: Scan) boundary pos =
    let move =
        match (pos.X - 1, pos.Y + 1), (pos.X, pos.Y + 1), (pos.X + 1, pos.Y + 1) with
        | _, (x, y), _ when scan[x, y] = Empty -> Move(position x y)
        | (x, y), _, _ when scan[x, y] = Empty -> Move(position x y)
        | _, _, (x, y) when scan[x, y] = Empty -> Move(position x y)
        | _ -> Blocked pos

    match move with
    | Move p when p.Y >= boundary.Max.Y -> FreeFall
    | _ -> move

let sandStart = { X = 500; Y = 0 }

type Simulation =
    { Scan: Scan
      Boundary: ScanBoundary
      Pos: Position }

let move state =
    match step state.Scan state.Boundary state.Pos with
    | Move pos -> Some(Move pos, { state with Pos = pos })
    | Blocked pos ->
        state.Scan[pos.X, pos.Y] <- Sand
        None
    | FreeFall -> None
    
    
let rec movex state =
    match step state.Scan state.Boundary state.Pos with
    | Move pos -> movex { state with Pos = pos }
    | Blocked pos ->
        state.Scan[pos.X, pos.Y] <- Sand
        Blocked pos
    | FreeFall -> FreeFall


let start = { Scan = scan;  Pos = sandStart;  Boundary = boundary }
 
let mutable count = 0
while (movex start) <> FreeFall do
    count <- count + 1
    
printScan start.Scan (start.Boundary.Min.X - 1)
printfn ""
printfn $"%d{count} units of sand come to rest"