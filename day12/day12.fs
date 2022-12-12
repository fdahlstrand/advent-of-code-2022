type Position = { X: int; Y: int }

type Marker =
    | Height of int
    | Start
    | End

type HeightMap = Marker [,]

type Path = Position list

type TravelState =
    { Map: HeightMap
      Visited: Set<Position> }

type WalkState =
    { Map: HeightMap
      Distance: int option [,]
      Counter: int }

let chToHeight ch = int ch - int 'a' + 1

let height (map: HeightMap) (p: Position) =
    match map[p.X, p.Y] with
    | Start -> 0
    | End -> chToHeight 'z'
    | Height h -> h

let printMap (map: int option [,]) =
    for y in 0 .. (Array2D.length2 map) - 1 do
        for x in 0 .. (Array2D.length1 map) - 1 do
            match map[x, y] with
            | Some d -> printf $"[%3d{d}] "
            | None -> printf $"[   ] "

        printfn ""

    printfn ""

let isStart (map: HeightMap) (pos: Position) =
    match map[pos.X, pos.Y] with
    | Start -> true
    | _ -> false

let isValidMove (state: WalkState) (src: Position) (dst: Position) =
    let currentHeight = height state.Map src

    match (dst.X, dst.Y) with
    | x, y when x < 0 || y < 0 -> false
    | x, _ when x >= Array2D.length1 state.Map -> false
    | _, y when y >= Array2D.length2 state.Map -> false
    | x, y ->
        match state.Distance[x, y] with
        | Some d when d <= state.Counter -> false
        | Some _
        | None ->
            match state.Map[x, y] with
            | Start when currentHeight > 1 -> false
            | Start -> true
            | End -> false
            | Height h when (currentHeight - h) > 1 -> false
            | Height _ -> true


let generateMoves (state: WalkState) (pos: Position) =
    [ { pos with X = pos.X + 1 }
      { pos with X = pos.X - 1 }
      { pos with Y = pos.Y + 1 }
      { pos with Y = pos.Y - 1 } ]
    |> List.filter (isValidMove state pos)

let rec walk (state: WalkState) (pos: Position) =
    match state.Distance[pos.X, pos.Y] with
    | Some d when d <= state.Counter -> state
    | Some _
    | None ->
        state.Distance[ pos.X, pos.Y ] <- Some state.Counter

        let _ =
            generateMoves state pos
            |> List.map (walk { state with Counter = state.Counter + 1 })

        state

let fromStrings (strings: string seq) : HeightMap * Position * Position*Position list =
    let data = strings |> Seq.map Array.ofSeq |> Array.ofSeq
    let mutable goal = { X = 0; Y = 0 }
    let mutable start = { X = 0; Y = 0 }
    let mutable starts: Position list = List.Empty

    let map =
        Array2D.init (Array.length data[0]) (Array.length data) (fun x y ->
            match data[y].[x] with
            | 'S' ->
                start <- { X = x; Y = y }
                starts <- { X = x; Y = y }::starts
                Start
            | 'E' ->
                goal <- { X = x; Y = y }
                End
            | 'a' ->
                starts <- { X = x; Y = y }::starts
                Height(chToHeight 'a')
            | ch -> Height(chToHeight ch))
            

    (map, start, goal, starts)

let fromFile = System.IO.File.ReadAllLines >> fromStrings

let sampleMap = fromFile "./day12/sample.txt"
let inputMap = fromFile "./day12/input.txt"

let m, s, e, ss = inputMap

ss |> List.length |> printfn "%d"

let state =
    { Map = m
      Distance = Array2D.init (Array2D.length1 m) (Array2D.length2 m) (fun _ _ -> None)
      Counter = 0 }

let distance = (state, e) ||> walk |> (fun s -> s.Distance)

distance[s.X, s.Y] |> Option.iter (printfn "The shortest path from S to the E is %d steps")

ss |> List.map (fun p -> distance[p.X,p.Y]) |> List.choose id |> List.min |> printfn "%d"