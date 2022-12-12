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

let EndHeight = int ('z') - int ('a') + 1

let _isValidMove (state: TravelState) (src: Position) (dst: Position) =
    let currentHeight =
        match state.Map[src.X, src.Y] with
        | Height h -> h
        | _ -> 0

    match dst.X, dst.Y with
    | (x, y) when x < 0 || y < 0 -> false
    | (x, _) when x >= Array2D.length1 state.Map -> false
    | (_, y) when y >= Array2D.length2 state.Map -> false
    | (x, y) when Set.contains { X = x; Y = y } state.Visited -> false
    | (x, y) ->
        match state.Map[x, y] with
        | Start -> false
        | End when (EndHeight - currentHeight) > 1 -> false
        | End -> true
        | Height h when (h - currentHeight) > 1 -> false
        | Height _ -> true

let generateValidMoves (state: TravelState) (src: Position) =
    [ { src with X = src.X + 1 }
      { src with X = src.X - 1 }
      { src with Y = src.Y + 1 }
      { src with Y = src.Y - 1 } ]
    |> List.filter (_isValidMove state src)

let isEnd (state: TravelState) (pos: Position) =
    match state.Map[pos.X, pos.Y] with
    | End -> true
    | _ -> false

let rec visit (state: TravelState) (pos: Position) : TravelState * Path list =
    if isEnd state pos then
        (state, [ [ pos ] ])
    else
        let nextState = { state with Visited = Set.add pos state.Visited }

        let paths =
            generateValidMoves state pos
            |> List.map (visit nextState)
            |> List.map snd
            |> List.concat
            |> List.map (fun p -> pos :: p)

        (state, paths)

type WalkState =
    { Map: HeightMap
      Distance: int option [,]
      Counter: int }

let isStart (map: HeightMap) (pos: Position) =
    match (map[pos.X, pos.Y]) with
    | Start -> true
    | _ -> false

let isValidMove (state: WalkState) (src: Position) (dst: Position) =
    let currentHeight =
        match state.Map[src.X, src.Y] with
        | Height h -> h
        | End -> EndHeight
        | Start -> 0

    match (dst.X, dst.Y) with
    | (x, y) when x < 0 || y < 0 -> false
    | (x, _) when x >= Array2D.length1 state.Map -> false
    | (_, y) when y >= Array2D.length2 state.Map -> false
    | (x, y) ->
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

let printMap (map: int option [,]) =
    for y in 0 .. (Array2D.length2 map) - 1 do
        for x in 0 .. (Array2D.length1 map) - 1 do
            match map[x, y] with
            | Some d -> printf $"[%3d{d}] "
            | None -> printf $"[   ] "

        printfn ""

    printfn ""


let rec walk (state: WalkState) (pos: Position) =
    match state.Distance[pos.X, pos.Y] with
    | Some d when d <= state.Counter -> state
    | Some _
    | None ->
        //printMap state.Distance
        state.Distance[ pos.X, pos.Y ] <- Some state.Counter

        if isStart state.Map pos then
            state
        else
            let moves =
                generateMoves state pos
                |> List.map (walk { state with Counter = state.Counter + 1 })

            state

let fromStrings (strings: string seq) : HeightMap * Position * Position =
    let data = strings |> Seq.map Array.ofSeq |> Array.ofSeq
    let mutable goal = { X = 0; Y = 0 }
    let mutable start = { X = 0; Y = 0 }

    let map =
        Array2D.init (Array.length data.[0]) (Array.length data) (fun x y ->
            match data.[y].[x] with
            | 'S' ->
                start <- { X = x; Y = y }
                Start
            | 'E' ->
                goal <- { X = x; Y = y }
                End
            | ch -> Height(int (ch) - int ('a') + 1))

    (map, start, goal)


let fromFile = System.IO.File.ReadAllLines >> fromStrings

let height (map: HeightMap) (p: Position) =
    match map[p.X, p.Y] with
    | Start -> 0
    | End -> EndHeight
    | Height h -> h

let heightDiff (map: HeightMap) (p1: Position) (p2: Position) =
    let h1 = height map p1
    let h2 = height map p2

    h2 - h1

let printJourney (map: HeightMap) (path: Path) =
    path
    |> List.pairwise
    |> List.map (fun (p1, p2) -> (p1, p2), heightDiff map p1 p2)
    |> List.mapi (fun i ((p1, p2), h) -> sprintf $"[%d{i}] (%d{p1.X},%d{p1.Y}) -> (%d{p2.X},%d{p2.Y}):%d{h}")
    |> List.iter (printfn "%s")

let sampleMap = fromFile "./day12/sample.txt"
let inputMap = fromFile "./day12/input.txt"


let m, s, e = inputMap

let state =
    { Map = m
      Distance = Array2D.init (Array2D.length1 m) (Array2D.length2 m) (fun _ _ -> None)
      Counter = 0 }

let distance = (state, e) ||> walk |> (fun s -> s.Distance)

printfn "%A" distance[s.X, s.Y]
