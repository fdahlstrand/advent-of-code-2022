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

type Chart =
    { Map: HeightMap
      EndMarker: Position
      StartMarker: Position
      PossibleStartMarkers: Position list }

let chToHeight ch = int ch - int 'a' + 1

let height (map: HeightMap) (p: Position) =
    match map[p.X, p.Y] with
    | Start -> chToHeight 'a'
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

let heightFromData (data: char [] []) (x: int) (y: int) : Marker =
    match data[y].[x] with
    | 'S' -> Start
    | 'E' -> End
    | ch -> Height(chToHeight ch)

let analyzeHeightData chart (x, y, h) =
    match h with
    | Start -> { chart with StartMarker = { X = x; Y = y } }
    | End -> { chart with EndMarker = { X = x; Y = y } }
    | Height h when h = chToHeight 'a' ->
        { chart with PossibleStartMarkers = { X = x; Y = y } :: chart.PossibleStartMarkers }
    | _ -> chart

let analyzeChart chart =
    let state =
        { Map = chart.Map
          Distance = Array2D.init (Array2D.length1 chart.Map) (Array2D.length2 chart.Map) (fun _ _ -> None)
          Counter = 0 }

    let distance = (state, chart.EndMarker) ||> walk |> (fun s -> s.Distance)

    distance[chart.StartMarker.X, chart.StartMarker.Y]
    |> Option.iter (printfn "The shortest path from S to the E is %d steps")

    chart.PossibleStartMarkers
    |> List.map (fun p -> distance[p.X, p.Y])
    |> List.choose id
    |> List.min
    |> printfn "The shortest possible path from a low point is %d steps"

let fromStrings (strings: string seq) =
    let data = strings |> Seq.map Array.ofSeq |> Array.ofSeq

    let map =
        Array2D.init (Array.length data[0]) (Array.length data) (heightFromData data)

    ({ Map = map
       EndMarker = { X = -99; Y = -99 }
       StartMarker = { X = -99; Y = -99 }
       PossibleStartMarkers = List.Empty },
     seq {
         for x in 0 .. Array2D.length1 map - 1 do
             for y in 0 .. Array2D.length2 map - 1 do
                 yield (x, y, map[x, y])
     })
    ||> Seq.fold analyzeHeightData

let fromFile = System.IO.File.ReadAllLines >> fromStrings

let sampleMap = fromFile "./day12/sample.txt"
let inputMap = fromFile "./day12/input.txt"

printfn "==== SAMPLE MAP ===="
analyzeChart sampleMap
printfn "\n==== INPUT MAP ===="
analyzeChart inputMap