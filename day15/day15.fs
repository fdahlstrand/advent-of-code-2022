type Coordinate = { X: int; Y: int }

type Range = { Start: int; End: int }

type Sensor = { Position: Coordinate; Distance: int }
type Beacon = { Position: Coordinate }

let coord x y = { X = x; Y = y }

let dist p q = (abs (p.X - q.X)) + (abs (p.Y - q.Y))
let yDist p y = dist p (coord p.X y)

let range s e = { Start = s; End = e }

module Range =
    let length r = max (r.End - r.Start + 1) 0

    let subtract r1 r2 =
        match (length r1, length r2) with
        | 0, _ -> [ r1 ]
        | _, 0 -> [ r1 ]
        | _ ->
            [ { Start = r1.Start; End = r2.Start - 1 }
              { Start = r2.End + 1; End = r1.End } ]
            |> List.filter (fun r -> (length r) > 0)


    let isOverlapping r1 r2 =
        match (length r1, length r2) with
        | 0, 0 -> false
        | 0, _ -> false
        | _, 0 -> false
        | _ ->
            match r1.Start, r1.End, r2.Start, r2.End with
            | s1, e1, s2, _ when s1 <= s2 && s2 <= e1 -> true
            | s1, e1, _, e2 when s1 <= e2 && e2 <= e1 -> true
            | s1, _, s2, e2 when s2 <= s1 && s1 <= e2 -> true
            | _, e1, s2, e2 when s2 <= e1 && e1 <= e2 -> true
            | _ -> false


    let intersect r1 r2 =
        { Start = max r1.Start r2.Start
          End = min r1.End r2.End }

    let union r1 r2 =
        match isOverlapping r1 r2 with
        | false -> [ r1; r2 ]
        | true ->
            [ { Start = min r1.Start r2.Start
                End = max r1.End r2.End } ]



let sensors =
    [ { Sensor.Position = coord 8 7
        Distance = dist (coord 8 7) (coord 2 10) } ]

let beacons = [ { Beacon.Position = coord 2 10 } ]

sensors
|> List.map (fun s ->
    match yDist s.Position 10 with
    | d when d <= s.Distance -> Some(s, d)
    | _ -> None)
|> List.choose id
|> List.map (fun (s, d) -> range (s.Position.X - (s.Distance - d)) (s.Position.X + (s.Distance - d)))
|> printfn "%A"

let d = dist (coord 8 7) (coord 2 10)
let dy = yDist (coord 8 7) 10
let r = (range (8 - (d - dy)) (8 + (d - dy)))
let r2 = Range.subtract r (range 2 2)
printfn "%d" d
printfn "%d" dy
printfn "%A" r
printfn "%A" (r2 |> List.map Range.length)



open System.Text.RegularExpressions

let (|Reading|_|) input =
    let m =
        Regex.Match(
            input,
            @"^Sensor\sat\sx=(?<sx>-?[0-9]+),\sy=(?<sy>-?[0-9]+):\sclosest\sbeacon\sis\sat\sx=(?<bx>-?[0-9]+),\sy=(?<by>-?[0-9]+)$"
        )

    if m.Success then
        let toInt = System.Int32.Parse

        Some(
            (toInt m.Groups["sx"].Value, toInt m.Groups["sy"].Value),
            (toInt m.Groups["bx"].Value, toInt m.Groups["by"].Value)
        )
    else
        None


let parseReading str =
    match str with
    | Reading((sx, sy), (bx, by)) ->
        ({ Sensor.Position = coord sx sy
           Distance = dist (coord sx sy) (coord bx by) },
         { Beacon.Position = coord bx by })
    | _ -> failwith "Not a valid sensor reading"


type SensorData =
    { Sensors: Sensor list
      Beacons: Set<Beacon> }

let fromStrings (ss: string seq) =
    ss
    |> Seq.map parseReading
    |> Seq.fold
        (fun readings (s, b) ->
            { Sensors = s :: readings.Sensors
              Beacons = Set.add b readings.Beacons })
        { Sensors = List.Empty
          Beacons = Set.empty }

let fromFile = System.IO.File.ReadAllLines >> fromStrings



printfn "==============="

// let sample = fromFile "./day15/sample.txt"
// let yTarget = 10
let input = fromFile "./day15/input.txt"
let yTarget = 2000000

let bs = input.Beacons |> Set.filter (fun b -> b.Position.Y = yTarget) |> Set.map (fun b -> b.Position)
// let sensors = sample.Sensors |> List.map (fun s -> s.Position) |> Set.ofList |> Set.filter (fun b -> b.Position.Y = 10) |> Set.map (fun b -> b.Position)
let sets =
    input.Sensors
    |> List.map (fun s ->
        match yDist s.Position yTarget with
        | d when d <= s.Distance -> Some(s, d)
        | _ -> None)
    |> List.choose id
    |> List.map (fun (s, d) -> [(s.Position.X - (s.Distance - d))..(s.Position.X + (s.Distance - d))])
    |> List.map Set.ofList
    |> List.map (Set.map (fun x -> coord x yTarget))
    |> List.map (fun s -> Set.difference s bs)
    //|> List.iter (printfn "%A")
    |> Set.unionMany
    // |> Set.count
    
    
    
// sets |> Set.iter (fun c -> printfn $"(%d{c.X},%d{c.Y})")
sets |> Set.count |> printfn "%d"
    
    