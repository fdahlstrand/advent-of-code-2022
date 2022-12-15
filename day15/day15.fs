open System

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

let sensorCoverage data target =
    let bs =
        data.Beacons
        |> Set.filter (fun b -> b.Position.Y = target)
        |> Set.map (fun b -> b.Position)

    data.Sensors
    |> List.map (fun s ->
        match yDist s.Position target with
        | d when d <= s.Distance -> Some(s, d)
        | _ -> None)
    |> List.choose id
    |> List.map (fun (s, d) -> [ (s.Position.X - (s.Distance - d)) .. (s.Position.X + (s.Distance - d)) ])
    |> List.map Set.ofList
    |> List.map (Set.map (fun x -> coord x target))
    |> List.map (fun s -> Set.difference s bs)
    |> Set.unionMany

let reachable y (sensor: Sensor) =
    match yDist sensor.Position y with
    | d when d <= sensor.Distance -> true
    | _ -> false

let coverage y sensor =
    let dx = sensor.Distance - (yDist sensor.Position y)
    range (sensor.Position.X - dx) (sensor.Position.X + dx)


let rec cullOptions o r =
    match o.Start, o.End, r.Start, r.End with
    | a, b, _, _ when b < a -> []
    | _, _, s, e when e < s -> [ o ]
    | a, b, s, e when s < a && e > b -> []
    | a, _, s, e when s < a && e < a -> [ o ]
    | a, b, s, e when s < a && e <= b -> [ range (e + 1) b ]
    | a, b, s, e when s >= a && s <= b && e >= b -> [ range a (s - 1) ]
    | a, b, s, e when s >= a && s <= b && e <= b -> [ range a (s - 1); range (e + 1) b ]
    | a, b, s, e when s > b && e > b -> [ o ]
    | _ -> failwith "I didn't think of that..."
    |> List.filter (fun r -> Range.length r > 0)

let removeOptions options range =
    options |> List.collect (fun o -> cullOptions o range)

let removeOptionsFromLine sensors range line =
    line,
    sensors
    |> List.filter (reachable line)
    |> List.map (coverage line)
    |> List.fold removeOptions [ range ]

let frequency x y = int64 (x) * 4_000_000L + int64 (y)

let findTuningFrequency data range =
    [ range.Start .. range.End ]
    |> List.map (removeOptionsFromLine data.Sensors range)
    |> List.filter (fun (_, r) -> r.Length > 0)
    |> List.collect (fun (y, l) ->
        l
        |> List.collect (fun r -> [ r.Start .. r.End ] |> List.map (fun x -> frequency x y)))

let sample = fromFile "./day15/sample.txt"
let input = fromFile "./day15/input.txt"

sensorCoverage sample 10 |> Set.count |> printfn "Row 10 contains %d non-beacon positions"
sensorCoverage input 2_000_000 |> Set.count |> printfn "Row 2.000.000 contains %d non-beacon positions"

findTuningFrequency sample (range 0 20) |> printfn "Possible tuning frequencies: %A"
findTuningFrequency input (range 0 4_000_000) |> printfn "Possible tuning frequencies: %A"