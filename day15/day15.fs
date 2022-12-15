open System.Text.RegularExpressions

type Coordinate = { X: int; Y: int }

type Range = { Start: int; End: int }

type Sensor = { Position: Coordinate; Distance: int }
type Beacon = { Position: Coordinate }

type SensorData =
    { Sensors: Sensor list
      Beacons: Set<Beacon> }

let coord x y = { X = x; Y = y }

let dist p q = (abs (p.X - q.X)) + (abs (p.Y - q.Y))
let yDist p y = dist p (coord p.X y)

let range s e = { Start = s; End = e }

module Range =
    let length r = max (r.End - r.Start + 1) 0

module SensorData =
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

let isReachable y (sensor: Sensor) =
    match yDist sensor.Position y with
    | d when d <= sensor.Distance -> true
    | _ -> false

let coverage y sensor =
    let dx = sensor.Distance - (yDist sensor.Position y)
    range (sensor.Position.X - dx) (sensor.Position.X + dx)

let cullOptions r o =
    match o.Start, o.End, r.Start, r.End with
    | a, b, _, _ when b < a -> []
    | _, _, s, e when e < s -> [ o ]
    | a, b, s, e when s < a && e > b -> []
    | a, _, s, e when s < a && e < a -> [ o ]
    | a, b, s, e when s < a && e <= b -> [ range (e + 1) b ]
    | a, b, s, e when s >= a && s <= b && e >= b -> [ range a (s - 1) ]
    | a, b, s, e when s >= a && s <= b && e <= b -> [ range a (s - 1); range (e + 1) b ]
    | _, b, s, e when s > b && e > b -> [ o ]
    | _ -> failwith "I didn't think of that..."
    |> List.filter (fun r -> Range.length r > 0)

let removeOptions options range =
    options |> List.collect (cullOptions range)

let removeOptionsFromLine sensors range line =
    line,
    sensors
    |> List.filter (isReachable line)
    |> List.map (coverage line)
    |> List.fold removeOptions [ range ]

let findTuningFrequency data range =
    let notEmptyLine (_, r: Range list) = r.Length > 0
    let frequency x y = int64 x * 4_000_000L + int64 y
    let allFrequencies row range = [ range.Start .. range.End ] |> List.map (fun x -> frequency x row)
    let allOptions (row, ranges) = List.collect (allFrequencies row) ranges
    
    [ range.Start .. range.End ]
    |> List.map (removeOptionsFromLine data.Sensors range)
    |> List.filter notEmptyLine
    |> List.collect allOptions

let sample = SensorData.fromFile "./day15/sample.txt"
let input = SensorData.fromFile "./day15/input.txt"

sensorCoverage sample 10 |> Set.count |> printfn "Row 10 contains %d non-beacon positions"
sensorCoverage input 2_000_000 |> Set.count |> printfn "Row 2.000.000 contains %d non-beacon positions"

findTuningFrequency sample (range 0 20) |> printfn "Possible tuning frequencies: %A"
findTuningFrequency input (range 0 4_000_000) |> printfn "Possible tuning frequencies: %A"