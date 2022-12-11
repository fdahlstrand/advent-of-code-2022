open System
type Throw = { Target: int; Item: Int64 }

type Monkey =
    { Items: Int64 list
      Operation: Int64 -> Int64
      Test: Int64 -> Throw
      InspectionCounter: Int64 }

module Monkey =
    let inspect (worryHandler: Int64 -> Int64) (monkey: Monkey) (item: Int64) =
        let newItem = worryHandler (monkey.Operation item)

        if (newItem < 0) then failwith "Integer type too small"

        (monkey.Test newItem,
         { monkey with
             Items = monkey.Items.Tail
             InspectionCounter = monkey.InspectionCounter + 1L })

    let catch (monkey: Monkey) (throw: Throw) =
        { monkey with Items = monkey.Items @ [ throw.Item ] }

    let defineTest divisor ifTarget elseTarget =
        fun i ->
            if (i % divisor = 0L) then
                { Target = ifTarget; Item = i }
            else
                { Target = elseTarget; Item = i }

let troop1 =
    [ { Items = [ 79; 98 ]
        Operation = fun i -> i * 19L
        Test = Monkey.defineTest 23 2 3
        InspectionCounter = 0 }
      { Items = [ 54; 65; 75; 74 ]
        Operation = fun i -> i + 6L
        Test = Monkey.defineTest 19 2 0
        InspectionCounter = 0 }
      { Items = [ 79; 60; 97 ]
        Operation = fun i -> i * i
        Test = Monkey.defineTest 13 1 3
        InspectionCounter = 0 }
      { Items = [ 74 ]
        Operation = fun i -> i + 3L
        Test = Monkey.defineTest 17 0 1
        InspectionCounter = 0 } ]

let troop2 =
    [ { Items = [ 80 ]
        Operation = fun i -> i * 5L
        Test = Monkey.defineTest 2 4 3
        InspectionCounter = 0 }
      { Items = [ 75; 83; 74 ]
        Operation = fun i -> i + 7L
        Test = Monkey.defineTest 7 5 6
        InspectionCounter = 0 }
      { Items = [ 86; 67; 61; 96; 52; 63; 73 ]
        Operation = fun i -> i + 5L
        Test = Monkey.defineTest 3 7 0
        InspectionCounter = 0 }
      { Items = [ 85; 83; 55; 85; 57; 70; 85; 52 ]
        Operation = fun i -> i + 8L
        Test = Monkey.defineTest 17 1 5
        InspectionCounter = 0 }
      { Items = [ 67; 75; 91; 72; 89 ]
        Operation = fun i -> i + 4L
        Test = Monkey.defineTest 11 3 1
        InspectionCounter = 0 }
      { Items = [ 66; 64; 68; 92; 68; 77 ]
        Operation = fun i -> i * 2L
        Test = Monkey.defineTest 19 6 2
        InspectionCounter = 0 }
      { Items = [ 97; 94; 79; 88 ]
        Operation = fun i -> i * i
        Test = Monkey.defineTest 5 2 7
        InspectionCounter = 0 }
      { Items = [ 77; 85 ]
        Operation = fun i -> i + 6L
        Test = Monkey.defineTest 13 4 0
        InspectionCounter = 0 } ]

let worryHandler1 (i: Int64) = i / 3L
let worryHandler2 (i: Int64) = i % (23L * 19L * 13L * 17L)

let worryHandler3 (i: Int64) =
    i % (2L * 7L * 3L * 17L * 11L * 19L * 5L * 13L)

let doRound (worryHandler: Int64 -> Int64) (troop: Monkey []) =
    for m in 0 .. Array.length troop - 1 do
        let throws, monkey =
            (troop[m], troop[m].Items) ||> List.mapFold (Monkey.inspect worryHandler)

        Array.set troop m monkey

        throws
        |> List.iter (fun throw -> Array.set troop throw.Target (Monkey.catch troop[throw.Target] throw))

let doRounds N worryHandler troop =
    let monkeys = Array.ofList troop

    for _ in 1..N do
        doRound worryHandler monkeys

    monkeys |> Array.map (fun m -> m.InspectionCounter)

let calculateLevel inspectionCounts =
    inspectionCounts
    |> Array.sortDescending
    |> Array.take 2
    |> (fun arr -> arr[0] * arr[1])

doRounds 20 worryHandler1 troop1
|> calculateLevel
|> printfn "Troop1: Monkey business level after 20 rounds %d"

doRounds 20 worryHandler1 troop2
|> calculateLevel
|> printfn "Troop2: Monkey business level after 20 rounds %d"

printfn "===="

doRounds 1 worryHandler2 troop1
|> printfn "Troop1: Inspections after     1 round : %A"

doRounds 20 worryHandler2 troop1
|> printfn "Troop1: Inspections after    20 rounds: %A"

doRounds 1000 worryHandler2 troop1
|> printfn "Troop1: Inspections after  1000 rounds: %A"

doRounds 10000 worryHandler2 troop1
|> printfn "Troop1: Inspections after 10000 rounds: %A"

printfn "===="

doRounds 10000 worryHandler3 troop2
|> calculateLevel
|> printfn "Troop2: Monkey business level after 10000 rounds %d"
