type Throw = { Target: int; Item: int }

type Monkey =
    { Items: int list
      Operation: int -> int
      Test: int -> Throw
      InspectionCounter: int }

module Monkey =
    let inspect (monkey: Monkey) (item: int) =
        let newItem = (monkey.Operation item) / 3

        (monkey.Test newItem,
         { monkey with
             Items = monkey.Items.Tail
             InspectionCounter = monkey.InspectionCounter + 1 })

    let catch (monkey: Monkey) (throw: Throw) =
        { monkey with Items = monkey.Items @ [ throw.Item ] }

    let defineTest divisor ifTarget elseTarget =
        fun i ->
            if (i % divisor = 0) then
                { Target = ifTarget; Item = i }
            else
                { Target = elseTarget; Item = i }

let troop1 =
    [| { Items = [ 79; 98 ]
         Operation = fun i -> i * 19
         Test = Monkey.defineTest 23 2 3
         InspectionCounter = 0 }
       { Items = [ 54; 65; 75; 74 ]
         Operation = fun i -> i + 6
         Test = Monkey.defineTest 19 2 0
         InspectionCounter = 0 }
       { Items = [ 79; 60; 97 ]
         Operation = fun i -> i * i
         Test = Monkey.defineTest 13 1 3
         InspectionCounter = 0 }
       { Items = [ 74 ]
         Operation = fun i -> i + 3
         Test = Monkey.defineTest 17 0 1
         InspectionCounter = 0 } |]
    
let troop2 =
    [|
       { Items = [80]
         Operation = fun i -> i*5
         Test = Monkey.defineTest 2 4 3
         InspectionCounter = 0 }
       { Items = [75; 83; 74]
         Operation = fun i -> i + 7
         Test = Monkey.defineTest 7 5 6
         InspectionCounter = 0 }
       { Items = [86; 67; 61; 96; 52; 63; 73]
         Operation = fun i -> i + 5
         Test = Monkey.defineTest 3 7 0
         InspectionCounter = 0 }
       { Items = [85; 83; 55; 85; 57; 70; 85; 52]
         Operation = fun i -> i + 8 
         Test = Monkey.defineTest 17 1 5
         InspectionCounter = 0 }
       { Items = [67; 75; 91; 72; 89]
         Operation = fun i -> i + 4
         Test = Monkey.defineTest 11 3 1
         InspectionCounter = 0 }
       { Items = [66; 64; 68; 92; 68; 77]
         Operation = fun i -> i * 2
         Test = Monkey.defineTest 19 6 2
         InspectionCounter = 0 }
       { Items = [97; 94; 79; 88]
         Operation = fun i -> i * i
         Test = Monkey.defineTest 5 2 7
         InspectionCounter = 0 }
       { Items = [77; 85]
         Operation = fun i -> i + 6
         Test = Monkey.defineTest 13 4 0
         InspectionCounter = 0 }        
    |]

let doRound (troop: Monkey[]) =
    for m in 0 .. Array.length troop - 1 do
        let throws, monkey = (troop[m], troop[m].Items) ||> List.mapFold Monkey.inspect
        Array.set troop m monkey

        throws
        |> List.iter (fun throw -> Array.set troop throw.Target (Monkey.catch troop[throw.Target] throw))

for _ in 1..20 do
    doRound (troop2)

troop2
|> Array.map (fun m -> m.InspectionCounter)
|> Array.sortDescending
|> Array.take 2
|> (fun arr -> arr[0] * arr[1])
|> printfn "Level of monkey business: %d"
