type CPU = {
    X: int
    Cycles: int []
}
type Instruction = | Noop | Addx of int

type Program = Instruction list

module Instruction =
    let noop cpu =
        { X = cpu.X; Cycles = [| yield! cpu.Cycles; cpu.X |]  }
        
    let addx cpu v =
        { X = cpu.X + v; Cycles = [| yield! cpu.Cycles; cpu.X; cpu.X |] }

module CPU =
    let startState = {
        X = 1
        Cycles = [| 1 |]
    }
    
    let execute state instr =
        match instr with
        | Noop -> Instruction.noop state
        | Addx v -> Instruction.addx state v
        
    let run program =    
        (startState, program) ||> List.fold execute |> (fun cpu -> [|yield! cpu.Cycles; cpu.X|])
        
module Program =
    open System
    open System.IO
    open System.Text.RegularExpressions
    let (|Noop|_|) input =
        let m = Regex.Match(input, "^noop$")
        
        if (m.Success) then
            Some ()
        else
            None
            
    let (|Addx|_|) input =
        let m = Regex.Match(input, "^addx\s+(?<v>\-?[0-9]+)$")
        
        if (m.Success) then
            Some (Int32.Parse(m.Groups["v"].Value))
        else
            None
            
    let parse str =
        match str with
        | Noop -> Instruction.Noop
        | Addx v -> Instruction.Addx v
        | _ -> failwith $"Unsupported instruction '%s{str}'"

    let fromStrings: string seq -> Instruction list = Seq.map parse >> Seq.toList
    
    let fromFile = File.ReadAllLines >> fromStrings
        

let sample1 = Program.fromFile "./day10/sample1.txt"
let sample2 = Program.fromFile "./day10/sample2.txt"
let input = Program.fromFile "./day10/input.txt"


let indices = [for i in 20 .. 40 .. 220 -> i]

let d = input |> CPU.run
[|for i in 20 .. 40 .. 220 -> i*d[i]|] |> Array.sum |> printfn "%A"

let coord cycle =
    ((cycle-1)/40,(cycle-1)%40)
    
let cycle r c = (r*40 + c) + 1

let getPixel (cycles: int[]) r c =
    let x = cycles[cycle r c]
    
    if (x - 1 <= c && c <= x + 1) then
        '#'
    else
        '.'

[| for r in 0..5 do
       ((Array2D.init 6 40 (getPixel d))[r, *]) |> Array.map (sprintf "%c") |> String.concat "" |] |> Array.iter (printfn "%s")


