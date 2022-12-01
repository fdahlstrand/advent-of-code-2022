open System
open System.IO

let tryParse (s: string) =
    match Int32.TryParse(s) with
    | (true, n) -> Some n
    | (false, _) -> None

let folder (n: int option) state =
    match state with
    | x :: xs ->
        match n with
        | Some z -> ([ z ] @ x) :: xs
        | None -> [] :: (x :: xs)
    | [] ->
        match n with
        | Some z -> [ [ z ] ]
        | None -> state

let collect seq = Seq.foldBack folder seq []

File.ReadLines("./day1/input.txt")
|> Seq.map tryParse
|> collect
|> Seq.map (fun l -> List.sum l)
|> Seq.max
|> printfn "Largest batch: %d"

File.ReadLines("./day1/input.txt")
|> Seq.map tryParse
|> collect
|> Seq.map (fun l -> List.sum l)
|> Seq.sortDescending
|> Seq.take 3
|> Seq.sum
|> printfn "Total calories carried by top 3 elves: %d"
