type SectionID = int
type Range = SectionID * SectionID
type Assignment = Range * Range
type AssignmentList = Assignment list

module Range =
    let isSubset (range1: Range) (range2: Range) =
        let (a1, b1) = range1
        let (a2, b2) = range2
        
        a2 >= a1 && b2 <= b1
        
module Assignment =
    let isOverlapping (assignment: Assignment) =
        let (range1, range2) = assignment
        (Range.isSubset range1 range2) || (Range.isSubset range2 range1)
 
module AssignmentList =
    open System
    open System.IO
    open System.Text.RegularExpressions
    
    let private sectionID (collection: GroupCollection) (index: string): SectionID =
        match Int32.TryParse(collection.[index].Value) with
        | (true, a) -> a
        | _ -> failwith $"Not a valid number (%s{collection.[index].Value})" 
    
    let private (|Assignment|_|) input: Assignment option =
        let m = Regex.Match(input, @"^(?<a1>\d+)-(?<b1>\d+),(?<a2>\d+)-(?<b2>\d+)$")
        
        if (m.Success) then
            let id = sectionID m.Groups
            Some ((id "a1", id "b1"),(id "a2", id "b2"))
        else
            None
            
    let ofSeq (assignments: string seq): AssignmentList =
        assignments
        |> Seq.map (fun str -> match str with | Assignment a -> a | _ -> failwith $"Not a valid assignment '%s{str}'")
        |> Seq.toList

    let fromFile = File.ReadLines >> ofSeq    
    let countOverlapping = List.map Assignment.isOverlapping >> List.filter id >> List.length         

AssignmentList.fromFile "./day4/input.txt"
|> AssignmentList.countOverlapping
|> printfn "%d total overlapping regions"