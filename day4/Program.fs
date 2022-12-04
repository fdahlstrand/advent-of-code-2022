type SectionID = int
type Range = SectionID * SectionID
type Assignment = Range * Range
type AssignmentList = Assignment list

module Range =
    let isSubset (range1: Range) (range2: Range) =
        let (a1, b1) = range1
        let (a2, b2) = range2
        
        a2 >= a1 && b2 <= b1
        
    let intersect (range1: Range) (range2: Range): Range =
        let (a1, b1) = range1
        let (a2, b2) = range2

        (max a1 a2, min b1 b2)
        
    let isEmpty (range: Range) =
        let (a, b) = range
        
        (b - a + 1) <= 0        
        
module Assignment =
    let isFullyOverlapping (assignment: Assignment) =
        let (range1, range2) = assignment
        
        (Range.isSubset range1 range2) || (Range.isSubset range2 range1)
        
    let isPartlyOverlapping (assignment: Assignment) =
        let (range1, range2) = assignment
        
        not (Range.isEmpty (Range.intersect range1 range2))
 
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
        
        
    let private count pred = List.map pred >> List.filter id >> List.length     

    let fromFile = File.ReadLines >> ofSeq    
    let countFullyOverlapping = count Assignment.isFullyOverlapping
    let countPartlyOverlapping = count Assignment.isPartlyOverlapping

AssignmentList.fromFile "./day4/input.txt"
|> AssignmentList.countFullyOverlapping
|> printfn "%d total overlapping regions"

AssignmentList.fromFile "./day4/input.txt"
|> AssignmentList.countPartlyOverlapping
|> printfn "%d total overlapping regions"