open System

module Example =
    
    let given = """
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
"""
    
    let expected = 40
    
    
module Problem =
    type JunctionBox = int list
    type Circuit = Set<JunctionBox>

    let distances jbs limit = 
        let distance jb1 jb2 =
            jb2
            |> List.zip jb1
            |> List.sumBy (fun (x, y) -> abs (x - y))
        
        let conditionalTake limit (lst: 'a list) =
            if lst.Length > limit then
                lst |> List.take limit
            else
                lst
                
        let rec pairings max result xss = 
            match xss with
            | [] -> result
            | x::xs ->
                let result' =
                    xs
                    |> List.map (fun y -> (x, y))
                    |> List.map (fun (a, b) -> ((a, b), distance a b))
                    |> List.filter (fun (_, d) -> d < max)
                    |> List.sortBy snd
                    |> conditionalTake limit
                    
                let max' =
                    result' |> List.maxBy snd |> snd

                pairings max' result' xs
        jbs |> pairings Int32.MaxValue []
                
    let parse (input: string) =
        input
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.collect (_.Split(',', StringSplitOptions.RemoveEmptyEntries)
                         >> Array.map int
                         >> Array.toList)
        |> List.chunkBySize 3

    let solve (input: string) =
        let jbs = parse input
        
        0 // Placeholder for actual solution logic


// System.IO.File.ReadAllText "2025/input-08.txt"
Example.given
|> Problem.parse


let mutable xdict =
    System.Collections.Generic.Dictionary<Problem.JunctionBox, Problem.Circuit>()
    
let xss = 
    Example.given
    |> Problem.parse
    
let pairs =
    xss
    |> List.rev
    |> List.zip xss
    |> List.take 3

pairs
|> List.map (fun (a,b) -> [ a; b ])
|> List.map Set.ofList
|> List.iter (fun xs ->
    xs |> Set.iter (fun jb ->
        xdict[jb] <- xs)
    )

let a = xdict[[57; 618; 57]]
let b = xdict[[162; 817; 812]]
let c = xdict[[984; 92; 344]]

a = b
a = c
