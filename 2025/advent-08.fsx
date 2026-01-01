open System
open System.Collections.Generic

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
    type JunctionBox = int64 * int64 * int64
    type Circuit = Set<JunctionBox>

    let (|Triple|_|) (lst: int64[]) =
        match lst with
        | [|a; b; c|] -> Some (a, b, c)
        | _ -> None
    
    let parse (input: string) =
        input
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
        |> Array.collect (_.Split(',', StringSplitOptions.RemoveEmptyEntries)
                         >> Array.map int64)
        |> Array.chunkBySize 3
        |> Array.choose (function Triple (a, b, c) -> Some (a, b, c) | _ -> None)
    
    let allPairsByCost (xs: JunctionBox[]) =
        let pow a b = pown (a - b) 2 |> float
        [|
            for i in 0 .. xs.Length - 1 do
                let ax, ay, az = xs[i]
                for j in i + 1 .. xs.Length - 1 do
                    let bx, by, bz = xs[j]
                    let cost = pow ax bx + pow ay by + pow az bz |> sqrt
                    yield (xs[i], xs[j], cost)
        |]
        |> Array.sortBy (fun (_, _, cost) -> cost)

    let connectCircuits (index: IDictionary<JunctionBox, int>) (table: IDictionary<int, Circuit>) a b =
        let updateCircuitMembership sourceCircuit targetIndex =
            sourceCircuit
            |> Seq.iter (fun junctionBox ->
                table.Remove(index[junctionBox]) |> ignore
                index[junctionBox] <- targetIndex)
        
        let mergeCircuits smallerIndex largerIndex =
            let smallerCircuit = table[smallerIndex]
            let largerCircuit = table[largerIndex]
            table[largerIndex] <- Set.union largerCircuit smallerCircuit
            updateCircuitMembership smallerCircuit largerIndex
        
        match index[a], index[b] with
        | circuitIndexA, circuitIndexB when circuitIndexA = circuitIndexB -> ()
        | circuitIndexA, circuitIndexB ->
            let circuitA = table[circuitIndexA]
            let circuitB = table[circuitIndexB]
            if circuitA.Count >= circuitB.Count then
                mergeCircuits circuitIndexB circuitIndexA
            else
                mergeCircuits circuitIndexA circuitIndexB        
    
    let solve1 limit (input: string)=
        let jbs = parse input

        let capacity = (jbs.Length * jbs.Length / 2) - (jbs.Length / 2)
        let mutable index = Dictionary<JunctionBox, int>(capacity)
        let mutable table = Dictionary<int, Circuit>(capacity)
        
        let insert i jb =
            index[jb] <- i
            table[i] <- set [ jb ]
        
        jbs |> Array.iteri insert

        let pairs = allPairsByCost jbs

        let rec loop cnt xs =
            if cnt <= 0 then
                table.Values |> Seq.toList
            else
                match xs with
                | [] -> failwith "This should not happen"
                | (a, b, _) :: rest -> 
                    connectCircuits index table a b
                    loop (cnt - 1) rest

        try 
            pairs
            |> Array.toList
            |> loop limit
            |> List.map (fun set -> set.Count, set)
            |> List.sortByDescending fst
            |> List.map fst
            |> List.take 3
            |> List.reduce (*)
        with ex ->
            eprintfn $"Exception: %s{ex.Message}\n%s{ex.StackTrace}"
            index |> Seq.iter (fun kvp -> eprintfn $"Index Key: %A{kvp.Key}, Value: %d{kvp.Value}")
            table |> Seq.iter (fun kvp -> eprintfn $"Table Key: %d{kvp.Key}, Value Count: %A{kvp.Value}")
            -1

#time
System.IO.File.ReadAllText "2025/input-08.txt"
// Example.given
|> Problem.solve1 1000
|> printfn "Result: %d"
