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
    
    let solve1 limit (input: string)=
        let jbs = parse input

        let capacity = (jbs.Length * jbs.Length / 2) - (jbs.Length / 2)
        let mutable index =
            System.Collections.Generic.Dictionary<JunctionBox, int>(capacity)
        let mutable table =
            System.Collections.Generic.Dictionary<int, Circuit>(capacity)
        
        let insert i jb =
            index[jb] <- i
            table[i] <- set [ jb ]
        
        jbs |> Array.iteri insert

        let pow a b = pown (a - b) 2 |> float

        let pairs =
            [|
                for i in 0 .. jbs.Length - 1 do
                    let ax, ay, az = jbs[i]
                    for j in i + 1 .. jbs.Length - 1 do
                        let bx, by, bz = jbs[j]
                        let cost = pow ax bx + pow ay by + pow az bz |> sqrt
                        yield (jbs[i], jbs[j], cost)
            |]
            |> Array.sortBy (fun (_, _, cost) -> cost)
        
        let connectCircuits a b =
            match index[a], index[b] with
            | ai, bi when ai = bi -> true
            | ai, bi ->
                let circuitA = table[ai]
                let circuitB = table[bi]
                if circuitA.Count >= circuitB.Count then
                    // Merge B into A
                    table[ai] <- Set.union circuitA circuitB
                    circuitB
                    |> Seq.iter (fun jb ->
                        table.Remove(index[jb]) |> ignore
                        index[jb] <- ai)
                else
                    // Merge A into B
                    table[bi] <- Set.union circuitB circuitA
                    circuitA
                    |> Seq.iter (fun jb ->
                        table.Remove(index[jb]) |> ignore
                        index[jb] <- bi)
                true

        let rec loop cnt xs =
            if cnt <= 0 then
                table.Values |> Seq.toList
            else
                match xs with
                | [] -> failwith "This should not happen"
                | (a, b, _) :: rest -> 
                    let cnt' = if connectCircuits a b then cnt - 1 else cnt
                    loop cnt' rest

        try 
            pairs |> Array.toList |> loop limit
        with ex ->
            eprintfn $"Exception: %s{ex.Message}\n%s{ex.StackTrace}"
            index |> Seq.iter (fun kvp -> eprintfn $"Index Key: %A{kvp.Key}, Value: %d{kvp.Value}")
            table |> Seq.iter (fun kvp -> eprintfn $"Table Key: %d{kvp.Key}, Value Count: %A{kvp.Value}")
            List.Empty


System.IO.File.ReadAllText "2025/input-08.txt"
// Example.given
|> Problem.solve1 1000
|> List.map (fun set -> set.Count, set)
|> List.sortByDescending fst
// |> printfn "Result: %A"
|> List.map fst
|> List.take 3
|> List.reduce (*)
|> printfn "Result: %d"
