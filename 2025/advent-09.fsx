open System

module Problem =
    let example = """
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
"""

    let file = System.IO.File.ReadAllText "2025/input-09.txt"

    let parse (input: string) =
        input.Trim().Split("\n")
        |> Array.map _.Split(',')
        |> Array.map (Array.map int64)
        |> Array.map (fun arr -> (arr.[0], arr.[1]))
        |> Array.toList

    let distinctPairs xs =
        let xs' = List.indexed xs
        [
            for p in xs' do
                let ys = Seq.skip (p |> fst |> (+) 1) xs
                for q in ys do
                    yield (snd p, q)
        ]
    
    let rectArea (p, q) =
        let x1, y1 = p
        let x2, y2 = q
        let width = abs (x2 - x1) + 1L
        let height = abs (y2 - y1) + 1L
        width * height, (p, q)
    
    let solve1 (input: string) =
        let points = parse input
        
        points
        |> distinctPairs
        |> List.map rectArea
        // |> List.sortByDescending fst
        |> List.maxBy fst
        |> fst


#time
Problem.file
// Problem.example
|> Problem.solve1
