open System
open System.IO

module Example =

    let given = """123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  """

    let expected = [ 33210L; 490L; 4243455L; 401L ]
    let sum = 4277556L
    
    let expected2 = [ 1058L; 3253600L; 625L; 8544L ]
    let sum2 = 3263827L

module Problem =
    
    type Operation =
        | Add
        | Multiply

    type Input =
        | Op of Operation
        | Number of int64

    let mapping s =
        match s with
        | "*" -> Op Multiply
        | "+" -> Op Add
        | _ -> Number (int64 s)

    let parse (input: string) =
        let splitOptions = StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries
        input
            .Trim()
            .Split(Environment.NewLine, StringSplitOptions.None)
        |> Array.map _.Split(' ', StringSplitOptions.None)

    let identity op =
        match op with
        | Add -> 0L
        | Multiply -> 1L

    let folder op acc n =
        match op with
        | Add -> acc + n
        | Multiply -> acc * n
        
    let Op input =
        match input with
        | Op op -> op
        | _ -> failwith "unexpected"
    
    let solve1 (input: string) =
        let input =
            parse input
            |> Array.map (Array.map mapping)
        let ops = input[input.Length - 1]
        let xs =
            input[0..input.Length - 2]
            |> Array.transpose
            |> Array.map (Array.map (function Number n -> n | _ -> failwith "unexpected"))

        xs
        |> Array.zip ops
        |> Array.map (fun (op, ns) ->
            let op' = Op op
            let initial = identity op'
            ns |> Array.fold (folder op') initial)
        |> Array.sum
                    
    let solve2 (input: string) =
        let lines =
            input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)

        let lineLen = lines |> Array.last |> _.ToCharArray().Length

        let ops, cols =
            let indexedOps = 
                lines
                |> Array.last
                |> _.ToCharArray()
                |> Array.indexed
                |> Array.filter (fun (_, c) -> c <> ' ')
                
            let indices =
                indexedOps
                |> Array.map fst
                |> (Array.rev >> Array.append [| lineLen |] >> Array.rev)
                |> Array.pairwise
                |> Array.map (fun (i, j) -> if j = lineLen then (i, j - 1) else (i, i + j - i - 2))
                
            (indexedOps |> Array.map snd |> Array.rev, indices)

        let chunks =
            cols
            |> Array.rev
            |> Array.map (fun (first, last) -> last - first + 1)

        let chunk sizes xss =
            sizes
            |> Array.scan (fun (_, rest) size ->
                rest |> Array.splitAt size
                ) (Array.empty, xss)
            |> Array.map fst
            |> Array.tail

        let data = 
            lines
            |> Array.rev
            |> Array.tail
            |> Array.map _.ToCharArray()
            |> Array.rev
            |> Array.map Array.indexed
            |> Array.map (Array.filter (fun (i, _) ->
                let set = cols |> Array.collect (fun (i, j) -> [|i..j|]) |> Set.ofArray
                set |> Set.contains i))
            |> Array.map (Array.map snd >> Array.rev)
            |> Array.transpose
            |> Array.map (String.Concat >> int64)

        data
        |> chunk chunks
        |> Array.zip ops
        |> Array.map (fun (ch, xs) ->
            let op =
                match ch with
                | '+' -> (+)
                | '*' -> (*)
                | _ -> failwith "unexpected"
                
            xs |> Array.reduce op
            )
        |> Array.sum


File.ReadAllText "2025/input-06.txt"
// Example.given
|> Problem.solve2

