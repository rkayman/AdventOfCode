open System
open System.IO

module Example =

    let given = """
        123 328  51 64
         45 64  387 23
          6 98  215 314
        *   +   *   +
        """

    let expected = [ 33210L; 490L; 4243455L; 401L ]
    let sum = 4277556L

module Problem =

    type Input =
        | Add
        | Multiply
        | Number of int64

    let mapping s =
        match s with
        | "*" -> Multiply
        | "+" -> Add
        | _ -> Number (int64 s)

    let parse (input: string) =
        let splitOptions = StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries
        input
            .Trim()
            .Split(Environment.NewLine, splitOptions)
        |> Array.map _.Split(' ', splitOptions)
        |> Array.map (Array.map mapping)

    let identity op =
        match op with
        | Add -> 0L
        | Multiply -> 1L
        | _ -> failwith "unexpected"

    let folder op acc n =
        match op with
        | Add -> acc + n
        | Multiply -> acc * n
        | _ -> failwith "unexpected"

    let solve1 (input: string) =
        let input = parse input
        let ops = input[input.Length - 1]
        let xs =
            input[0..input.Length - 2]
            |> Array.transpose
            |> Array.map (Array.map (function Number n -> n | _ -> failwith "unexpected"))

        xs
        |> Array.zip ops
        |> Array.map (fun (op, ns) ->
            let initial = identity op
            ns |> Array.fold (folder op) initial)
        |> Array.sum


File.ReadAllText "2025/input-06.txt"
// Example.given
|> Problem.solve1
