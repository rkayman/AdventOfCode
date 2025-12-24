open System
open System.IO

module Example =
    
    let given = """
        3-5
        10-14
        16-20
        12-18

        1
        5
        8
        11
        17
        32
        """
    
    let expected = 3
    
    
module Problem =

    type IngredientDB =
        {
            freshRanges: (int64 * int64) array
            availableIds: int64 array
        }

    let parse (input: string) =
        let doubleNewLine = Environment.NewLine + Environment.NewLine
        let splitOptions = StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries
        let sections = input.Split(doubleNewLine, splitOptions)
        let freshRanges =
            sections[0]
                .Split(Environment.NewLine, splitOptions)
                |> Array.map _.Split("-", splitOptions)
                |> Array.map (fun range -> (int64 range[0], int64 range[1]))
        let availableIds =
            sections[1]
                .Split(Environment.NewLine, splitOptions)
                |> Array.map int64
        { freshRanges = freshRanges; availableIds = availableIds }

    let inline filter x (min, max) =
        Int64.Clamp(x, min, max) = x
    
    let inline exists xs x =
        xs |> Array.exists (filter x)
    
    let part1 (input: string) =
        let db = parse input
        db.availableIds
        |> Array.filter (exists db.freshRanges)
        |> Array.length
    
    let part2 (input: string) =
        (parse input).freshRanges
        |> Array.sortBy fst
        |> Array.fold
            (fun acc (min, max) ->
                match acc with
                | [] -> [ (min, max) ]
                | (lastMin, lastMax) :: tail when min <= lastMax + 1L ->
                    (lastMin, Int64.Max(lastMax, max)) :: tail
                | _ ->
                    (min, max) :: acc)
            []
        |> List.rev
        |> List.sumBy (fun (min, max) -> max - min + 1L)
        

File.ReadAllText "2025/input-05.txt"
// Example.given
|> Problem.part2
