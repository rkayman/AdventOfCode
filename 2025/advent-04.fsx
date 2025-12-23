open System
open System.IO

module Example =

    let given = """
        ..@@.@@@@.
        @@@.@.@.@@
        @@@@@.@.@@
        @.@@@@..@.
        @@.@@@@.@@
        .@@@@@@@.@
        .@.@.@.@@@
        @.@@@.@@@@
        .@@@@@@@@.
        @.@.@@@.@.
        """

    let expected = 13
    let removable = 43

    let answer = """
        ..xx.xx@x.
        x@@.@.@.@@
        @@@@@.x.@@
        @.@@@@..@.
        x@.@@@@.@x
        .@@@@@@@.@
        .@.@.@.@@@
        x.@@@.@@@@
        .@@@@@@@@.
        x.x.@@@.x.
        """

module Problem =
    let parse (input: string) =
        input
            .Trim()
            .Split(Environment.NewLine, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        |> Array.map _.ToCharArray()

    let findAccessibleRolls (grid: char[][]) =
        let rows = grid.Length
        let cols = grid[0].Length

        let isValid max value = value >= 0 && value < max
        let isValidRow = isValid rows
        let isValidCol = isValid cols
        let notItself (r, c) (x, y) = not (r = x && c = y)
        let adjacentRolls r c =
            seq {
                for i in r - 1 .. r + 1 do
                    for j in c - 1 .. c + 1 do
                        if (isValidRow i)
                           && (isValidCol j)
                           && (notItself (r, c) (i, j))
                        then yield (i, j)
            }

        seq {
            for r in 0 .. rows - 1 do
                for c in 0 .. cols - 1 do
                    if grid[r][c] = '@' then
                        let accessibleRolls =
                            adjacentRolls r c
                            |> Seq.filter (fun (i, j) -> grid[i][j] = '@')
                            |> Seq.length

                        if accessibleRolls < 4 then
                            yield (r, c)
        }

    let solve (input: string) =
        parse input
        |> findAccessibleRolls
        |> Seq.length

    let solve2 (input: string) =
        let warehouse = parse input
        let rec loop (grid: char[][]) numRemoved removableList =
            removableList
            |> List.iter (fun (r, c) -> grid[r][c] <- 'x')

            match findAccessibleRolls grid |> Seq.toList with
            | [] -> numRemoved
            | rs ->
                loop grid (numRemoved + rs.Length) rs

        loop warehouse 0 []

File.ReadAllText "2025/input-04.txt"
// Example.given
|> Problem.solve2
