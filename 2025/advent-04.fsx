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
        input.Trim().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line -> line.ToCharArray())

    let solve (input: string) =
        let grid = parse input

// File.ReadAllText "2025/input-04.txt"
// Example.given
