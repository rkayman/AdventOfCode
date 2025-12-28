open System
open System.IO

module Example =
    let given = """.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."""

    let expected = 21

    let answer = """.......S.......
.......|.......
......|^|......
......|.|......
.....|^|^|.....
.....|.|.|.....
....|^|^|^|....
....|.|.|.|....
...|^|^|||^|...
...|.|.|||.|...
..|^|^|||^|^|..
..|.|.|||.|.|..
.|^|||^||.||^|.
.|.|||.||.||.|.
|^|^|^|^|^|||^|
|.|.|.|.|.|||.|"""



module Array =
    let get' i xs = Array.get xs i

module Problem =

    type Grid = (int * char) array array
    
    let parse (input: string) : Grid =
        input.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Seq.toArray
        |> Array.map Array.indexed

    let gridSize (grid: Grid) =
        grid.Length, grid[0].Length
            
    let get2d r c (grid: Grid) =
        grid[r] |> Array.get' c |> snd

    let startPos (grid: Grid) =
        0, grid[0] |> Array.find (fun (_, c) -> c = 'S') |> fst

    let findSplitters (grid: Grid) =
        grid
        |> Array.mapi (fun r cells ->
            cells
            |> Array.filter (fun (_, ch) -> ch = '^')
            |> Array.map (fun (c, _) -> (r, c)))
        |> Array.collect id
        |> Set.ofArray
    
    let solve1 (input: string) = 
        let grid = parse input
        let rows, cols = gridSize grid
        let start = startPos grid
        let splits = findSplitters grid
        
        let outOfBounds (r, c) =
            r < 0 || r >= rows || c < 0 || c >= cols
        
        let rec loop pos (hits: Set<_>, visited: Set<_>) =
            if (outOfBounds pos) || (visited |> Set.contains pos) then
                hits, visited
            else
                let visited' = visited |> Set.add pos
                if splits |> Set.contains pos then
                    let hits' = hits |> Set.add pos
                    let left = (fst pos, snd pos - 1)
                    let right = (fst pos, snd pos + 1)
                    loop right (loop left (hits', visited'))
                else
                    let down = (fst pos + 1, snd pos)
                    loop down (hits, visited')
        
        loop start (Set.empty, Set.empty)


File.ReadAllText "2025/input-07.txt"
// Example.given
|> Problem.solve1
|> fst
|> Set.count

