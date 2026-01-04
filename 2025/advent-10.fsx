open System
open System.Collections.Generic
open System.IO

module Problem =

    let given = """
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"""

    let file = File.ReadAllText "2025/input-10.txt"
    
    type Indicator = char
    module Indicator =
        let toggle = function
            | '#' -> '.'
            | '.' -> '#'
            | ch -> failwithf $"Invalid indicator char: %c{ch}"
            
        let (|On|Off|) = function
            | '#' -> On
            | '.' -> Off
            | ch -> failwithf $"Invalid indicator char: %c{ch}"
    
    type Indicators = Indicator[]            
    module Indicators =
        let init length: Indicators =
            Array.create length '.'
            
        let toggle (indexes: int[]) (indicators: Indicators) : Indicators =
            let indicators' = Array.copy indicators
            for i in indexes do
                indicators'[i] <- Indicator.toggle indicators[i]
            indicators'
            
    type Button =
        {
            presses: int 
            wiredTo: int[]
        }
    type Buttons = Button list
    
    type Machine =
        {
            indicatorsGoal: Indicators
            buttons: Buttons
            requiredJoltages: int[]
        }

    let parseIndicators (input: string) : Indicators =
        input.ToCharArray()

    let parseButtonSchematics (input: string) : Button =
        let parts =
            input
                .Trim([|'('; ')'|])
                .Split([|','|], StringSplitOptions.RemoveEmptyEntries)
        { presses = parts.Length; wiredTo = parts |> Array.map int }
        
    let parseJoltages (input: string) : int[] =
        input.Trim([|'{' ; '}'|])
            .Split([|','|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Int32.Parse
        
    let parseMachinePieces (lineParts: string[]) : Machine =
        let indicators = lineParts[0].Trim([|'['; ']'|]) |> parseIndicators 

        let buttons =
            lineParts[1 .. (lineParts.Length - 2)]
            |> Array.map parseButtonSchematics
            |> Array.toList
        
        let joltages = Array.last lineParts |> parseJoltages

        {
            indicatorsGoal = indicators
            buttons = buttons
            requiredJoltages = joltages
        }
        
    // implement a breadth-first search to find the minimum button presses to reach the goal indicators
    let findMinPresses machine =
        let numIndicators = machine.indicatorsGoal.Length
        let initialState = Indicators.init numIndicators
        let goalKey = String machine.indicatorsGoal
        
        if initialState = machine.indicatorsGoal then Some 0
        else
            let queue = Queue<Indicators * int>()
            let visited = HashSet<string>()
            
            queue.Enqueue((initialState, 0))
            visited.Add(String initialState) |> ignore
            
            let rec bfs () =
                if queue.Count = 0 then
                    None
                else
                    let currentState, presses = queue.Dequeue()

                    let rec loop presses' buttons =
                        match buttons with
                        | [] -> presses'
                        | button::buttons' ->
                            let newState = currentState |> Indicators.toggle button.wiredTo
                            let stateKey = String newState

                            if stateKey = goalKey then
                                Some (presses + 1)
                            elif visited.Contains(stateKey) then
                                loop presses' buttons'
                            else
                                visited.Add(stateKey) |> ignore
                                queue.Enqueue((newState, presses + 1))
                                loop presses' buttons'
                                    
                    match loop None machine.buttons with
                    | None -> bfs ()
                    | result -> result
            
            bfs ()
            
    let parse (input: string) =
        input
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
            |> Array.map _.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map parseMachinePieces

    let solve1 (input: string) = 
        input
        |> parse
        |> Array.map findMinPresses
        |> Array.sumBy (Option.defaultValue 0)

#time
Problem.file
|> Problem.solve1
|> printfn "%A"

