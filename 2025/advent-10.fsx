open System

module Problem =

    let given = """
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"""

    type Indicator = On | Off
    module Indicator =
        let fromChar = function
            | '#' -> On
            | '.' -> Off
            | ch -> failwithf $"Invalid indicator char: %c{ch}"

        let toggle = function On -> Off | Off -> On

    type Indicators = Indicator[]
    module Indicators =
        let init length: Indicators =
            Array.create length Off
        
        let toggle (indexes: int[]) (indicators: Indicators) : Indicators =
            indexes
            |> Array.map (fun idx -> idx, indicators[idx])
            |> Array.iter (fun (idx, state) ->
                indicators[idx] <- Indicator.toggle state)
            indicators
            
    type Button =
        {
            presses: int 
            wiredTo: int[]
        }
    type Buttons = Button[]
    
    type Machine =
        {
            indicatorsGoal: Indicators
            buttons: Buttons
            reqdJoltages: int[]
        }

    let parseIndicators (input: string) : Indicators =
        input
            .ToCharArray()
            |> Array.map Indicator.fromChar

    let parseButtonSchematics (input: string) : Button =
        let parts = input.Trim([|'('; ')'|]).Split([|','|], StringSplitOptions.RemoveEmptyEntries)
        {
            presses = parts.Length
            wiredTo = parts |> Array.map Int32.Parse
        }
        
    let parseJoltages (input: string) : int[] =
        input.Trim([|'{' ; '}'|])
            .Split([|','|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Int32.Parse
        
    let parseMachinePieces (lineParts: string[]) : Machine =
        let indicators = lineParts[0].Trim([|'['; ']'|]) |> parseIndicators 

        let buttons =
            lineParts[1 .. (lineParts.Length - 2)]
            |> Array.map parseButtonSchematics
        
        let joltages = Array.last lineParts |> parseJoltages

        {
            indicatorsGoal = indicators
            buttons = buttons
            reqdJoltages = joltages
        }
            
    let parse (input: string) =
        input
            .Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
            |> Array.map _.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map parseMachinePieces
        

Problem.given
|> Problem.parse
|> Array.iter (printfn "%A")
