open System

module Problem =

    let given = """
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"""

    type Indicator =
        | On
        | Off
        
    type Indicators = Indicator[]
    module Indicators =
        let init length: Indicators =
            Array.create length Off

        let toggle = function On -> Off | Off -> On
        
        let toggle (indexes: int[]) (indicators: Indicators) : Indicators =
            indexes
            |> Array.map (fun idx -> idx, indicators[idx])
            |> Array.iter (fun (idx, state) ->
                indicators[idx] <- toggle state)
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
    
    let parse (input: string) =
        input.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line ->
            let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            let indicators =
                parts[0].Trim([|'['; ']'|])
                |> Seq.map (fun c -> c = '#')
                |> Seq.toArray
            
            let wiringSchemas =
                parts[1..(parts.Length - 2)]
                |> Array.map _.Trim([|'('; ')'|])
                |> Array.map Int32.Parse)
            
            let reqdJoltages =
                parts[parts.Length - 1]
                .Trim([|'{' ; '}'|])
                .Split([|','|], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map Int32.Parse
            {
                indicators = indicators
                wiringSchemas = wiringSchemas
                reqdJoltages = reqdJoltages
            }
        )
        

Problem.given
|> Problem.parse
|> printfn "%A"
