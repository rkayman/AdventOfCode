open System
open System.IO

module Example =
    let given = """
        987654321111111
        811111111111119
        234234234234278
        818181911112111
        """

    let maxJoltage = [ 98L; 89L; 78L; 92L ]
    let expected = maxJoltage |> List.sum
    let answerPart1 = 16854L
    let maxJoltage2 = [ 987654321111L; 811111111119L; 434234234278L; 888911112111L ]
    let expected2 = 3121910778619L

module Problem =
    type BatteryBank =
        {
            text: string
            bank: char array
            values: int array
        }
        
    let parse (input: string) =
        input
            .Split(Environment.NewLine, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun line -> line, line.ToCharArray())
            |> Array.map (fun (text, bank) ->
                {
                    text = text
                    bank = bank
                    values = [| for c in bank -> (int c) - (int '0') |]
                })

    let findLargestValue battery =
        // Within the bank, you need to turn on exactly two batteries;
        // the joltage produced equals the number formed by the digits on the batteries turned on.
        // For example, if you have a bank like 12345 AND you turn on batteries 2 and 4,
        //   the bank would produce 24 jolts.
        // Batteries cannot be rearranged.
        // Find the largest possible joltage the bank can produce.
        let idx, tens =
            battery.values[..battery.values.Length - 2]
            |> Array.indexed
            |> Array.maxBy snd
            
        let ones =
            battery.values[idx + 1..]
            |> Array.max
            
        tens * 10 + ones
        
    // ---- Part 2 ----

    let findLargestValueEx numDigits battery =
        let rec loop start digitsLeft digits =
            match digitsLeft with
            | 0 ->
                digits
                |> List.rev
                |> List.map string
                |> String.concat ""
                |> Int64.Parse
            | _ ->
                let idx, digit =
                    battery.values[start..battery.values.Length - digitsLeft]
                    |> Array.indexed
                    |> Array.maxBy snd
                loop (start + idx + 1) (digitsLeft - 1) (digit :: digits)

        loop 0 numDigits []
    

File.ReadAllText "2025/input-03.txt"
// Example.given
|> Problem.parse
|> Array.map (Problem.findLargestValueEx 12)
|> Array.sum
