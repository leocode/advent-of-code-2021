open Common.ActivePatterns

type Position = {
  Depth: int
  Horizontal: int
}

type AimedPosition = {
  Depth: int
  Horizontal: int
  Aim: int
}

[<EntryPoint>]
let main argv =
  let commands =
    System.IO.File.ReadLines "input.txt"
    |> Seq.toList

  let part1Result =
    commands
    |> List.fold (fun (position: Position) -> function
      | Regex "forward (\d+)" [Integer move] -> { position with Horizontal = position.Horizontal + move }
      | Regex "up (\d+)" [Integer move] -> { position with Depth = position.Depth - move }
      | Regex "down (\d+)" [Integer move] -> { position with Depth = position.Depth + move }
      | _ -> position
    ) { Depth = 0; Horizontal = 0 }
    |> fun position -> position.Depth * position.Horizontal
  
  let part2Result =
    commands
    |> List.fold (fun (position: AimedPosition) -> function
      | Regex "forward (\d+)" [Integer move] -> { position with Horizontal = position.Horizontal + move; Depth = position.Depth + position.Aim * move }
      | Regex "up (\d+)" [Integer move] -> { position with Aim = position.Aim - move }
      | Regex "down (\d+)" [Integer move] -> { position with Aim = position.Aim + move }
      | _ -> position
    ) { Depth = 0; Horizontal = 0; Aim = 0 }
    |> fun position -> position.Depth * position.Horizontal

  printfn "Part 1: %d" part1Result
  printfn "Part 2: %d" part2Result
  
  0