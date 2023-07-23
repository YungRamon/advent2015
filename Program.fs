// For more information see https://aka.ms/fsharp-console-apps
open System
printf "Day(yet none): "
let input = Console.ReadLine()

match input with
    | _ -> printfn "Wrong Input"

Console.ReadKey() |> ignore
