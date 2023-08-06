// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO
printf "Day(1_1,1_2,2_1,2_2,3_1,3_2): "
let input = Console.ReadLine()

let readInput(day:int32,env:string) : string[]= File.ReadAllLines("../../../input/day"+day.ToString()+"-"+env+".txt")

let day1_1 ()= 
    let input=readInput(1,"game")
    let count= (input[0] |> Seq.filter (fun xs -> xs='(') |> Seq.length) - (input[0] |> Seq.filter (fun xs -> xs =')') |> Seq.length)
    Console.WriteLine(count.ToString());

let day1_2 () =
    let input=readInput(1,"game")
    let mutable floor= 0;
    let mutable i=0;
    while floor <> (-1) do
        if input[0][i]='(' then floor <- floor+1 else floor <- floor-1
        i <-i+1
    Console.WriteLine(i.ToString());

let day2_1 ()= 
    let input=readInput(2,"game")
    let mutable total=0
    for line in input do
        let dimensionsString=line.Split "x";
        let dimensions=[|Convert.ToInt32(dimensionsString[0]);Convert.ToInt32(dimensionsString[1]);Convert.ToInt32(dimensionsString[2])|]
        let sides=[|dimensions[0]*dimensions[1];dimensions[0]*dimensions[2];dimensions[1]*dimensions[2]|]
        let minimum=sides |> Array.min
        let size=Seq.fold(fun acc x ->acc+(x*2)) 0 sides
        total <- (total + size + minimum)
    Console.WriteLine(total.ToString());

let day2_2 ()= 
    let input=readInput(2,"game")
    let mutable total=0
    for line in input do
        let dimensionsString=line.Split "x"
        let dimensions=[|Convert.ToInt32(dimensionsString[0]);Convert.ToInt32(dimensionsString[1]);Convert.ToInt32(dimensionsString[2])|] |> Array.sort
        let wrap=dimensions[0]+dimensions[0]+dimensions[1]+dimensions[1]
        let bow=dimensions[0]*dimensions[1]*dimensions[2]
        total <- (total + wrap + bow )
    Console.WriteLine(total.ToString());

//Struct House used in day 3 to define set of coordinates(x,y) for each house visited by Santa
type House =
    struct
        val X: int32
        val Y: int32
        new(x: int32, y: int32) = { X = x; Y = y }
    end
let day3_1 ()= 
    let input=readInput(3,"game")
    let s=seq {
        let mutable x=0
        let mutable y=0
        yield House(x,y)
        for i in 0..input[0].Length-1 do
            match input[0][i] with
            | '>' -> x <-x+1
            | '<' -> x <-x-1
            | '^' -> y <-y+1
            | 'v' -> y <-y-1
            | _ -> Console.WriteLine("Wrong Input File")
            yield House(x,y)
    }
    let set= Set.ofSeq  s
    Console.WriteLine(set.Count)

let day3_2 ()= 
    let input=readInput(3,"game")
    let s=seq {
        let mutable x=0
        let mutable y=0
        let mutable x'=0
        let mutable y'=0
        yield House(x,y)
        for i in 0..2..input[0].Length-1 do
            match input[0][i] with
            | '>' -> x <-x+1
            | '<' -> x <-x-1
            | '^' -> y <-y+1
            | 'v' -> y <-y-1
            | _ -> Console.WriteLine("Wrong Input File")
            yield House(x,y)
            match input[0][i+1] with
            | '>' -> x' <-x'+1
            | '<' -> x' <-x'-1
            | '^' -> y' <-y'+1
            | 'v' -> y' <-y'-1
            | _ -> Console.WriteLine("Wrong Input File")
            yield House(x',y')
    }
    let set= Set.ofSeq  s
    Console.WriteLine(set.Count)

match input with
    | "1_1" -> day1_1()
    | "1_2" -> day1_2()
    | "2_1" -> day2_1()
    | "2_2" -> day2_2()
    | "3_1" -> day3_1()
    | "3_2" -> day3_2()
    | _ -> printfn "Wrong Input"

Console.ReadKey() |> ignore
