// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Collections.Generic
printf "Day(1_1,1_2,2_1,2_2,3_1,3_2,4_1,4_2,5_1,5_2,6_1,6_2,7_1,7_2,8_1,8_2): "
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

let day4_1 ()= 
    let input= readInput(4,"game")[0]
    use md5=MD5.Create()
    let mutable i=1
    let s()=input + i.ToString()
    while not (((StringBuilder(), md5.ComputeHash(Encoding.ASCII.GetBytes(s()))) ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))|> string)[0..4]="00000") do
        i <- i+1
    Console.WriteLine(i.ToString())

let day4_2 ()= 
    let input= readInput(4,"game")[0]
    use md5=MD5.Create()
    let mutable i=1
    let s()=input + i.ToString()
    while not (((StringBuilder(), md5.ComputeHash(Encoding.ASCII.GetBytes(s()))) ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))|> string)[0..5]="000000") do
        i <- i+1
    Console.WriteLine(i.ToString())

let day5_1 ()= 
    let input= readInput(5,"game")
    let rx=new RegularExpressions.Regex(@"^(?=.*([aeiou]).*([aeiou]).*([aeiou]).*)(?!.*(ab|cd|pq|xy).*).*(.)\5.*$",RegularExpressions.RegexOptions.Compiled)
    let mutable count=0
    for i in input do
        if rx.IsMatch(i) then
            count <- count+1
    Console.WriteLine(count.ToString())

let day5_2 ()= 
    let input= readInput(5,"game")
    let rx=new RegularExpressions.Regex(@"^(?=.*(..).*\1.*).*(.).\2.*$",RegularExpressions.RegexOptions.Compiled)
    let mutable count=0
    for i in input do
        if rx.IsMatch(i) then
            count <- count+1
    Console.WriteLine(count.ToString())

let day6_1 ()= 
    let input= readInput(6,"game")
    let grid= Array2D.create 1000 1000 false
    for i in input do
        let split= if i.StartsWith("turn") then i.Substring(5).Split(" ") else i.Split(" ")
        let change= split[0]
        let x1=Convert.ToInt32(split[1].Split(",")[0]) 
        let y1=Convert.ToInt32(split[1].Split(",")[1]) 
        let x2=Convert.ToInt32(split[3].Split(",")[0]) 
        let y2=Convert.ToInt32(split[3].Split(",")[1]) 
        for x = x1 to x2 do
            for y = y1 to y2 do
                grid.[x,y] <- match change with | "on" -> true | "off" -> false | _ -> grid.[x,y] <> true
    let total=grid |> Seq.cast<bool> |> Seq.fold (fun acc x ->if x then acc+1 else acc) 0
    Console.WriteLine(total.ToString())

let day6_2 ()= 
    let input= readInput(6,"game")
    let grid= Array2D.create 1000 1000 0
    for i in input do
        let split= if i.StartsWith("turn") then i.Substring(5).Split(" ") else i.Split(" ")
        let change= split[0]
        let x1=Convert.ToInt32(split[1].Split(",")[0]) 
        let y1=Convert.ToInt32(split[1].Split(",")[1]) 
        let x2=Convert.ToInt32(split[3].Split(",")[0]) 
        let y2=Convert.ToInt32(split[3].Split(",")[1]) 
        for x = x1 to x2 do
            for y = y1 to y2 do
                grid.[x,y] <- match change with | "on" -> grid[x,y]+1 | "off" -> Math.Max(0,grid[x,y]-1) | _ -> grid[x,y]+2
    let total=grid |> Seq.cast<int> |> Seq.fold (fun acc x ->acc+x) 0
    Console.WriteLine(total.ToString())

let day7_1 ()= 
    let input= readInput(7,"game")
    let dic: Dictionary<string,Func<uint16>>= new Dictionary<string,Func<uint16>>()
    for i in input do
        let equation=i.Split(" -> ")
        match equation[0] with
        | _ when equation[0] |> Seq.forall Char.IsDigit ->
            let n=Convert.ToUInt16(equation[0])
            dic.Add(equation[1],fun ()-> n)
        | _ when equation[0].Contains("AND") -> 
            let first=equation[0].Split(" ")[0]
            let second=equation[0].Split(" ")[2]
            if first |> Seq.forall Char.IsDigit then
                let n=Convert.ToUInt16(first)
                dic.TryAdd(first,fun ()-> n) |> ignore
            if second |> Seq.forall Char.IsDigit then
                let n=Convert.ToUInt16(second)
                dic.TryAdd(second,fun ()-> n) |> ignore
            dic.Add(equation[1],fun ()-> 
                let n'=dic.Item(first).Invoke() &&& dic.Item(second).Invoke()
                dic.Remove(equation[1]) |> ignore
                dic.Add(equation[1],fun ()-> n')
                n'
                )
        | _ when equation[0].Contains("OR") -> 
            let first=equation[0].Split(" ")[0]
            let second=equation[0].Split(" ")[2]
            if first |> Seq.forall Char.IsDigit then
                let n=Convert.ToUInt16(first)
                dic.TryAdd(first,fun ()-> n) |> ignore
            if second |> Seq.forall Char.IsDigit then
                let n=Convert.ToUInt16(second)
                dic.TryAdd(second,fun ()-> n) |> ignore
            dic.Add(equation[1],fun ()-> 
                let n'= dic.Item(first).Invoke() ||| dic.Item(second).Invoke()
                dic.Remove(equation[1]) |> ignore
                dic.Add(equation[1],fun ()-> n')
                n'
                )
        | _ when equation[0].Contains("LSHIFT") -> 
            let first=equation[0].Split(" ")[0]
            let second=Convert.ToInt32(equation[0].Split(" ")[2])
            if first |> Seq.forall Char.IsDigit then
                let n=Convert.ToUInt16(first)
                dic.TryAdd(first,fun ()-> n) |> ignore
            dic.Add(equation[1],fun ()-> 
                let n' =dic.Item(first).Invoke() <<< second 
                dic.Remove(equation[1]) |> ignore
                dic.Add(equation[1],fun ()-> n')
                n'
                )
        | _ when equation[0].Contains("RSHIFT") -> 
            let first=equation[0].Split(" ")[0]
            let second=Convert.ToInt32(equation[0].Split(" ")[2])
            if first |> Seq.forall Char.IsDigit then
                let n=Convert.ToUInt16(first)
                dic.TryAdd(first,fun ()-> n) |> ignore
            dic.Add(equation[1],fun ()-> 
                let n'= dic.Item(first).Invoke() >>> second 
                dic.Remove(equation[1]) |> ignore
                dic.Add(equation[1],fun ()-> n')
                n'
                )
        | _ when equation[0].Contains("NOT") -> 
            let value=equation[0].Split(" ")[1]
            if value |> Seq.forall Char.IsDigit then
                let n=Convert.ToUInt16(value)
                dic.TryAdd(value,fun ()-> n) |> ignore
            dic.Add(equation[1],fun ()-> 
                let n' = ~~~ dic.Item(value).Invoke() 
                dic.Remove(equation[1]) |> ignore
                dic.Add(equation[1],fun ()-> n')
                n'
                )
        | _ -> dic.Add(equation[1],fun ()-> 
            let n'= dic.Item(equation[0]).Invoke()
            dic.Remove(equation[1]) |> ignore
            dic.Add(equation[1],fun ()-> n')
            n'
            )
    
    Console.WriteLine(dic.Item("a").Invoke())

let day7_2 ()= 
    let input= readInput(7,"game")
    let generateDictionary(input:string array)=
        let dic: Dictionary<string,Func<uint16>>= new Dictionary<string,Func<uint16>>()
        for i in input do
            let equation=i.Split(" -> ")
            match equation[0] with
            | _ when equation[0] |> Seq.forall Char.IsDigit ->
                let n=Convert.ToUInt16(equation[0])
                dic.Add(equation[1],fun ()-> n)
            | _ when equation[0].Contains("AND") -> 
                let first=equation[0].Split(" ")[0]
                let second=equation[0].Split(" ")[2]
                if first |> Seq.forall Char.IsDigit then
                    let n=Convert.ToUInt16(first)
                    dic.TryAdd(first,fun ()-> n) |> ignore
                if second |> Seq.forall Char.IsDigit then
                    let n=Convert.ToUInt16(second)
                    dic.TryAdd(second,fun ()-> n) |> ignore
                dic.Add(equation[1],fun ()-> 
                    let n'=dic.Item(first).Invoke() &&& dic.Item(second).Invoke()
                    dic.Remove(equation[1]) |> ignore
                    dic.Add(equation[1],fun ()-> n')
                    n'
                    )
            | _ when equation[0].Contains("OR") -> 
                let first=equation[0].Split(" ")[0]
                let second=equation[0].Split(" ")[2]
                if first |> Seq.forall Char.IsDigit then
                    let n=Convert.ToUInt16(first)
                    dic.TryAdd(first,fun ()-> n) |> ignore
                if second |> Seq.forall Char.IsDigit then
                    let n=Convert.ToUInt16(second)
                    dic.TryAdd(second,fun ()-> n) |> ignore
                dic.Add(equation[1],fun ()-> 
                    let n'= dic.Item(first).Invoke() ||| dic.Item(second).Invoke()
                    dic.Remove(equation[1]) |> ignore
                    dic.Add(equation[1],fun ()-> n')
                    n'
                    )
            | _ when equation[0].Contains("LSHIFT") -> 
                let first=equation[0].Split(" ")[0]
                let second=Convert.ToInt32(equation[0].Split(" ")[2])
                if first |> Seq.forall Char.IsDigit then
                    let n=Convert.ToUInt16(first)
                    dic.TryAdd(first,fun ()-> n) |> ignore
                dic.Add(equation[1],fun ()-> 
                    let n' =dic.Item(first).Invoke() <<< second 
                    dic.Remove(equation[1]) |> ignore
                    dic.Add(equation[1],fun ()-> n')
                    n'
                    )
            | _ when equation[0].Contains("RSHIFT") -> 
                let first=equation[0].Split(" ")[0]
                let second=Convert.ToInt32(equation[0].Split(" ")[2])
                if first |> Seq.forall Char.IsDigit then
                    let n=Convert.ToUInt16(first)
                    dic.TryAdd(first,fun ()-> n) |> ignore
                dic.Add(equation[1],fun ()-> 
                    let n'= dic.Item(first).Invoke() >>> second 
                    dic.Remove(equation[1]) |> ignore
                    dic.Add(equation[1],fun ()-> n')
                    n'
                    )
            | _ when equation[0].Contains("NOT") -> 
                let value=equation[0].Split(" ")[1]
                if value |> Seq.forall Char.IsDigit then
                    let n=Convert.ToUInt16(value)
                    dic.TryAdd(value,fun ()-> n) |> ignore
                dic.Add(equation[1],fun ()-> 
                    let n' = ~~~ dic.Item(value).Invoke() 
                    dic.Remove(equation[1]) |> ignore
                    dic.Add(equation[1],fun ()-> n')
                    n'
                    )
            | _ -> dic.Add(equation[1],fun ()-> 
                let n'= dic.Item(equation[0]).Invoke()
                dic.Remove(equation[1]) |> ignore
                dic.Add(equation[1],fun ()-> n')
                n'
                )
        dic
    let dic=generateDictionary(input)
    let b=dic.Item("a").Invoke()
    let bString=b.ToString() + " -> b"
    let ib= Array.FindIndex (input,fun e-> e.EndsWith(" b"))
    input.SetValue(bString,ib)
    let dic'=generateDictionary(input)
    Console.WriteLine(dic'.Item("a").Invoke())

let day8_1 ()= 
    let input= readInput(8,"game")
    let mutable total= 0
    for i in input do
        let i2=RegularExpressions.Regex.Replace(i.Substring(1,i.Length-2),"(\\\\\"|\\\\x[0-9a-f][0-9a-f]|\\\\\\\\)","a")
        total <- total + i.Length - i2.Length
    Console.WriteLine(total.ToString())

let day8_2 ()= 
    let input= readInput(8,"game")
    let mutable total= 0
    for i in input do
        let i2=RegularExpressions.Regex.Replace(i,"(\"|\\\\)","aa")
        total <- total + i2.Length + 2 - i.Length
    Console.WriteLine(total.ToString())

match input with
    | "1_1" -> day1_1()
    | "1_2" -> day1_2()
    | "2_1" -> day2_1()
    | "2_2" -> day2_2()
    | "3_1" -> day3_1()
    | "3_2" -> day3_2()
    | "4_1" -> day4_1()
    | "4_2" -> day4_2()
    | "5_1" -> day5_1()
    | "5_2" -> day5_2()
    | "6_1" -> day6_1()
    | "6_2" -> day6_2()
    | "7_1" -> day7_1()
    | "7_2" -> day7_2()
    | "8_1" -> day8_1()
    | "8_2" -> day8_2()
    | _ -> printfn "Wrong Input"

Console.ReadKey() |> ignore
