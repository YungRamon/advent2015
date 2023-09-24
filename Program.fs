// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Collections.Generic

printf "Day(1_1,1_2,2_1,2_2,3_1,3_2,4_1,4_2,5_1,5_2,6_1,6_2,7_1,7_2,8_1,8_2,9_1,9_2,10_1,10_2,11_1,11_2,12_1,12_2,13_1,13_2): "
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

//Struct Route to define each route for Santa
type RouteFromTo =
    struct
        val From: String
        val To: String

        val distance: int32
        new(origin: string, termination: string, distance: int32) = { From = origin ; To = termination ; distance = distance}
    end
type FullRoute =
    struct
        val Cities: list<String>
        val TotalDistance: int
        new(origin: String, destination: String, distance: int32) = { Cities=[ origin; destination ] ; TotalDistance = distance}
        new(cities:list<String>, distance: int32) = { Cities=cities ; TotalDistance = distance}
    end
let day9_1 ()= 
    let input= readInput(9,"game")
    let routes= seq {for line in input -> new RouteFromTo(line.Split(" ")[0],line.Split(" ")[2],line.Split(" ")[4] |> int)}
    let cityExist cityList city = List.exists (fun x -> x = city) cityList
    let printRoute (cityList: string list) = cityList |> FSharp.Core.String.concat "-"
    let mutable count=1;
    let mutable lines=input.Length
    while lines>0 do
        lines <- lines - count
        count <- count + 1
    count <- count - 2
    let mutable fullRoutes= List.empty<FullRoute>
    for route: RouteFromTo in routes do
        fullRoutes <- [new FullRoute(route.From,route.To,route.distance)] |>  List.append fullRoutes
        fullRoutes <- [new FullRoute(route.To,route.From,route.distance)] |>  List.append fullRoutes
    while count>0 do
        count <- count - 1
        let iterationRoutes=[
            for fr in fullRoutes do
                for r in routes do
                    if ( String.Equals(fr.Cities.Item(fr.Cities.Length - 1), r.From)) && (not(cityExist fr.Cities r.To))then new FullRoute( [ r.To ] |> List.append fr.Cities, fr.TotalDistance + r.distance )
        ]
        let inverseIterationRoutes = [
            for fr in fullRoutes do
                for r in routes do
                    if ( String.Equals(fr.Cities.Item(fr.Cities.Length - 1), r.To)) && (not(cityExist fr.Cities r.From))then new FullRoute( [ r.From ] |> List.append fr.Cities, fr.TotalDistance + r.distance )
        ]
        fullRoutes <- iterationRoutes |> List.append inverseIterationRoutes
    let min= List.min (fullRoutes |> List.map (fun r -> r.TotalDistance))
    Console.WriteLine(min)

let day9_2 ()= 
    let input= readInput(9,"game")
    let routes= seq {for line in input -> new RouteFromTo(line.Split(" ")[0],line.Split(" ")[2],line.Split(" ")[4] |> int)}
    let cityExist cityList city = List.exists (fun x -> x = city) cityList
    let printRoute (cityList: string list) = cityList |> FSharp.Core.String.concat "-"
    let mutable count=1;
    let mutable lines=input.Length
    while lines>0 do
        lines <- lines - count
        count <- count + 1
    count <- count - 2
    let mutable fullRoutes= List.empty<FullRoute>
    for route: RouteFromTo in routes do
        fullRoutes <- [new FullRoute(route.From,route.To,route.distance)] |>  List.append fullRoutes
        fullRoutes <- [new FullRoute(route.To,route.From,route.distance)] |>  List.append fullRoutes
    while count>0 do
        count <- count - 1
        let iterationRoutes=[
            for fr in fullRoutes do
                for r in routes do
                    if ( String.Equals(fr.Cities.Item(fr.Cities.Length - 1), r.From)) && (not(cityExist fr.Cities r.To))then new FullRoute( [ r.To ] |> List.append fr.Cities, fr.TotalDistance + r.distance )
        ]
        let inverseIterationRoutes = [
            for fr in fullRoutes do
                for r in routes do
                    if ( String.Equals(fr.Cities.Item(fr.Cities.Length - 1), r.To)) && (not(cityExist fr.Cities r.From))then new FullRoute( [ r.From ] |> List.append fr.Cities, fr.TotalDistance + r.distance )
        ]
        fullRoutes <- iterationRoutes |> List.append inverseIterationRoutes
    let max= List.max (fullRoutes |> List.map (fun r -> r.TotalDistance))
    Console.WriteLine(max)

let day10 (iterations)=
    let input= readInput(10,"game")
    let mutable line=input[0];
    let mutable i = iterations
    while i > 0 do
        let rx = new RegularExpressions.Regex(@"(\d)\1*",RegularExpressions.RegexOptions.Compiled)
        let matches= rx.Matches line
        let stringSequence = seq {
            for matcher in matches do
                yield (string matcher.Value.Length + matcher.Value.[0].ToString())
        }
        line <- String.Concat stringSequence
        i <- i - 1
    Console.WriteLine line.Length

let day11(iterations)=
    let input= readInput(11,"game")[0]
    let mutable line = input
    let rxZ= new RegularExpressions.Regex(@"z+$",RegularExpressions.RegexOptions.Compiled)
    //Three Letter Sequence
    let rxTLS= new RegularExpressions.Regex(@"(abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz)",RegularExpressions.RegexOptions.Compiled)
    //Not i o l
    let rxNotIOL = new RegularExpressions.Regex(@"^[^iol]+$",RegularExpressions.RegexOptions.Compiled)
    //Two pairs
    let rxPair = new RegularExpressions.Regex(@"^.*(([a-z])\2).*(?!\1)(([a-z])\4).*$",RegularExpressions.RegexOptions.Compiled)
    let addOneToPass pass= 
        let length= rxZ.Match(pass).Value.Length + 1
        let ch=char ((int pass[pass.Length - length]) + 1)
        pass.Substring(0 , pass.Length - length) + ch.ToString() + String.replicate (length-1) "a"
    let mutable i = iterations
    while i > 0 do
        i <- i-1
        line <- addOneToPass line
        while not(rxTLS.IsMatch(line) && rxNotIOL.IsMatch(line) && rxPair.IsMatch(line)) do
            line <- addOneToPass line
    Console.WriteLine line

let day12_1()=
    let input= readInput(12,"game")
    let rxN = new RegularExpressions.Regex(@"-?\d+",RegularExpressions.RegexOptions.Compiled)
    let seqNumeros= seq{
        for matcher in rxN.Matches(input[0]) do
        int matcher.Value 
    }
    let count= seqNumeros |> Seq.sum
    Console.WriteLine count

//Struct ValueJSON to define each value in array
type ValueJSON=
    struct
        val lvl: int
        val obj: bool
        val content: int
        val id: string
        val red: bool
        new(lvl: int,obj: bool,content: int,id: string, red: bool) = {lvl=lvl; obj=obj; content=content; id=id; red=red}
    end
let day12_2()=
    let input= readInput(12,"game")
    let mutable inputPrep=RegularExpressions.Regex.Replace(RegularExpressions.Regex.Replace(input[0], @":""red""", ":n"), @""".*?""", "0")
    let mutable values: ValueJSON list = []
    let mutable lvl = 0
    let mutable obj = false 
    let mutable id=0
    let mutable idCount=0
    let mutable prevIDs: int list=[0]
    let mutable prevobjs: bool list=[false]
    while inputPrep.Length > 0 do
        match inputPrep[0] with
            | '{' -> 
                lvl <- lvl + 1
                prevobjs <- [obj] |> List.append prevobjs
                obj<-true
                prevIDs <- [id] |> List.append prevIDs
                idCount <- idCount + 1
                id <- idCount
                inputPrep <- inputPrep.Substring 1
            | '}' ->
                lvl <- lvl - 1
                obj <- prevobjs |> List.last
                prevobjs <- prevobjs[..(prevobjs.Length - 2)]
                id <- prevIDs |> List.last
                prevIDs <- prevIDs[..(prevIDs.Length - 2)]
                inputPrep <- inputPrep.Substring 1
            | '[' ->
                lvl <- lvl + 1
                prevobjs <- [obj] |> List.append prevobjs
                obj<-false
                inputPrep <- inputPrep.Substring 1
                prevIDs <- [id] |> List.append prevIDs
                idCount <- idCount + 1
                id <- idCount
            | ']' ->
                lvl <- lvl - 1
                obj <- prevobjs |> List.last
                prevobjs <- prevobjs[..(prevobjs.Length - 2)]
                id <- prevIDs |> List.last
                prevIDs <- prevIDs[..(prevIDs.Length - 2)]
                inputPrep <- inputPrep.Substring 1
            | ',' -> inputPrep <- inputPrep.Substring 1
            | _ ->
                if obj then
                    inputPrep <- inputPrep.Substring(1 + (inputPrep.IndexOf ':'))
                    if inputPrep[0] <> '{' && inputPrep[0] <> '[' then
                        let matcher = RegularExpressions.Regex.Match (inputPrep, @"((-?[0-9]+)|n)")
                        let content = if matcher.Value = "n" then 0 else  (matcher.Value |> int)
                        inputPrep <- inputPrep.Substring(matcher.Value.Length)
                        let idJSON = String.Join ("-", (prevIDs |> List.map(fun id -> id |> string))) + "-" + (id |> string)
                        values <- [new ValueJSON(lvl,obj,content,idJSON,(matcher.Value = "n"))] |> List.append values
                else 
                    let matcher = RegularExpressions.Regex.Match (inputPrep, @"-?[0-9]+")
                    let content = matcher.Value |> int
                    inputPrep <- inputPrep.Substring(matcher.Value.Length)
                    let idJSON = String.Join ("-", (prevIDs |> List.map(fun id -> id |> string))) + "-" + (id |> string)
                    values <- [new ValueJSON(lvl,obj,content,idJSON,false)] |> List.append values
    let redList= values |> List.filter ( fun v -> v.red)
    for valueRed in redList do
        values <- values |> List.filter (fun n -> not(n.id.StartsWith(valueRed.id)))
    let sum = values |> List.fold( fun total n -> total + n.content) 0
    Console.WriteLine sum

//Struct Relationship respresnts points gained or lost from certain relationship
type Relationship=
    struct
        val person: String
        val price: int32
        val relation: String

        new (person: String, price: int32, relation: String) = { person = person; price = price; relation = relation}
    end
let day13_1()= 
    let lines = readInput(13,"game")
    let prepLines= [ for line in lines do line.Substring(0,line.Length - 1).Split(" ") ]
    let peeps = prepLines |> List.map (fun line -> line[0]) |> List.distinct
    let rels = prepLines |> List.map (fun line -> new Relationship(line[0],(if line[2]="lose" then (- (line[3] |> int32) ) else (line[3] |> int32)),line[10]))
    let rec lister ( list: string list ) = 
        if list.Length = 1 then seq{[list[0]]}
        else
            seq {
                for i = 0 to list.Length - 1 do
                    for ls in ((list[..i-1] @ list[i+1..]) |> lister) do
                        [list[i]] @ ls
            }
    let orders = lister peeps
    let mutable max = 0 
    for order in orders do 
        let relationships = seq{
            if order.Length > 1 then
                yield rels |> List.find ( fun rel -> rel.person = order[0] && rel.relation=order[order.Length - 1])
                yield rels |> List.find ( fun rel -> rel.person = order[0] && rel.relation=order[1])
                yield rels |> List.find ( fun rel -> rel.person = order[order.Length - 1] && rel.relation=order[order.Length - 2])
                yield rels |> List.find ( fun rel -> rel.person = order[order.Length - 1] && rel.relation=order[0])
                if order.Length - 2 > 1 then
                    for i = 1 to order.Length - 2 do
                        yield rels |> List.find ( fun rel -> rel.person = order[i] && rel.relation=order[i - 1])
                        yield rels |> List.find ( fun rel -> rel.person = order[i] && rel.relation=order[i + 1])
        }
        let sum = relationships |> Seq.fold (fun acc rel -> acc + rel.price) 0
        if sum > max then max <- sum
    Console.WriteLine max

let day13_2()= 
    let lines = readInput(13,"game")
    let prepLines= [ for line in lines do line.Substring(0,line.Length - 1).Split(" ") ]
    let peeps = (prepLines |> List.map (fun line -> line[0]) |> List.distinct) @ ["Me"]
    let rels = (prepLines |> List.map (fun line -> new Relationship(line[0],(if line[2]="lose" then (- (line[3] |> int32) ) else (line[3] |> int32)),line[10]))) @ (peeps |> List.fold(fun acc peep -> acc @ [new Relationship("Me",0,peep);new Relationship(peep,0,"Me")]) ([new Relationship("Me",0,"Me")]))
    let rec lister ( list: string list ) = 
        if list.Length = 1 then seq{[list[0]]}
        else
            seq {
                for i = 0 to list.Length - 1 do
                    for ls in ((list[..i-1] @ list[i+1..]) |> lister) do
                        [list[i]] @ ls
            }
    let orders = lister peeps
    let mutable max = 0 
    for order in orders do 
        let relationships = seq{
            if order.Length > 1 then
                yield rels |> List.find ( fun rel -> rel.person = order[0] && rel.relation=order[order.Length - 1])
                yield rels |> List.find ( fun rel -> rel.person = order[0] && rel.relation=order[1])
                yield rels |> List.find ( fun rel -> rel.person = order[order.Length - 1] && rel.relation=order[order.Length - 2])
                yield rels |> List.find ( fun rel -> rel.person = order[order.Length - 1] && rel.relation=order[0])
                if order.Length - 2 > 1 then
                    for i = 1 to order.Length - 2 do
                        yield rels |> List.find ( fun rel -> rel.person = order[i] && rel.relation=order[i - 1])
                        yield rels |> List.find ( fun rel -> rel.person = order[i] && rel.relation=order[i + 1])
        }
        let sum = relationships |> Seq.fold (fun acc rel -> acc + rel.price) 0
        if sum > max then max <- sum
    Console.WriteLine max

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
    | "9_1" -> day9_1()
    | "9_2" -> day9_2()
    | "10_1" -> day10(40)
    | "10_2" -> day10(50)
    | "11_1" -> day11(1)
    | "11_2" -> day11(2)
    | "12_1" -> day12_1()
    | "12_2" -> day12_2()
    | "13_1" -> day13_1()
    | "13_2" -> day13_2()
    | _ -> printfn "Wrong Input"

Console.ReadKey() |> ignore
