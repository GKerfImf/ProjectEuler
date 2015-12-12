//Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
//If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
//For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
//Evaluate the sum of all the amicable numbers under 10000.

let fi n = 
    List.filter (fun x -> n % x = 0) [1..n-1]

let d n = 
    n |> fi |> List.sum 

let solve n =
    let ndn = [1..n] |> List.map (fun x -> (x, d x)) |> List.filter (fun (x,y) -> x <> y && y <> 1)
    let revndn = ndn |> List.map (fun (x,y) -> (y,x))  

    revndn |> List.map (fun (y,dy) -> ndn |> List.tryFind (fun (x,dx) -> (x,dx) = (y,dy))) 
        |> List.filter (fun x -> x <> Option.None) |> List.map (Option.get) 
        |> List.map (Operators.fst) |> List.sum    
 
[<EntryPoint>]
let main argv = 
    10000 |> solve |> printfn "%A"
    0 // return an integer exit code