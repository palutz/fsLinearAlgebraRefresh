namespace SPMath

module Vector = 
  type ResultBuilder<'a, 'b> () = 
    member this.Bind (e: Result<'a, 'b>, l: ('a -> Result<'c, 'b>)) : Result<'c, 'b> =
      match e with 
      | Error er -> Error er
      | Ok x -> x |> l

    member this.Return (x : 'a) : Result<'a, 'b> = 
      x |> Ok

    member this.ReturnFrom x =
      x

(*
  let addRB a b = 
    if (a > 0) && (b > 0) then
      a + b |> Ok
    else
      Error "errorrrrr"

  let a : Result<int, string> = 12 |> Ok
  let b : Result<int, string> = 0 |> Ok
  let rb = new ResultBuilder<int, string> ()
  rb {
    let! a1 = a
    let! b1 = b
    let! c = addRB a1 b1
    return c
  }

*)
module test = 

  let myFold<'a, 'b> (f: 'b -> 'a -> 'b) (state: 'b) (l : 'a list) : 'b =
    let rec inner acc l1 =
      match l1 with 
      | x :: xs -> inner (f acc x) xs
      | _ -> acc 

    inner state l 

  let ll = [1; 2; 3]
  ll |> myFold (fun s x -> s * x) 1

  // n -> repeat the action n times
  // f -> the function to apply (a folder)
  // zv -> Zero Value, the init value for the sequence
  // st -> the initial state of the action 
  let doitN (n: int) (f: 'b -> 'a -> 'b) (zv: 'a) (st: 'b) =
    if n > 0 then 
      Seq.init n (fun _ -> zv)
      |> Seq.fold f st
    else 
      st

  let revertIt (str: string) : string =
    str 
    |> Seq.map string
    |> Seq.rev
    |> Seq.fold (+) ""
    

  let act st n = revertIt n |> (+) st

  act "uno" "Steo"
  act "" "Steo"
  

  doitN 3 act "" "Steo"
  doitN<int> 2 (+) 0 
