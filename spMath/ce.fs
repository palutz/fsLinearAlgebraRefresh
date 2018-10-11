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
