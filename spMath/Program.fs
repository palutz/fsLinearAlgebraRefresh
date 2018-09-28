open System

type spVector (c : float list) =
  let vect =
    match c with 
    | [] -> [0.0; 0.0]
    | _ -> c


  override x.ToString () = sprintf "Vector: %A" vect


[<EntryPoint>]
let main argv =
  let sv = spVector [1.;2.]
  sv.ToString() |> printfn "%s"
  0 // return an integer exit code
