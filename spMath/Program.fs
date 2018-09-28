open System

type spVector (c : float list) =
  let vect =
    match c with 
    | [] -> [0.0; 0.0]
    | [x] -> [x; 0.0]
    | _ -> c

  member this.ToList () =
    vect

  override x.GetHashCode() =
    hash (vect)

  override x.Equals y =
    match y with
    | :? spVector as v -> v.ToList() = vect
    | _ -> false

  override x.ToString () = sprintf "Vector: %A" vect


[<EntryPoint>]
let main argv =
  let sv = spVector [1.;2.]
  let sv1 = spVector [1.;2.]
  let sv2 = spVector [1.]
  if sv = sv1 then "true" else "false"
  |> printfn "sv and sv1 %s"
  if sv = sv2 then "true" else "false"
  |> printfn "sv and sv2 %s"
  printfn "sv %s, sv2 %s" (sv.ToString()) (sv2.ToString())
  0 // return an integer exit code
