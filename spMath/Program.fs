open SPMath.Vector

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
