open System
open FsAlg.Generic

[<EntryPoint>]
let main argv =
  let v = vector [1.; 2.]
  printfn "vector %A" v
  0 // return an integer exit code
