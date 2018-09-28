open System
open MathNet.Numerics.LinearAlgebra

[<EntryPoint>]
let main argv =
  let v = vector [1.2; 2.0]
  printfn "Vector %A" v
  0 // return an integer exit code
