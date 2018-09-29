namespace SPMath

module Vector = 

  open System

  type spVector (c : float list) =
    let vect =
      match c with 
      | [] -> [0.0; 0.0]
      | [x] -> [x; 0.0]
      | _ -> c

    let dv = vect |> List.length

    member this.toList = vect

    member this.length = dv

    override this.GetHashCode() =
      hash (vect)

    override this.Equals y =
      match y with
      | :? spVector as v -> v.toList = vect
      | _ -> false

    override this.ToString () = sprintf "Vector: %A" vect



  let mathVect (f: float -> float -> float)(v1: spVector)(v2: spVector) : Result<spVector, string> = 
    if v1.length = v2.length then
      List.map2 f (v1.toList) (v2.toList)
      |> spVector
      |> Ok
    else 
      Error "Vectors with different dimension"

  // Add 2 vectors. If there are with diff dimensions return None
  let inline (+) (v1: spVector) (v2: spVector) : Result<spVector, string> = 
    mathVect (+) v1 v2

  let inline (-) (v1: spVector) (v2: spVector) : Result<spVector, string> = 
    mathVect (-) v1 v2

  let inline (*) (v1: spVector) a : spVector = 
    v1.toList 
    |> List.map (fun x -> x * a) 
    |> spVector
