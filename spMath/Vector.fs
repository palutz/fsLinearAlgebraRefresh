namespace SPMath

module Vector = 

  open System

  type spVector (c : float list) =
    let vect =
      match c with 
      | [] -> [0.0; 0.0]
      | [x] -> [x; 0.0]
      | _ -> c

    let dv : int = vect |> List.length

    member this.toList : float list = vect

    member this.length : int = dv

    // calculate the magnitude of the vector
    // sqrt (sum of elem_i^2) 
    member this.magnitude : float = 
      vect 
      |> List.fold(fun acc x -> x * x + acc) 0.0
      |> sqrt

    // calculate the unit vector in the direction of the vector
    // (1 / magnitude) * vector
    member this.direction : Result<spVector, string> = 
      let m = this.magnitude
      match m with 
      | 0.0 -> Error "Vector with magnitude = 0.0"
      | _ ->  vect 
              |> List.map (fun x -> x * 1.0 / m)
              |> spVector
              |> Ok

    // --- Override methods --- 
    override this.GetHashCode() =
      hash (vect)

    override this.Equals y : bool =
      match y with
      | :? spVector as v -> v.toList = vect
      | _ -> false

    override this.ToString () : string = sprintf "Vector: %A" vect

    static member fVect (f: float -> float -> float)(v1: spVector)(v2: spVector) : Result<spVector, string> = 
      if v1.length = v2.length then
        List.map2 f (v1.toList) (v2.toList)
        |> spVector
        |> Ok
      else 
        Error "Vectors with different dimension"
      
    // Add 2 vectors. If there are with diff dimensions return None
    static member (+.) (v1: spVector) (v2: spVector) : Result<spVector, string> = 
      fVect (+) v1 v2

    // Subtract 2 vectors. If there are with diff dimensions return None
    static member (-.) (v1: spVector) (v2: spVector) : Result<spVector, string> = 
      fVect (-) v1 v2

    // Scalar moltiplication
    static member ( *.) (v1: spVector) a : spVector = 
      v1.toList 
      |> List.map (fun x -> x * a) 
      |> spVector

    // Negate all the elements in the vector
    static member (~-.) (v: spVector) : spVector = 
      v *. (-1.0)

    static member ( *.*) (v1: spVector) (v2: spVector) : Result<spVector, string> = 
      fVect (*) v1 v2
