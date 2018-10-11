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



  let fVect (f: float -> float -> float)(v1: spVector)(v2: spVector) : Result<spVector, string> = 
    if v1.length = v2.length then
      List.map2 f (v1.toList) (v2.toList)
      |> spVector
      |> Ok
    else 
      Error "Vectors with different dimension"

  // Add 2 vectors. If there are with diff dimensions return None
  let inline (+.) (v1: spVector) (v2: spVector) : Result<spVector, string> = 
    fVect (+) v1 v2

  // Subtract 2 vectors. If there are with diff dimensions return None
  let inline (-.) (v1: spVector) (v2: spVector) : Result<spVector, string> = 
    fVect (-) v1 v2

  // Scalar moltiplication
  let inline ( *.) (v1: spVector) a : spVector = 
    v1.toList 
    |> List.map (fun x -> x * a) 
    |> spVector

  // Negate all the elements in the vector
  let inline (~-.) (v: spVector) : spVector = 
    v *. (-1.0)

  // Inner product
  let inline ( *.*) (v1: spVector) (v2: spVector) : Result<spVector, string> = 
    fVect (*) v1 v2

  // dot product
  let inline ( *.. ) (v1: spVector) (v2: spVector) : Result<float, string> = 
    let vr = v1 *.* v2
    match vr with
    | Ok r -> r.toList |> List.sum |> Ok
    | Error e -> Error e

  // calculate the angle between 2 vectors (in radiants)
  let inline ( <.> ) (v1: spVector) (v2: spVector) : Result<float, string> = 
    let denom = v1.magnitude * v2.magnitude
    if denom > 0.0 then
      let numer = v1 *.. v2
      match numer with 
      | Ok x -> Math.Acos (x / denom) |> Ok
      | Error e -> Error e
    else 
      Error "0 vector detected"

  //calculate the angle between 2 vector in
  let inline ( <.~> ) (v1: spVector) (v2: spVector) : Result<float, string> = 
    match (v1 <.> v2) with
    | Error e -> Error e
    | Ok x when x <> 0.0 -> (x * 180.0 / Math.PI) |> Ok
    | Ok x when x = 0.0 -> Ok x