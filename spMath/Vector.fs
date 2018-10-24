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

    let mgntd : float =  // magnitude,
      vect 
      |> List.fold(fun acc x -> x * x + acc) 0.0
      |> sqrt

    // convert the Vector to a List of float
    member this.ToList : float list = vect

    // number of element in the vector
    member this.length : int = dv

    // Times scalar - return a new vector as a result
    // of this vector times a scalar value
    member this.TimesScalar (a : float) : spVector = 
      match mgntd with
      | 0.0 -> this
      | _ -> vect 
            |> List.map (fun x -> x * a) 
            |> spVector

    // Negate all the element of the vector
    member this.Negate : spVector = 
      match mgntd with 
      | 0.0 -> this 
      | _ -> this.TimesScalar (-1.0)

    // calculate the magnitude of the vector
    // sqrt (sum of elem_i^2) 
    member this.Magnitude : float = mgntd

    // Normalize the vector (multiply every element of the vector
    // by 1 / magnitude) 
    member this.Normalize : Result<spVector,string> =
      match mgntd with
      | 0.0 ->  Error "Cannot normalize 0 vector"
      | _ ->  this.TimesScalar (1.0 / mgntd) |> Ok

    // calculate the unit vector in the direction of the vector
    // (1 / magnitude) * vector
    member this.Direction : Result<spVector, string> = 
      match mgntd with 
      | 0.0 -> Error "Vector with magnitude = 0.0"
      | _ ->  vect 
              |> List.map (fun x -> x * 1.0 / mgntd)
              |> spVector
              |> Ok

    member this.IsParallel (other: spVector) : bool = 
      match mgntd, other.Magnitude with
      | 0.0, 0.0 -> true
      | _, 0.0 -> true
      | 0.0, _ -> true
      | _ , _ -> (vect.Head / other.ToList.Head) = (vect |> List.sum) / (other.ToList |> List.sum)
      // check if there are both not 0 -> if at least is zero they are parallel (return true)
      // calculate the ratio between the 2 heads
      // sum and check that the ratio is the same 

    // --- Override methods --- 
    override this.GetHashCode() =
      hash (vect)

    override this.Equals y : bool =
      match y with
      | :? spVector as v -> v.ToList = vect
      | _ -> false

    override this.ToString () : string = sprintf "Vector: %A" vect

  // ------ type spVector end of code ------


  let fVect (f: float -> float -> float)(v1: spVector)(v2: spVector) : Result<spVector, string> = 
    if v1.length = v2.length then
      List.map2 f (v1.ToList) (v2.ToList)
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
    v1.TimesScalar a

  // Negate all the elements in the vector
  let inline (~-.) (v: spVector) : spVector = 
    v.Negate

  let inline ( *.*) (v1: spVector) (v2: spVector) : Result<spVector, string> = 
    fVect (*) v1 v2

  //let inline ( /./ )  (v1: spVector) (v2: spVector) : bool =
    //let a b = v1.toList.

