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

    let mgntd : float =  // magnitude, Calculate only once
      vect 
      |> List.fold(fun acc x -> x * x + acc) 0.0
      |> sqrt

    let negateIt : spVector = // the negate of the vector
      this.timesScalar (-1.0)

    // convert the Vector to a List of float
    member this.toList : float list = vect

    // number of element in the vector
    member this.length : int = dv

    // Times scalar - return a new vector as a result
    // of this vector times a scalar value
    member this.timesScalar (a : float) : spVector = 
      vect 
      |> List.map (fun x -> x * a) 
      |> spVector

    // Negate all the element of the vector
    member this.negate : spVector = negateIt

    // calculate the magnitude of the vector
    // sqrt (sum of elem_i^2) 
    member this.magnitude : float = mgntd

    // Normalize the vector (multiply every element of the vector
    // by 1 / magnitude) 
    member this.normalize : Result<spVector,string> =
      let m = this.magnitude
      match m with 
      | 0.0 ->  Error "Cannot normalize 0 vector"
      | _ ->  this.timesScalar (1.0 / m) |> Ok

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

    member this.IsParallel (other: spVector) : bool = 
      let h = vect.tryH
      let h1 = other.toList.tr head |> Option
      match
      // check if there are both not 0 -> if at least is zero they are parallel (return true)
      // calculate the ratio between the 2 heads
      // sum and check that the ratio is the same 

    // --- Override methods --- 
    override this.GetHashCode() =
      hash (vect)

    override this.Equals y : bool =
      match y with
      | :? spVector as v -> v.toList = vect
      | _ -> false

    override this.ToString () : string = sprintf "Vector: %A" vect

  // ------ type spVector end of code ------


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
    v1.timesScalar a

  // Negate all the elements in the vector
  let inline (~-.) (v: spVector) : spVector = 
    v.negate

  let inline ( *.*) (v1: spVector) (v2: spVector) : Result<spVector, string> = 
    fVect (*) v1 v2

  //let inline ( /./ )  (v1: spVector) (v2: spVector) : bool =
    //let a b = v1.toList.

