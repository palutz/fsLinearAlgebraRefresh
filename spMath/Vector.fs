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

    member this.ToList () = vect

    member this.Dimension = dv

    // Add 2 vectors. If there are with diff dimensions return None
    static member (+) (v1: spVector, v2: spVector) = 
      if v1.Dimension = v2.Dimension then
        List.map2 (+) (v1.ToList()) (v2.ToList())
      else
        failwith "Vectors with different dimension" 

    override this.GetHashCode() =
      hash (vect)

    override this.Equals y =
      match y with
      | :? spVector as v -> v.ToList() = vect
      | _ -> false

    override this.ToString () = sprintf "Vector: %A" vect
