module Q = struct
   type t = int
    [@@deriving qcheck]
end

type t = Q.t
 [@@deriving qcheck]
