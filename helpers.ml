module ImmuHashtbl = struct
  module type S = sig
    type key

    type 'a t

    val find_opt : 'a t -> key -> 'a option

    val mem : 'a t -> key -> bool

    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    val iter : (key -> 'a -> unit) -> 'a t -> unit
  end

  module Make (H : Hashtbl.S) : S with type key = H.key and type 'a t = 'a H.t =
  struct
    include H
  end
end
