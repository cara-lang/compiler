open Base
open Core

(* Identifier is a qualified name: Dict.Any.Dict, maxInt and such *)
module Identifier = struct
  module T = struct
    type t = string list * string [@@deriving sexp]
    let compare = [%compare: string list * string]
    let sexp_of_t = Tuple2.sexp_of_t (List.sexp_of_t String.sexp_of_t) String.sexp_of_t
  end

  include T
  include Comparator.Make (T)
end

let identifier_to_string (q,x) =
  match q with
  | [] -> x
  | _ -> String.concat ~sep:"." q ^ "." ^ x

module Env = Map.M(Identifier)
  [@@deriving sexp]