open Seashell
open Compile_utils
open Js_of_ocaml.Js

let _ =
  export "seashell"
    (object%js
       method compile prog =
         string @@ compile_string_without_print (to_bytestring prog) false
       method add x y = x +. y
    end)
