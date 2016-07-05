#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"virtual_dom"
  [ oasis_lib "virtual_dom"
  ; file "META" ~section:"lib"
  ; file "_build/namespace_wrappers/tyxml_f.cmi" ~section:"lib"
  ; file "_build/namespace_wrappers/js_of_ocaml.cmi" ~section:"lib"
  ]
