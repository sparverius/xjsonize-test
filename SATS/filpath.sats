#include "./../HATS/x.hats"
#staload "{$x}/SATS/filpath.sats"

#staload "./json.sats"

(*
implement
fprint_val<filpath> = fprint_filpath_full2
*)

fun
jsonize_filpath : jsonize_type(filpath)
overload jsonize with jsonize_filpath
