#include "./../HATS/x.hats"
#staload "{$x}/SATS/lexing.sats"

#staload "./json.sats"

fun
jsonize_tnode : jsonize_type(tnode)
overload jsonize with jsonize_tnode

fun
jsonize_token : jsonize_type(token)
overload jsonize with jsonize_token
