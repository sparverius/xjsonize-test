#include "share/atspre_staload.hats"
#staload UN = "prelude/SATS/unsafe.sats"

#staload "./../SATS/json.sats"

(*
implement
node(x, y) =
jsonval_labval2("node", jsonize(x), "json", y)
*)

implement
node(x, y) =
jsonval_labval1(x, y)


implement
node2(x, y, z) =
jsonval_labval1(x, z)
(*
implement
node2(x, y, z) =
jsonval_labval2("node", jsonize(x), "json", z)
*)
(*
implement
node2(x, y, z) =
jsonval_labval3("name", jsonize(x), "loc", y, "node", z)
*)

(*
implement
node2(x, y, z) =
jsonval_labval1(x, jsonval_labval2("loc", y, "node", z))
*)



implement{a}
jsonize_list(nm, xs) =
jsonval_labval1(nm, JSONlist(res)) where
{
  val xys = list_map<a><jsonval>(xs) where
  {
    implement
    list_map$fopr<a><jsonval>(x) = jsonize_val<a>(x)
  }
  val res = list_of_list_vt{jsonval}(xys)
}


implement{a}
jsonize_option(nm, xs) =
jsonval_labval1(nm, res) where
val res =
(
  case+ xs of
  | None() => jnul()
  | Some(x) => jsonize_val<a>(x)
  (*
  case+ xs of
  | None() => jsonize("None")
  | Some(x) => jsonval_labval1("Some", jsonize_val<a>(x))
  *)
)
end

implement{a}
jsonize_option_vt(nm, xs) =
jsonval_labval1(nm, res) where
val res =
(
  case+ xs of
  | ~None_vt() => jsonize("None")
  | ~Some_vt(x) => jsonval_labval1("Some_vt", jsonize_val<a>(x))
)
end

implement
jsonval_int (i) = JSONint (i)

(*
implement
jsonval_intinf (i) = JSONintinf (i)
*)

implement
jsonval_bool (b) = JSONbool (b)

implement
jsonval_double (d) = JSONfloat (d)

implement
jsonval_string (str) = JSONstring (str)

implement
lab(x:string, rst:jsonval): jsonval =
jsonval_labval1(x, rst)

implement
jnul() = JSONnul()

implement
jint(x:int) = JSONint(x)

implement
jbool(x:bool) = JSONbool(x)

implement
jfloat(x:double) = JSONfloat(x)

implement
jstr(x:string) = JSONstring(x)

implement
jsonize_int(x) = JSONint(x)

implement
jsonize_char(x) = JSONstring(tostring_char(x))

implement
jsonize_string(x) = JSONstring(x)

implement
jsonize_bool(x) = JSONbool(x)

implement
jsonize_double(x) = JSONstring(tostring_val<double>(x))

implement
jsonize_val<int>(x) = jsonize_int(x)

implement
jsonize_val<string>(x) = jsonize_string(x)

implement
jsonize_val<bool>(x) = jsonize_bool(x)

implement
jsonize_val<double>(x) = jsonize_double(x)

(* implement
jsonize_val<float>(x) = jsonize_float(x) *)



implement
jsonval_none () = JSONoption (None())
implement
jsonval_some (x) = JSONoption (Some(x))

implement
listj(x) = JSONlist(list_sing(x))

implement
listj2(x, y) = JSONlist(cons(x, cons(y, nil())))



(* ****** ****** *)
//
#define nil list_nil
//
#define :: list_cons
#define cons list_cons
//
(* ****** ****** *)

implement
jsonval_nil() = JSONlist (list_nil())

implement
jsonval_sing (x) = JSONlist (list_sing(x))
implement
jsonval_pair (x1, x2) = JSONlist (list_pair(x1, x2))

(* ****** ****** *)
//
implement
jsonval_labval1 (l, x) =
JSONlablist
(
  $list{labjsonval}((l, x))
)

implement
jsonval_labval2 (l1, x1, l2, x2) =
JSONlablist
(
  $list{labjsonval}((l1, x1),  (l2, x2))
)

implement
jsonval_labval3(l1, x1, l2, x2, l3, x3) =
JSONlablist
(
  $list{labjsonval}((l1, x1),  (l2, x2),  (l3, x3))
)

implement
jsonval_labval4
(l1, x1, l2, x2, l3, x3, l4, x4) =
JSONlablist
(
  $list{labjsonval}
  (
    (l1, x1), (l2, x2), (l3, x3), (l4, x4)
  )
)

implement
jsonval_labval5
(l1, x1, l2, x2, l3, x3, l4, x4, l5, x5) =
JSONlablist
(
  $list{labjsonval}
  (
    (l1, x1), (l2, x2), (l3, x3), (l4, x4), (l5, x5)
  )
)

implement
jsonval_labval6
(
  l1, x1, l2, x2, l3, x3,
  l4, x4, l5, x5, l6, x6
) =
JSONlablist
(
  $list{labjsonval}
  (
    (l1, x1), (l2, x2), (l3, x3),
    (l4, x4), (l5, x5), (l6, x6)
  )
)
implement
jsonval_labval7
(
  l1, x1, l2, x2, l3, x3, l4, x4,
  l5, x5, l6, x6, l7, x7
) =
JSONlablist
(
  $list{labjsonval}
  (
    (l1, x1), (l2, x2), (l3, x3), (l4, x4),
    (l5, x5), (l6, x6), (l7, x7)
  )
)

implement
jsonval_labval8
(
  l1, x1, l2, x2, l3, x3, l4, x4,
  l5, x5, l6, x6, l7, x7, l8, x8
) =
JSONlablist
(
  $list{labjsonval}
  (
    (l1, x1), (l2, x2), (l3, x3), (l4, x4),
    (l5, x5), (l6, x6), (l7, x7), (l8, x8)
  )
)
implement
jsonval_labval9
(
  l1, x1, l2, x2, l3, x3, l4, x4, l5, x5,
  l6, x6, l7, x7, l8, x8, l9, x9
) =
JSONlablist
(
  $list{labjsonval}
  (
    (l1, x1), (l2, x2), (l3, x3), (l4, x4), (l5, x5),
    (l6, x6), (l7, x7), (l8, x8), (l9, x9)
  )
)
implement
jsonval_labval10
(
  l1, x1, l2, x2, l3, x3, l4, x4, l5, x5,
  l6, x6, l7, x7, l8, x8, l9, x9, l10, x10
) =
JSONlablist
(
  $list{labjsonval}
  (
    (l1, x1), (l2, x2), (l3, x3), (l4, x4), (l5, x5),
    (l6, x6), (l7, x7), (l8, x8), (l9, x9), (l10, x10)
  )
)



implement
jsonval_conarg0(con) =
(
  jsonval_conarglst (con, list_nil)
)

implement
jsonval_conarg1(con, arg1) =
(
  jsonval_conarglst (con, list_sing (arg1))
)

implement
jsonval_conarg2(con, arg1, arg2) =
(
  jsonval_conarglst (con, list_pair (arg1, arg2))
)

implement
jsonval_conarg3(con, arg1, arg2, arg3) =
(
  jsonval_conarglst (con, arg1 :: arg2 :: arg3 :: list_nil)
)

implement
jsonval_conarg4(con, arg1, arg2, arg3, arg4) =
(
  jsonval_conarglst (con, arg1 :: arg2 :: arg3 :: arg4 :: list_nil())
)

implement
jsonval_conarglst(con, arglst) =
(
  jsonval_labval1 (con, JSONlist (arglst))
)


local

fun
fprint_jsonval_string
(
  out: FILEref, str: string
) : void = let
//
fun
auxch
(
  out: FILEref, c: char
) : void = let
in
//
case+ c of
| '"' => fprint_string (out, "\\\"")
| '\\' => fprint_string (out, "\\\\")
| '\n' => fprint_string (out, "\\n")
| '\r' => fprint_string (out, "\\r")
| '\t' => fprint_string (out, "\\t")
| '\b' => fprint_string (out, "\\b")
| '\f' => fprint_string (out, "\\f")
| _ (*rest-of-char*) =>
  (
    if isprint_char(c)
      then fprint_char(out, c)
      else let
        (* val uc = uchar_of_char(c)  *)
      in
        (* fprintf (out, "\\u00%.2X", @($UN.cast2uint(uc))) *)
      exit(1);
      end // end of [else]
    // end of [if]
  ) (* end of [_] *)
//
end // end of [auxch]
//
fun
loop
(
  out: FILEref, p: ptr
) : void = let
//
val c = $UN.ptr0_get<char> (p)
//
in
//
if c != '\000'
(*
then (auxch(out, c); loop (out, p+sizeof<char>)) else ()
*)
  then (auxch(out, c); loop (out, nxtptr)) where
      val nxtptr = ptr0_add_guint<char>(p, sizeof<char>)
    end
  else ()
//
end // end of [loop]
//
in
//
fprint_char (out, '"');
loop (out, $UN.cast{ptr}(str));
fprint_char (out, '"');
//
end // end of [fprint_jsonval_string]

in (* in-of-local *)

implement
fprint_jsonval
  (out, x0) = let
//
macdef
prstr (str) = fprint_string (out, ,(str))
//
in
//
case+ x0 of
//
| JSONnul () => prstr "{}"
//
| JSONint (i) => fprint_int (out, i)
(*
| JSONintinf (i) =>
  {
    val () = fprint_char (out, '"')
    val () = $INTINF.fprint_intinf (out, i)
    val () = fprint_char (out, '"')
  }
*)
//
| JSONbool (b) => fprint_bool (out, b)
| JSONfloat (d) => fprint_double (out, d)
//
| JSONstring (str) => fprint_jsonval_string (out, str)
| JSONlist (xs) =>
  {
    val () = prstr "["
    val () = fprint_jsonvalist (out, xs)
    val () = prstr "]"
  }
| JSONlablist (lxs) =>
  {
    val () = prstr "{"
    val () = fprint_labjsonvalist (out, lxs)
    val () = prstr "}"
  }
//
| JSONoption (opt) =>
  {
    val () = prstr "["
    val () =
    (
      case+ opt of
      | Some x => fprint_jsonval (out, x) | None () => ()
    ) : void // end of [val]
    val () = prstr "]"
  }
(*
| JSONobject (lst) =>
  {
    val () = prstr "{"
(*
    val () =
    (
      case+ opt of
      | Some x => fprint_jsonval (out, x) | None () => ()
    ) : void // end of [val]
*)
    val () = prstr "}"
  }
*)

//
end // end of [fprint_jsonval]

end // end of [local]

(* ****** ****** *)

implement
fprint_jsonvalist
  (out, xs0) = let
//
fun aux
(
  out: FILEref
, xs0: jsonvalist, i: int
) : void = let
in
//
case+ xs0 of
| list_nil () => ()
| list_cons (x, xs) => let
    val () =
      if i > 0
        then fprint (out, ",") // fprint (out, ", ")
      // end of [if]
    val () = fprint_jsonval (out, x)
  in
    aux (out, xs, i+1)
  end // end of [list_cons]
//
end // end of [aux]
//
in
  aux (out, xs0, 0)
end // end of [fprint_jsonvalist]

(* ****** ****** *)

implement
fprint_labjsonvalist
  (out, lxs0) = let
//
fun aux
(
  out: FILEref
, lxs0: labjsonvalist, i: int
) : void = let
in
//
case+ lxs0 of
| list_nil () => ()
| list_cons
    (lx, lxs) => let
    val () =
      if i > 0
        then fprint (out, ",") (* fprint (out, ", ") *)
      // end of [if]
    (*
    val () =
      fprintf (out, "\"%s\"", @(lx.0))
    *)
    val () =
      fprintf (out, "\"%s\"", $vararg(lx.0))


    val () = fprint_string (out, ":") (* val () = fprint_string (out, ": ") *)
    val () = fprint_jsonval (out, lx.1)
  in
    aux (out, lxs, i+1)
  end // end of [list_cons]
//
end // end of [aux]
//
in
  aux (out, lxs0, 0)
end // end of [fprint_labjsonvalist]

(* ****** ****** *)

implement
print_jsonval(x) = fprint_jsonval(stdout_ref, x)
implement
prerr_jsonval(x) = fprint_jsonval(stderr_ref, x)

implement
fprint_val<jsonval> = fprint_jsonval


(* ****** ****** *)
//
implement
jsonize_ignored(x0) = JSONnul((*void*))
//
(* ****** ****** *)

implement
{a}(*tmp*)
jsonize_list_fun(xs, f) = let
//
(*
val () =
  println! ("jsonize_list_fun")
*)
//
(*
val
jsvs = list_map_fun<a> (xs, f)
*)
val
jsvs = list_map<a><jsonval>(xs) where
  implement
  list_map$fopr<a><jsonval>(a) = f(a)
end
val
jsvs = list_of_list_vt(jsvs)

in
  JSONlist (jsvs)
end // end of [jsonize_list_fun]

(* ****** ****** *)

implement
{a}(*tmp*)
jsonize_option_fun(opt, f) = let
//
(*
val () =
  println! ("jsonize_option_fun")
*)
//
in
//
case+ opt of
| None() => jsonval_none() | Some(x) => jsonval_some(f(x))
//
end // end of [jsonize_option_fun]




fun jsontyp(xs: jsonval): string =
(
  case+ xs of
    | JSONnul() => "JSONnul"
    | JSONint(int) => "JSONint"
    | JSONbool(bool) => "JSONbool"
    | JSONfloat(double) => "JSONfloat"
    | JSONstring(string) => "JSONstring"
    | JSONlist(jsonvalist) => "JSONlist"
    | JSONlablist(labjsonvalist) => "JSONlablist"
    | JSONoption(jsonvalopt) => "JSONoption"
)

#define JSON_OUTPUT 0


implement
jsonify0(guard_name) = JSONstring(guard_name)

implement
jsonify1(guard_name, name, json) =
//jsonval_labval1(guard_name, jsonval_labval1(name, json))
  jsonval_labval1(guard_name, json)


implement
jsonify2(guard_name, names, json) =
jsonval_labval1(guard_name, rst) where
  val rst =
  (
    case JSON_OUTPUT of
    | 0 => JSONlist($list{jsonval}(json.0, json.1))
    | 1 =>
      (
        jsonval_labval2 (
          names.0, json.0,
          names.1, json.1
        )
      )
    | _ => (
      JSONlablist($list{labjsonval}(
        (names.0, json.0),
        (names.1, json.1)
      ))
    )
  )
end

implement
jsonify3(guard_name, names, json) =
jsonval_labval1(guard_name, rst) where
  val rst =
  (
    case JSON_OUTPUT of
    |
    0 => (
      JSONlist($list{jsonval}(json.0, json.1, json.2))
    )
    | 1 => (
      jsonval_labval3 (
        names.0, json.0,
        names.1, json.1,
        names.2, json.2
      )
    )
    | _ => (
      JSONlablist($list{labjsonval}(
        (names.0, json.0),
        (names.1, json.1),
        (names.2, json.2)
      ))
    )
  )end

implement
jsonify4(guard_name, names, json) =
jsonval_labval1(guard_name, rst) where
  val rst =
  (
    case JSON_OUTPUT of
    | 0 => (
      JSONlist
      (
        $list{jsonval}
        (
          json.0, json.1, json.2, json.3
        )
      )
    )
    | 1 => (
      jsonval_labval4 (
        names.0, json.0,
        names.1, json.1,
        names.2, json.2,
        names.3, json.3
      )
    )
    | _ => (
      JSONlablist($list{labjsonval}(
        (names.0, json.0),
        (names.1, json.1),
        (names.2, json.2),
        (names.3, json.3)
      ))
    )
  )
end

implement
jsonify5 (guard_name, names, json) =
jsonval_labval1(guard_name, rst) where
  val rst =
  (
    case JSON_OUTPUT of
    | 0 => (
      JSONlist
      (
        $list{jsonval}
        (
          json.0, json.1, json.2, json.3, json.4
        )
      )
    )
    | 1 => (
      jsonval_labval5 (
        names.0, json.0,
        names.1, json.1,
        names.2, json.2,
        names.3, json.3,
        names.4, json.4
      )
    )
    | _ => (
      JSONlablist($list{labjsonval}(
        (names.0, json.0),
        (names.1, json.1),
        (names.2, json.2),
        (names.3, json.3),
        (names.4, json.4)
      ))
    )
  )
end

implement
jsonify6 (guard_name, names, json) =
jsonval_labval1(guard_name, rst) where
  val rst =
  (
    case JSON_OUTPUT of
    | 0 => (
      JSONlist(
        $list{jsonval}
        (
          json.0, json.1, json.2, json.3, json.4, json.5
        )
      )
    )
    | 1 => (
      jsonval_labval6 (
        names.0, json.0,
        names.1, json.1,
        names.2, json.2,
        names.3, json.3,
        names.4, json.4,
        names.5, json.5
      )
    )
    | _ => (
      JSONlablist($list{labjsonval}(
        (names.0, json.0),
        (names.1, json.1),
        (names.2, json.2),
        (names.3, json.3),
        (names.4, json.4),
        (names.5, json.5)
      ))
    )
  )
end

implement
jsonify7 (guard_name, names, json) =
jsonval_labval1(guard_name, rst) where
  val rst =
  (
    case JSON_OUTPUT of
    | 0 => (
      JSONlist(
        $list{jsonval}
        (
          json.0, json.1, json.2, json.3, json.4, json.5, json.6
        )
      )
    )
    | 1 => (
      jsonval_labval7 (
        names.0, json.0,
        names.1, json.1,
        names.2, json.2,
        names.3, json.3,
        names.4, json.4,
        names.5, json.5,
        names.6, json.6
      )
    )
    | _ => (
      JSONlablist($list{labjsonval}(
        (names.0, json.0),
        (names.1, json.1),
        (names.2, json.2),
        (names.3, json.3),
        (names.4, json.4),
        (names.5, json.5),
        (names.6, json.6)
      ))
    )
  )
end

implement
jsonify8 (guard_name, names, json) =
jsonval_labval1(guard_name, rst) where
  val rst =
  (
    case JSON_OUTPUT of
    | 0 => (
      JSONlist
      (
        $list{jsonval}
        (
          json.0,
          json.1,
          json.2,
          json.3,
          json.4,
          json.5,
          json.6,
          json.7
        )
      )
    )
    | 1 => (
      jsonval_labval8 (
        names.0, json.0,
        names.1, json.1,
        names.2, json.2,
        names.3, json.3,
        names.4, json.4,
        names.5, json.5,
        names.6, json.6,
        names.7, json.7
      )
    )
    | _ => (
      JSONlablist($list{labjsonval}(
        (names.0, json.0),
        (names.1, json.1),
        (names.2, json.2),
        (names.3, json.3),
        (names.4, json.4),
        (names.5, json.5),
        (names.6, json.6),
        (names.7, json.7)
      ))
    )
  )
end

implement
jsonify9 (guard_name, names, json) =
jsonval_labval1(guard_name, rst) where
  val rst =
  (
    case JSON_OUTPUT of
    | 0 => (
      JSONlist($list{jsonval}(
        json.0,
        json.1,
        json.2,
        json.3,
        json.4,
        json.5,
        json.6,
        json.7,
        json.8
      ))
    )
    | 1 => (
      jsonval_labval9 (
        names.0, json.0,
        names.1, json.1,
        names.2, json.2,
        names.3, json.3,
        names.4, json.4,
        names.5, json.5,
        names.6, json.6,
        names.7, json.7,
        names.8, json.8
      )
    )
    | _ => (
      JSONlablist($list{labjsonval}(
        (names.0, json.0),
        (names.1, json.1),
        (names.2, json.2),
        (names.3, json.3),
        (names.4, json.4),
        (names.5, json.5),
        (names.6, json.6),
        (names.7, json.7),
        (names.8, json.8)
      ))
    )
  )
end

implement
jsonify10 (guard_name, names, json) =
jsonval_labval1(guard_name, rst) where
  val rst =
  (
    case JSON_OUTPUT of
    | 0 => (
        JSONlist($list{jsonval}(
          json.0,
          json.1,
          json.2,
          json.3,
          json.4,
          json.5,
          json.6,
          json.7,
          json.8,
          json.9
        ))
      )
    | 1 => (
      jsonval_labval10 (
        names.0, json.0,
        names.1, json.1,
        names.2, json.2,
        names.3, json.3,
        names.4, json.4,
        names.5, json.5,
        names.6, json.6,
        names.7, json.7,
        names.8, json.8,
        names.9, json.9
      )
    )
    | _ => (
      JSONlablist($list{labjsonval}(
        (names.0, json.0),
        (names.1, json.1),
        (names.2, json.2),
        (names.3, json.3),
        (names.4, json.4),
        (names.5, json.5),
        (names.6, json.6),
        (names.7, json.7),
        (names.8, json.8),
        (names.9, json.9)
      ))
    )
  )
end
