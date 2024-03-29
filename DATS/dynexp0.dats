#include "share/atspre_staload.hats"
#staload UN = "prelude/SATS/unsafe.sats"

#include "./../HATS/libxatsopt.hats"
#staload "{$x}/SATS/dynexp0.sats"
#staload "{$x}/SATS/staexp0.sats"

#staload "./../SATS/json.sats"
#staload "./../SATS/lexing.sats"
#staload "./../SATS/dynexp0.sats"
#staload "./../SATS/staexp0.sats"

#staload _ = "./json.dats"


implement
jsonize_val<token> = jsonize_token
implement
jsonize_val<s0exp> = jsonize_s0exp
implement
jsonize_val<sort0> = jsonize_sort0
implement
jsonize_val<s0arg> = jsonize_s0arg
implement
jsonize_val<t0arg> = jsonize_t0arg
implement
jsonize_val<s0marg> = jsonize_s0marg
implement
jsonize_val<t0marg> = jsonize_t0marg
implement
jsonize_val<sq0arg> = jsonize_sq0arg
implement
jsonize_val<tq0arg> = jsonize_tq0arg
implement
jsonize_val<ti0arg> = jsonize_ti0arg
implement
jsonize_val<f0arg> = jsonize_f0arg

implement
jsonize_q0arg
  (x0) =
(
//
case+
x0.node() of
(*
| Q0ARGnone(tok) =>
  jsonify("Q0ARGnone", "tok", jsonize(tok))
*)
| Q0ARGsome(sid, opt) =>
  jsonify("Q0ARGsome", ("sid", "opt"),
    (jsonize(sid), jsonize_option<sort0>("sort0opt", opt)))
//
) (* end of [jsonize_q0arg] *)

implement
jsonize_a0typ
  (x0) =
(
//
case+ x0.node() of
(*
| A0TYPnone(tok) =>
  jsonify("A0TYPnone", tok)
*)
| A0TYPsome(s0e, opt) =>
  jsonify("A0TYPsome", ("s0e", "opt"),
    (
      jsonize(s0e),
      jsonize_option<token>("tokenopt", opt)
    )
  )
//
) (* end of [jsonize_a0typ] *)


local
//
implement
jsonize_val<a0typ> = jsonize_a0typ

fun
jsonize_a0typlstopt
(opt: a0typlstopt): jsonval =
(
case+ opt of
| None() =>
  jsonify("None")
| Some(a0ts) =>
  jsonify("Some", "a0ts", jsonize_list<a0typ>("a0typlst", a0ts))
)
//
overload jsonize with jsonize_a0typlstopt of 100
//

implement
jsonize_val<s0qua> = jsonize_s0qua

in (* in-of-local *)

implement
jsonize_d0arg
  (x0) =
(
//
case+ x0.node() of
//
| D0ARGnone(tok) =>
  jsonify("D0ARGnone", "tok", jsonize(tok))
//
| D0ARGsome_sta
  (tbeg, s0qs, tend) =>
  jsonify("D0ARGsome_sta", ("tbeg", "s0qs", "tend"),
  (
    jsonize(tbeg),
    jsonize_list<s0qua>("s0qualst", s0qs),
    jsonize(tend))
  )
//
| D0ARGsome_dyn1
  (tok) =>
  jsonify("D0ARGsome_dyn1", "tok", jsonize(tok))
| D0ARGsome_dyn2
  (tbeg, arg0, opt1, tend) =>
  jsonify(
    "D0ARGsome_dyn",
    ("tbeg", "arg0", "opt1", "tend"),
    (
      jsonize(tbeg),
      jsonize_list<a0typ>("a0typlst", arg0),
      jsonize(opt1),
      jsonize(tend)
    )
  )
//
) (* end of [jsonize_d0arg] *)

end // end of [local]


implement
jsonize_val<s0qua> = jsonize_s0qua
implement
jsonize_val<s0exp> = jsonize_s0exp

implement
jsonize_f0arg
  (x0) =
(
//
case+
x0.node() of
| F0ARGnone(tok) =>
  jsonify("F0ARGnone", "tok", jsonize(tok))
| F0ARGsome_dyn(d0p) =>
  jsonify("F0ARGsome_dyn", "d0p", jsonize(d0p))
| F0ARGsome_sta(tbeg, s0qs, tend) =>
  jsonify(
    "F0ARGsome_sta", ("tbeg", "s0qs", "tend"),
    (
      jsonize(tbeg),
      jsonize_list<s0qua>("s0qualst", s0qs),
      jsonize(tend)
    )
  )

| F0ARGsome_met(tbeg, s0es, tend) =>
  jsonify(
    "F0ARGsome_met", ("tbeg", "s0es", "tend"),
    (
      jsonize(tbeg),
      jsonize_list<s0exp>("s0explst", s0es),
      jsonize(tend)
    )
  )
) (* end of [jsonize_f0arg] *)

implement
jsonize_val<q0arg> = jsonize_q0arg

implement
jsonize_sq0arg
  (x0) =
node("sq0arg", res) where
val res =
(
case+
x0.node() of
| SQ0ARGnone(tok) =>
  jsonify("SQ0ARGnone", "tok", jsonize(tok))
| SQ0ARGsome(tbeg, q0as, tend) =>
  jsonify(
    "SQ0ARGsome", ("tbeg", "q0as", "tend"),
    (
      jsonize(tbeg),
      jsonize_list<q0arg>("q0arglst", q0as),
      jsonize(tend)
    )
  )
) (* end of [jsonize_sq0arg] *)
end

implement
jsonize_tq0arg
  (x0) =
node("tq0arg", res) where
val res =
(
case+
x0.node() of
| TQ0ARGnone(tok) =>
  jsonify("TQ0ARGnone", "tok", jsonize(tok))
| TQ0ARGsome(tbeg, q0as, tend) =>
  jsonify("TQ0ARGsome", ("tbeg", "q0as", "tend"),
    (
      jsonize(tbeg),
      jsonize_list<q0arg>("q0arglst", q0as),
      jsonize(tend)
    )
  )
)
end

implement
jsonize_ti0arg
  (x0) =
node("ti0arg", res) where
val res =
(
case+
x0.node() of
| TI0ARGnone(tok) =>
  jsonify("TI0ARGnone", "tok", jsonize(tok))
| TI0ARGsome(tbeg, q0as, tend) =>
  jsonify(
    "TI0ARGsome", ("tbeg", "q0as", "tend"),
    (jsonize(tbeg), jsonize_list<s0exp>("s0explst", q0as), jsonize(tend))
  )
)
end


implement
{a}(*tmp*)
jsonize_dl0abeled
  (x0) = let
//
val+DL0ABELED(l0, t0, x1) = x0
//
in
node("dl0abeled", res) where
val res =
  jsonify("SL0ABELED", ("l0", "t0", "x1"),
    (jsonize(l0), jsonize(t0), jsonize_val<a>(x1))
  )
end

end // end of [jsonize_dl0abeled]

implement(a:type) jsonize_val<dl0abeled(a)> = jsonize_dl0abeled<a>


local

implement
jsonize_val<d0pat> = jsonize_d0pat

implement
jsonize_val<dl0abeled(d0pat)> = jsonize_dl0abeled<d0pat>


in (* in-of-local *)

implement
jsonize_d0pat
  (x0) =
node("d0pat", res) where
val res =
(
case+ x0.node() of
| D0Pid(id) =>
  jsonify("D0Pid", "id", jsonize(id))
| D0Pint(i0) =>
  jsonify("D0Pint", "i0", jsonize(i0))
| D0Pchr(c0) =>
  jsonify("D0Pchr", "c0", jsonize(c0))
| D0Pflt(f0) =>
  jsonify("D0Pflt", "f0", jsonize(f0))
| D0Pstr(s0) =>
  jsonify("D0Pstr", "s0", jsonize(s0))
| D0Papps(d0ps) =>
  jsonify("D0Papps", "d0ps", jsonize_list<d0pat>("d0patlst", d0ps))
| D0Psqarg(tbeg, s0as, tend) =>
  jsonify("D0Psqarg", ("tbeg", "s0as", "tend"),
    (
      jsonize(tbeg),
      jsonize_list<s0arg>("s0arglst", s0as),
      jsonize(tend)
    )
  )
| D0Pparen(tbeg, d0ps, tend) =>
  jsonify("D0Pparen", ("tbeg", "d0ps", "tend"),
    (
      jsonize(tbeg),
      jsonize_list<d0pat>("d0patlst", d0ps),
      jsonize(tend)
    )
  )
| D0Ptuple(tbeg, topt, d0ps, tend) =>
  jsonify("D0Ptuple", ("tbeg", "topt", "d0ps", "tend"),
    (
      jsonize(tbeg),
      jsonize_option<token>("tokenopt", topt),
      jsonize_list<d0pat>("d0patlst", d0ps),
      jsonize(tend)
    )
  )
| D0Precord(tbeg, topt, ld0ps, tend) =>
  jsonify("D0Precord", ("tbeg", "topt", "ld0ps", "tend"),
    (
      jsonize(tbeg),
      jsonize_option<token>("tokenopt", topt),
      jsonize_list<dl0abeled(d0pat)>("labd0patlst", ld0ps),
      jsonize(tend)
    )
  )
| D0Panno(d0p, ann) =>
  jsonify("D0Panno", ("d0p", "ann"), (jsonize(d0p), jsonize(ann)))
| D0Pqual(tok, d0p) =>
  jsonify("D0Pqual", ("tok", "d0p"), (jsonize(tok), jsonize(d0p)))
| D0Pnone(tok) =>
  jsonify("D0Pnone", "tok", jsonize(tok))
)
end

end // end of [local]


local

implement
jsonize_val<d0pat> = jsonize_d0pat

in (* in-of-local *)

implement
jsonize_d0pat_RPAREN
  (x0) =
node("d0pat_RPAREN", res) where
val res =
(
case+ x0 of
| d0pat_RPAREN_cons0(tok) =>
  jsonify("d0pat_RPAREN_cons0", "tok", jsonize(tok))
| d0pat_RPAREN_cons1(tok1, d0ps, tok2) =>
  jsonify("d0pat_RPAREN_cons1", ("tok1", "d0ps", "tok2"),
    (
      jsonize(tok1),
      jsonize_list<d0pat>("d0patlst", d0ps),
      jsonize(tok2)
    )
  )
)
end

end // end of [local]


local

implement
jsonize_val<d0pat> = jsonize_d0pat

in (* in-of-local *)

implement
jsonize_labd0pat_RBRACE
  (x0) =
node("labd0pat_RBRACE", res) where
val res =
(
case+ x0 of
| labd0pat_RBRACE_cons0(tok) =>
  jsonify("labd0pat_RBRACE_cons0", "tok", jsonize(tok))
| labd0pat_RBRACE_cons1(tok1, ld0ps, tok2) =>
  jsonify(
    "labd0pat_RBRACE_cons1", ("tok1", "ld0ps", "tok2"),
    (
      jsonize(tok1),
      jsonize_list<dl0abeled(d0pat)>("labd0patlst", ld0ps),
      jsonize(tok2)
    )
  )
)
end

end // end of [local]

implement
jsonize_d0clau
  (x0) =
node("d0clau", res) where
val res =
(
case+
x0.node() of
| D0CLAUgpat(d0gp) =>
  jsonify("D0CLAUgpat", "d0gp", jsonize(d0gp))
| D0CLAUclau(d0gp, tok, d0e0) =>
  jsonify("D0CLAUclau", ("d0gp", "tok", "d0e0"),
    (jsonize(d0gp), jsonize(tok), jsonize(d0e0))
  )
)
end (* end of [jsonize_d0clau] *)

implement
jsonize_val<d0gua> = jsonize_d0gua

implement
jsonize_d0gpat
  (x0) =
node("d0gpat", res) where
val res =
(
case+
x0.node() of
| D0GPATpat(d0p) =>
  jsonify("D0GPATpat", "d0p", jsonize(d0p))
| D0GPATgua(d0p, tok, d0gs) =>
  jsonify("D0GPATgua", ("d0p", "tok", "d0gs"),
    (
      jsonize(d0p),
      jsonize(tok),
      jsonize_list<d0gua>("d0gualst", d0gs)
    )
  )
)
end (* end of [jsonize_d0gpat] *)


implement
jsonize_d0gua
  (x0) =
node("d0gua", res) where
val res =
(
case+
x0.node() of
| D0GUAexp(d0e) =>
  jsonify("D0GUAexp", "d0e", jsonize(d0e))
| D0GUAmat(d0e, tok, d0p) =>
  jsonify("D0GUAexp", ("d0e", "tok", "d0p"),
    (jsonize(d0e), jsonize(tok), jsonize(d0p))
  )
)
end (* end of [jsonize_d0gua] *)


local

implement
jsonize_val<d0exp> = jsonize_d0exp
implement
jsonize_val<d0clau> = jsonize_d0clau
implement
jsonize_val<s0exp> = jsonize_s0exp
implement
jsonize_val<d0ecl> = jsonize_d0ecl
implement
jsonize_val<f0arg> = jsonize_f0arg

implement
jsonize_val<dl0abeled(d0exp)> = jsonize_dl0abeled<d0exp>


in (* in-of-local *)

implement
jsonize_d0exp
  (x0) =
node("d0exp", res) where
val res =
(
case+ x0.node() of
//
| D0Eid(id) =>
  jsonify("D0Eid", "id", jsonize(id))
//
| D0Eint(i0) =>
  jsonify("D0Eint", "i0", jsonize(i0))
| D0Echr(c0) =>
  jsonify("D0Echr", "c0", jsonize(c0))
| D0Eflt(f0) =>
  jsonify("D0Eflt", "f0", jsonize(f0))
| D0Estr(s0) =>
  jsonify("D0Estr", "s0", jsonize(s0))
//
| D0Eapps(d0es) =>
  jsonify("D0Eapps", "d0es", jsonize_list<d0exp>("d0explst", d0es))
//
| D0Esqarg(tbeg, s0es, tend) =>
  jsonify("D0Esqarg", ("tbeg", "s0es", "tend"),
    (jsonize(tbeg), jsonize_list<s0exp>("s0explst", s0es), jsonize(tend))
  )
| D0Etqarg(tbeg, s0es, tend) =>
  jsonify("D0Etqarg", ("tbeg", "s0es", "tend"),
    (jsonize(tbeg), jsonize_list<s0exp>("s0explst", s0es), jsonize(tend))
  )
//
| D0Eparen(tbeg, d0es, tend) =>
  jsonify("D0Eparen", ("tbeg", "d0es", "tend"),
    (jsonize(tbeg), jsonize_list<d0exp>("d0explst", d0es), jsonize(tend))
  )
//
| D0Etuple(tbeg, topt, d0es, tend) =>
  jsonify("D0Etuple", ("tbeg", "topt", "d0es", "tend"),
    (
      jsonize(tbeg),
      jsonize_option<token>("tokenopt", topt),
      jsonize_list<d0exp>("d0explst", d0es),
      jsonize(tend)
    )
  )
| D0Erecord(tbeg, topt, ld0es, tend) =>
  jsonify("D0Erecord", ("tbeg", "topt", "ld0es", "tend"),
    (
      jsonize(tbeg),
      jsonize_option<token>("tokenopt", topt),
      jsonize_list<dl0abeled(d0exp)>("labd0explst", ld0es),
      jsonize(tend)
    )
  )
//
| D0Eif0(tif0, d0e1, d0e2, d0e3, tend) =>
  jsonify("D0Eif0", ("tif0", "d0e1", "d0e2", "d0e3", "tend"),
    (
      jsonize(tif0),
      jsonize(d0e1),
      jsonize(d0e2),
      jsonize(d0e3),
      jsonize_option<token>("tokenopt", tend)
    )
  )
//
| D0Ecase(tok0, d0e1, tof2, tbar, d0cs, tend) =>
  jsonify("D0Ecase", ("tok0", "d0e1", "tof2", "tbar", "d0cs", "tend"),
    (
      jsonize(tok0),
      jsonize(d0e1),
      jsonize(tof2),
      jsonize_option<token>("tokenopt", tbar),
      (* jsonize("..."), (* jsonize_list<d0clau>(d0cs), *) *)
      jsonize_list<d0clau>("d0claulst", d0cs),
      jsonize_option<token>("tokenopt", tend)
    )
  )
//
| D0Elet(tok0, d0cs, topt, d0es, tok2) =>
  jsonify("D0Elet", ("tok0", "d0cs", "topt", "d0es", "tok2"),
    (
      jsonize(tok0),
      jsonize_list<d0ecl>("d0eclst", d0cs),
      jsonize_option<token>("tokenopt", topt),
      jsonize_list<d0exp>("d0explst", d0es),
      jsonize(tok2)
    )
  )
//
| D0Ewhere(d0e1, d0cs) =>
  jsonify("D0Ewhere", ("d0e1", "d0cs"), (jsonize(d0e1), jsonize(d0cs)))
//
//
| D0Ebrack(tbeg, d0es, tend) =>
  jsonify("D0Ebrack", ("tbeg", "d0es", "tend"),
    (jsonize(tbeg), jsonize_list<d0exp>("d0explst", d0es), jsonize(tend))
  )
| D0Edtsel(tdot, lab1, arg2) =>
  jsonify(
    "D0Edtsel", ("tdot", "lab1", "arg2"),
    (jsonize(tdot), jsonize(lab1), jsonize_option<d0exp>("d0expopt", arg2))
  )
//
| D0Elam(tok0, arg1, res2, farrw, fbody, tend) =>
  jsonify("D0Elam", ("tok0", "arg1", "res2", "farrw", "fbody", "tend"),
    (
      jsonize(tok0),
      jsonize_list<f0arg>("f0arglst", arg1),
      jsonize(res2),
      jsonize(farrw),
      jsonize(fbody),
      jsonize_option<token>("tokenopt", tend)
    )
  )
| D0Efix(tok0, fid0, arg1, res2, farrw, fbody, tend) =>
  jsonify("D0Efix", ("tok0", "fid0", "arg1", "res2", "farrw", "fbody", "tend"),
    (
      jsonize(tok0),
      jsonize(fid0),
      jsonize_list<f0arg>("f0arglst", arg1),
      jsonize(res2),
      jsonize(farrw),
      jsonize(fbody),
      jsonize_option<token>("tokenopt", tend)
    )
  )
//
| D0Eanno(d0e, ann) =>
  jsonify("D0Eanno", ("d0e", "ann"), (jsonize(d0e), jsonize(ann)))
//
| D0Equal(tok, d0e) =>
  jsonify("D0Equal", ("tok", "d0e"), (jsonize(tok), jsonize(d0e)))
//
| D0Enone(tok) =>
  jsonify("D0Enone"," tok", jsonize(tok))
//
) (* end of [jsonize_d0exp] *)
end

end // end of [local]


local

implement
jsonize_val<d0exp> = jsonize_d0exp

in (* in-of-local *)

implement
jsonize_d0exp_RPAREN
  (x0) =
node("d0exp_RPAREN", res) where
val res =
(
case+ x0 of
| d0exp_RPAREN_cons0(tok) =>
  jsonify("d0exp_RPAREN_cons0", "tok", jsonize(tok))
| d0exp_RPAREN_cons1(tok1, d0es, tok2) =>
  jsonify("d0exp_RPAREN_cons1", ("tok1", "d0es", "tok2"),
    (
      jsonize(tok1),
      jsonize_list<d0exp>("d0explst", d0es),
      jsonize(tok2)
    )
  )
| d0exp_RPAREN_cons2(tok1, d0es, tok2) =>
  jsonify("d0exp_RPAREN_cons2", ("tok1", "d0es", "tok2"),
    (
      jsonize(tok1),
      jsonize_list<d0exp>("d0explst", d0es),
      jsonize(tok2)
    )
  )
)
end

end // end of [local]


local

implement
jsonize_val<d0exp> = jsonize_d0exp
implement
jsonize_val<dl0abeled(d0exp)> = jsonize_dl0abeled<d0exp>


in (* in-of-local *)

implement
jsonize_labd0exp_RBRACE
  (x0) =
node("labd0exp_RBRACE", res) where
val res =
(
case+ x0 of
| labd0exp_RBRACE_cons0(tok) =>
  jsonify("labd0exp_RBRACE_cons0", "tok", jsonize(tok))
| labd0exp_RBRACE_cons1(tok1, ld0es, tok2) =>
  jsonify("labd0exp_RBRACE_cons1", ("tok1", "ld0es", "tok2"),
    (
      jsonize(tok1),
      jsonize_list<dl0abeled(d0exp)>("labd0explst", ld0es),
      jsonize(tok2)
    )
  )
)
end

end // end of [local]


implement
jsonize_d0exp_THEN
  (x0) =
node("d0exp_THEN", res) where
val res =
(
case+ x0 of
| d0exp_THEN(tok, d0e) =>
  jsonify("d0exp_THEN", ("tok", "d0e"), (jsonize(tok), jsonize(d0e)))
)
end

implement
jsonize_d0exp_ELSE
  (x0) =
node("d0exp_ELSE", res) where
val res =
(
case+ x0 of
| d0exp_ELSEnone() =>
  jsonify("d0exp_ELSEnone")
| d0exp_ELSEsome(tok, d0e) =>
  jsonify("d0exp_ELSEsome", ("tok", "d0e"), (jsonize(tok), jsonize(d0e)))
)
end

implement
jsonize_endwhere
  (x0) =
node("endwhere", res) where
val res =
(
case+ x0 of
| endwhere_cons1(tok) =>
  jsonify("endwhere_cons1", "tok", jsonize(tok))
| endwhere_cons2(tok1, opt2) =>
  jsonify("endwhere_cons2", ("tok1", "opt2"),
    (jsonize(tok1), jsonize_option<token>("tokenopt", opt2))
  )
)
end


implement
jsonize_val<d0ecl> = jsonize_d0ecl

implement
jsonize_d0eclseq_WHERE
  (x0) =
node("d0eclseq_WHERE", res) where
val res =
(
case+ x0 of
| d0eclseq_WHERE
  (tok0, opt1, d0cs, opt2) =>
  jsonify(
    "d0eclseq_WHERE", ("tok0", "opt1", "d0cs", "opt2"),
    (
      jsonize(tok0),
      jsonize_option<token>("tokenopt", opt1),
      jsonize_list<d0ecl>("d0eclst", d0cs),
      jsonize(opt2)
    )
  )
)
end


implement
jsonize_val<s0exp> = jsonize_s0exp

implement
jsonize_f0unarrow
  (x0) =
node("f0unarrow", res) where
val res =
(
case+ x0 of
| F0UNARROWnone(tok) =>
  jsonify("F0UNARROWnone", "tok", jsonize(tok))
| F0UNARROWdflt(tok) =>
  jsonify("F0UNARROWdflt", "tok", jsonize(tok))
| F0UNARROWlist(tbeg, s0es, tend) =>
  jsonify("F0UNARROWlist", ("tbeg", "s0es", "tend"),
    (
      jsonize(tbeg),
      jsonize_list<s0exp>("s0explst", s0es),
      jsonize(tend)
    )
  )
)
end


implement
jsonize_val<i0dnt>(x) = jsonize_i0dnt(x)

implement
jsonize_decmodopt
  (x0) =
node("decmodopt", res) where
val res =
(
case+ x0 of
//
| DECMODnone() =>
  jsonify("DECMODnone")
//
| DECMODsing(tok, id0) =>
  jsonify("DECMODsing", ("tok", "id0"), rst) where
    val rst = (jsonize(tok), jsonize(id0))
  end
| DECMODlist(tok, tbeg, ids, tend) =>
  (* jsonize!( out, "DECMODlist", tok, tbeg, ids, tend) *)
  jsonify("DECMODsing", ("tok", "tbeg", "ids", "tend"), rst) where
    (* val _ = $showtype(ids) *)
    val xys = jsonize_list<i0dnt>("i0dntlst", ids)
    val rst = (jsonize(tok), jsonize(tbeg), xys, jsonize(tend))
  end
)
end

implement
jsonize_teqd0expopt
  (x0) =
node("teqd0expopt", res) where
val res =
(
case+ x0 of
| TEQD0EXPnone() =>
  jsonify("TEQD0EXPnone")
| TEQD0EXPsome(tok, d0e) =>
  jsonify("TEQD0EXPsome", ("tok", "d0e"), (jsonize(tok), jsonize(d0e)))
)
end

implement
jsonize_wths0expopt
  (x0) =
node("wths0expopt", res) where
val res =
(
case+ x0 of
| WTHS0EXPnone() =>
  jsonify("WTHS0EXPnone")
| WTHS0EXPsome(tok, d0e) =>
  jsonify("WTHS0EXPsome", ("tok", "d0e"), (jsonize(tok), jsonize(d0e)))
)
end



local

implement
jsonize_val<d0ecl> = jsonize_d0ecl
implement
jsonize_val<g0marg> = jsonize_g0marg
implement
jsonize_val<v0aldecl> = jsonize_v0aldecl
implement
jsonize_val<v0ardecl> = jsonize_v0ardecl
implement
jsonize_val<f0undecl> = jsonize_f0undecl
implement
jsonize_val<d0cstdecl> = jsonize_d0cstdecl
implement
jsonize_val<i0dnt> = jsonize_i0dnt

implement
jsonize_val<d0tsort> = jsonize_d0tsort
implement
jsonize_val<d0atype> = jsonize_d0atype

implement
jsonize_val<t0int> = jsonize_t0int

in (* in-of-local *)

implement
jsonize_d0ecl
  (x0) =
node("d0ecl", res) where
val res =
(
case+ x0.node() of
//
| D0Cnone(tok) =>
  jsonify("D0Cnone", "tok", jsonize(tok))
//
| D0Ctokerr(tok) =>
  jsonify("D0Ctokerr", "tok", jsonize(tok))
//
| D0Cnonfix(tok, ids) =>
  jsonify("D0Cnonfix", ("tok", "ids"),
    (
      jsonize(tok),
      jsonize_list<i0dnt>("i0dntlst", ids)
    )
  )
| D0Cfixity(tok, ids, opt) =>
  jsonify("D0Cfixity", ("tok", "ids", "opt"),
    (
      jsonize(tok),
      jsonize_list<i0dnt>("i0dntlst", ids),
      jsonize(opt)
    )
  )
//
| D0Cstatic(tok, d0c) =>
  jsonify("D0Cstatic", ("tok", "d0c"), (jsonize(tok), jsonize(d0c)))
| D0Cextern(tok, d0c) =>
  jsonify("D0Cextern", ("tok", "d0c"), (jsonize(tok), jsonize(d0c)))
//
| D0Cdefine(tok, gid, gmas, gdef) =>
  jsonify("D0Cdefine", ("tok", "gid", "gmas", "gdef"),
    (
      jsonize(tok),
      jsonize(gid),
      jsonize_list<g0marg>("g0marglst", gmas),
      jsonize(gdef)
    )
  )
//
| D0Cmacdef(tok, gid, gmas, mdef) =>
  jsonify("D0Cmacdef", ("tok", "gid", "gmas", "mdef"),
    (
      jsonize(tok),
      jsonize(gid),
      jsonize_list<g0marg>("g0marglst", gmas),
      jsonize(mdef)
    )
  )
//
| D0Cinclude(tok, d0e) =>
  jsonify("D0Cinclude", ("tok", "d0e"), (jsonize(tok), jsonize(d0e)))
//
| D0Cstaload(tok, d0e) =>
  jsonify("D0Cstaload", ("tok", "d0e"), (jsonize(tok), jsonize(d0e)))
(*
| D0Cdynload(tok, d0e) =>
  jsonify("D0Cdynload", tok, d0e)
*)
//
| D0Cabssort(tok, tid) =>
  jsonify("D0Cabssort", ("tok", "tid"), (jsonize(tok), jsonize(tid)))
//
| D0Cstacst0(tok, sid, tmas, tok1, s0t2) =>
  jsonify("D0Cstacst0", ("tok", "sid", "tmas", "tok1", "s0t2"),
    (
      jsonize(tok),
      jsonize(sid),
      jsonize_list<t0marg>("t0marglst", tmas),
      jsonize(tok1), jsonize(s0t2)
    )
  )
//
| D0Csortdef(tok, tid, tok1, def2) =>
  jsonify(
    "D0Csortdef", ("tok", "tid", "tok1", "def2"),
    (jsonize(tok), jsonize(tid), jsonize(tok1), jsonize(def2))
  )
| D0Csexpdef(tok, sid, arg, res, tok1, tdef) =>
  jsonify("D0Csexpdef", ("tok", "sid", "arg", "res", "tok1", "tdef"),
    (
      jsonize(tok),
      jsonize(sid),
      jsonize_list<s0marg>("s0marglst", arg),
      jsonize_option<sort0>("sort0opt", res),
      jsonize(tok1),
      jsonize(tdef)
    )
  )
//
| D0Cabstype(tok, sid, arg, res, tdef) =>
  jsonify("D0Cabstype", ("tok", "sid", "arg", "res", "tdef"),
    (
      jsonize(tok),
      jsonize(sid),
      jsonize_list<t0marg>("t0marglst", arg),
      jsonize_option<sort0>("sort0opt", res),
      jsonize(tdef)
    )
  )
//
| D0Cabsimpl(tok, sqid, smas, res0, teq1, def2) =>
  jsonify("D0Cabsimpl", ("tok", "sqid", "smas", "res0", "teq1", "def2"),
    (
      jsonize(tok),
      jsonize(sqid),
      jsonize_list<s0marg>("s0marglst", smas),
      jsonize_option<sort0>("sort0opt", res0),
      jsonize(teq1),
      jsonize(def2)
    )
  )
//
| D0Cvaldecl(tok, mopt, d0cs) =>
  jsonify("D0Cvaldecl", ("tok", "mopt", "d0cs"),
    (jsonize(tok), jsonize(mopt), jsonize_list<v0aldecl>("v0aldeclst", d0cs))
  )
//
| D0Cvardecl(tok, mopt, d0cs) =>
  jsonify(
    "D0Cvardecl", ("tok", "mopt", "d0cs"),
    (jsonize(tok), jsonize(mopt), jsonize_list<v0ardecl>("v0ardeclst", d0cs))
  )
//
| D0Cfundecl(tok, mopt, tqas, d0cs) =>
  jsonify("D0Cfundecl", ("tok", "mopt", "tqas", "d0cs"),
    (
      jsonize(tok),
      jsonize(mopt),
      jsonize_list<tq0arg>("tq0arglst", tqas),
      jsonize_list<f0undecl>("f0undeclst", d0cs)
    )
  )
//
| D0Cimpdecl
  (tok, mopt, sqas, tqas, dqid, tias, f0as, res0, teq1, d0e2) =>
  jsonify("D0Cimpdecl",
    (
    "tok", "mopt", "sqas", "tqas", "dqid", "tias", "f0as", "res0", "teq1", "d0e2"
    ),
    (
      jsonize(tok),
      jsonize(mopt),
      jsonize_list<sq0arg>("sq0arglst", sqas),
      jsonize_list<tq0arg>("tq0arglst", tqas),
      jsonize(dqid),
      jsonize_list<ti0arg>("ti0arglst", tias),
      jsonize_list<f0arg>("f0arglst", f0as),
      jsonize(res0),
      jsonize(teq1),
      jsonize(d0e2)
    )
  )
//
| D0Csymload(tok, sym, twth, dqid, tint) =>
  jsonify("D0Csymload", ("tok", "sym", "twth", "dqid", "tint"),
    (
      jsonize(tok),
      jsonize(sym),
      jsonize(twth),
      jsonize(dqid),
      jsonize_option<t0int>("t0intopt", tint)
    )
  )
//
| D0Cdatasort(tok, d0cs) =>
  jsonify("D0Cdatasort", ("tok", "d0cs"),
    (
      jsonize(tok),
      jsonize_list<d0tsort>("d0tsortlst", d0cs)
    )
  )
//
| D0Cdatatype(tok, d0cs, wopt) =>
  jsonify("D0Cdatatype", ("tok", "d0cs", "wopt"),
    (
      jsonize(tok),
      jsonize_list<d0atype>("d0atypelst",d0cs),
      jsonize(wopt)
    )
  )
//
| D0Cdynconst
  (tok, tqas, d0cs) =>
  jsonify("D0Cdynconst", ("tok", "tqas", "d0cs"),
    (
      jsonize(tok),
      jsonize_list<tq0arg>("tq0arglst", tqas),
      jsonize_list<d0cstdecl>("d0cstdeclst", d0cs)
    )
  )
//
| D0Clocal
  (tbeg, d0cs0, topt, d0cs1, tend) =>
  jsonify(
    "D0Clocal", ("tbeg", "d0cs0", "topt", "d0cs1", "tend"),
    (
      jsonize(tbeg),
      jsonize_list<d0ecl>("d0eclst", d0cs0),
      jsonize_option<token>("tokenopt", topt),
      jsonize_list<d0ecl>("d0eclst", d0cs1),
      jsonize(tend)
    )
  )
//
(*
| _(*rest-of-d1ecl*) =>
    jsonify("jsonize_d1ecl: D0C...: not-yet-implemented")
*)
//
) (* end of [jsonize_d0ecl] *)
end

end // end of [local]


implement
jsonize_precopt
  (x0) =
node("precopt", res) where
val res =
(
case+ x0 of
| PRECOPTnil() =>
  jsonify("PRECOPTnil")
| PRECOPTint(tint) =>
  jsonify("PRECOPTint", "tint", jsonize(tint))
| PRECOPTopr(topr, pmod) =>
  jsonify("PRECOPTopr", ("topr", "pmod"), (jsonize(topr), jsonize(pmod)))
)
end


implement
jsonize_signint
  (x0) =
node("signint", res) where
val res =
(
case+ x0 of
| SIGNINTint(tint) =>
  jsonify("SIGNINTint", "tint", jsonize(tint))
| SIGNINTopr(topr, tint) =>
  jsonify("SIGNINTopr", ("topr", "tint"), (jsonize(topr), jsonize(tint)))
)
end


implement
jsonize_precmod
  (x0) =
node("precmod", res) where
val res =
(
case+ x0 of
| PRECMODnone() =>
  jsonify("PRECMODnone")
| PRECMODsome(tbeg, sint, tend) =>
  jsonify("PRECMODsome", ("tbeg", "sint", "tend"),
    (
      jsonize(tbeg),
      jsonize(sint),
      jsonize(tend)
    )
  )
)
end


implement
jsonize_abstdf0
  (x0) =
node("abstdf0", res) where
val res =
(
case+ x0 of
| ABSTDF0some() =>
  jsonify("ABSTDF0some")
| ABSTDF0lteq(tok, s0e) =>
  jsonify("ABSTDF0lteq", ("tok", "s0e"), (jsonize(tok), jsonize(s0e)))
| ABSTDF0eqeq(tok, s0e) =>
  jsonify("ABSTDF0eqeq", ("tok", "s0e"), (jsonize(tok), jsonize(s0e)))
)
end


implement
jsonize_g0expdef
  (x0) =
node("g0expdef", res) where
val res =
(
case+ x0 of
| G0EDEFnone() =>
  jsonify("G0EDEFnone")
| G0EDEFsome(topt, g0e1) =>
  jsonify("G0EDEFsome", ("topt", "g0e1"),
    (
      jsonize_option<token>("tokenopt", topt),
      jsonize(g0e1)
    )
  )
)
end


implement
jsonize_d0macdef
  (x0) =
node("d0macdef", res) where
val res =
(
case+ x0 of
| D0MDEFnone() =>
  jsonify("D0MDEFnone")
| D0MDEFsome(topt, d0e1) =>
  jsonify("D0MDEFsome", ("topt", "d0e1"),
    (
      jsonize_option<token>("tokenopt", topt),
      jsonize(d0e1)
    )
  )
)
end

implement
jsonize_wd0eclseq
  (x0) =
node("wd0eclseq", res) where
val res =
(
case+ x0 of
| WD0CSnone() =>
  jsonify("WD0CSnone")
| WD0CSsome(tbeg, topt, d0cs, tend) =>
  jsonify("WD0CSsome", ("tbeg", "topt", "d0cs", "tend"),
    (
      jsonize(tbeg),
      jsonize_option<token>("tokenopt", topt),
      jsonize_list<d0ecl>("d0eclst", d0cs),
      jsonize(tend)
    )
  )
)
end


implement
jsonize_val<d0exp> = jsonize_d0exp

implement
jsonize_v0aldecl
  (x0) = let
//
val+V0ALDECL(rcd) = x0
//
in
node("v0aldecl", res) where
val res =
  jsonify("V0ALDECL", ("pat", "teq", "def", "wtp"),
    (
      jsonize(rcd.pat),
      jsonize_option<token>("tokenopt", rcd.teq),
      jsonize_option<d0exp>("d0expopt", rcd.def),
      jsonize(rcd.wtp)
    )
  )
end
end


implement
jsonize_val<i0dnt> = jsonize_i0dnt
implement
jsonize_val<s0exp> = jsonize_s0exp

implement
jsonize_v0ardecl
  (x0) = let
//
val+V0ARDECL(rcd) = x0
//
in
node("v0ardecl", res) where
val res =
  jsonify("V0ARDECL", ("nam", "wth", "res", "ini"),
    (
      jsonize(rcd.nam),
      jsonize_option<i0dnt>("i0dntopt", rcd.wth),
      jsonize_option<s0exp>("s0expopt", rcd.res),
      jsonize(rcd.ini)
    )
  )
end

end // end of [jsonize_v0ardecl]


implement
jsonize_val<f0arg> = jsonize_f0arg

implement
jsonize_f0undecl
  (x0) = let
//
val+F0UNDECL(rcd) = x0
//
in
node("f0undecl", res) where
val res =
  jsonify("F0UNDECL", ("nam", "arg", "res", "teq", "def", "wtp"),
    (
      jsonize(rcd.nam),
      jsonize_list<f0arg>("f0arglst", rcd.arg),
      jsonize(rcd.res),
      jsonize_option<token>("tokenopt", rcd.teq),
      jsonize_option<d0exp>("d0expopt", rcd.def),
      jsonize(rcd.wtp)
    )
  )
end

end


implement
jsonize_val<d0arg> = jsonize_d0arg

implement
jsonize_d0cstdecl
  (x0) = let
//
val+D0CSTDECL(rcd) = x0
//
in
node("d0cstdecl", res) where
val res =
  jsonify(
    "D0CSTDECL", ("nam", "arg", "res", "def"),
    (
      jsonize(rcd.nam),
      jsonize_list<d0arg>("d0arglst", rcd.arg),
      jsonize(rcd.res),
      jsonize(rcd.def)
    )
  )
end

end
