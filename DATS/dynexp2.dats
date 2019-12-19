#include "share/atspre_staload.hats"
#staload UN = "prelude/SATS/unsafe.sats"

#include "./../HATS/libxatsopt.hats"
#staload "{$x}/SATS/dynexp1.sats"
#staload "{$x}/SATS/dynexp2.sats"

#staload "./../SATS/json.sats"
#staload "./../SATS/lexing.sats"
#staload "./../SATS/locinfo.sats"
#staload "./../SATS/label0.sats"
#staload "./../SATS/filpath.sats"
#staload "./../SATS/dynexp0.sats"
#staload "./../SATS/staexp0.sats"
#staload "./../SATS/dynexp1.sats"
#staload "./../SATS/dynexp2.sats"
#staload "./../SATS/staexp2.sats"

#staload _ = "./json.dats"

#staload SYM_J = "./../SATS/symbol.sats"
#staload STM_J = "./../SATS/stamp0.sats"

(* overload jsonize with $D1.jsonize_d1exp *)
(* overload jsonize with $D1.jsonize_d1ecl *)

overload jsonize with $SYM_J.jsonize_symbol
overload jsonize with $STM_J.jsonize_stamp


implement
jsonize_val<s2exp> = jsonize_s2exp
implement
jsonize_val<filpath> = jsonize_filpath


implement
jsonize_val<d2con> = jsonize_d2con

implement
jsonize_d2con
  (x0) =
(
  node2("d2con", jsonize(x0.loc()), rst) where
    val lst = $list{labjsonval}
    (
      $SYM_J.labify_symbol(x0.sym()),
      $STM_J.labify_stamp(x0.stamp())
    )
    val rst = JSONlablist(lst)
  end

)


implement
jsonize_val<d2cst> = jsonize_d2cst

implement
jsonize_d2cst
  (x0) =
(
  node2("d2cst", jsonize(x0.loc()), rst) where
    val lst = $list{labjsonval}(
      $SYM_J.labify_symbol(x0.sym()),
      $STM_J.labify_stamp(x0.stamp())
    )
    val rst = JSONlablist(lst)
  end
)


implement
jsonize_val<d2var> = jsonize_d2var

implement
jsonize_d2var
  (x0) =
  node2("d2var", jsonize(x0.loc()), rst) where
    val lst = $list{labjsonval}(
      $SYM_J.labify_symbol(x0.sym()),
      $STM_J.labify_stamp(x0.stamp())
    )
    val rst = JSONlablist(lst)
  end


implement
jsonize_val<f2arg> = jsonize_f2arg
implement
jsonize_val<d2pat> = jsonize_d2pat
implement
jsonize_val<s2var> = jsonize_s2var
implement
jsonize_val<s2exp> = jsonize_s2exp


implement
jsonize_f2arg
  (x0) =
node2("f2arg", jsonize(x0.loc()), res) where
val res =
(
//
case+
x0.node() of
(*
| F2ARGnone(tok) =>
  jsonize("F2ARGnone", "tok", jsonize(tok))
*)
| F2ARGsome_met(s2es) =>
  jsonify("F2ARGsome_met", "s2es", jsonize_list<s2exp>("s2explst", s2es))
| F2ARGsome_dyn(npf, d2ps) =>
  jsonify("F2ARGsome_dyn", ("npf", "d2ps"),
    (labval("npf", jsonize(npf)),
      jsonize_list<d2pat>("d2patlst", d2ps)
    )
  )
| F2ARGsome_sta(s2vs, s2ps) =>
  jsonify("F2ARGsome_sta", ("s2vs", "s2ps"),
    (
      jsonize_list<s2var>("s2varlst", s2vs),
      jsonize_list<s2exp>("s2explst", s2ps)
    )
  )
)
end




implement
jsonize_val<d2pat> = jsonize_d2pat
implement
jsonize_val<s2var> = jsonize_s2var

implement
jsonize_val<d2pitm> = jsonize_d2pitm


implement
jsonize_d2pat
  (x0) =
node2("d2pat", jsonize(x0.loc()), res) where
val res =
(
case- x0.node() of
//
| D2Pint(tok) =>
  jsonify("D2Pint", "tok", jsonize(tok))
| D2Pbtf(tok) =>
  jsonify("D2Pbtf", "tok", jsonize(tok))
| D2Pchr(tok) =>
  jsonify("D2Pchr", "tok", jsonize(tok))
| D2Pflt(tok) =>
  jsonify("D2Pflt", "tok", jsonize(tok))
| D2Pstr(tok) =>
  jsonify("D2Pstr", "tok", jsonize(tok))
//
| D2Pvar(d2v) =>
  jsonify("D2Pvar", "d2v", jsonize(d2v))
//
| D2Pcon1(d2c0) =>
  jsonify("D2Pcon1", "d2c0", jsonize(d2c0))
| D2Pcon2(d2cs) =>
  jsonify("D2Pcon2", "d2cs", jsonize_list<d2con>("d2conlst", d2cs))
//
| D2Psym0(sym, d2pis) =>
  jsonify("D2Psym0", ("sym", "d2pis"), (jsonize(sym), jsonize_list<d2pitm>("d2pitmlst", d2pis)))
//
| D2Psapp(d2f0, s2vs) =>
  jsonify("D2Psapp", ("d2f0", "s2vs"),
    (jsonize(d2f0), jsonize_list<s2var>("s2varlst", s2vs))
  )
| D2Pdapp(d2f0, npf0, d2ps) =>
  jsonify("D2Pdapp", ("d2f0", "npf0", "d2ps"),
    (jsonize(d2f0), labval("npf", jsonize(npf0)), jsonize_list<d2pat>("d2patlst", d2ps))
  )
//
| D2Ptuple(knd, npf, d2ps) =>
  jsonify("D2Ptuple", ("knd", "npf", "d2ps"),
    (jsonize(knd), labval("npf", jsonize(npf)), jsonize_list<d2pat>("d2patlst", d2ps))
  )
//
| D2Panno(d2p1, s2e2) =>
  jsonify("D2Panno", ("d2p1", "s2e2"), (jsonize(d2p1), jsonize(s2e2)))
//
| D2Pnone0() =>
  jsonify("D2Pnone0")
| D2Pnone1(d1psrc) =>
  jsonify("D2Pnone1", "d1psrc", jsonize(d1psrc))
//
) (* end of [jsonize_d2pat] *)
end


implement
jsonize_val<d2exp> = jsonize_d2exp
implement
jsonize_val<d2ecl> = jsonize_d2ecl
implement
jsonize_val<d2pitm> = jsonize_d2pitm
implement
jsonize_val<s2exp> = jsonize_s2exp
implement
jsonize_val<d2clau> = jsonize_d2clau

implement
jsonize_val<f2arg> = jsonize_f2arg
implement
jsonize_val<effs2expopt> = jsonize_effs2expopt


implement
jsonize_d2exp
  (x0) =
node2("d2exp", jsonize(x0.loc()), res) where
val res =
(
case+ x0.node() of
//
| D2Eint(tok) =>
  jsonify("D2Eint", "tok", jsonize(tok))
| D2Ebtf(tok) =>
  jsonify("D2Ebtf", "tok", jsonize(tok))
| D2Echr(tok) =>
  jsonify("D2Echr", "tok", jsonize(tok))
| D2Eflt(tok) =>
  jsonify("D2Eflt", "tok", jsonize(tok))
| D2Estr(tok) =>
  jsonify("D2Estr", "tok", jsonize(tok))
//
| D2Etop(tok) =>
  jsonify("D2Etop", "tok", jsonize(tok))
//
| D2Evar(d2v) =>

  jsonify("D2Evar", "d2v", jsonize(d2v))
//
| D2Ecst1(d2c) =>
  jsonify("D2Ecst1", "d2c", jsonize(d2c))
| D2Econ1(d2c) =>
  jsonify("D2Econ1", "d2c", jsonize(d2c))
| D2Ecst2(d2cs) =>
  jsonify("D2Ecst2", "d2cs", jsonize_list<d2cst>("d2cstlst", d2cs))
| D2Econ2(d2cs) =>
  jsonify("D2Econ2", "d2cs", jsonize_list<d2con>("d2conlst", d2cs))
//
| D2Esym0
  (d1e1, dpis) =>
  jsonify("D2Esym0", ("d1e1", "dpis"), (jsonize(d1e1), jsonize_list<d2pitm>("d2pitmlst", dpis)))
//
| D2Esapp
  (d2f0, s2as) =>
  jsonify("D2Esapp", ("d2f0", "s2as"), (jsonize(d2f0), jsonize_list<s2exp>("s2explst", s2as)))
| D2Etapp
  (d2f0, s2as) =>
  jsonify("D2Etapp", ("d2f0", "s2as"), (jsonize(d2f0), jsonize_list<s2exp>("s2explst", s2as)))
| D2Edapp
  (d2f0, npf0, d2as) =>
  jsonify(
    "D2Edapp", ("d2f0", "npf0", "d2as"),
    (jsonize(d2f0), labval("npf", jsonize(npf0)), jsonize_list<d2exp>("d2explst", d2as)))
//
| D2Elet
  (d2cs, d2e2) =>
  jsonify("D2Elet", ("d2cs", "d2e2"), (jsonize_list<d2ecl>("d2eclst", d2cs), jsonize(d2e2)))
| D2Ewhere
  (d2e1, d2cs) =>
  jsonify("D2Ewhere", ("d2e1", "d2cs"), (jsonize(d2e1), jsonize_list<d2ecl>("d2eclst", d2cs)))
//
| D2Eseqn
  (d2es, d1e1) =>
  jsonify("D2Eseqn", ("d2es", "d1e1"(*last*)),
    (jsonize_list<d2exp>("d2explst", d2es), jsonize(d1e1)))
//
| D2Etuple
  (knd, npf, d2es) =>
  jsonify("D2Etuple", ("knd", "npf", "d2es"),
    (jsonize(knd), labval("npf", jsonize(npf)), jsonize_list<d2exp>("d2explst", d2es)))
//
| D2Eassgn
  (d2e1, d2e2) =>
  jsonify("D2Eassgn", ("d2e1", "d2e2"), (jsonize(d2e1), jsonize(d2e2)))
//
| D2Edtsel
  (lab0, dpis, npf2, arg3) =>
  (
  case+ arg3 of
  | None() =>
    jsonify("D2Edtsel", ("lab0", "dpis"), (jsonize(lab0), jsonize_list<d2pitm>("d2pitmlst", dpis)))
  | Some(d2es) =>
    jsonify("D2Edtsel", ("lab0", "dpis", "npf2", "d2es"),
      (
        jsonize(lab0),
        jsonize_list<d2pitm>("d2pitmlst", dpis),
        labval("npf", jsonize(npf2)),
        jsonize_list<d2exp>("d2explst", d2es)
      )
    )
  )
//
| D2Eif0
  (d2e1, d2e2, opt3) =>
  jsonify("D2Eif0", ("d2e1", "d2e2", "opt3"),
    (jsonize(d2e1), jsonize(d2e2), jsonize_option<d2exp>("d2expopt", opt3)))
//
| D2Ecase
  (knd, d2e1, d2cls) =>
  jsonify("D2Ecase", ("knd", "d2e1", "d2cls"),
    (jsonize(knd), jsonize(d2e1), jsonize_list<d2clau>("d2claulst", d2cls)))
//
| D2Elam
  (knd, f2as, tres, arrw, body) =>
  jsonify("D2Elam", ("knd", "f2as", "tres", "arrw", "body"),
    (
      jsonize(knd),
      jsonize_list<f2arg>("f2arglst", f2as),
      jsonize(tres),
      jsonize(arrw),
      jsonize(body)
    )
  )
| D2Efix
  (knd, fid, f2as, tres, arrw, body) =>
  jsonify("D2Efix", ("knd", "fid", "f2as", "tres", "arrw", "body"),
    (
      jsonize(knd),
      jsonize(fid),
      jsonize_list<f2arg>("f2arglst", f2as),
      jsonize(tres),
      jsonize(arrw),
      jsonize(body)
    )
  )
//
(*
| D2Eflat(d2e1) =>
  fprint!(out, "D2Eflat(", d2e1, ")")
*)
//
| D2Eaddr(d2e1) =>
  jsonify("D2Eaddr", "d2e1", jsonize(d2e1))
| D2Eeval(d2e1) =>
  jsonify("D2Eeval", "d2e1", jsonize(d2e1))
| D2Efold(d2e1) =>
  jsonify("D2Efold", "d2e1", jsonize(d2e1))
//
| D2Elazy(d2e1) =>
  jsonify("D2Elazy", "d2e1", jsonize(d2e1))
| D2Ellazy(d2e1, opt2) =>
  jsonify("D2Ellazy", ("d2e1", "opt2"),
    (
      jsonize(d2e1),
      jsonize_option<d2exp>("d2expopt", opt2)
    )
  )
//
| D2Eanno(d2e1, s2e2) =>
  jsonify("D2Eanno", ("d2e1", "s2e2"), (jsonize(d2e1), jsonize(s2e2)))
//
| D2Enone0() =>
  jsonify("D2Enone0")
| D2Enone1(d1esrc) =>
  jsonify("D2Enone1", "d1esrc", jsonize(d1esrc))
//
)
end // end of where






local

implement
jsonize_val<d2ecl> = jsonize_d2ecl
implement
jsonize_val<v2aldecl> = jsonize_v2aldecl
implement
jsonize_val<v2ardecl> = jsonize_v2ardecl
implement
jsonize_val<f2undecl> = jsonize_f2undecl

implement
jsonize_val<f2arg> = jsonize_f2arg
implement
jsonize_val<ti2arg> = jsonize_ti2arg
implement
jsonize_val<tq2arg> = jsonize_tq2arg
implement
jsonize_val<sq2arg> = jsonize_sq2arg

overload jsonize with jsonize_loc_t of 1

in (* in-of-local *)

implement
jsonize_d2ecl
  (x0) =
node2("d2ecl", jsonize(x0.loc()), res) where
val res =
(
case- x0.node() of
//
| D2Cstatic
  (tok, d2c) =>
  jsonify("D2Cstatic", "d2c", jsonize(d2c))
| D2Cextern
  (tok, d2c) =>
  jsonify("D2Cextern", ("tok", "d2c"), (jsonize(tok), jsonize(d2c)))
//
| D2Cinclude
  ( tok
  , src, knd
  , fopt, body) =>
  jsonify("D2Cinclude",
    (
      "tok", "src", "knd", "fopt", "body"
    ),
    (
      jsonize(tok),
      jsonize(src),
      labval("knd", jsonize(knd)),
      jsonize_option<filpath>("filpathopt", fopt),
      body
    ) where

      val body =
      (
      case+ body of
      | None _ => "None()"
      | Some _ => "Some(<d2cls>)"
      ): string
      val body = jsonize(body)

      // to expand the list of d2ecls from included file
      (*
      val body =
      (
      case+ body of
      | None _ => jnul()
      | Some d => labval("Some", jsonize_list<d2ecl>("d2eclst", d))
      )
      *)

    end
  )
//
| D2Cstaload
  ( tok
  , src, knd
  , fopt, flag, body) =>
  jsonify("D2Cinclude",
    (
      "tok", "src", "knd", "fopt", "flag", "body"
    ),
    (
      jsonize(tok),
      jsonize(src),
      labval("knd", jsonize(knd)),
      jsonize_option<filpath>("filpathopt", fopt),
      labval("flag", jsonize(flag)),
      body
    ) where
      val body =
      (
      case+ body of
      | None _ => "None()"
      | Some _ => "Some(<fmodenv>)"
      ): string
      val body = jsonize(body)

      // to expand the list of d1ecls from included file
      (*
      val body =
      (
      case+ body of
      | None _ => jnul()
      | Some d => labval("Some", jsonize_list<d2ecl>("d2eclst", d))
      )
      *)
      end
  )
//
| D2Clocal(head, body) =>
  jsonify("D2Clocal", ("head", "body"),
    (
      jsonize_list<d2ecl>("d2eclst", head),
      jsonize_list<d2ecl>("d2eclst", body)
    )
  )
//
| D2Cabssort(d1c) =>
  jsonify("D2Cabssort", "d1c", jsonize(d1c))
//
| D2Cstacst0(s2c, s2t) =>
  jsonify("D2Cstacst0", ("s2c", "s2t"), (jsonize(s2c), jsonize(s2t)))
//
| D2Csortdef(sym, s2tx) =>
  jsonify("D2Csortdef", ("sym", "s2tx"),
    (jsonize(sym), jsonize(s2tx))
  )
//
| D2Csexpdef(s2c, s2e) =>
  jsonify("D2Csexpdef", ("s2c", "s2e"), (jsonize(s2c), jsonize(s2e)))
//
| D2Cabstype(s2c, df2) =>
  jsonify("D2Cabstype", ("s2c", "df2"), (jsonize(s2c), jsonize(df2)))
//
| D2Cabsimpl
  (knd, sqid, def0) =>
  jsonify(
    "D2Csymload",
    (
      "knd", "sqid", "def0"
    ),
    (
      jsonize(knd),
      jsonize(sqid),
      jsonize(def0)
    )
  )

//
| D2Csymload
  (tok, sym0, dpi1) =>
  jsonify(
    "D2Csymload",
    (
      "tok", "sym0", "dpi1"
    ),
    (
      jsonize(tok),
      jsonize(sym0),
      jsonize(dpi1)
    )
  )
//
| D2Cvaldecl
  (knd, mopt, v2ds) =>
  jsonify(
    "D2Cvaldecl",
    (
      "knd", "mopt", "v2ds"
    ),
    (
      jsonize(knd),
      jsonize(mopt),
      jsonize_list<v2aldecl>("v2aldeclst", v2ds)
    )
  )

| D2Cfundecl
  (knd, mopt, tqas, f2ds) =>
  jsonify(
    "D2Cfundecl",
    (
      "knd", "mopt", "tqas", "f2ds"
    ),
    (
      jsonize(knd),
      jsonize(mopt),
      jsonize_list<tq2arg>("tq2arglst", tqas),
      jsonize_list<f2undecl>("f2undeclst", f2ds)
    )
  )

//
| D2Cvardecl(knd, mopt, v2ds) =>
  jsonify(
    "D2Cvardecl", ("knd", "mopt", "v2ds"),
    (
      jsonize(knd),
      jsonize(mopt),
      jsonize_list<v2ardecl>("v2ardeclst", v2ds)
    )
  )
//
| D2Cimpdecl1
  ( knd, mopt, sqas, tqas
  , dqid, tias, f2as, res0, d2e1) =>
  jsonify(
    "D2Cimpdecl1",
    (
      "knd", "mopt", "sqas", "tqas", "dqid", "tias", "f2as", "res0", "d2e1"
    ),
    (
      jsonize(knd),
      jsonize(mopt),
      jsonize_list<sq2arg>("sq2arglst", sqas),
      jsonize_list<tq2arg>("tq2arglst", tqas),
      jsonize(dqid),
      jsonize_list<ti2arg>("ti2arglst", tias),
      jsonize_list<f2arg>("f2arglst", f2as),
      jsonize(res0),
      jsonize(d2e1)
    )
  )
| D2Cimpdecl2
  ( knd, mopt, sqas, tqas
  , dqid, tias, f2as, res0, d2e1) =>
  jsonify(
    "D2Cimpdecl2",
    (
      "knd", "mopt", "sqas", "tqas", "dqid", "tias", "f2as", "res0", "d2e1"
    ),
    (
      jsonize(knd),
      jsonize(mopt),
      jsonize_list<sq2arg>("sq2arglst", sqas),
      jsonize_list<tq2arg>("tq2arglst", tqas),
      jsonize(dqid),
      jsonize_list<ti2arg>("ti2arglst", tias),
      jsonize_list<f2arg>("f2arglst", f2as),
      jsonize(res0),
      jsonize(d2e1)
    )
  )
//
| D2Cdatasort(d1c) =>
  jsonify("D2Cdatasort", "d1c", jsonize(d1c))
| D2Cdatatype(d1c) =>
  jsonify("D2Cdatatype", "d1c", jsonize(d1c))
//
| D2Cdynconst
  (knd, tqas, d2cs) =>
  jsonify(
    "D2Cdynconst",
    (
      "knd", "tqas", "d2cs"
    ),
    (
      jsonize(knd),
      jsonize_list<tq2arg>("tq2arglst", tqas),
      jsonize_list<d2cst>("d2cstlst", d2cs)
    )
  )
//
| D2Cnone0() =>
  jsonify("D2Cnone0")
| D2Cnone1(d1csrc) =>
  jsonify("D2Csortdef", "d1csrc", jsonize(d1csrc))
//
) (* end of [jsonize_d2ecl] *)
end

end // end of [local]



implement
jsonize_d2itm
  (x0) =
node("d2itm", res) where
val res =
(
case+ x0 of
//
| D2ITMvar(d2v0) =>
  jsonify("D2ITMvar", "d2v0", jsonize(d2v0))
//
| D2ITMcon(d2cs) =>
  jsonify("D2ITMcon", "d2cs", jsonize_list<d2con>("d2conlst", d2cs))
//
| D2ITMcst(d2cs) =>
  jsonify("D2ITMcst", "d2cs", jsonize_list<d2cst>("d2cstlst", d2cs))
//
| D2ITMsym(sym, dpis) =>
  jsonify("D2ITMsym", ("sym", "dpis"),
    (
      jsonize(sym),
      jsonize_list<d2pitm>("d2pitmlst", dpis)
    )
  )
//
) (* end of [jsonize_d2itm] *)
end

implement
jsonize_d2pitm
  (x0) =
node("d2pitm", res) where
val res =
(
case+ x0 of
| D2PITMnone(dqid) =>
  jsonify("D2PITMnone", "dqid", jsonize(dqid))
| D2PITMsome(pval, d2i0) =>
  jsonify("D2PITMsome", ("pval", "d2i0"),
    (
      labval("pval", jsonize(pval)),
      jsonize(d2i0)
    )
  )
) (* end of [jsonize_d2pitm] *)
end

local

implement
jsonize_val<s2var> = jsonize_s2var

in

implement
jsonize_sq2arg
  (x0) =
(
  node2(
    "sq2arg",
    jsonize(x0.loc()),
    jsonize_list<s2var>("s2varlst", x0.s2vs())
  )
) (* end of [jsonize_sq2arg] *)

implement
jsonize_tq2arg
  (x0) =
(
  node2(
    "tq2arg",
    jsonize(x0.loc()),
    jsonize_list<s2var>("s2varlst", x0.s2vs())
  )
) (* end of [jsonize_tq2arg] *)

end

implement
jsonize_ti2arg
  (x0) =
(
  node2(
    "ti2arg",
    jsonize(x0.loc()),
    jsonize_list<s2exp>("s2explst", x0.s2es())
  )
) (* end of [jsonize_ti2arg] *)


local

implement
jsonize_val<d2cst> = jsonize_d2cst

in

implement
jsonize_impld2cst
  (x0) =
node("impld2cst", res) where
val res =
(
case+ x0 of
|
IMPLD2CST1(dqid, d2cs) =>
  jsonify("IMPLD2CST1", ("d2qid", "d2cs"), (
    jsonize(dqid), jsonize_list<d2cst>("d2cstlst", d2cs))
  )
|
IMPLD2CST2(dqid, d2cs, opt2) =>
  jsonify("IMPLD2CST2", ("d2qid", "d2cs", "opt2"),
    (
      jsonize(dqid),
      jsonize_list<d2cst>("d2cstlst", d2cs),
      jsonize_option<d2cst>("d2cstopt", opt2)
    )
  )
) // end of [jsonize_impld2cst]
end

implement
jsonize_val<s2cst> = jsonize_s2cst

implement
jsonize_impls2cst
  (x0) =
node("impls2cst", res) where
val res =
(
case+ x0 of
| IMPLS2CST1(sqid, s2cs) =>
  jsonify("IMPLS2CST1", ("sqid", "s2cs"),
    (jsonize(sqid), jsonize_list<s2cst>("s2cstlst", s2cs))
  )
| IMPLS2CST2(sqid, s2cs, opt2) =>
  jsonify("IMPLS2CST2", ("sqid", "s2cs", "opt2"),
    (
      jsonize(sqid),
      jsonize_list<s2cst>("s2cstlst", s2cs),
      jsonize_option<s2cst>("s2cstopt", opt2)
    )
  )
) // end of [jsonize_impls2cst]
end

end


implement
jsonize_d2clau
  (x0) =
node2("d2clau", jsonize(x0.loc()), res) where
val res =
(
case+
x0.node() of
| D2CLAUpat(d2gp) =>
  jsonify("D2CLAUpat", "d2gp", jsonize(d2gp))
| D2CLAUexp(d2gp, d0e0) =>
  jsonify("D2CLAUexp", ("d2gp", "d0e0"), (jsonize(d2gp), jsonize(d0e0)))
)
end


implement
jsonize_d2gua
  (x0) =
node("d2gua", res) where
val res =
(
case+
x0.node() of
| D2GUAexp(d2e) =>
  jsonify("D2GUAexp", "d2e", jsonize(d2e))
| D2GUAmat(d2e, d2p) =>
  jsonify("D2GUAmat", ("d2e", "d2p"), (jsonize(d2e), jsonize(d2p)))
)
end (* end of [jsonize_d2gua] *)


implement
jsonize_val<d2gua> = jsonize_d2gua

implement
jsonize_d2gpat
  (x0) =
node2("d2gpat", jsonize(x0.loc()), res) where
val res =
(
case+
x0.node() of
| D2GPATpat(d2p) =>
  jsonify("D2GPATpat", "d2p", jsonize(d2p))
| D2GPATgua(d2p, d2gs) =>
  jsonify("D2GPATgua", ("d2p", "d2gs"),
    (
      jsonize(d2p),
      jsonize_list<d2gua>("d2gualst", d2gs)
    )
  )
)
end

implement
jsonize_v2aldecl
  (x0) = let
//
val+V2ALDECL(rcd) = x0
//
in
  node2("v2arlecl", jsonize(rcd.loc), res) where
  val res =
  jsonify("V2ALDECL", ("pat", "def", "wtp"),
    (
      jsonize(rcd.pat),
      jsonize_option<d2exp>("d2expopt", rcd.def),
      jsonize_option<s2exp>("s2expopt", rcd.wtp)
    )
  )
  end
end // end of [jsonize_v2aldecl]


implement
jsonize_v2ardecl
  (x0) = let
//
val+V2ARDECL(rcd) = x0
//
in
  node2("v2ardecl", jsonize(rcd.loc), res) where
  val res =
  jsonify(
  "V2ARDECL", ("d2v", "wth", "res", "ini"),
    (
      jsonize(rcd.d2v),
      jsonize_option<d2var>("d2varopt", rcd.wth),
      jsonize_option<s2exp>("s2expopt", rcd.res),
      jsonize_option<d2exp>("d2expopt", rcd.ini)
    )
  )
  end
end // end of [jsonize_v2ardecl]


implement
jsonize_f2undecl
  (x0) = let
//
val+F2UNDECL(rcd) = x0
//
in
  node2("f2undecl", jsonize(rcd.loc), res) where
  val res =
  jsonify("F2UNDECL", ("nam", "d2c", "arg", "res", "def", "wtp"),
    (
      jsonize(rcd.nam),
      jsonize(rcd.d2c),
      jsonize_list<f2arg>("f2arglst", rcd.arg),
      jsonize(rcd.res),
      jsonize_option<d2exp>("d2expopt", rcd.def),
      jsonize_option<s2exp>("s2expopt", rcd.wtp)
    )
  )
  end
end // end of [jsonize_f2undecl]
