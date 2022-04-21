package fuselang.common

import scala.language.implicitConversions

import Syntax._
import PrettyPrint.Doc
import PrettyPrint.Doc._

object Pretty:

  def emitProg(p: Prog)(implicit debug: Boolean): String =
    val layout = vsep(p.includes.map(emitInclude)) <@>
      vsep(p.defs.map(emitDef)) <@>
      vsep(p.decors.map(d => text(d.value))) <@>
      vsep(p.decls.map(d => text("decl") <+> emitDecl(d) <> semi)) <@>
      emitCmd(p.cmd)

    layout.pretty

  def emitInclude(incl: Include)(implicit debug: Boolean): Doc =
    text("import") <+>
      vsep(incl.backends.map({
        case (b, incl) => text(b.toString) <> parens(quote(text(incl)))
      })) <+> scope(
      vsep(incl.defs.map(emitDef))
    )

  def emitDef(defi: Definition)(implicit debug: Boolean): Doc = defi match
    case FuncDef(id, args, ret, bodyOpt) => {
      val retDoc = ret match
        case _: TVoid => emptyDoc
        case _ => colon <+> emitTyp(ret)
      text("def") <+> id <> parens(ssep(args.map(emitDecl), comma <> space)) <+>
        retDoc <> bodyOpt.map(c => equal <+> scope(emitCmd(c))).getOrElse(semi)
    }
    case RecordDef(name, fields) => {
      text("record") <+> name <+> scope(vsep(fields.map({
        case (id, typ) => id <> colon <+> emitTyp(typ) <> semi
      })))
    }

  def emitDecl(d: Decl): Doc = emitId(d.id)(false) <> colon <+> emitTyp(d.typ)

  def emitConsume(ann: Annotations.Consumable): Doc = ann match
    case Annotations.Consumable.ShouldConsume => text("consume")
    case Annotations.Consumable.SkipConsume => text("skip")

  implicit def emitId(id: Id)(implicit debug: Boolean): Doc =
    val idv = value(id.v)
    if debug then
      id.typ.map(t => idv <> text("@") <> emitTyp(t)).getOrElse(idv)
    else idv

  def emitTyp(t: Type): Doc = text(t.toString)

  def emitBaseInt(v: Int, base: Int): String = base match
    case 8 => s"0${Integer.toString(v, 8)}"
    case 10 => v.toString
    case 16 => s"0x${Integer.toString(v, 16)}"

  implicit def emitExpr(e: Expr)(implicit debug: Boolean): Doc = e match
    case ECast(e, typ) => parens(e <+> text("as") <+> emitTyp(typ))
    case EApp(fn, args) => fn <> parens(commaSep(args.map(emitExpr)))
    case EInt(v, base) => value(emitBaseInt(v, base))
    case ERational(d) => value(d)
    case EBool(b) => value(if b then "true" else "false")
    case EVar(id) => emitId(id)
    case EBinop(op, e1, e2) => parens(e1 <+> text(op.toString) <+> e2)
    case acc @ EArrAccess(id, idxs) => {
      val doc = id <> ssep(idxs.map(idx => brackets(emitExpr(idx))), emptyDoc)
      if debug then
        acc.consumable
          .map(ann => brackets(doc <> colon <+> emitConsume(ann)))
          .getOrElse(doc)
      else doc
    }
    case acc @ EPhysAccess(id, idxs) => {
      val doc = id <>
        ssep(
          idxs.map({
            case (bank, idx) =>
              braces(text(bank.toString)) <> brackets(emitExpr(idx))
          }),
          emptyDoc
        )

      if debug then
        brackets(
          doc <> colon <+> acc.consumable.map(emitConsume).getOrElse(emptyDoc)
        )
      else doc
    }
    case EArrLiteral(idxs) => braces(commaSep(idxs.map(idx => emitExpr(idx))))
    case ERecAccess(rec, field) => rec <> dot <> field
    case ERecLiteral(fs) =>
      scope {
        hsep(fs.toList.map({
          case (id, expr) => id <+> equal <+> expr <> semi
        }))
      }

  def emitRange(range: CRange)(implicit debug: Boolean): Doc =
    val CRange(id, t, rev, s, e, u) = range

    val typAnnot =
      t.map(x => text(":") <+> text(x.toString)).getOrElse(emptyDoc)
    parens(
      text("let") <+> id <> typAnnot <+> equal <+>
        (if rev then text("rev") <+> emptyDoc else emptyDoc) <>
        value(s) <+> text("..") <+> value(
        e
      )
    ) <>
      (if u > 1 then space <> text("unroll") <+> value(u) else emptyDoc)

  def emitView(view: View)(implicit debug: Boolean): Doc =
    val View(suf, pre, sh) = view

    val sufDoc = suf match
      case Aligned(f, e) => value(f) <+> text("*") <+> e
      case Rotation(e) => e <> text("!")

    sufDoc <+> colon <>
      pre.map(p => space <> text("+") <+> value(p)).getOrElse(emptyDoc) <>
      sh.map(sh => space <> text("bank") <+> value(sh)).getOrElse(emptyDoc)

  def emitAttributes(attrs: Map[String, Int]): Doc =
    hsep(attrs.map({
      case (attr, v) => text(s"@${attr}") <> parens(text(v.toString()))
    }))

  implicit def emitCmd(c: Command)(
      implicit debug: Boolean
  ): Doc =
    val attr =
      if c.attributes.isEmpty then emptyDoc
      else
        text("/*") <+>
        emitAttributes(c.attributes) <+> text("*/") <> space

    attr <> emitCmdBare(c)(debug)

  def emitCmdBare(c: Command)(implicit debug: Boolean): Doc = c match
    case CPar(cmds) => vsep(cmds.map(emitCmd))
    case CSeq(cmds) => vsep(cmds.map(emitCmd), text("---"))
    case CLet(id, typ, e) =>
      text("let") <+> id <>
        typ.map(colon <+> emitTyp(_)).getOrElse(emptyDoc) <>
        e.map(space <> equal <+> emitExpr(_)).getOrElse(emptyDoc) <> semi
    case CIf(cond, cons, alt) => {
      text("if") <+> parens(cond) <+> scope(cons) <> (alt match {
        case CEmpty => emptyDoc
        case _: CIf => space <> text("else") <+> alt
        case _ => space <> text("else") <+> scope(alt)
      })
    }
    case CFor(r, pipe, par, com) =>
      text("for") <+> emitRange(r) <>
        (if pipe then space <> text("pipeline") else emptyDoc) <+>
        scope(emitCmd(par)) <>
        (if com != CEmpty then
           space <> text("combine") <+> scope(emitCmd(com))
         else
           emptyDoc)
    case CWhile(cond, pipe, body) =>
      text("while") <+> parens(cond) <>
        (if pipe then space <> text("pipeline") else emptyDoc) <+> scope(
        emitCmd(body)
      )
    case CDecorate(dec) => text("decor") <+> quote(value(dec))
    case CUpdate(lhs, rhs) => lhs <+> colon <> equal <+> rhs <> semi
    case CReduce(rop, lhs, rhs) => lhs <+> text(rop.toString) <+> rhs <> semi
    case CReturn(e) => text("return") <+> e <> semi
    case CExpr(e) => e <> semi
    case CEmpty => emptyDoc
    case CView(view, arr, dims) =>
      text("view") <+> view <+> equal <+> arr <+>
        ssep(dims.map(v => brackets(emitView(v))), emptyDoc) <> semi
    case CSplit(vId, arrId, factors) =>
      text("split") <+> vId <+> equal <+> arrId <>
        ssep(
          factors.map(factor => brackets(text("by") <+> value(factor))),
          emptyDoc
        ) <> semi
    case CBlock(cmd) => scope(emitCmd(cmd))
