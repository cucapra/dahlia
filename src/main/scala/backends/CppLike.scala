package fuselang.backend

import fuselang.common._
import Syntax._
import CompilerError._
import PrettyPrint.Doc._
import PrettyPrint.Doc

object Cpp:

  /**
    * A C++ backend that only emits one dimensionals arrays and one dimensional
    * array accesses.
    */
  trait CppLike:

    /**
      * This class aggressively uses Scala's implicitConversions. Make sure
      * that implicits classes never leak.
      * Implicit classes: https://docs.scala-lang.org/tour/implicit-conversions.html
      *
      */
    import scala.language.implicitConversions

    val defaultIndent = 2

    /**
      * Helper to generate a variable declaration with an initial value.
      */
    def cBind(id: String, rhs: Doc): Doc =
      text("auto") <+> text(id) <+> text("=") <+> rhs <> semi

    /**
      * Helper to generate a function call that might have a type parameter
      */
    def cCall(f: String, tParam: Option[Doc], args: Seq[Doc]): Doc =
      text(f) <> (if tParam.isDefined then angles(tParam.get) else emptyDoc) <>
        parens(commaSep(args))

    /**
      * Function used for converting types from Fuse to C++.
      */
    def emitType(typ: Type): Doc

    /**
      * Used for generating declarations for array. For example, an array
      * parameter `arr` in a function can be passed as `*arr` or `arr[size]`.
      */
    def emitArrayDecl(typ: TArray, id: Id): Doc

    /**
      * Used to emit for loops. We specifically separate this out from other
      * commands since for loops might require pragmas.
      */
    def emitFor(cmd: CFor): Doc

    /**
      * Emit while loops.
      */
    def emitWhile(cmd: CWhile): Doc =
      text("while") <> parens(cmd.cond) <+> scope(cmd.body)

    /**
      * Used to emit function headers. All C++ backends will convert the function
      * bodies in the same way but might require different pragrams for arguments
      * or setup code. `entry` distinguishes the top-level entry point
      * function.
      */
    def emitFuncHeader(func: FuncDef, entry: Boolean = false): Doc

    /**
      * Generate code for a "let" binding. (Might need to be followed by
      * pragmas.)
      */
    def emitLet(let: CLet): Doc =
      emitDecl(let.id, let.typ.get) <>
        (if let.e.isDefined then space <> equal <+> emitExpr(let.e.get)
         else emptyDoc) <>
        semi

    implicit def IdToString(id: Id): Doc = value(id.v)

    def emitBaseInt(v: Int, base: Int): String = base match
      case 8 => s"0${Integer.toString(v, 8)}"
      case 10 => v.toString
      case 16 => s"0x${Integer.toString(v, 16)}"

    implicit def emitExpr(e: Expr): Doc = e match
      case ECast(e, typ) => parens(emitType(typ)) <> emitExpr(e)
      case EApp(fn, args) => fn <> parens(commaSep(args.map(emitExpr)))
      case EInt(v, base) => value(emitBaseInt(v, base))
      case ERational(d) => value(d)
      case EBool(b) => value(if b then 1 else 0)
      case EVar(id) => value(id)
      case EBinop(op, e1, e2) => parens(e1 <+> text(op.toString) <+> e2)
      case EArrAccess(id, idxs) =>
        id <> ssep(idxs.map(idx => brackets(emitExpr(idx))), emptyDoc)
      case EArrLiteral(idxs) => braces(commaSep(idxs.map(idx => emitExpr(idx))))
      case _: EPhysAccess =>
        throw NotImplemented("Physical access code gen for cpp-like backends.")
      case ERecAccess(rec, field) => rec <> dot <> field
      case ERecLiteral(fs) =>
        scope {
          commaSep(fs.toSeq.map({
            case (id, expr) => dot <> id <+> equal <+> expr
          }))
        }

    /**
      * Turns a range object into the parameter of a `for` loop.
      * (int <id> = <s>; <id> < <e>; <id>++)
      */
    def emitRange(range: CRange): Doc = parens {
      val CRange(id, _, rev, s, e, _) = range
      if rev then
        text("int") <+> id <+> equal <+> value(e - 1) <> semi <+>
        id <+> text(">=") <+> value(s) <> semi <+>
        id <> text("--")
      else
        text("int") <+> id <+> equal <+> value(s) <> semi <+>
        id <+> text("<") <+> value(e) <> semi <+>
        id <> text("++")
    }

    implicit def emitCmd(c: Command): Doc = c match
      case CPar(cmds) => vsep(cmds.map(emitCmd))
      case CSeq(cmds) => vsep(cmds.map(emitCmd), text("//---"))
      case l: CLet => emitLet(l)
      case CIf(cond, cons, alt) => {
        text("if") <+> parens(cond) <+> scope(cons) <> (alt match {
          case CEmpty => emptyDoc
          case _:CIf => space <> text("else") <+> alt
          case _ => space <> text("else") <+> scope(alt)
        })
      }
      case f: CFor => emitFor(f)
      case w: CWhile => emitWhile(w)
      case CDecorate(dec) => value(dec)
      case CUpdate(lhs, rhs) => lhs <+> equal <+> rhs <> semi
      case CReduce(rop, lhs, rhs) => lhs <+> text(rop.toString) <+> rhs <> semi
      case CReturn(e) => text("return") <+> e <> semi
      case CExpr(e) => e <> semi
      case CEmpty => emptyDoc
      case _: CView | _: CSplit =>
        throw Impossible("Views should not exist during codegen.")
      case CBlock(cmd) => scope(cmd)

    def emitDecl(id: Id, typ: Type): Doc = typ match
      case ta: TArray => emitArrayDecl(ta, id)
      case _ => emitType(typ) <+> id

    def emitFunc(func: FuncDef, entry: Boolean = false): Doc = func match
      case func @ FuncDef(id, args, ret, bodyOpt) =>
        val as = commaSep(args.map(decl => emitDecl(decl.id, decl.typ)))
        // If body is not defined, this is an extern. Elide the definition.
        val body = bodyOpt
          .map(body =>
            emitType(ret) <+> id <> parens(as) <+>
              scope { emitFuncHeader(func, entry) <@> body }
          )
          .getOrElse(emptyDoc)

        if entry then text("extern") <+> quote(text("C")) <+> scope(body)
        else body

    def emitDef(defi: Definition): Doc = defi match
      case func: FuncDef => emitFunc(func)
      case RecordDef(name, fields) =>
        text("typedef struct") <+> scope {
          vsep(fields.toList.map({
            case (id, typ) => emitType(typ) <+> id <> semi
          }))
        } <+> name <> semi

    def emitInclude(incl: String): Doc =
      text("#include") <+> quote(text(incl))


