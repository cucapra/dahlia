package fuselang.backend

import org.bitbucket.inkytonik.kiama.output._
import fuselang.common._
import Syntax._
import CompilerError._

object Cpp {
  /**
   * A C++ backend that only emits one dimensionals arrays and one dimensional
   * array accesses.
   */
  trait CppLike extends PrettyPrinter {
    /**
     * This class aggressively uses Scala's implicitConversions. Make sure
     * that implicits classes never leak.
     * Implicit classes: https://docs.scala-lang.org/tour/implicit-conversions.html
     *
     * We also use the Kiama pretty printer combinators to generate the code.
     * For reference: https://bitbucket.org/inkytonik/kiama/src/master/wiki/PrettyPrinting.md?fileviewer=file-view-default
     */
    import scala.language.implicitConversions

    override val defaultIndent = 2

    def quote(id: Doc) = dquotes(id)

    /**
     * Helper to generate a variable declaration with an initial value.
     */
    def cBind(id: Doc, rhs: Doc): Doc = {
      "auto" <+> id <+> "=" <+> rhs <> semi
    }

    /**
     * Helper to generate a function call that might have a type parameter
     */
    def cCall(f: Doc, tParam: Option[Doc], args: List[Doc]): Doc = {
      f <> (if (tParam.isDefined) angles(tParam.get) else emptyDoc) <>
      parens(hsep(args, ","))
    }

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
      (if (let.e.isDefined) space <> equal <+> emitExpr(let.e.get) else emptyDoc) <>
      semi

    implicit def IdToString(id: Id): Doc = value(id.v)

    def scope(doc: Doc): Doc =
      lbrace <@> indent(doc) <@> rbrace

    def emitBaseInt(v: Int, base: Int) = base match {
      case 8 => s"0${Integer.toString(v, 8)}"
      case 10 => v
      case 16 => s"0x${Integer.toString(v, 16)}"
    }

    implicit def emitExpr(e: Expr): Doc = e match {
      case ECast(e, typ) => parens(emitType(typ)) <> emitExpr(e)
      case EApp(fn, args) => fn <> parens(hsep(args.map(emitExpr), comma))
      case EInt(v, base) => value(emitBaseInt(v, base))
      case EFloat(f) => value(f)
      case EBool(b) => value(if(b) 1 else 0)
      case EVar(id) => value(id)
      case EBinop(op, e1, e2) => parens(e1 <+> op.toString <+> e2)
      case EArrAccess(id, idxs) =>
        id <> ssep(idxs.map(idx => brackets(emitExpr(idx))), emptyDoc)
      case EArrLiteral(idxs) => braces(hsep(idxs.map(idx => emitExpr(idx)), comma))
      case ERecAccess(rec, field) => rec <> dot <> field
      case ERecLiteral(fs) => scope {
        hsep(fs.toList.map({ case (id, expr) => "." <> id <+> "=" <+> expr }), comma)
      }
    }

    /**
     * Turns a range object into the parameter of a `for` loop.
     * (int <id> = <s>; <id> < <e>; <id>++)
     */
    def emitRange(range: CRange): Doc = parens {
      "int" <+> range.iter <+> "=" <+> value(range.s) <> semi <+>
      range.iter <+> "<" <+> value(range.e) <> semi <+>
      range.iter <> "++"
    }

    implicit def emitCmd(c: Command): Doc = c match {
      case CPar(c1, c2) => c1 <@> c2
      case CSeq(c1, c2) => c1 <@> text("//---") <@> c2
      case l:CLet => emitLet(l)
      case CIf(cond, cons, alt) =>
        "if" <> parens(cond) <> scope (cons) <+> "else" <> scope(alt)
      case f:CFor => emitFor(f)
      case CWhile(cond, body) => "while" <> parens(cond) <+> scope(body)
      case CDecorate(value) => value
      case CUpdate(lhs, rhs) => lhs <+> "=" <+> rhs <> semi
      case CReduce(rop, lhs, rhs) => lhs <+> rop.toString <+> rhs <> semi
      case CReturn(e) => "return" <+> e <> semi
      case CExpr(e) => e <> semi
      case CEmpty => emptyDoc
      case _:CView | _:CSplit =>
        throw Impossible("Views should not exist during codegen.")
    }

    def emitDecl(id: Id, typ: Type): Doc = typ match {
      case ta:TArray => emitArrayDecl(ta, id)
      case _ => emitType(typ) <+> id
    }

    def emitFunc(func: FuncDef, entry: Boolean = false): Doc = func match { case func@FuncDef(id, args, ret, bodyOpt) =>
      val as = hsep(args.map(decl => emitDecl(decl.id, decl.typ)), comma)
      // If body is not defined, this is an extern. Elide the definition.
      bodyOpt.map(body => emitType(ret) <+> id <> parens(as) <+> scope {
        emitFuncHeader(func, entry) <@>
        body
      }).getOrElse(emptyDoc)
    }

    def emitDef(defi: Definition): Doc = defi match {
      case func:FuncDef => emitFunc(func)
      case RecordDef(name, fields) =>
        "typedef struct" <+> scope {
          vsep(fields.toList.map({ case (id, typ) => emitType(typ) <+> id <> semi }))
        } <+> name <> semi
    }

    def emitInclude(incl: Include): Doc = "#include" <+> quote(incl.name)

  }

}
