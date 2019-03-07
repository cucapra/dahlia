package fuselang.backend

import org.bitbucket.inkytonik.kiama.output._
import fuselang.Syntax._
import fuselang.Errors._
import fuselang.CodeGenHelpers._

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

    def quote(id: Any) = dquotes(id.toString)

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
      f <> (if (tParam.isDefined) angles(tParam.get) else value("")) <>
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
     * Used to emit function headers. All C++ backend will convert the function
     * bodies in the same way but might require different pragrams for arguments\
     * or setup code.
     */
    def emitFuncHeader(func: FuncDef): Doc

    implicit def IdToString(id: Id): Doc = value(id.v)

    // FIXME(rachit): This is probably incorrect.
    def flattenIdx(idxs: List[Expr], dimSizes: List[Int]) =
      idxs.zip(dimSizes).foldRight[Expr](EInt(0))({
        case ((idx, dim), acc) => idx + (EInt(dim) * acc)
      })

    def scope(doc: Doc): Doc =
      lbrace <@> indent(doc) <@> rbrace

    def emitBaseInt(v: Int, base: Int) = base match {
      case 8 => s"0${Integer.toString(v, 8)}"
      case 10 => v
      case 16 => s"0x${Integer.toString(v, 16)}"
    }

    implicit def emitExpr(e: Expr): Doc = e match {
      case EApp(fn, args) => fn <> parens(hsep(args.map(emitExpr), comma))
      case EInt(v, base) => value(emitBaseInt(v, base))
      case EFloat(f) => value(f)
      case EBool(b) => value(if(b) 1 else 0)
      case EVar(id) => value(id)
      case EBinop(op, e1, e2) => parens(e1 <+> op.toString <+> e2)
      case EArrAccess(id, idxs) => id.typ match {
        case Some(TArray(_, dims)) => id <> brackets(flattenIdx(idxs, dims.map(_._1)))
        case Some(_) => throw Impossible("array access doesnt use array")
        case None => throw Impossible("type information missing in exprToDoc")
      }
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
        case CLet(id, typ, e) => emitType(typ.get) <+> value(id) <+> equal <+> e <> semi
        case CIf(cond, cons, alt) =>
          "if" <> parens(cond) <> scope (cons) <+> "else" <> scope(alt)
        case f:CFor => emitFor(f)
        case CWhile(cond, body) => "while" <> parens(cond) <+> scope(body)
        case CUpdate(lhs, rhs) => lhs <+> "=" <+> rhs <> semi
        case CReduce(rop, lhs, rhs) => lhs <+> rop.toString <+> rhs <> semi
        case CExpr(e) => e <> semi
        case CEmpty => ""
        case _:CView => throw Impossible("Views should not exist during codegen.")
    }

    def emitDecl(d: Decl): Doc = d.typ match {
      case ta:TArray => emitArrayDecl(ta, d.id)
      case _ => emitType(d.typ) <+> d.id
    }

    def emitFunc: FuncDef => Doc = { case func@FuncDef(id, args, bodyOpt) =>
      val as = hsep(args.map(emitDecl), comma)
      // If body is not defined, this is an extern. Elide the definition.
      bodyOpt.map(body => "void" <+> id <> parens(as) <+> scope {
        emitFuncHeader(func) <@>
        body
      }).getOrElse(value(""))
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
