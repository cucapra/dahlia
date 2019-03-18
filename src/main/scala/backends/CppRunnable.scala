package fuselang.backend

import fuselang.Syntax._
import fuselang.Utils._
import fuselang.Errors._

import Cpp._

/**
 * Same as [[fuselang.backend.VivadoBackend]] except this creates a main
 * method that that parses input data to the kernel and includes the
 * header file for parsing. It also emits `int` instead of `ap_int` so
 * that the code runnable by gcc.
 */
private class CppRunnable extends CppLike {

  def emitType(typ: Type) = typ match {
    case _:TVoid => "void"
    case _:TBool | _:TIndex | _:TStaticInt | _:TSizedInt => "int"
    case _:TFloat => "float"
    case TArray(typ, _) => "vector" <> angles(emitType(typ))
    case TRecType(n, _) => n
    case _:TFun => throw Impossible("Cannot emit function types")
    case TAlias(n) => n
  }

  def emitArrayDecl(ta: TArray, id: Id) = emitType(ta) <+> s"&$id"

  def emitFor(cmd: CFor): Doc =
    "for" <> emitRange(cmd.range) <+> scope {
      cmd.par <@>
      (if (cmd.combine != CEmpty) text("// combiner:") <@> cmd.combine else value(""))
    }

  def emitFuncHeader(func: FuncDef) = value("")

  /**
   * Emit code to parse the value for declaration `d`. Assumes that the
   * program has already created a value `v` of the type picojson::value to
   * store the data. Each parameter generates two statements:
   *
   * auto id_t = get_arg<type>("id", "type", v); // v is the picojson value parsed earlier.
   * auto id = `transform`(id_t);
   *
   * For the second statement, `transform` is generated based on the type of the
   * param:
   *
   * - Arrays are turned in n-dimensional vectors and then flattened.
   * - Primitives are cast to primitive type from `double`. (All numbers in JSON
   *   are initially parsed as doubles.)
   */
  def emitParseDecl: Decl => Doc = { case Decl(id, typ) => {
    val comment = value(s"// parsing parameter $id")

    val parseStmt = typ match {
      case _:TBool | _:IntType | _:TFloat => {
        cBind(s"${id}_t",
          cCall("get_arg", Some("double"),
            List(quote(id), quote("int"), "v")))
      }
      case arr@TArray(_:IntType | _:TFloat | _:TBool, _) => {
        cBind(s"${id}_t",
          cCall(
            "get_arg",
            Some("picojson::array"),
            List(quote(id), quote(s"${arr.typ}[]"), "v")))
      }
      case t => throw Impossible(s"Cannot parse type $t with CppRunnable backend.")
    }

    val alignType = typ match {
      case _:TBool | _:IntType | _:TFloat => {
        cBind(s"$id",
          cCall("to_num", Some(emitType(typ)), List(s"${id}_t")))
      }
      case arr@TArray(_:IntType | _:TFloat | _:TBool, dims) => {
        val funcName = s"to_order_${dims.length}_tensor"
        cBind(s"$id",
          cCall(s"flatten_order_${dims.length}_tensor", Some(emitType(arr.typ)),
            List(cCall(
              funcName,
              Some(emitType(arr.typ)),
              s"${id}_t" :: dims.map(t => value(t._1))))))
      }
      case t => throw Impossible(s"Cannot parse type $t with CppRunnable backend.")
    }

    comment <@> parseStmt <@> alignType
  }}

  def emitProg(p: Prog, c: Config) = {
    val prog =
      vsep(p.includes.map(emitInclude)) <@>
      vsep(p.defs.map(emitDef)) <@>
      emitFunc(FuncDef(Id(c.kernelName), p.decls, Some(p.cmd)))

    val getArgs: Doc = vsep(p.decls.map(emitParseDecl), line)

    val modProg = "#include" <+> dquotes("parser.cpp") <@> prog <@>
    value("int main(int argc, char** argv)") <+> scope {
      cBind("v", cCall("parse_data", None, List("argc", "argv"))) <> semi <@>
      getArgs <@>
      cCall(c.kernelName, None, p.decls.map(decl => value(decl.id.v))) <> semi <@>
      "return 0" <> semi
    }

    super.pretty(modProg).layout
  }
}

case object CppRunnable extends Backend {
  private val emitter = new CppRunnable()
  def emitProg(p: Prog, c: Config) = emitter.emitProg(p, c)
}
