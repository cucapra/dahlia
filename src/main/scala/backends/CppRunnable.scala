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

  def emitType(typ: Type): Doc = typ match {
    case _:TVoid => "void"
    case _:TBool => "bool"
    case _:TIndex | _:TStaticInt | _:TSizedInt => "int"
    case _:TFloat => "float"
    case TArray(typ, dims) =>
      dims.foldLeft(emitType(typ))({ case (acc, _) => "vector" <> angles(acc) })
    case TRecType(n, _) => n
    case _:TFun =>
      throw Impossible("emitType", "Cannot emit function types")
    case TAlias(n) => n
  }

  def emitArrayDecl(ta: TArray, id: Id) = emitType(ta) <+> s"&$id"

  def emitFor(cmd: CFor): Doc =
    "for" <> emitRange(cmd.range) <+> scope {
      cmd.par <>
      (if (cmd.combine != CEmpty) line <> text("// combiner:") <@> cmd.combine else emptyDoc)
    }

  def emitFuncHeader(func: FuncDef) = emptyDoc

  /**
   * Emit code to parse the value for declaration `d`. Assumes that the
   * program has already created a value `v` of the type json to
   * store the data. Each parameter generates two statements:
   *
   * auto id = <align>(get_arg<type>("id", "type", v)); // v is the json value parsed earlier.
   *
   * <align> is generated based on the type of the param:
   */
  def emitParseDecl: Decl => Doc = { case Decl(id, _) => {
    // Use the type decoration for id since it's guaranteed to be resolved.
    val typ = id.typ.get

    val (typeName, cTyp): (Doc, Doc) = typ match {
      case _:TAlias | _:TRecType | _:TBool | _:IntType | _:TFloat => {
        val typeName = emitType(typ)
        (quote(typeName), typeName)
      }
      case arr@TArray(_, dims) => {
        val typeName = quote(s"${arr.typ}${dims.map(_ => "[]").mkString}")
        val cType = "n_dim_vec_t" <> angles(emitType(arr.typ) <> comma <+> dims.length.toString)
        (typeName, cType)
      }
      case t => throw NotImplemented(s"Cannot parse type `$t' with CppRunnable backend.")
    }

    cBind(s"${id}",
      cCall("get_arg", Some(cTyp), List(quote(id), typeName, "v")))

  }}

  /**
   * Generates [[from_json]] and [[to_json]] for a given record. Used by the
   * json library to extract records from json.
   * See: https://github.com/nlohmann/json#basic-usage
   */
  private def recordHelpers: RecordDef => Doc = { case RecordDef(name, fields) =>
    "void to_json" <> parens(s"nlohmann::json& j, const ${name}& r") <+> scope {
      "j =" <+> "nlohmann::json" <> braces(hsep({
        fields.map({ case (id, _) => braces(quote(id) <> comma <+> s"r.$id")}).toList
      }, comma)) <> semi
    } <@>
    "void from_json" <> parens(s"const nlohmann::json& j, ${name}& r") <+> scope {
      vsep({
        fields.map({ case (id, _) =>
          "j.at" <> parens(quote(id)) <> ".get_to" <> parens(s"r.$id") <> semi
        }).toList
      })
    }
  }

  def emitProg(p: Prog, c: Config) = {
    val startHelpers = value("/***************** Parse helpers  ******************/")
    val endHelpers = value("/***************************************************/")
    val includes = Include("parser.cpp", List()) :: p.includes
    val parseHelpers =
      vsep(p.defs.collect({ case rec: RecordDef => recordHelpers(rec)}))
    val kernel =
      vsep(includes.map(emitInclude)) <@>
      vsep(p.defs.map(emitDef)) <@>
      startHelpers <@>
      parseHelpers <@>
      endHelpers <@>
      emitFunc(FuncDef(Id(c.kernelName), p.decls, Some(p.cmd)))

    val getArgs: Doc = vsep(p.decls.map(emitParseDecl))

    val main = value("int main(int argc, char** argv)") <+> scope {
      "using namespace flattening;" <@>
      cBind("v", cCall("parse_data", None, List("argc", "argv"))) <> semi <@>
      getArgs <@>
      cCall(c.kernelName, None, p.decls.map(decl => value(decl.id.v))) <> semi <@>
      "return 0" <> semi
    }

    super.pretty(kernel <@> main).layout
  }
}

case object CppRunnable extends Backend {
  private val emitter = new CppRunnable()
  def emitProg(p: Prog, c: Config) = emitter.emitProg(p, c)
}
