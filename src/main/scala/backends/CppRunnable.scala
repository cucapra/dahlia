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
  def typeToDoc(typ: Type): Doc = typ match {
    case _:TVoid => "void"
    case _:TBool | _:TIndex | _:TStaticInt | _:TSizedInt => "int"
    case _:TFloat => "float"
    case TArray(typ, _) => "vector" <> angles(typeToDoc(typ))
    case TRecType(n, _) => n
    case _:TFun => throw Impossible("Cannot emit function types")
    case TAlias(n) => n
  }

  def cBind(id: Doc, rhs: Doc): Doc = {
    "auto" <+> id <+> "=" <+> rhs <> semi
  }

  def cCall(f: Doc, tParam: Option[Doc], args: List[Doc]): Doc = {
    f <> (if (tParam.isDefined) angles(tParam.get) else value("")) <>
    parens(hsep(args, ","))
  }

  def quote(id: Any) = dquotes(id.toString)

  override def withArrayType(id: Id) = s"&$id"

  override def emitProg(p: Prog, c: Config) = {
    val prog: Doc = super.progToDoc(p, c)
    val getArgs: Doc = vsep(p.decls.map({ case Decl(id, typ) => {
      value(s"// parsing parameter $id") <@>
      (typ match {
        case _:TBool | _:IntType | _:TFloat => {
          cBind(s"${id}_t",
            cCall("get_arg", Some("double"),
                   List(quote(id), quote("int"), "v"))) <@>
          cBind(s"$id",
            cCall("to_num", Some(typeToDoc(typ)), List(s"${id}_t")))
        }
        case TArray(ta, dims) => ta match {
          case _:IntType | _:TFloat => {
            val parseVal = cBind(s"${id}_t",
              cCall(
                "get_arg",
                Some("picojson::array"),
                List(quote(id), quote(s"$ta[]"), "v")))

            val funcName = dims.length match {
              case 1 => "to_num_array"
              case 2 => "to_num_matrix"
              case n => throw Impossible(s"Cannot parse rank-$n matrices")
            }
            parseVal <@>
            cBind(s"$id",
              cCall("flatten_matrix", Some(typeToDoc(ta)),
                List(cCall(
                  funcName,
                  Some(typeToDoc(ta)),
                  s"${id}_t" :: dims.map(t => value(t._1))))))
          }
          case _ => throw Impossible(s"Parsing for $typ not implemented")
        }
        case t => throw Impossible(s"Cannot parse type $t")
      })
    }}), line)

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
