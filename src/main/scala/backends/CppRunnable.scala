package fuselang.backend

import Cpp._

import fuselang.common._
import Syntax._
import Configuration._
import CompilerError._
import PrettyPrint.Doc
import PrettyPrint.Doc._
import fuselang.common.{Configuration => C}

/**
  * Same as [[fuselang.backend.VivadoBackend]] except this creates a main
  * method that that parses input data to the kernel and includes the
  * header file for parsing. It also emits `int` instead of `ap_int` so
  * that the code runnable by gcc.
  */
private class CppRunnable extends CppLike {

  // Variable to store the results of the updated arrays.
  val serializer = text("__")

  def emitType(typ: Type): Doc = typ match {
    case _: TVoid => text("void")
    case _: TBool => text("bool")
    case _: TIndex => text("int")
    case _: TStaticInt => throw Impossible("TStaticInt type should not exist")
    case TSizedInt(_, un) => text(if un then "unsigned int" else "int")
    case _: TFloat => text("float")
    case _: TDouble | _: TFixed => text("double")
    case _: TRational => throw Impossible("Rational type should not exist")
    case TArray(typ, dims, _) =>
      dims.foldLeft(emitType(typ))({
        case (acc, _) => text("vector") <> angles(acc)
      })
    case TRecType(n, _) => value(n)
    case _: TFun =>
      throw Impossible("Cannot emit function types")
    case TAlias(n) => value(n)
  }

  def emitArrayDecl(ta: TArray, id: Id) = emitType(ta) <+> text(s"&$id")

  override def emitLet(l: CLet) = l match {
    case CLet(id, Some(TArray(typ, dims, _)), init) => {

      /*
        1D array with initializers - let b0: bit<32>[4] = {3, 5, 7, 11};
          vector<int> b0{3, 5, 7, 11};
        1D array - let arr: ubit<32>[16];
          vector<unsigned int> arr(16, 0);
        2D array - let arr: ubit<32>[16][16];
          vector<vector<unsigned int>> arr(16, vector<unsigned int>(16, 0));
        nD array - let arr: ubit<32>[16][8][4][5][3];
          vector<vector<vector<vector<vector<unsigned int>>>>> arr(16,
            vector<vector<vector<vector<unsigned int>>>>(8,
              vector<vector<vector<unsigned int>>>(4,
                vector<vector<unsigned int>>(5,
                  vector<unsigned int>(3, 0)))));
       */
      val initVal = init match {
        case Some(expr) => emitExpr(expr)
        case None =>
          parens(value(dims.head._1) <> comma <+> dims.tail.foldRight((text("vector") <> angles(emitType(typ)), value(0)))({
            case ((len, _), acc) => (text("vector") <> angles(acc._1), acc._1 <> parens(value(len) <> comma <+> acc._2))
          })._2)
      }


      dims.foldLeft(emitType(typ))({
        case (acc, _) => text("vector") <> angles(acc)
      }) <+> id <> initVal <> semi

    }
    case _ => super.emitLet(l)
  }

  def emitFor(cmd: CFor): Doc =
    text("for") <> emitRange(cmd.range) <+> scope {
      cmd.par <> {
        if cmd.combine != CEmpty then
          line <> text("// combiner:") <@> cmd.combine
        else
          emptyDoc
      }
    }

  def emitFuncHeader(func: FuncDef, entry: Boolean = false) = emptyDoc

  /**
    * Emit code to parse the value for declaration `d`. Assumes that the
    * program has already created a value `v` of the type json to
    * store the data. Each parameter generates two statements:
    *
    * auto id = get_arg<type>("id", "type", v); // v is the json value parsed earlier.
    *
    * <align> is generated based on the type of the param:
    */
  def emitParseDecl: Decl => Doc = {
    case Decl(id, _) => {
      // Use the type decoration for id since it's guaranteed to be resolved.
      val typ = id.typ.get

      val (typeName, cTyp): (Doc, Doc) = typ match {
        case _: TAlias | _: TRecType | _: TBool | _: IntType | _: TFloat |
            _: TFixed => {
          val typeName = emitType(typ)
          (quote(typeName), typeName)
        }
        case arr @ TArray(_, dims, _) => {
          val typeName = quote(
            text(s"${arr.typ}${dims.map(_ => "[]").mkString}")
          )
          val cType =
            text("n_dim_vec_t") <>
              angles(emitType(arr.typ) <> comma <+> value(dims.length))
          (typeName, cType)
        }
        case t =>
          throw NotImplemented(
            s"Cannot parse type `$t' with CppRunnable backend."
          )
      }

      cBind(
        s"${id}",
        cCall("get_arg", Some(cTyp), List(quote(id), typeName, text("v")))
      )

    }
  }

  def emitSerializeDecl: Decl => Doc = {
    case Decl(id, _) => {
      serializer <> brackets(quote(id)) <+> text("=") <+> id <> semi
    }
  }

  /**
    * Generates [[from_json]] and [[to_json]] for a given record. Used by the
    * json library to extract records from json.
    * See: https://github.com/nlohmann/json#basic-usage
    */
  private def recordHelpers: RecordDef => Doc = {
    case RecordDef(name, fields) =>
      text("void to_json") <>
        parens(text(s"nlohmann::json& j, const ${name}& r")) <+> scope {
        text("j =") <+> text("nlohmann::json") <> braces(commaSep({
          fields
            .map({
              case (id, _) => braces(quote(id) <> comma <+> text(s"r.$id"))
            })
            .toList
        })) <> semi
      } <@>
        text("void from_json") <>
        parens(text(s"const nlohmann::json& j, ${name}& r")) <+> scope {
        vsep({
          fields
            .map({
              case (id, _) =>
                text("j.at") <> parens(quote(id)) <> text(".get_to") <> parens(
                  text(s"r.$id")
                ) <> semi
            })
            .toList
        })
      }
  }

  private def emitKernel(func: FuncDef): Doc = {
    val FuncDef(id, args, ret, bodyOpt) = func

    // Generate serialization of decls
    val serializeArgs: Doc = vsep(args.map(emitSerializeDecl))
    val as = commaSep(args.map(decl => emitDecl(decl.id, decl.typ)))
    // If body is not defined, this is an extern. Elide the definition.
    val body = bodyOpt
      .map(body =>
        emitType(ret) <+> id <> parens(as) <+>
          scope {
            emitFuncHeader(func) <@> body <@>
              text("json_t") <+> serializer <> semi <@>
              serializeArgs <@>
              text("std::cout <<") <+> serializer <> text(
              ".dump(2) << std::endl"
            ) <>
              semi
          }
      )
      .getOrElse(emptyDoc)

    body
  }

  def emitProg(p: Prog, c: Config) = {
    // Comments to demarcate autogenerated struct parsing helpers
    val startHelpers = value(
      "/***************** Parse helpers  ******************/"
    )
    val endHelpers = value(
      "/***************************************************/"
    )

    // Add parsing library to the list of includes.
    val includes = "parser.cpp" +: p.includes.flatMap(_.backends.get(C.Cpp))

    // Generate parsing helpers for all record defintions.
    val parseHelpers =
      vsep(p.defs.collect({ case rec: RecordDef => recordHelpers(rec) }))

    // Generate code for the main kernel in this file.
    val kernel = vsep {
      includes.map(emitInclude) ++
        p.defs.map(emitDef) ++
        (startHelpers ::
          parseHelpers ::
          endHelpers ::
          emitKernel(FuncDef(Id(c.kernelName), p.decls, TVoid(), Some(p.cmd))) ::
          Nil)
    }

    // Generate function calls to extract all kernel parameters from the JSON
    val getArgs: Doc = vsep(p.decls.map(emitParseDecl))

    // Generate a main function that parses are kernel parameters and calls
    // the kernel function.
    val main = value("int main(int argc, char** argv)") <+> scope {
      text("using namespace flattening;") <@>
        cBind("v", cCall("parse_data", None, List(text("argc"), text("argv")))) <>
        semi <@>
        getArgs <@>
        cCall(
          c.kernelName,
          None,
          p.decls.map(decl => value(decl.id.v))
        ) <> semi <@>
        text("return 0") <> semi
    }

    // Emit string
    (kernel <@> main).pretty
  }
}

private class CppRunnableHeader extends CppRunnable {
  override def emitCmd(c: Command): Doc = emptyDoc

  override def emitFunc(func: FuncDef, entry: Boolean): Doc = func match {
    case FuncDef(id, args, ret, _) => {
      val as = commaSep(args.map(d => emitDecl(d.id, d.typ)))
      emitType(ret) <+> id <> parens(as) <> semi
    }
  }

  override def emitProg(p: Prog, c: Config) = {
    val includes: Seq[String] =
      p.includes.flatMap(_.backends.get(C.Cpp)) :+ "parser.cpp"

    val declarations =
      vsep(includes.map(emitInclude)) <@>
        vsep(p.defs.map(emitDef)) <@>
        emitFunc(FuncDef(Id(c.kernelName), p.decls, TVoid(), None), true)

    declarations.pretty
  }
}

case object CppRunnable extends Backend {
  def emitProg(p: Prog, c: Config) = c.header match {
    case true => (new CppRunnableHeader()).emitProg(p, c)
    case false => (new CppRunnable()).emitProg(p, c)
  }
  val canGenerateHeader = true
}
