package fuselang.backend

import Cpp._
//import org.bitbucket.inkytonik.kiama.output._
import fuselang.common._
import Syntax._
import Configuration._
import CompilerError._

private class IntelBackend extends CppLike {
  import scala.language.implicitConversions

  def unroll(n: Int): Doc = n match {
           case 1 => emptyDoc
           case n => value(s"#pragma unroll $n\n")
     }

  def emitFor(cmd: CFor): Doc =
    unroll(cmd.range.u) <+>
    "for" <> emitRange(cmd.range) <+> scope {
      cmd.par <>
      (if (cmd.combine != CEmpty) line <> text("// combiner:") <@> cmd.combine
       else emptyDoc)
    } 

  def emitFuncHeader(func: FuncDef) = emptyDoc

  def emitArrayDecl(ta: TArray, id: Id) =

    if (generateBanksInt(ta.dims) != 1) {
      emitType(ta.typ) <+> "*" <> id <+> s"__attribute__((numbanks(${generateBanksInt(ta.dims)}), bankwidth(" <>  s"${generateDimsInt(ta.dims)/generateBanksInt(ta.dims)}" <> "*sizeof(" <> emitType(ta.typ) <> "))))"
    }

    else {
      emitType(ta.typ) <+> "*" <> id// <> generateDims(ta.dims)  
    }

  def emitArrayDeclCmd(ta: TArray, id: Id) =

    if (generateBanksInt(ta.dims) != 1) {
      val arrayType: Doc = emitType(ta.typ)
      arrayType <+> s"__attribute__((numbanks(${generateBanksInt(ta.dims)}), bankwidth(" <>  s"${generateDimsInt(ta.dims)/generateBanksInt(ta.dims)}" <> "*sizeof(" <> arrayType <> "))))" <+> id <> generateDims(ta.dims)
    }

    else {
      emitType(ta.typ) <+> id// <> generateDims(ta.dims)  /
    }
  
  //used for decl: elements in fuse (pointers passed into the kernel)
  override def emitDecl(id: Id, typ: Type): Doc = typ match {
      case ta:TArray => "__global" <+> emitArrayDecl(ta, id)
      case _ => "__global" <+> emitType(typ) <+> "*" <> id 
    }
  
  //used for let assignments
  def emitCmdDecl(id: Id, typ: Type): Doc = typ match {
      case ta:TArray => emitArrayDeclCmd(ta, id)
      case _ => emitType(typ) <+> id
    }

 
  override implicit def emitCmd(c: Command): Doc = c match {
      case CPar(c1, c2) => c1 <@> c2
      case CSeq(c1, c2) => c1 <@> text("//---") <@> c2
      case CLet(id, typ, init) =>
        emitCmdDecl(id, typ.get) <>
        (if (init.isDefined) space <> equal <+> emitExpr(init.get) else emptyDoc) <>
        semi
      case CIf(cond, cons, alt) =>
        "if" <> parens(cond) <> scope (cons) <+> "else" <> scope(alt)
      case f:CFor => emitFor(f)
      case CWhile(cond, body) => "while" <> parens(cond) <+> scope(body)
      case CUpdate(lhs, rhs) => lhs <+> "=" <+> rhs <> semi
      case CReduce(rop, lhs, rhs) => lhs <+> rop.toString <+> rhs <> semi
      case CExpr(e) => e <> semi
      case CEmpty => emptyDoc
      case _:CView | _:CSplit =>
        throw Impossible("Views should not exist during codegen.")
    }

  override def emitFunc: FuncDef => Doc = { case func@FuncDef(id, args, bodyOpt) =>
      val as = hsep(args.map(decl => emitCmdDecl(decl.id, decl.typ)), comma)
      // If body is not defined, this is an extern. Elide the definition.
      bodyOpt.map(body => "void" <+> id <> parens(as) <+> scope {
        emitFuncHeader(func) <@>
        body
      }).getOrElse(emptyDoc)
    }

  def emitProgFunc: FuncDef => Doc = { case func@FuncDef(id, args, bodyOpt) =>
      val as = hsep(args.map(decl => emitDecl(decl.id, decl.typ)), comma)
      // If body is not defined, this is an extern. Elide the definition.
      bodyOpt.map(body => "void" <+> id <> parens(as) <+> scope {
        emitFuncHeader(func) <@>
        body
      }).getOrElse(emptyDoc)
    }

  def generateDims(dims: List[(Int, Int)]): Doc =
    ssep(dims.map(d => brackets(value(d._1))), emptyDoc)

  def generateBanks(dims: List[(Int, Int)]): Doc =
    ssep(dims.map(d => brackets(value(d._2))), emptyDoc)

  def generateBanksInt(dims: List[(Int, Int)]) =
    dims.head._2  

  def generateDimsInt(dims: List[(Int, Int)]) = 
    dims.head._1

  def emitType(typ: Type) = typ match {
    case _:TVoid => "void"
    case _:TBool | _:TIndex | _:TStaticInt | _:TSizedInt => "int"
    case _:TFloat => "float"
    case _:TDouble => "double"
    case TArray(typ, _) => emitType(typ)
    case TRecType(n, _) => n
    case _:TFun => throw Impossible("Cannot emit function types")
    case TAlias(n) => n
  }


  def emitProg(p: Prog, c: Config): String = {
    val layout =
      vsep(p.includes.map(emitInclude)) <@>
      vsep(p.defs.map(emitDef)) <@>
      "__kernel " <> emitProgFunc(FuncDef(Id(c.kernelName), p.decls, Some(p.cmd)))

    super.pretty(layout).layout
  }
}

private class IntelBackendHeader extends IntelBackend {
  override def emitCmd(c: Command): Doc = emptyDoc

  override def emitFunc = { case FuncDef(id, args, _) =>
    val as = hsep(args.map(d => (emitCmdDecl(d.id, d.typ))), comma)
    "__kernel void" <+> id <> parens(as) <> semi
  }

  override def emitProg(p: Prog, c: Config) = {
    val declarations =
      vsep(p.includes.map(emitInclude) ++ p.defs.map(emitDef)) <@>
      emitFunc(FuncDef(Id(c.kernelName), p.decls, None))

    super.pretty(declarations).layout
  }
}

case object IntelBackend extends Backend {
  def emitProg(p: Prog, c: Config) = c.header match {
    case true => (new IntelBackendHeader()).emitProg(p, c)
    case false => (new IntelBackend()).emitProg(p, c)
  }
  val canGenerateHeader = true
}