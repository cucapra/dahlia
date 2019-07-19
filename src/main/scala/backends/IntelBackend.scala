package fuselang.backend

import Cpp._

import fuselang.common._
import Syntax._
import Configuration._
import CompilerError._

private class IntelBackend extends CppLike {
    
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
      emitType(ta.typ) <+> s"__attribute__(numbanks(${generateBanksInt(ta.dims)})) " <> id <> generateDims(ta.dims)
    }

    else {
      emitType(ta.typ) <+> id <> generateDims(ta.dims)  
    }
  
    
  

  def generateDims(dims: List[(Int, Int)]): Doc =
    ssep(dims.map(d => brackets(value(d._1))), emptyDoc)

  def generateBanks(dims: List[(Int, Int)]): Doc =
    //ssep(dims.map(d => brackets(value(d._0))), emptyDoc)
    ssep(dims.map(d => brackets(value(d._2))), emptyDoc)

  def generateBanksInt(dims: List[(Int, Int)]) =
    dims.head._2
  

  def emitType(typ: Type) = typ match {
    case _:TVoid => "void"
    case _:TBool | _:TIndex | _:TStaticInt | _:TSizedInt => "int"
    case _:TFloat => "float"
    case _:TDouble => "double"
    case TArray(typ, dims) => dims.foldLeft(emitType(typ))({ case (acc, _) => "buffer" <> angles(acc) })
    case TRecType(n, _) => n
    case _:TFun => throw Impossible("Cannot emit function types")
    case TAlias(n) => n
  }


  def emitProg(p: Prog, c: Config): String = {
    val layout =
      vsep(p.includes.map(emitInclude)) <@>
      vsep(p.defs.map(emitDef)) <@>
      "__kernel " <> emitFunc(FuncDef(Id(c.kernelName), p.decls, Some(p.cmd)))

    super.pretty(layout).layout
  }
}

private class IntelBackendHeader extends IntelBackend {
  override def emitCmd(c: Command): Doc = emptyDoc

  override def emitFunc = { case FuncDef(id, args, _) =>
    val as = hsep(args.map(d => (emitDecl(d.id, d.typ))), comma)
    "void" <+> id <> parens(as) <> semi
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