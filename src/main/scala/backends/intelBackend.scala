 package fuselang.backend

 //import fuselang.common._
 import fuselang.Syntax._
 import fuselang.Configuration._
 import fuselang.CompilerError._

 import Cpp._

 private class IntelBackend extends CppLike {
 
     def unroll(n: Int): Doc = n match {
           case 1 => emptyDoc
           case n => value(s"__attribute__((opencl_unroll_hint($n)))") <@> line
     }

     
     def emitType(typ: Type): Doc = typ match {
           case _:TVoid => "void"
           case _:TBool => "bool"
           case _:TIndex | _:TStaticInt | _:TSizedInt => "int"
           case _:TFloat => "float"
           case _:TDouble => "double"
           case TArray(typ, dims) =>
           dims.foldLeft(emitType(typ))({ case (acc, _) => "vector" <> angles(acc) })
           case TRecType(n, _) => n
           case _:TFun =>
               throw Impossible("Cannot emit function types")
           case TAlias(n) => n
       }

     def emitArrayDecl(ta: TArray, id: Id) = emitType(ta) <+> s"$id"

     def emitFor(cmd: CFor): Doc =
           "for" <> emitRange(cmd.range) <+> scope {
                   cmd.par <> {
                   if (cmd.combine != CEmpty)
                       line <> text("// combiner:") <@> cmd.combine
                   else
                       emptyDoc
                   }
             }

     def emitFuncHeader(func: FuncDef): Doc = emptyDoc
    


     def generateDims(dims: List[(Int, Int)]): Doc =
           ssep(dims.map(d => brackets(value(d._1))), emptyDoc)

     def emitProg(p: Prog, c: Config): String = {
    val layout =
      vsep(p.includes.map(emitInclude)) <@>
      vsep(p.defs.map(emitDef)) <@>
      emitFunc(FuncDef(Id(c.kernelName), p.decls, Some(p.cmd)))

    super.pretty(layout).layout
  }      

  } 
  
  private class IntelBackendHeader extends IntelBackend {
    override def emitCmd(c: Command): Doc = emptyDoc

    override def emitFunc = { case FuncDef(id, args, _) =>
      val as = hsep(args.map(d => emitDecl(d.id, d.typ)), comma)
      "void" <+> id <> parens(as) <> semi
  }

  override def emitProg(p: Prog, c: Config) = {
    val includes = Include("parser.cpp", List()) :: p.includes

    val declarations =
      vsep(includes.map(emitInclude)) <@>
      vsep (p.defs.map(emitDef)) <@>
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
