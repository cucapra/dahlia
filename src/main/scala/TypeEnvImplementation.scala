package fuselang

import scala.util.parsing.input.Position

import TypeInfo._
import ScopeMap._
import Syntax._
import Errors._
import Utils.RichOption
import TypeEnv.Environment

object TypeEnvImplementation {

  val emptyEnv: Environment = Env()(1)

  private case class Env(
    typeMap: ScopedMap[Id, Info] = ScopedMap(),
    capMap: ScopedMap[Expr, Capability] = ScopedMap(),
    typeDefMap: Map[Id, Type] = Map())
    (implicit val res: Int) extends Environment {

    type TypeScope = Map[Id, Info]
    type CapScope = Map[Expr, Capability]

    /** Capability methods */
    def getCap(expr: Expr): Option[Capability] = capMap(expr)
    def addCap(expr: Expr, cap: Capability): Environment = capMap.add(expr, cap) match {
      case Some(cMap) => this.copy(capMap = cMap)
      case None => throw Impossible("addCap", s"Capability for $expr already exists.")
    }

    /** Type defintions */
    def resolveType(typ: Type): Type = typ match {
      case TAlias(n) => getType(n)
      case TFun(args) => TFun(args.map(resolveType(_)))
      case arr@TArray(t, _) => arr.copy(typ = resolveType(t))
      case t => t
    }
    def addType(alias: Id, typ: Type) = typeDefMap.get(alias) match {
      case Some(_) => throw AlreadyBoundType(alias)
      case None => this.copy(typeDefMap = typeDefMap + (alias -> typ))
    }
    def getType(alias: Id) = typeDefMap.get(alias) match {
      case Some(t) => t
      case None => throw UnboundType(alias)
    }

    /** Type binding methods */
    def apply(id: Id) = getInfo(id).typ
    def add(id: Id, typ: Type) = typeMap.add(id, Info(id, resolveType(typ))) match {
      case None => throw AlreadyBound(id)
      case Some(tMap) => this.copy(typeMap = tMap)
    }
    def update(id: Id, typ: Info) = typeMap.update(id, typ) match {
      case None => throw UnboundVar(id)
      case Some(tMap) => this.copy(typeMap = tMap)
    }
    def consumeBank(id: Id, dim: Int, bank: Int)
                   (implicit pos: Position): Environment = {
      val tMap = typeMap
                  .update(id, this.getInfo(id).consumeBank(dim, bank))
                  .getOrThrow(UnboundVar(id))
      this.copy(typeMap = tMap)
    }

    val getResources = res

    /** Helper functions for Mergable[Env] */
    def getInfo(id: Id): Info = typeMap(id) match {
      case Some(info) => info
      case None => throw UnboundVar(id)
    }
    lazy val getBoundIds = typeMap.keys

    /** Helper functions for ScopeManager */
    def addScope(resources: Int) = {
      Env(typeMap.addScope, capMap.addScope, typeDefMap)(res * resources)
    }
    def endScope(resources: Int) = {
      val scopes = for {
        (tScope, tMap) <- typeMap.endScope;
        (_, cMap) <- capMap.endScope
      } yield (Env(tMap, cMap, typeDefMap)(res / resources), tScope)

      scopes match {
        case None => throw Impossible("endScope", "Removed topmost scope")
        case Some((env, map)) => env -> map.map({ case (id, info) => id -> info.typ })
      }
    }

  }
}
