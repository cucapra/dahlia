package fuselang

import Syntax._
import Errors._

object TypeEnv {
  // Capabilities for read/write
  sealed trait Capability
  case object Read extends Capability
  case object Write extends Capability

  val emptyEnv: Environment = Env(List(Map[Id, Info]()), List(Map[Expr, Capability]()))

  // Product of all unroll factors enclosing the current context.
  type ReqResources = Int

  /**
   * An Environment to keep track of the types associated with identifiers
   * and capabilities associated with expressions.
   */
  trait Environment {

    // A Scope in the environment
    type Scope[K, V] = Map[K, V]

    /**
     * Methods to manipulate the scopes with the environment.
     * INVARIANT: There is at least one scope in the environment.
     */
    /** Create a new binding scope in the environment. */
    def addScope: Environment
    /**
     *  Remove the topmost scope from the environment.
     *  @returns A new environment without the topmost scope, A Scope containing
     *           all bindings in the topmost scope.
     */
    def endScope: (Environment, Scope[Id, Info])

    /**
     * Type binding manipulation
     */
    /**
     * Use application syntax to get a type bindings for [[id]].
     * @param id The id whose binding is needed
     * @returns Information associated with the id
     * @throws [[UnboundVar]] If there is no bindings for id
     */
    def apply(id: Id): Info

    /**
     * Add a new type binding in the current scope.
     * @param id The identifier to which the type is mapped.
     * @param typ The [[Info]] associated with the identifier.
     * @return A new environment which contains the mapping.
     * @throws [[AlreadyBound]] if a bindings for Id already exists.
     */
    def add(id: Id, typ: Info): Environment

    /**
     * Update bindings associated with Id. Method traverses the entire scope chain.
     * @param id The identifier whose type is being updated.
     * @param typ The new type associated with the identifier.
     * @return A new environment where Id is bound to this Type.
     * @throw [[UnboundVar]] If the Id doesn't already have a binding
     */
    def update(id: Id, typ: Info): Environment

    /**
     * Create a new Environment with all the bindings in [[binds]] added to the
     * current scope.
     * @param binds A scope with bindings to be added to the environment.
     * @returns A new environment with all the bindings in the environment.
     */
    def ++(binds: Scope[Id, Info]): Environment

    /**
     * Capability manipulation
     */
    /** Get the capability associated with [[e]].
     *  @param e The Expr to get the capability for.
     *  @returns The Capability associated with e. Returns None if no association is found.
     */
    def getCap(e: Expr): Option[Capability]
    /**
     * Associate the capabilitie [[cap]] to the expressions [[e]].
     * @param e The param with which the capability is associated
     * @param cap The capability the expression has.
     * @return A new environment which contains the capability mapping.
     */
    def addCap(e: Expr, cap: Capability): Environment

  }

  case class Info(
    id: Id,
    typ: Type,
    avBanks: Map[Int, Set[Int]],
    conBanks: Map[Int, Set[Int]]) {

    def consumeBank(dim: Int, bank: Int): Info = avBanks.contains(dim) match {
      case true => if (avBanks(dim).contains(bank)) {
        Info(
          id,
          typ,
          avBanks + (dim -> (avBanks(dim) - bank)),
          conBanks + (dim -> (conBanks(dim) + bank)))
      } else if (conBanks(dim).contains(bank)){
        throw AlreadyConsumed(id, dim, bank)
      } else {
        throw MsgError(s"Bank $bank does not exist for dimension $dim of $id.")
      }
      case false => throw UnknownDim(id, dim)
    }
    def consumeDim(dim: Int, unrollFactor: Int) = typ match {
      case TArray(_, dims) => dims.lift(dim) match {
        case Some((_, bank)) => {
          if (unrollFactor != bank) {
            throw BankUnrollInvalid(bank, unrollFactor)
          }
          val banks = 0.until(bank)
          banks.foldLeft(this)({ case (info, bank) => info.consumeBank(dim, bank) })
        }
        case None => throw UnknownDim(id, dim)
      }
      case t => throw Impossible(s"consumeDim called on non-array type $t")
    }
    def consumeAll = typ match {
      case TArray(_, dims) => dims.zipWithIndex.foldLeft(this)({
        case (info, ((_, bank), dim)) => info.consumeDim(dim, bank)
      })
      case _ => throw Impossible("consumeAll called on non-array")
    }

    override def toString = s"{$typ, $avBanks, $conBanks}"
  }

  // Companion object to allow for easier construction of Info.
  object Info {
    def apply(id: Id, typ: Type): Info = typ match {
      case TArray(_, dims) => {
        val banksWithIndex = dims.map({case (_, b) => b}).zipWithIndex
        Info(
          id,
          typ,
          banksWithIndex.map({case (banks, i) => i -> 0.until(banks).toSet}).toMap,
          banksWithIndex.map({case (_, i) => i -> Set[Int]()}).toMap)
      }
      case _ => Info(id, typ, Map(), Map())
    }
  }

  private case class Env(
    e: List[Map[Id, Info]],
    caps: List[Map[Expr, Capability]]) extends Environment {

    type Stack[T] = List[T]
    type TypeScope = Map[Id, Info]
    type CapScope = Map[Expr, Capability]

    def addScope = Env(Map[Id, Info]() :: e, Map[Expr, Capability]() :: caps)
    def endScope = (Env(e.tail, caps.tail), e.head)
    def apply(id: Id): Info = find(e, id) match {
      case Some(info) => info
      case None => throw UnboundVar(id)
    }

    def getCap(expr: Expr): Option[Capability] =
      caps.find(c => c.get(expr).isDefined).map(c => c(expr))

    def addCap(expr: Expr, cap: Capability) =
      Env(e, caps.head + (expr -> cap) :: caps.tail)

    private def find(e: Stack[TypeScope], id: Id): Option[Info] =
      e.find(m => m.get(id).isDefined) match {
        case None => None
        case Some(map) => Some(map(id))
      }

    def add(id: Id, typ: Info) = find(e, id) match {
      case Some(_) => throw AlreadyBound(id)
      case None => Env(e.head + (id -> typ) :: e.tail, caps)
    }
    def update(id: Id, typ: Info) = find(e, id) match {
      case None => throw UnboundVar(id)
      case Some(_) => {
        val scope = e.indexWhere(m => m.get(id).isDefined)
        Env(e.updated(scope, e(scope) + (id -> typ)), caps)
      }
    }
    def ++(binds: TypeScope) =
      binds.foldLeft[Environment](this)({ case (e, b) => e.add(b._1, b._2) })
  }
}
