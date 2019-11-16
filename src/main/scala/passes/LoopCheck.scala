package fuselang.passes

import scala.{PartialFunction => PF}

import fuselang.common._
import ScopeMap._
import Syntax._
import Errors._
import CompilerError._
import Checker._
import EnvHelpers._

object LoopChecker {

  // Possible mappings for stateMap
  sealed trait States
  case object Use extends States
  case object Def extends States
  case object DontKnow extends States

  def check(p: Prog) = LCheck.check(p)

  // Loop checker environment doesn't need to track any information. Empty
  // environment that just runs the commands.
  private case class LEnv(
    stateMap: ScopedMap[Id, States] = ScopedMap(),
    nameMap: ScopedMap[Id, Id] = ScopedMap(),
    exprMap: ScopedMap[Id, List[Expr]] = ScopedMap())(implicit val res: Int = 1)
    extends ScopeManager[LEnv] {


    // Helper functions for nameMap
    def addName(vid:Id, tid:Id):LEnv = nameMap.add(vid,tid) match{
      case None => throw Impossible("nameMap has this view id before, redefinition")
      case Some(m) => LEnv(stateMap,m)
    }
    
    def getName(aid:Id):Id = nameMap.get(aid).getOrElse(aid)

    // Helper functions for stateMap
    def atDef(id:Id): LEnv = stateMap.head.get(id) match{
      case None | Some(DontKnow) => LEnv(stateMap.addShadow(id, Def), nameMap, exprMap)
      case Some(Def) => this
      case Some(Use) => throw LoopDepSequential(id)
    }
    def atDk(id:Id):LEnv = stateMap.head.get(id) match{
      case None | Some(Def) => LEnv(stateMap.addShadow(id, DontKnow), nameMap, exprMap)
      case Some(DontKnow) => this
      case Some(Use) => throw LoopDepSequential(id)
    }
    def atUse(id:Id):LEnv = stateMap.head.get(id) match {
      case None => LEnv(stateMap.addShadow(id, Use), nameMap, exprMap)
      case Some(DontKnow) => throw LoopDepSequential(id)
      case Some(Def)| Some(Use) => this //Use/Def -> Use don't update
    }
    
    def checkExprMap(idxs: Option[EArrAccess]):(LEnv, Boolean) = res match{
      case 1 => (this, false)
      case _ => idxs match {      
        case Some(EArrAccess(id, idxs)) => exprMap.get(id) match {
          case None =>(this.copy(exprMap=exprMap.add(id, idxs).get), true)
          case Some(idxlist) => {
            for (i <- 0 to stateMap.length - 1) {
              if(idxlist(i) != idxs(i)){
                return (this, true)
              }
            }
            (this, false)
          }
        }
        case None => (this, true)
      }
    }
    //check and update the state table
    def updateState(id:Id, state: States, idxs: Option[EArrAccess] = None):LEnv = {
      val (env, check) = checkExprMap(idxs)
      if (check) {
        val e2 = state match {
          case DontKnow => env.atDk(env.getName(id))
          case Def => env.atDef(env.getName(id))
          case Use => env.atUse(env.getName(id))
        }
        e2
      }
      else
        env
    }
    // Helper functions for ScopeManager
    def withScope(resources: Int)(inScope: LEnv => LEnv): LEnv = {
      if (resources == 1){
          inScope(this.addNameScope) match {
            case env:LEnv => env.endNameScope
          }
      }else{
          inScope(this.addScope(resources)) match {
            case env:LEnv => env.endScope(resources)
          }
      }
    }
    
    // To satisfy envhelper
    def withScope(inScope: LEnv => LEnv): LEnv = withScope(1)(inScope)
    def addScope(resources: Int) = {
      LEnv(stateMap.addScope, nameMap.addScope, exprMap.addScope)(res * resources)
    }
    def endScope(resources: Int) = {
      val nmap = nameMap.endScope.get._2
      val emap = exprMap.endScope.get._2
      val (innermap, outermap) = stateMap.endScope.get
      var outerenv = LEnv(outermap, nmap, emap)(res/resources)
      val keys = innermap.keys
      for (k <- keys){
        outerenv = outerenv.updateState(k, innermap(k))//inner map is a scala map
      }
      outerenv
    }
    def addNameScope = {
      LEnv(stateMap, nameMap.addScope, exprMap)
    }
    def endNameScope = {
      val nmap = nameMap.endScope.get._2
      LEnv(stateMap, nmap, exprMap)
    }

    def mergeHelper(k: Id, v1:  Option[States], v2: Option[States], env: LEnv): LEnv = (v1, v2) match {
      case (None, None) => throw Impossible("No such merging")
      case (None, Some(Use)) =>  env.copy(stateMap = env.stateMap.addShadow(k, Use) )
      case (None, _) =>  env.copy(stateMap = env.stateMap.addShadow(k, DontKnow) )
      case (Some(Def), Some(DontKnow)) =>  env.copy(stateMap = env.stateMap.addShadow(k, DontKnow) )
      case (Some(Def), Some(Use)) => throw LoopDepMerge(k)
      case (Some(DontKnow), Some(Use)) =>  throw LoopDepMerge(k)
      case (v1, v2) => if (v1 == v2) env else mergeHelper(k, v2, v1, env)
    }

    // If statement
    def merge(that: LEnv): LEnv = {
      val m1 = this.stateMap
      val m2 = that.stateMap
      val res = m1.head.keys.foldLeft[LEnv](LEnv(m1, nameMap, exprMap))({
        case (env, k) => mergeHelper(k, m1.get(k), m2.get(k), env)
      })
      res
    }
  }

  private final case object LCheck extends PartialChecker {

    type Env = LEnv

    val emptyEnv = LEnv()(1)

    def myCheckLVal(e: Expr, env: Env): Env = { e match
      {
        case EVar(id) => env.updateState(id, Def)
        case EArrAccess(id, idxs) => env.updateState(id, Def, Some(EArrAccess(id, idxs)))
        case ERecAccess(rec, _) => myCheckLVal(rec, env)
        case _ => throw Impossible("Cannot be lhs value")
      }
    }

    override def myCheckE: PF[(Expr, Env), Env] = {
    // by default, this is rval
      case (EVar(id), e) => e.updateState(id, Use)
      case (EArrAccess(id, idxs), e) => {
        e.updateState(id, Use, Some(EArrAccess(id, idxs)))
      }
    }

    override def myCheckC: PF[(Command, Env), Env] = {
      case (CUpdate(lhs, rhs), e) => myCheckLVal(lhs,checkE(rhs)(e))
      case (CReduce(_, lhs, rhs), e) => myCheckLVal(lhs,checkE(rhs)(e))
      case (CLet(id, _, None), e) => myCheckLVal(EVar(id), e)
      case (CLet(id, _, eOpt), e) => myCheckLVal(EVar(id), checkE(eOpt.get)(e) )
      case (_@CView(viewId, arrId, _), e) => {
        val id = e.getName(arrId)
        e.addName(viewId,id)
      }
      case (_@CSplit(viewId, arrId, _), e) => {
        val id = e.getName(arrId)
        e.addName(viewId,id)
      }
      case (CFor(range, _, par, _), e) => {
        e.withScope(range.u){ e1=>
          checkC(par)(e1)
        }
      }
      case (CWhile(cond, _, body), e) => {
        val e1 = checkE(cond)(e)
        e1.withScope(1)(env => checkC(body)(env))
      }
      case (CIf(cond, c1, c2), e) => {
        e.withScope( someScope =>{
          val nEnv = checkE(cond)(someScope)
          val e1 = nEnv.withScope(checkC(c1)(_))
          val e2 = nEnv.withScope(checkC(c2)(_))
          e1 merge e2
        })
      }
    }
  }
}
