package fuselang.passes

import scala.{PartialFunction => PF}

//import fuselang.Utils.RichOption

import fuselang.common._
import ScopeMap._
import ScopeMapExt._
import Syntax._
import Errors._
import CompilerError._
import Checker._
import EnvHelpers._

object LoopChecker {

  def check(p: Prog) = LCheck.check(p)
  object States extends Enumeration{
    type States = Value
    val USE, DEF, DK = Value
  }
  import States._

  // Bounds checker environment doesn't need to track any information. Empty
  // environment that just runs the commands.
  private case class LEnv(
    StateMap: ScopedMapExt[Id, States] = ScopedMapExt(),
    NameMap: ScopedMap[Id,Id] = ScopedMap())(implicit val res: Int) 
    extends ScopeManager[LEnv] {
    
    
    // Helper functions for NameMap
    def addName(vid:Id, tid:Id):LEnv = NameMap.add(vid,tid) match{
      case None => throw Impossible("NameMap has this view id before, redefinition")
      case Some(m) => LEnv(StateMap,m)
    }
    // in the case of for loop i,  
    def getName(aid:Id):Id = NameMap.get(aid).getOrElse(aid)
    
    // Helper functions for StateMap
    def atDef(id:Id): LEnv = StateMap.get(id) match{
      case None | Some(DK) => LEnv(StateMap+(id -> DEF), NameMap)
      case Some(DEF) => this 
      case Some(USE) => throw LoopDepSequential(id)
      case _ => throw Impossible("No such state")
    }
    def atDk(id:Id):LEnv = StateMap.get(id) match{
      case None | Some(DEF) => LEnv(StateMap+(id -> DK), NameMap)
      case Some(DK) => this
      case Some(USE) => throw LoopDepSequential(id) 
      case _ => throw Impossible("No such state")
    }
    def atUse(id:Id):LEnv = StateMap.get(id) match {
      case None => LEnv(StateMap+(id -> USE), NameMap) 
      case Some(DK) => throw LoopDepSequential(id)
      case Some(DEF)| Some(USE) => this //Use/Def -> Use don't update
      case _ => throw Impossible("No such state")
    }
    //check and update the state table
    def updateState(id:Id, state: States):LEnv = {
      if (res > 1){
        state match {
          case DK => atDk(id)
          case DEF => atDef(id)
          case USE => atUse(id)
        }
      }
      else
        this
    }
    // Helper functions for ScopeManager
    def withScope(resources: Int)(inScope: LEnv => LEnv): LEnv = {
      inScope(this.addScope(resources)) match {
        case env:LEnv => env.endScope(resources)
      }
    }
    // To satisfy envhelper
    def withScope(inScope: LEnv => LEnv): LEnv = withScope(1)(inScope)
    def addScope(resources: Int) = {
      LEnv(StateMap.addScope, NameMap.addScope)(res * resources)
    }
    def endScope(resources: Int) = {
      val nmap = NameMap.endScope.get._2
      val (innermap, outermap) = StateMap.endScope.get
      var outerenv = LEnv(outermap, nmap)(res/resources)
      val keys = innermap.keys
      for (k <- keys){
        outerenv = outerenv.updateState(k, innermap(k)) 
      }
      outerenv
    }
    
    def mergeHelper(k: Id, v1: Option[States], v2: Option[States], env: LEnv): LEnv = (v1, v2) match {
      case (None, None) => throw Impossible("No such merging")
      case (None, Some(s)) =>  env.copy(StateMap = env.StateMap + (k->s) )
      case (Some(DEF), Some(DK)) =>  env.copy(StateMap = env.StateMap + (k->DK) )
      case (Some(DEF), Some(USE)) => throw LoopDepMerge(k)
      case (Some(DK), Some(USE)) =>  throw LoopDepMerge(k)
      case (v1, v2) => if (v1 == v2) env else mergeHelper(k, v2, v1, env)
    }
    
    // If statement
    def merge(that: LEnv): LEnv = {
      val m1 = this.StateMap
      val m2 = that.StateMap
      var env = LEnv(m1, NameMap)
      if (res > 1){
        for (k <- m1.keys){
          env = mergeHelper(k, m1.get(k), m2.get(k), env)
        }
      }
      env
    }
  }

  private final case object LCheck extends PartialChecker {

    type Env = LEnv

    val emptyEnv = LEnv()(1)

    def myCheckLVal(e: Expr, env: Env): Env = { e match 
      {
        case EVar(id) => env.updateState(id, DEF)
        case EArrAccess(id, _) => println("id");env.updateState(id, DEF)
        case ERecAccess(rec, _) => myCheckLVal(rec, env)
        case _ => throw Impossible("Cannot be lhs value")
      }
    }
    
    override def myCheckE: PF[(Expr, Env), Env] = {
    // by default, this is rval
      case (EVar(id), e) => e.updateState(id, USE)
      case (EArrAccess(id, _), e) => e.updateState(id, USE)
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
