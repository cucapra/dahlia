package fuselang.passes

import scala.{PartialFunction => PF}

//import fuselang.Utils.RichOption

import fuselang.common._
import ScopeMap._
import ScopeMapExt._
import Syntax._
//import Errors._
import CompilerError._
//import Logger._
import Checker._
import EnvHelpers._

object LoopChecker {

  def check(p: Prog) = LCheck.check(p)

  // Bounds checker environment doesn't need to track any information. Empty
  // environment that just runs the commands.
  private case class LEnv(
    StateMap: ScopedMapExt[Id, Int] = ScopedMapExt(),
    NameMap: ScopedMap[Id,Id] = ScopedMap())(implicit val res: Int) 
    extends ScopeManager[LEnv] {
    
    //states 
    val USE = 0
    val DEF = 1
    val DK = 2
    
    // Whether we need to check loop dependency depends on 
    // 1. if there is a loop
    // 2. if the loop needs unrolling
    //def needCheck: Boolean = res>1//loop && res > 1
    
    // Helper functions for NameMap
    def addName(vid:Id, tid:Id):LEnv = NameMap.add(vid,tid) match{
      case None => {Impossible("NameMap has this view id before, redefinition");this}
      case Some(m) => LEnv(StateMap,m)
    }
    // in the case of for loop i,  
    def getName(aid:Id):Id = NameMap.get(aid).getOrElse(aid)
    
    // Helper functions for StateMap
    def atDef(id:Id): LEnv = StateMap.get(id) match{
      case None | Some(DK) => LEnv(StateMap+(id->DEF), NameMap)//DK update to DEF
      case Some(DEF) => this 
      case Some(USE) => {Impossible("Loop cannot be unrolled");this} //impossible->error 
      case _ => throw Impossible("No such state")
    }
    def atDk(id:Id):LEnv = StateMap.get(id) match{
      case None | Some(DEF) => LEnv(StateMap+(id->DK), NameMap)
      case Some(DK) => this
      case Some(USE) => {Impossible("Loop cannot be unrolled");this} //impossible->error 
      case _ => throw Impossible("No such state")
    }
    def atUse(id:Id):LEnv = StateMap.get(id) match {
      case None => LEnv(StateMap+(id->USE), NameMap) 
      case Some(DK) => {Impossible("Loop cannot be unrolled") ;this}
      case Some(DEF)| Some(USE) => this //Use/Def -> Use don't update
      case _ => throw Impossible("No such state")
    }
    //check and update the state table
    def updateState(id:Id, state: Int):LEnv = state match {
      case DK => atDk(id)
      case DEF => atDef(id)
      case USE => atUse(id)
      case _ => throw Impossible("No such state")
    }
    // Helper functions for ScopeManager
    def withScope(resources: Int = 1, loop: Boolean = false )(inScope: LEnv => LEnv): LEnv = {
      inScope(this.addScope(resources, loop)) match {
        case env:LEnv => env.endScope(resources, loop)
      }
    }
    def withScope(inScope: LEnv => LEnv): LEnv = this//To satisfy envhelper
    def addScope(resources: Int, loop:Boolean) = {
      if (res * resources > 1 && loop == true)
        LEnv(StateMap.addScope, NameMap.addScope)(res * resources)
      else 
        LEnv(StateMap, NameMap.addScope)(res * resources)
    }
    def endScope(resources: Int, loop:Boolean) = {
      val nmap = NameMap.endScope.get._2
      if (res > 1 && loop == true){
        val keys = StateMap.keys
        val (innermap, outermap) = StateMap.endScope.get
        var outerenv = LEnv(outermap, nmap)(res/resources)
        for (k <- keys) outerenv = outerenv.updateState(k, innermap(k))      
        outerenv
      }
      else{
        this.copy(NameMap = nmap)
      }
    }
    // If statement
    def merge(that: LEnv): LEnv = {
      val m1 = this.StateMap
      val m2 = that.StateMap
      var env = LEnv(m1, NameMap)
      for (k <- m1.keys){
        val v = m1.get(k) 
        env = m2.get(k) match{
          case Some(DEF) => v match {
            case Some(DEF) => env
            case Some(USE) => throw Impossible("Conflict in stateMap, cannot merge to one environment")
            case Some(DK) | None => env.copy(StateMap = env.StateMap + (k->DK) )
            case _ => throw Impossible("No such state")
          }
          case Some(DK) => v match {
            case Some(USE) => throw Impossible("Conflict in stateMap, cannot merge to one environment")
            case  Some(DK) | Some(DEF) | None => env.copy(StateMap = env.StateMap + (k->DK) )
            case _ => throw Impossible("No such state")
          }
          case Some(USE) => v match {
            case None | Some(USE) => env.copy(StateMap = env.StateMap + (k->USE) )
            case Some(DK) | Some(DEF) => throw Impossible("Conflict in stateMap, cannot merge to one environment")
            case _ => throw Impossible("No such state")
          }
          case None => v match {
            case Some(DK) | Some(DEF) => env.copy(StateMap = env.StateMap + (k->USE) )
            case Some(USE) => env
            case _ => throw Impossible("No such state")
          }
          case _ => throw Impossible("No such state")
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
        case EVar(id) => env.atDef(env.getName(id))
        case EArrAccess(id, _) => env.atDef(env.getName(id))
        //case ERecAccess(rec, _) => env.atDef(rec)//TODO: fix it
        case _ => {Impossible("Cannot be lhs value"); env}//throw an error here
      }
    }
    
    override def myCheckE: PF[(Expr, Env), Env] = {
    // by default, this is rval
      case (EVar(id), e) => e.atUse(e.getName(id))
      case (EArrAccess(id, _), e) => e.atUse(e.getName(id))
    }
    
    override def myCheckC: PF[(Command, Env), Env] = {
      case (CUpdate(lhs, rhs), e) => checkE(rhs)(myCheckLVal(lhs,e))
      case (CReduce(_, lhs, rhs), e) => checkE(rhs)(myCheckLVal(lhs,e))
      case (_@CView(viewId, arrId, _), e) => {
        val id = e.getName(arrId)
        e.addName(viewId,id)
      }        
      case (_@CSplit(viewId, arrId, _), e) => {
        val id = e.getName(arrId)
        e.addName(viewId,id)
      }      
      case (CFor(range, _, par, _), e) => {
        e.withScope(range.u, true){ e1=>
          checkC(par)(e1)
        }
      } 
      case (CWhile(cond, _, body), e) => {
        val e1 = checkE(cond)(e)
        e1.withScope(1, false)(env => checkC(body)(env))
      }
      /*
        cmd match{
          case CUpdate(lhs, rhs) => checkE(rhs)(myCheckLVal(lhs,e))
          case CReduce(_, lhs, rhs) => checkE(rhs)(myCheckLVal(lhs,e))
          case _@CView(viewId, arrId, _) => {
            val id = e.getn(arrId)
            e.addn(viewId,id)
          }      
          case _@CSplit(viewId, arrId, _) => {
            val id = e.getn(arrId)
            e.addn(viewId,id)
          }      
          case CFor(range, _, par, _) => {
            e.withScope(range.u){ e1=>
              checkC(par)(e1)
            }
          }  
        }
      else
        e*/
    }
  }
}
