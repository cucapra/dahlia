package fuselang.passes

import scala.{PartialFunction => PF}

//import fuselang.Utils.RichOption

import fuselang.common._
import ScopeMap._
import ScopeMapExt._
import Syntax._
import Errors._
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
    
    // Helper functions for NameMap
    def addName(vid:Id, tid:Id):LEnv = NameMap.add(vid,tid) match{
      case None => throw Impossible("NameMap has this view id before, redefinition")
      case Some(m) => LEnv(StateMap,m)
    }
    // in the case of for loop i,  
    def getName(aid:Id):Id = NameMap.get(aid).getOrElse(aid)
    
    // Helper functions for StateMap
    def atDef(id:Id): LEnv = StateMap.get(id) match{
      case None | Some(DK) => LEnv(StateMap+(id -> DEF), NameMap)//DK update to DEF
      case Some(DEF) => this 
      case Some(USE) => throw LoopDepSequential(id)
      case _ => throw Impossible("No such state")
    }
    def atDk(id:Id):LEnv = StateMap.get(id) match{
      case None | Some(DEF) => LEnv(StateMap+(id->DK), NameMap)
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
    def updateState(id:Id, state: Int):LEnv = {
      if (res > 1){
        state match {
          case DK => atDk(id)
          case DEF => atDef(id)
          case USE => atUse(id)
          case _ => throw Impossible("No such state")
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
    def withScope(inScope: LEnv => LEnv): LEnv = withScope(1)(inScope)//To satisfy envhelper
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
    // If statement
    def merge(that: LEnv): LEnv = {
      val m1 = this.StateMap
      val m2 = that.StateMap
      var env = LEnv(m1, NameMap)
      if (res > 1){
        for (k <- m1.keys){
          val v = m1.get(k) 
          env = m2.get(k) match{
            case Some(DEF) => v match {
              case Some(DEF) => env
              case Some(USE) => throw LoopDepMerge(k)
              case Some(DK) | None => env.copy(StateMap = env.StateMap + (k->DK) )
              case _ => throw Impossible("No such state")
            }
            case Some(DK) => v match {
              case Some(USE) =>throw LoopDepMerge(k)
              case  Some(DK) | Some(DEF) | None => env.copy(StateMap = env.StateMap + (k->DK) )
              case _ => throw Impossible("No such state")
            }
            case Some(USE) => v match {
              case None | Some(USE) => env.copy(StateMap = env.StateMap + (k->USE) )
              case Some(DK) | Some(DEF) => throw LoopDepMerge(k)
              case _ => throw Impossible("No such state")
            }
            case None => v match {
              case Some(DK) | Some(DEF) => env.copy(StateMap = env.StateMap + (k->DK) )
              case Some(USE) => env
              case _ => throw Impossible("No such state")
            }
            case _ => throw Impossible("No such state")
          }
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
        case EVar(id) => env.updateState(id, env.DEF)
        case EArrAccess(id, _) => env.updateState(id, env.DEF)
        case ERecAccess(rec, _) => println(rec); env//TODO: fix it
        case _ => throw Impossible("Cannot be lhs value")
      }
    }
    
    override def myCheckE: PF[(Expr, Env), Env] = {
    // by default, this is rval
      case (EVar(id), e) => e.updateState(id, e.USE)
      case (EArrAccess(id, _), e) => e.updateState(id, e.USE)
    }
    
    override def myCheckC: PF[(Command, Env), Env] = {
      case (CUpdate(lhs, rhs), e) => myCheckLVal(lhs,checkE(rhs)(e))
      case (CReduce(_, lhs, rhs), e) => myCheckLVal(lhs,checkE(rhs)(e))
      case (CLet(id, _, eOpt), e) => myCheckLVal(EVar(id),checkE(eOpt.get)(e)) //TODO: None is deprecated. But what is None
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
