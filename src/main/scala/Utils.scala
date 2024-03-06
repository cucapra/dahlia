package fuselang
import scala.{PartialFunction => PF}
import scala.math.{log10, ceil}

object Utils {

  implicit class RichOption[A](opt: => Option[A]) {
    def getOrThrow[T <: Throwable](except: T) = opt match {
      case Some(v) => v
      case None => throw except
    }
  }

  // https://codereview.stackexchange.com/questions/14561/matching-bigints-in-scala
  // TODO: This can overflow and result in an runtime exception
  object Big {
    def unapply(n: BigInt) = Some(n.toInt)
  }

  def bitsNeeded(n: Int): Int = n match {
    case 0 => 1
    case n if n > 0 => ceil(log10(n + 1) / log10(2)).toInt
    case n if n < 0 => bitsNeeded(n.abs) + 1
  }

  def bitsNeeded(n: BigInt): Int = n match {
    case Big(0) => 1
    case n if n > 0 => ceil(log10((n + 1).toDouble) / log10(2)).toInt
    case n if n < 0 => bitsNeeded(n.abs) + 1
  }

  def cartesianProduct[T](llst: Seq[Seq[T]]): Seq[Seq[T]] = {
    def pel(e: T, ll: Seq[Seq[T]], a: Seq[Seq[T]] = Nil): Seq[Seq[T]] =
      ll match {
        case Nil => a.reverse
        case x +: xs => pel(e, xs, (e +: x) +: a)
      }

    llst match {
      case Nil => Nil
      case x +: Nil => x.map(Seq(_))
      case x +: _ =>
        x match {
          case Nil => Nil
          case _ =>
            llst
              .foldRight(Seq(x))((l, a) => l.flatMap(x => pel(x, a)))
              .map(_.dropRight(x.size))
        }
    }
  }


  @inline def asPartial[A, B, C](f: (A, B) => C): PF[(A, B), C] = {
    case (a, b) => f(a, b)
  }

  @inline def assertOrThrow[T <: Throwable](cond: Boolean, except: => T) = {
    if !cond then throw except
  }

  @deprecated(
    "pr is used for debugging. Remove all call to it before committing",
    "fuse 0.0.1"
  )
  @inline def pr[T](v: T) = {
    println(v)
    v
  }

}
