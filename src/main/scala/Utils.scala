package fuselang
import scala.math.{log10, ceil, abs}

object Utils {

  implicit class RichOption[A](opt: Option[A]) {
    def getOrThrow[T <: Throwable](except: T) = opt match {
      case Some(v) => v
      case None => throw except
    }
  }

  def bitsNeeded(n: Int): Int = n match {
    case 0 => 1
    case n if n > 0 => ceil(log10(n + 1) / log10(2)).toInt
    case n if n < 0 => bitsNeeded(abs(n)) + 1
  }

  @inline def assertOrThrow[T <: Throwable](cond: Boolean, except: => T) = {
    if (!cond) throw except
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
