package fuselang

object Utils {

  implicit class RichOption[A](opt: Option[A]) {
    def getOrThrow[T <: Throwable](except: T) = opt match {
      case Some(v) => v
      case None => throw except
    }
  }

  @inline def assertOrThrow[T <: Throwable](cond: Boolean, except: T) = {
    if (!cond) throw except
  }

}
