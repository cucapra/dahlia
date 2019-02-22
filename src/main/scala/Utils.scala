package fuselang

object Utils {

  val emptyConf = Config(null)

  case class Config(
    srcFile: java.io.File, // Required: Name of the source file
    kernelName: String = "kernel" // Name of the kernel to emit
  )

  case class State[S, A](v: S => (A, S)) extends AnyVal {
    def apply(k: S) = v(k)

    def map[B](f: A => B) = State[S, B] { state =>
      val (value, newState) = v(state)
      (f(value), newState)
    }
    def flatMap[B](f: A => State[S, B]) = State[S, B] { state =>
      val (value, newState) = v(state)
      f(value)(newState)
    }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State { state => (a, state) }

    def foldLeft[S, A](map: A => State[S, A])(es: List[A]): State[S, List[A]] =
      es match {
        case Nil => State.unit(Nil)
        case hd :: tl => for {
          hdn <- map(hd)
          tln <- foldLeft(map)(tl)
        } yield hdn :: tln
      }
  }

}
