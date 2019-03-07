package fuselang

import java.io.File

import backend.{VivadoBackend, CppRunnable, Backend}

object Utils {

  sealed trait Mode
  case object Compile extends Mode
  case object Run extends Mode

  val emptyConf = Config(null)

  val validBackends = Set("vivado", "c++")

  def toBackend(str: String): Backend = str match {
    case "vivado" => VivadoBackend
    case "c++" => CppRunnable
    case b => throw Errors.Impossible(s"Unknown backend $b")
  }

  case class Config(
    srcFile: File, // Required: Name of the source file
    kernelName: String = "kernel", // Name of the kernel to emit
    output: Option[String] = None, // Name of output file.
    backend: Backend = VivadoBackend,
    mode: Mode = Compile,
    compilerOpts: List[String] = List()
  )

}
