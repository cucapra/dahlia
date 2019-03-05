package fuselang

import java.io.File

import backend.{VivadoBackend, CppRunnable, Backend}

object Utils {

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
    backend: Backend = VivadoBackend
  )

}
