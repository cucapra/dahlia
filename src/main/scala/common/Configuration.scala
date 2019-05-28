package fuselang.common

import java.io.File

import fuselang.backend.{VivadoBackend, CppRunnable, Backend}

object Configuration {

  sealed trait Mode
  final case object Compile extends Mode
  final case object Run extends Mode

  val emptyConf = Config(null)

  val validBackends = Set("vivado", "c++")

  def toBackend(str: String): Backend = str match {
    case "vivado" => VivadoBackend
    case "c++" => CppRunnable
    case b => throw CompilerError.Impossible(s"Unknown backend $b")
  }

  case class Config(
    srcFile: File,                        // Required: Name of the source file
    kernelName: String = "kernel",        // Name of the kernel to emit
    output: Option[String] = None,        // Name of output file.
    backend: Backend = VivadoBackend,     // Backend used for code generation
    mode: Mode = Compile,                 // Compilation mode
    compilerOpts: List[String] = List(),  // Extra options to the generateExec Compiler
    header: Boolean = false,              // Generate a header
    logLevel: scribe.Level = scribe.Level.Info
  )

}
