package fuselang

import java.io.File

import fuselang.backend.{VivadoBackend, VizBackend, CppRunnable, Backend}

object CmdlineConfig {

  sealed trait Mode
  final case object Compile extends Mode
  final case object Run extends Mode

  val emptyConf = CmdlineConfig(null)

  val validBackends = Set("vivado", "c++", "viz")

  def toBackend(str: String): Backend = str match {
    case "vivado" => VivadoBackend
    case "c++" => CppRunnable
    case "viz" => VizBackend
    case b@_ => throw common.CompilerError.Impossible(s"Unknown backend $b")
  }

  case class CmdlineConfig(
    srcFile: File,                        // Required: Name of the source file
    kernelName: String = "kernel",        // Name of the kernel to emit
    output: Option[String] = None,        // Name of output file.
    backend: Backend = VivadoBackend,     // Backend used for code generation
    mode: Mode = Compile,                 // Compilation mode
    compilerOpts: List[String] = List(),  // Extra options to the generateExec Compiler
    header: Boolean = false,              // Generate a header
    logLevel: scribe.Level = scribe.Level.Info
  ) {
    def toCommonConfig(): common.Configuration.Config = {
      common.Configuration.Config(
        this.srcFile,
        this.kernelName,
        this.output,
        this.compilerOpts,
        this.header,
        this.logLevel)
    }
  }


}
