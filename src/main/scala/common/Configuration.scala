package fuselang.common

import java.io.File

object Configuration {

  sealed trait Mode
  final case object Compile extends Mode
  final case object Run extends Mode

  // What kind of code to generate.
  sealed trait BackendOption
  final case object Vivado extends BackendOption
  final case object Cpp extends BackendOption
  final case object Futil extends BackendOption

  val emptyConf = Config(null)

  case class Config(
      srcFile: File, // Required: Name of the source file
      kernelName: String = "kernel", // Name of the kernel to emit
      output: Option[String] = None, // Name of output file.
      backend: BackendOption = Vivado, // Backend used for code generation
      mode: Mode = Compile, // Compilation mode
      compilerOpts: List[String] = List(), // Extra options to the generateExec Compiler
      header: Boolean = false, // Generate a header
      passDebug: Boolean = false, // Show AST after every state
      logLevel: scribe.Level = scribe.Level.Info,
      enableLowering: Boolean = false // Enable lowering passes
  )

}
