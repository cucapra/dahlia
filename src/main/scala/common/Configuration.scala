package fuselang.common

import java.io.File

object Configuration {

  case class Config(
    srcFile: File,                        // Required: Name of the source file
    kernelName: String = "kernel",        // Name of the kernel to emit
    output: Option[String] = None,        // Name of output file.
    compilerOpts: List[String] = List(),  // Extra options to the generateExec Compiler
    header: Boolean = false,              // Generate a header
    logLevel: scribe.Level = scribe.Level.Info
  )

}
