package fuselang

import java.io.File

object Utils {

  val emptyConf = Config(null)

  case class Config(
    srcFile: File, // Required: Name of the source file
    kernelName: String = "kernel", // Name of the kernel to emit
    output: Option[String] = None // Name of output file.
  )

}
