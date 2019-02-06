package fuselang

object Utils {

  case class Config(
    srcFile: java.io.File, // Required: Name of the source file
    kernelName: String = "kernel" // Name of the kernel to emit
  )
}
