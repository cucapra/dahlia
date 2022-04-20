package fuselang.common

import java.io.File

object Configuration:

  sealed trait Mode
  case object Compile extends Mode
  case object Run extends Mode

  def stringToBackend(name: String): Option[BackendOption] = name match
    case "vivado" => Some(Vivado)
    case "c++" => Some(Cpp)
    case "futil" => Some(Calyx)
    case _ => None

  // What kind of code to generate.
  sealed trait BackendOption:
    override def toString(): String = this match
      case Vivado => "vivado"
      case Cpp => "c++"
      case Calyx => "calyx"
  case object Vivado extends BackendOption
  case object Cpp extends BackendOption
  case object Calyx extends BackendOption

  // The type of Vivado memory interface to generate
  sealed trait MemoryInterface
  case object ApMemory extends MemoryInterface
  case object Axi extends MemoryInterface

  val emptyConf: Config = Config(null)

  case class Config(
      srcFile: File, // Required: Name of the source file
      kernelName: String = "kernel", // Name of the kernel to emit
      output: Option[String] = None, // Name of output file.
      backend: BackendOption = Vivado, // Backend used for code generation
      mode: Mode = Compile, // Compilation mode
      compilerOpts: List[String] = List(), // Extra options for the backend
      header: Boolean = false, // Generate a header
      passDebug: Boolean = false, // Show AST after every state
      logLevel: scribe.Level = scribe.Level.Warn,
      enableLowering: Boolean = false, // Enable lowering passes
      memoryInterface: MemoryInterface = Axi, // The memory interface to use for vivado
  )

