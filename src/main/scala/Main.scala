package fuselang

import java.nio.file.{Files, Path}
import java.io.File
import scala.io.Source

import Compiler._
import common.Logger
import common.Configuration._

object Main:
  // Command-line names for backends.
  val backends = Map(
    "vivado" -> Vivado,
    "c++" -> Cpp,
    "futil" -> Calyx,
    "calyx" -> Calyx
  )

  val memoryInterfaces = Map(
    "ap_memory" -> ApMemory,
    "axi" -> Axi
  )

  val meta = scala.io.Source.fromResource("version.properties")
    .getLines()
    .filter(l => l.trim != "")
    .map(d => {
      val Array(key, v) = d.split("=")
      (key.trim, v.trim)
    })
    .toMap

  val parser = new scopt.OptionParser[Config]("fuse"):

    head(s"Dahlia (sha = ${meta("git.hash")}, status = ${meta("git.status")})")

    this.version('V', "version")

    arg[File]("<srcfile>")
      .required()
      .action((x, c) => c.copy(srcFile = x))
      .text("Source code file.")

    opt[String]('o', "out")
      .valueName("<outfile>")
      .action((f, c) => c.copy(output = Some(f)))
      .text("Output code in a file. Default: use stdout.")

    opt[String]("path-descriptor")
      .valueName("<jsonFile>")
      .action((f, c) => c.copy(pathDescriptorPath = Some(f)))
      .text("file to write path descriptor mapping JSON to. (MUST be absolute path)")

    opt[String]('n', "name")
      .valueName("<kernel>")
      .validate(x =>
        if x.matches("[A-Za-z0-9_]+") then success
        else failure("Kernel name should only contain alphanumerals and _")
      )
      .action((x, c) => c.copy(kernelName = x))
      .text("Name of the top level function. Default: `kernel`.")

    opt[String]('b', "backend")
      .valueName("<backend>")
      .validate(b =>
        if backends.contains(b) then success
        else
          failure(
            s"Invalid backend name. Valid backends are ${backends.keys.mkString(", ")}"
          )
      )
      .action((b, c) => c.copy(backend = backends(b)))
      .text("Name of the backend to use. Default: `vivado`.")

    opt[Unit]("lower")
      .action((_, c) => c.copy(enableLowering = true))
      .text("Enable passes to lower programs. Default: false")

    opt[String]('l', "log-level")
      .action((s, c) => c.copy(logLevel = Logger.stringToLevel(s)))
      .text("Set logging level for the compiler. Default: `warn`.")

    opt[Unit]('h', "header")
      .action((_, c) => c.copy(header = true))
      .text("Generate header file instead of code. Default: false.")

    opt[Unit]("pass-debug")
      .action((_, c) => c.copy(passDebug = true))
      .text("Show the AST after every compiler pass. Default: false.")

    opt[String]("memory-interface")
      .validate(b =>
        if memoryInterfaces.contains(b) then success
        else
          failure(
            s"Invalid memory interface. Valid memory interfaces are ${memoryInterfaces.keys.mkString(", ")}"
          )
      )
      .action((m, c) => c.copy(memoryInterface = memoryInterfaces(m)))
      .text("The memory interface to use for the Vivado backend. Default `axi`")

    opt[String]('x', "compiler-opt")
      .optional()
      .unbounded()
      .action((x, c) => c.copy(compilerOpts = x :: c.compilerOpts))
      .text("Options to be passed to the backend. Can be repeated.")

    cmd("run")
      .action((_, c) => c.copy(mode = Run, backend = Cpp))
      .text(
        "Generate a runnable object file. Assumes GCC and required headers are available. Implies mode=c++."
      )
      .children(
        opt[String]('o', "outfile")
          .required()
          .action((f, c) => c.copy(output = Some(f)))
          .text("Name of the output artifact.")
      )

  def runWithConfig(conf: Config): Either[String, Int] =
    type ErrString = String

    val path = conf.srcFile.toPath
    val prog = Files.exists(path) match
      case true => Right(new String(Files.readAllBytes(path)))
      case false => Left(s"$path: No such file in working directory")
    
    val cppPath: Either[ErrString, Option[Path]] = prog.flatMap(prog =>
      conf.output match {
        case Some(out) =>
          compileStringToFile(prog, conf, out).map(path => Some(path))
        case None =>
          compileString(prog, conf).map(res => { println(res); None })
      }
    )

    val status: Either[ErrString, Int] = cppPath.flatMap(pathOpt =>
      conf.mode match {
        case Run =>
          GenerateExec.generateExec(
            pathOpt.get,
            s"${conf.output.get}.o",
            conf.compilerOpts
          )
        case _ => Right(0)
      }
    )
    status

  def main(args: Array[String]): Unit =
    parser.parse(args, emptyConf) match
      case Some(conf) =>
        Logger.setLogLevel(conf.logLevel)
        val status = runWithConfig(conf)
        sys.exit(
          status.left
            .map(compileErr => { System.err.println(compileErr); 1 })
            .merge
        )
      case None =>
        sys.exit(1)
