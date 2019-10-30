package fuselang

import java.nio.file.{Files, Path}
import java.io.File

import Compiler._
import common.Logger
import common.Configuration._

object Main {

  // Command-line names for backends.
  val backends = Map(
    "vivado" -> Vivado,
    "c++" -> Cpp,
    "futil" -> Futil,
  )

  val parser = new scopt.OptionParser[Config]("fuse") {

    head("fuse", "0.0.1")

    arg[File]("<srcfile>")
      .required()
      .action((x, c) => c.copy(srcFile = x))
      .text("Source code file.")

    opt[String]('o', "out")
      .valueName("<outfile>")
      .action((f, c) => c.copy(output = Some(f)))
      .text("Output code in a file. Default: use stdout.")

    opt[String]('n', "name")
      .valueName("<kernel>")
      .validate(x =>
          if (x.matches("[A-Za-z0-9_]+")) success
          else failure("Kernel name should only contain alphanumerals and _"))
      .action((x, c) => c.copy(kernelName = x))
      .text("Name of the top level function. Default: `kernel`.")

    opt[String]('b', "backend")
      .valueName("<backend>")
      .validate(b => if (backends.contains(b)) success
                     else failure(s"Invalid backend name. Valid backends are ${backends.keys.mkString(", ")}"))
      .action((b, c) => c.copy(backend = backends(b)))
      .text("Name of the backend to use. Default: `vivado`.")

    opt[String]('l', "log-level")
      .action((s, c) => c.copy(logLevel = Logger.stringToLevel(s)))
      .text("Set logging level for the compiler. Default: `info`.")

    opt[Unit]('h', "header")
      .action((_, c) => c.copy(header = true))
      .text("Generate header file instead of code. Default: false.")

    opt[Unit]("pass-debug")
      .action((_, c) => c.copy(passDebug = true))
      .text("Show the AST after every compiler pass. Default: false.")

    cmd("run")
      .action((_, c) => c.copy(mode = Run, backend = Cpp))
      .text("Generate a runnable object file. Assumes GCC and required headers are available. Implies mode=c++.")
      .children(
        opt[String]('o', "outfile")
          .required()
          .action((f, c) => c.copy(output = Some(f)))
          .text("Name of the output artifact."),
        opt[String]('x', "compiler-opt")
          .optional()
          .unbounded()
          .action((x, c) => c.copy(compilerOpts = x :: c.compilerOpts))
          .text("Option to be passed to the C++ compiler. Can be repeated."))
  }

  def runWithConfig(conf: Config): Either[String, Int] = {
    type ErrString = String

    val path = conf.srcFile.toPath
    val prog = Files.exists(path) match {
      case true => Right(new String(Files.readAllBytes(path)))
      case false => Left(s"$path: No such file in working directory")
    }

    val cppPath: Either[ErrString, Option[Path]] = prog.flatMap(prog => conf.output match {
      case Some(out) => compileStringToFile(prog, conf, out).map(path => Some(path))
      case None => compileString(prog, conf).map(res => { println(res); None })
    })

    val status: Either[ErrString, Int] = cppPath.flatMap(pathOpt => conf.mode match {
      case Run =>
        GenerateExec.generateExec(pathOpt.get, s"${conf.output.get}.o", conf.compilerOpts)
      case _ => Right(0)
    })

    status
  }

  def main(args: Array[String]): Unit = {

    parser.parse(args, emptyConf) match {
      case Some(conf) => {
        Logger.setLogLevel(conf.logLevel)
        val status = runWithConfig(conf)
        sys.exit(
          status.left.map(compileErr => { System.err.println(compileErr); 1 }).merge)
      }
      case None => {
        sys.exit(1)
      }
    }
  }
}
