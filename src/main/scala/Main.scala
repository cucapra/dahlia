package fuselang

import java.nio.file.{Files, Path}
import java.io.File

import Utils._

object Main {

  val parser = new scopt.OptionParser[Config]("fuse"){

    head("fuse", "0.0.1")

    arg[File]("<srcfile>")
      .required()
      .action((x, c) => c.copy(srcFile = x))
      .text("source code file")

    opt[String]('o', "out")
      .valueName("<outfile>")
      .action((f, c) => c.copy(output = Some(f)))
      .text("Output code in a file. Emits code to standard out if absent.")

    opt[String]('n', "name")
      .valueName("<kernel>")
      .validate(x =>
          if (x.matches("[A-Za-z0-9_]+")) success
          else failure("Kernel name should only contain alphanumerals and _"))
      .action((x, c) => c.copy(kernelName = x))
      .text("Name of the top level function. Default name is `kernel`.")

    opt[String]('b', "backend")
      .valueName("<backend>")
      .validate(b => if (validBackends.contains(b)) success
                     else failure(s"Invalid backend name. Valid backes are ${validBackends.mkString(",")}"))
      .action((b, c) => c.copy(backend = toBackend(b)))
      .text("Name of the backend to use. Default backed is vivado.")

    cmd("run")
      .action((_, c) => c.copy(mode = Run, backend = toBackend("c++")))
      .text("Generate a runnable object file. Assumes GCC and required headers are available. Implies mode=c++.")
      .children(
        opt[String]('o', "outfile")
          .required()
          .action((f, c) => c.copy(output = Some(f)))
          .text("name of the output artifact."))
  }

  def main(args: Array[String]): Unit = {

    parser.parse(args, Config(null)) match {
      case Some(conf) => {
        val prog = new String(Files.readAllBytes(conf.srcFile.toPath))

        val cppPath: Either[String, Option[Path]] = conf.output match {
          case Some(out) => Compiler.compileStringToFile(prog, conf, out).map(path => Some(path))
          case None => Compiler.compileString(prog, conf).map(res => { println(res); None })
        }

        val status: Either[String, Unit] = cppPath.flatMap(pathOpt => conf.mode match {
          case Run => GenerateExec.generateExec(pathOpt.get, s"${conf.output.get}.o")
          case _ => Right(())
        })

        status.left.map(compileErr => println(compileErr)).merge
      }
      case None => {
        sys.exit(1)
      }
    }
  }
}
