package fuselang

import java.nio.file.Files
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
  }

  def main(args: Array[String]): Unit = {

    parser.parse(args, Config(null)) match {
      case Some(c) => {
        val prog = new String(Files.readAllBytes(c.srcFile.toPath))
        c.output match {
          case Some(out) => Compiler.compileStringToFile(prog, c, out)
          case None => println(Compiler.compileString(prog, c))
        }
      }
      case None => {
        sys.exit(1)
      }
    }
  }
}
