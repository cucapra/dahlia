/**
  * Lightweight library from the deprecated scala.text distribution.
  */
package fuselang.common

import java.io.Writer

object PrettyPrint {
  case object DocNil extends Doc
  case object DocBreak extends Doc
  case object DocSpace extends Doc
  case class DocText(txt: String) extends Doc
  case class DocNest(indent: Int, doc: Doc) extends Doc
  case class DocCons(hd: Doc, tl: Doc) extends Doc {}

  /**
    * A basic pretty-printing library, based on Lindig's strict version
    * of Wadler's adaptation of Hughes' pretty-printer.
    *
    * @author Michel Schinz
    * @version 1.0
    */
  abstract class Doc {
    def <@>(hd: Doc): Doc = {
      if hd == DocNil then this
      else this <> DocBreak <> hd
    }
    def <>(hd: Doc): Doc = (this, hd) match {
      case (_, DocNil) => this
      case (DocNil, _) => hd
      case _ => new DocCons(this, hd)
    }
    def <+>(hd: Doc): Doc = this <> DocSpace <> hd

    def pretty: String = {
      val writer = new java.io.StringWriter()
      format(writer)
      writer.toString
    }

    /**
      * Format this Doc on `writer`.
      */
    def format(writer: Writer): Unit = {
      type FmtState = (Int, Doc)

      def spaces(n: Int): Unit = {
        var rem = n
        while rem >= 16 do { writer write "                "; rem -= 16 }
        if rem >= 8 then { writer write "        "; rem -= 8 }
        if rem >= 4 then { writer write "    "; rem -= 4 }
        if rem >= 2 then { writer write "  "; rem -= 2 }
        if rem == 1 then { writer write " " }
      }

      def fmt(state: List[FmtState]): Unit = state match {
        case List() => ()
        case (_, DocNil) :: z => fmt(z)
        case (i, DocCons(h, t)) :: z => fmt((i, h) :: (i, t) :: z)
        case (_, DocText(t)) :: z => {
          writer.write(t)
          fmt(z)
        }
        case (i, DocNest(ii, d)) :: z => fmt((i + ii, d) :: z)
        case (i, DocBreak) :: z => {
          writer.write("\n")
          spaces(i)
          fmt(z)
        }
        case (_, DocSpace) :: z => {
          writer.write(" "); fmt(z)
        }
        case _ => ()
      }

      fmt(List((0, this)))
    }
  }

  object Doc {

    /** The empty Doc */
    def emptyDoc = DocNil
    def line = DocBreak
    def space = DocSpace

    /** A Doc consisting of some text literal */
    def text(s: String): Doc = DocText(s)

    def value(v: Any): Doc = text(v.toString)

    /** Common primitives */
    def semi: Doc = text(";")
    def colon: Doc = text(":")
    def comma: Doc = text(",")
    def dot: Doc = text(".")
    def equal: Doc = text("=")
    def lbrace: Doc = text("{")
    def rbrace: Doc = text("}")

    /** A nested Doc, which will be indented as specified. */
    def nest(d: Doc, i: Int): Doc = DocNest(i, d)

    def folddoc(ds: Iterable[Doc], f: (Doc, Doc) => Doc) =
      if ds.isEmpty then emptyDoc
      else ds.tail.foldLeft(ds.head)(f)

    /** Builder functions */
    def hsep(ds: Iterable[Doc], sep: Doc): Doc =
      folddoc(ds, (_ <> sep <> _))

    def ssep(ds: Iterable[Doc], sep: Doc): Doc =
      folddoc(ds, (_ <> sep <> _))

    def hsep(ds: Iterable[Doc]): Doc =
      folddoc(ds, (_ <+> _))

    def vsep(ds: Iterable[Doc], sep: Doc): Doc =
      folddoc(ds, (_ <@> sep <@> _))

    def vsep(ds: Iterable[Doc]): Doc =
      folddoc(ds, (_ <@> _))

    def enclose(l: Doc, d: Doc, r: Doc) = l <> d <> r

    def surround(d: Doc, s: Doc) = enclose(s, d, s)

    def commaSep(docs: Seq[Doc]) = hsep(docs, comma <> space)

    def scope(
        doc: Doc,
        left: Doc = lbrace,
        right: Doc = rbrace,
        indent: Int = 2,
    ): Doc =
      left <> nest(emptyDoc <@> doc, indent) <@> right

    /** Common functions **/
    def quote(d: Doc) = surround(d, text("\""))

    def parens(d: Doc) = enclose(text("("), d, text(")"))
    def braces(d: Doc) = enclose(text("{"), d, text("}"))
    def brackets(d: Doc) = enclose(text("["), d, text("]"))
    def angles(d: Doc) = enclose(text("<"), d, text(">"))
  }
}
