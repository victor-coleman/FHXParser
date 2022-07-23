import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.util.Date
import scala.annotation.targetName
import scala.io.{Codec, Source}
import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.util.parsing.combinator.*

sealed trait Writeable:
  def write(i: IndentationLevel): String

sealed trait Node extends Writeable
sealed trait AttValue extends Writeable:
  val s: String
  override def toString = s
  override def write(i: IndentationLevel) = s"${i.tab}$s"

type Index = String
type HexPair = String
type TagName = String
case class VectorValue(override val s: String) extends AttValue
case class StringValue(override val s: String) extends AttValue

case class ArrayValue(is: List[Index]) extends AttValue:
  override val s = is.mkString("[", ",", "]")
case class BitmaskValue(bs: List[HexPair]) extends AttValue:
  override val s = bs.mkString("{", " ", "}")

case object Comment extends Node:
  override def write(i: IndentationLevel) = ""

case class ValidAttribute(name: TagName, value: AttValue) extends Node:
  def write(i: IndentationLevel): String =
    if (value.toString.isEmpty) {
      s"${i.tab}<$name />\n"
    } else {
      s"${i.tab}<$name>$value</$name>\n"
    }

sealed trait IndentationLevel:
  def numTabs: Int

extension (l: IndentationLevel)
  @targetName("plus")
  def +(i: Int): IndentationLevel = new IndentationLevel:
    override def numTabs = i + l.numTabs
  def tab: String =  "\t" * l.numTabs

case class ValidElement(name: TagName, attributes: List[Node], body: List[Node])
  extends Node:
  override def write(i: IndentationLevel): String =
    if (attributes.isEmpty && body.isEmpty) {
      s"${i.tab}<$name />\n"
    } else {
      inline def f(xs: List[Node]) = xs.map(_.write(i + 1)).mkString("")
      s"${i.tab}<$name>\n${f(attributes)}${f(body)}${i.tab}</$name>\n"
    }

class FHXToXML extends RegexParsers {
  def comment: Parser[Comment.type] =
    "/*" ~> ("""((\*[^/])|[^*])""".r | comment).* <~ "*/" ^^^ Comment

  inline def tagName: Parser[TagName] = """[a-zA-Z_:][a-zA-Z\d._\-:]*""".r
  inline def index: Parser[Index] = """\d+(\.\.\d+)?""".r
  inline def byte: Parser[HexPair] = """[a-fA-F\d]{2}""".r
  def word: Parser[StringValue] = """[\w\-.+]+""".r ^^ StringValue.apply
  def vector: Parser[VectorValue] =
    """\([\d|/\-.+e]*\)""".r ^^ VectorValue.apply

  def stringSafe: Parser[StringValue] = "\"" ~> {
    """\s*[^"&<>]+""".r
      | ("""\s*[^"]+""".r ^^ xmlClean)
      | (""""(\n\t)?"""".r ^^ escapeClean)
  }.* <~ "\"" ^^ (s => StringValue(s.mkString("")))

  def array: Parser[ArrayValue] =
    "[" ~> repsep(index, ",") <~ "]" ^^ ArrayValue.apply

  def bitmask: Parser[BitmaskValue] = "{" ~> byte.+ <~ "}" ^^ BitmaskValue.apply

  def attributeContent: Parser[AttValue] =
    word | stringSafe | array | vector | bitmask

  def attribute: Parser[ValidAttribute] = (tagName <~ "=")
    ~ attributeContent ^^ { case n ~ c => ValidAttribute(n, c) }

  def preamble: Parser[List[Node]] = "=" ^^^ Nil | repOmitComment(attribute)

  def elementBody: Parser[List[Node]] =
    "{" ~> repOmitComment(attribute | element) <~ "}"

  def element: Parser[ValidElement] = tagName ~ preamble ~ elementBody
    ^^ { case n ~ as ~ b => ValidElement(n, as, b) }

  def root: Parser[ValidElement] = repOmitComment(element)
    ^^ (elems => ValidElement("FHX_ROOT", List(), elems))

  val escapeClean = (s: String) => s
    .replace("\"\n\t\"", "\n\t")
    .replace("\"\"", "\"")

  val xmlClean = (s: String) => s
    .replace("&", "&amp;")
    .replace(">", "&gt;")
    .replace("<", "&lt;")

  def repOmitComment(p: => Parser[Node]): Parser[List[Node]] =
    (p | comment).* ^^ {_.filter(_ match
      case Comment => false
      case _ => true
    )}
}

object FHXToXML extends FHXToXML {
  case object LeftJustified extends IndentationLevel:
    override def numTabs = 0

  val inputFilename = "POWER.fhx"
  val outputFilename = inputFilename.replace(".fhx", ".xml")

  val inputFilepath =
    Paths.get("src", "main", "IN", inputFilename).toString
  val outputFilepath =
    Paths.get("src", "main", "OUT", outputFilename).toString

  def main(args: Array[String]): Unit = {
    val (success, deltaT) =
      timeFunCall(() => convertFile(inputFilepath, outputFilepath))
    if (success) println(s"completed! - Time elapsed: ${deltaT / 1000.0}s")
    //    checkAgainstSample()
  }

  def checkAgainstSample(): Unit = {
    val sample =
      Paths.get("src", "main", "OUT", "POWER_EXEMPLAR.xml").toString

    val matched = checkCorrectness(outputFilepath, sample)
    if (matched) {
      println("It worked!")
    } else {
      println("Better luck next time")
    }
  }

  def timeFunCall[T](f: () => T): (T, Long) = {
    val startTime = Date().getTime
    val res = f()
    val endTime = Date().getTime
    (res, endTime - startTime)
  }

  def convertFile(inputFilepath: String, outputFilepath: String): Boolean = {
    print("Parsing")
    val bufferedSource = Source.fromFile(inputFilepath)(
      Codec.string2codec("UTF-16"))
    val fhx = bufferedSource.getLines().mkString("\n")
    print(".")
    val isSuccessful = parse(root, fhx) match {
      case Success(result, _): Success[Node] =>
        val file = new File(outputFilepath)
        val fw = new FileWriter(file, StandardCharsets.UTF_8)
        print(".")
        fw.write(result.write(LeftJustified))
        print(".")
        fw.close()
        true
      case o: Object =>
        println(s"FAILURE: ${o.toString}")
        false
    }
    bufferedSource.close()
    isSuccessful
  }

  def checkCorrectness(inputFilepath: String, exemplarFilepath: String) = {
    given Codec = Codec.string2codec("UTF-8")
    val bufferedSource = Source.fromFile(inputFilepath)
    val sourceLines = bufferedSource.getLines().mkString("\n")
    val bufferedCorrect = Source.fromFile(exemplarFilepath)
    val correctLines = bufferedCorrect.getLines().mkString("\n")
    bufferedSource.close()
    bufferedCorrect.close()
    sourceLines == correctLines
  }
}
