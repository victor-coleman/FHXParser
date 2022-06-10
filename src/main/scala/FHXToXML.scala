import java.io.File
import java.nio.file.{Path, Paths}
import java.util.Date
import scala.io.{Codec, Source}
import scala.util.matching.Regex
import scala.util.parsing.combinator.*
import scala.xml.{Attribute, MetaData, Node, Null, Text, TopScope, XML}

def format(left: String, delimiter: String, right: String)
          (arr: List[String]): String = s"$left${arr.mkString(delimiter)}$right"

val formatWord: String => String = identity
val formatArr: List[String] => String = format("[", ",", "]")
val formatBitmask: List[String] => String = format("{", " ", "}")
val formatVector: String => String = identity
val formatStr: List[String] => String = format("", "", "")

class FHXToXML extends RegexParsers {
  def comment: Parser[String] =
    "/*" ~ ("""((\*[^/])|[^*])""".r | comment).* ~ "*/" ^^^ ""

  def tagName: Parser[String] = """[a-zA-Z_:][a-zA-Z\d._\-:]*""".r
  def index: Parser[String] = """\d+\.\.\d+""".r | """\d+""".r
  def byte: Parser[String] = """[a-fA-F\d]{2}""".r

  def stringSafe: Parser[String] =
    """\s*[^"]+""".r
    | ("""\s*""".r <~ """"\n\t"""".r ^^ (_ + "\n\t"))
    | ("""\s*""".r <~  "\"\"" ^^ (_ + "\""))

  def attributeContent: Parser[xml.Text] = (
    ("""[\w\-.+]+""".r                    ^^ formatWord)
    | ("\"" ~> stringSafe.* <~ "\""       ^^ formatStr)
    | ("[" ~> repsep(index, ",") <~ "]"   ^^ formatArr)
    | ("""\([\d|/\-.+e]*\)""".r           ^^ formatVector)
    | ("{" ~> byte.+ <~ "}"               ^^ formatBitmask)
      ) <~ comment.* ^^ (Text(_))

  def attribute: Parser[xml.Elem] = (tagName <~ "=") ~ attributeContent ^^ {
    case n ~ c => xml.Elem(null, n, Null, TopScope, false, c)  }

  def internalElement: Parser[xml.Elem] =
    (tagName <~ "=") ~ elementBody <~ comment.* ^^ { case n ~ b =>
      xml.Elem(null, n, Null, TopScope, false, b*)
    }

  def elementBody: Parser[List[xml.Elem]] =
    ("{" ~> (attribute | internalElement | element).* <~ "}" <~ comment.*)
    | ("{" ~ comment.* ~ "}" <~ comment.* ^^^ List[xml.Elem]())

  def element: Parser[xml.Elem] =
    tagName ~ attribute.* ~ elementBody <~ comment.* ^^ {
    case n ~ as ~ b => xml.Elem(null, n, Null, TopScope, false, as ::: b *)
  }

  def root: Parser[Node] = comment.* ~> element.+ ^^ { case elems =>
    xml.Elem(null, "FHX_ROOT", Null, TopScope, false, elems*)
  }
}

object FHXToXML extends FHXToXML {
  val rootPath: String = Paths.get(System.getProperty("user.dir")).toString
  def main(args: Array[String]): Unit = {
    val inputFilename = "POWER.fhx"
    val outputFilename = inputFilename.replace(".fhx", ".xml")

    val inputFilepath =
      Paths.get("src", "main", "in", inputFilename).toString
    val outputFilepath =
      Paths.get("src", "main", "out", outputFilename).toString

    val startTime = Date().getTime
    convertFile(inputFilepath, outputFilepath)
    val endTime = Date().getTime
    println(s"Time Elapsed: ${(endTime - startTime) / 1000.0} s")
  }

  def convertFile(inputFilepath: String, outputFilepath: String): Unit = {
    println("Parsing started")
    val bufferedSource = Source.fromFile(inputFilepath)(
      Codec.string2codec("UTF-16"))
    val fhx = bufferedSource.getLines().mkString("\n")

    parse(root, fhx) match {
      case Success(result, _): Success[Node] =>
        println("Parsing complete!")
        val printer = new scala.xml.PrettyPrinter(80, 4)
        val formatted = XML.loadString(printer.format(result))
        XML.save(outputFilepath, formatted, "UTF-8")
        println("XML file created!")
      case o: Object => println(s"FAILURE: ${o.toString}")
    }
    bufferedSource.close()
  }
}
