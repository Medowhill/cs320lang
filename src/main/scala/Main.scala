import scala.util.parsing.combinator._

object Main {
  val langs = List(
    AE,
    VAE,
    FAE,
    FAES,
    RFAE,
    BFAE,
    MFAE.CBV,
    MFAE.CBR,
    LFAE,
  ).map(l => l.name -> l).toMap

  def read(): String = {
    def l: LazyList[String] = scala.Console.in.readLine() #:: l
    l.takeWhile(_.nonEmpty).mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    val name = args.head
    val expr = args.tail.headOption.getOrElse(read())
    println(langs(name).run(expr))
  }
}

trait Language extends RegexParsers {

  type Expr
  type Value

  def error(s: String) = scala.sys.error(s)

  def name: String

  lazy val n: Parser[Int] = "-?[0-9]+".r ^^ (_.toInt)
  lazy val x: Parser[String] = "[a-zA-Z][a-zA-Z0-9]*".r
  def wrapR[T](e: Parser[T]): Parser[T] = "(" ~> e <~ ")"
  def wrapC[T](e: Parser[T]): Parser[T] = "{" ~> e <~ "}"
  def wrapS[T](e: Parser[T]): Parser[T] = "[" ~> e <~ "]"

  def app[A, B, Z](f: (A, B) => Z): A ~ B => Z =
    { case a ~ b => f(a, b) }
  def app[A, B, C, Z](f: (A, B, C) => Z): A ~ B ~ C => Z =
    { case a ~ b ~ c => f(a, b, c) }
  def app[A, B, C, D, Z](f: (A, B, C, D) => Z): A ~ B ~ C ~ D => Z =
    { case a ~ b ~ c ~ d => f(a, b, c, d) }

  def e: Parser[Expr]

  def apply(str: String): Expr =
    parseAll(e, str).getOrElse(error(s"bad syntax: $str"))

  def interp(e: Expr): Value

  def run(s: String): String = interp(this(s)).toString
}
