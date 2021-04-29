object AE extends Language {

  val name = "AE"

  sealed trait Expr
  case class Num(n: Int) extends Expr
  case class Add(l: Expr, r: Expr) extends Expr
  case class Sub(l: Expr, r: Expr) extends Expr

  type Value = Int

  lazy val e: Parser[Expr] =
    n ^^ Num |
    wrapR((e <~ "+") ~ e) ^^ app(Add) |
    wrapR((e <~ "-") ~ e) ^^ app(Sub)

  def interp(e: Expr): Value = e match {
    case Num(n) => n
    case Add(l, r) => interp(l) + interp(r)
    case Sub(l, r) => interp(l) - interp(r)
  }
}
