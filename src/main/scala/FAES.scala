object FAES extends Language {

  val name = "FAES"

  sealed trait Expr
  sealed trait Value extends Expr
  case class Num(n: Int) extends Expr with Value
  case class Id(x: String) extends Expr
  case class Fun(x: String, b: Expr) extends Expr with Value
  case class App(f: Expr, a: Expr) extends Expr

  lazy val e: Parser[Expr] =
    e1 ~ rep(wrapR(e)) ^^ { case f ~ as => as.foldLeft(f)(App) }

  lazy val e1: Parser[Expr] =
    n ^^ Num |
    x ^^ Id |
    wrapC((x <~ "=>") ~ e) ^^ app(Fun)

  def subst(expr: Expr, x: String, e: Expr): Expr = expr match {
    case Num(n) => expr
    case Id(y) => if (y == x) e else expr
    case Fun(y, b) =>
      // Fun(y, if (y == x) b else subst(b, x, e))
      if (y == x)
        Fun(y, b)
      else {
        val ny = fresh(binding(b) ++ free(b) ++ free(e) ++ Set(x))
        Fun(ny, subst(subst(b, y, Id(ny)), x, e))
      }
    case App(f, a) => App(subst(f, x, e), subst(a, x, e))
  }

  def interp(e: Expr): Value = e match {
    case Num(n) => Num(n)
    case Id(x) => error("free identifier")
    case Fun(x, b) => Fun(x, b)
    case App(f, a) => interp(f) match {
      case Fun(x, b) => interp(subst(b, x, interp(a)))
      case _ => error("not a function")
    }
  }

  def binding(e: Expr): Set[String] = e match {
    case Num(n) => Set()
    case Id(x) => Set()
    case Fun(x, b) => binding(b) + x
    case App(f, a) => binding(f) ++ binding(a)
  }

  def bound(e: Expr): Set[String] = {
    def aux(e: Expr, env: Set[String]): Set[String] = e match {
      case Num(n) => Set()
      case Id(x) => if (env(x)) Set(x) else Set()
      case Fun(x, b) => aux(b, env + x)
      case App(f, a) => aux(f, env) ++ aux(a, env)
    }
    aux(e, Set())
  }

  def free(e: Expr): Set[String] = e match {
    case Num(n) => Set()
    case Id(x) => Set(x)
    case Fun(x, b) => free(b) - x
    case App(f, a) => free(f) ++ free(a)
  }

  def fresh(xs: Set[String]): String =
    (('w' to 'z').toSet.map((c: Char) => c.toString) -- xs).head
}
