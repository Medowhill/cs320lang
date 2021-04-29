object RFAE extends Language {

  val name = "RFAE"

  sealed trait Expr
  case class Num(n: Int) extends Expr
  case class Add(l: Expr, r: Expr) extends Expr
  case class Sub(l: Expr, r: Expr) extends Expr
  case class Id(x: String) extends Expr
  case class Val(x: String, e: Expr, b: Expr) extends Expr
  case class Fun(x: String, b: Expr) extends Expr
  case class App(f: Expr, a: Expr) extends Expr
  case class If0(c: Expr, t: Expr, f: Expr) extends Expr
  case class Rec(f: String, x: String, b: Expr, e: Expr) extends Expr

  sealed trait Value
  case class NumV(n: Int) extends Value
  case class CloV(p: String, b: Expr, var e: Env) extends Value

  type Env = Map[String, Value]

  lazy val e: Parser[Expr] =
    e1 ~ rep(wrapR(e)) ^^ { case f ~ as => as.foldLeft(f)(App) }

  lazy val e1: Parser[Expr] =
    n ^^ Num |
    wrapR((e <~ "+") ~ e) ^^ app(Add) |
    wrapR((e <~ "-") ~ e) ^^ app(Sub) |
    wrapC(("val" ~> x <~ "=") ~ (e <~ ";") ~ e) ^^ app(Val) |
    wrapC((x <~ "=>") ~ e) ^^ app(Fun) |
    ("if0" ~> wrapR(e)) ~ (wrapC(e) <~ "else") ~ wrapC(e) ^^ app(If0) |
    x ^^ Id |
    wrapC(("def" ~> x) ~ wrapR(x) ~ ("=" ~> e <~ ";") ~ e) ^^ app(Rec)

  def interp(e: Expr, env: Env): Value = e match {
    case Num(n) => NumV(n)
    case Add(l, r) =>
      val NumV(n) = interp(l, env)
      val NumV(m) = interp(r, env)
      NumV(n + m)
    case Sub(l, r) =>
      val NumV(n) = interp(l, env)
      val NumV(m) = interp(r, env)
      NumV(n - m)
    case Id(x) => env(x)
    case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
    case Fun(x, b) => CloV(x, b, env)
    case App(f, a) =>
      val CloV(x, b, fenv) = interp(f, env)
      interp(b, fenv + (x -> interp(a, env)))
    case If0(c, t, f) =>
      interp(if (interp(c, env) == NumV(0)) t else f, env)
    case Rec(f, x, b, e) =>
      val c = CloV(x, b, env)
      c.e += f -> c
      interp(e, c.e)
  }

  def interp(e: Expr): Value = interp(e, Map())
}
