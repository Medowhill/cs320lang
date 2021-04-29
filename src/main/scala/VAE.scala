object VAE extends Language {

  val name = "VAE"

  sealed trait Expr
  case class Num(n: Int) extends Expr
  case class Add(l: Expr, r: Expr) extends Expr
  case class Sub(l: Expr, r: Expr) extends Expr
  case class Id(x: String) extends Expr
  case class Val(x: String, e: Expr, b: Expr) extends Expr

  type Value = Int
  type Env = Map[String, Value]

  lazy val e: Parser[Expr] =
    n ^^ Num |
    wrapR((e <~ "+") ~ e) ^^ app(Add) |
    wrapR((e <~ "-") ~ e) ^^ app(Sub) |
    x ^^ Id |
    wrapC(("val" ~> x <~ "=") ~ (e <~ ";") ~ e) ^^ app(Val)

  def interp(e: Expr, env: Env): Value = e match {
    case Num(n) => n
    case Add(l, r) => interp(l, env) + interp(r, env)
    case Sub(l, r) => interp(l, env) - interp(r, env)
    case Id(x) => env(x)
    case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
  }

  def interp(e: Expr): Value = interp(e, Map())
}
