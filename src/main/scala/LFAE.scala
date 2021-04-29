object LFAE extends Language {

  val name = "LFAE"

  sealed trait Expr
  case class Num(n: Int) extends Expr
  case class Add(l: Expr, r: Expr) extends Expr
  case class Sub(l: Expr, r: Expr) extends Expr
  case class Id(x: String) extends Expr
  case class Val(x: String, e: Expr, b: Expr) extends Expr
  case class Fun(x: String, b: Expr) extends Expr
  case class App(f: Expr, a: Expr) extends Expr
  case class If0(c: Expr, t: Expr, f: Expr) extends Expr
  case class Pair(f: Expr, s: Expr) extends Expr
  case class Fst(p: Expr) extends Expr
  case class Snd(p: Expr) extends Expr

  sealed trait Value
  case class NumV(n: Int) extends Value
  case class CloV(p: String, b: Expr, e: Env) extends Value
  case class PairV(f: Value, s: Value) extends Value
  case class ExprV(e: Expr, env: Env) extends Value

  type Env = Map[String, Value]

  lazy val e: Parser[Expr] =
    e1 ~ rep(
      wrapR(e) ^^ FApp |
      "." ~ "_1" ^^^ FFst |
      "." ~ "_2" ^^^ FSnd
    ) ^^ {
      case f ~ as => as.foldLeft(f){
        case (f, FApp(a)) => App(f, a)
        case (p, FFst) => Fst(p)
        case (p, FSnd) => Snd(p)
      }
    }

  lazy val e1: Parser[Expr] =
    n ^^ Num |
    wrapR((e <~ "+") ~ e) ^^ app(Add) |
    wrapR((e <~ "-") ~ e) ^^ app(Sub) |
    wrapR((e <~ ",") ~ e) ^^ app(Pair) |
    ("if0" ~> wrapR(e)) ~ (wrapC(e) <~ "else") ~ wrapC(e) ^^ app(If0) |
    x ^^ Id |
    wrapC(("val" ~> x <~ "=") ~ (e <~ ";") ~ e) ^^ app(Val) |
    wrapC((x <~ "=>") ~ e) ^^ app(Fun)

  sealed trait F
  case class FApp(a: Expr) extends F
  case object FFst extends F
  case object FSnd extends F

  def strict(v: Value): Value = v match {
    case ExprV(e, env) =>
      strict(interp(e, env))
    case _ => v
  }

  def interp(e: Expr, env: Env): Value = e match {
    case Num(n) => NumV(n)
    case Add(l, r) =>
      val NumV(n) = strict(interp(l, env))
      val NumV(m) = strict(interp(r, env))
      NumV(n + m)
    case Sub(l, r) =>
      val NumV(n) = strict(interp(l, env))
      val NumV(m) = strict(interp(r, env))
      NumV(n - m)
    case Id(x) => env(x)
    case Val(x, e, b) => interp(b, env + (x -> ExprV(e, env)))
    case Fun(x, b) => CloV(x, b, env)
    case App(f, a) =>
      val CloV(x, b, fEnv) = strict(interp(f, env))
      interp(b, fEnv + (x -> ExprV(a, env)))
    case If0(c, t, f) =>
      interp(if (strict(interp(c, env)) == NumV(0)) t else f, env)
    case Pair(f, s) =>
      PairV(ExprV(f, env), ExprV(s, env))
    case Fst(p) =>
      strict(interp(p, env)) match {
        case PairV(v, _) => v
        case _ => error("not a pair")
      }
    case Snd(p) =>
      strict(interp(p, env)) match {
        case PairV(_, v) => v
        case _ => error("not a pair")
      }
  }

  def interp(e: Expr): Value = interp(e, Map())
}
