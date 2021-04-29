object BFAE extends Language {

  val name = "BFAE"

  sealed trait Expr
  case class Num(n: Int) extends Expr
  case class Add(l: Expr, r: Expr) extends Expr
  case class Sub(l: Expr, r: Expr) extends Expr
  case class Id(x: String) extends Expr
  case class Val(x: String, e: Expr, b: Expr) extends Expr
  case class Fun(x: String, b: Expr) extends Expr
  case class App(f: Expr, a: Expr) extends Expr
  case class NewBox(e: Expr) extends Expr
  case class SetBox(b: Expr, e: Expr) extends Expr
  case class OpenBox(b: Expr) extends Expr
  case class Seq(l: Expr, r: Expr) extends Expr

  sealed trait Value
  case class NumV(n: Int) extends Value
  case class CloV(p: String, b: Expr, e: Env) extends Value
  case class BoxV(a: Addr) extends Value

  type Env = Map[String, Value]
  type Addr = Int
  type Sto = Map[Addr, Value]

  lazy val e: Parser[Expr] =
    e1 ~ rep(
      wrapR(e) ^^ FApp |
      "." ~> "set" ~> wrapR(e) ^^ FSet |
      "." ~ "get" ^^^ FOpen
    ) ^^ {
      case f ~ as => as.foldLeft(f){
        case (f, FApp(a)) => App(f, a)
        case (b, FSet(e)) => SetBox(b, e)
        case (b, FOpen) => OpenBox(b)
      }
    }

  lazy val e1: Parser[Expr] =
    n ^^ Num |
    wrapR((e <~ "+") ~ e) ^^ app(Add) |
    wrapR((e <~ "-") ~ e) ^^ app(Sub) |
    "Box" ~> wrapR(e) ^^ NewBox |
    x ^^ Id |
    wrapC(("val" ~> x <~ "=") ~ (e <~ ";") ~ e) ^^ app(Val) |
    wrapC((x <~ "=>") ~ e) ^^ app(Fun) |
    wrapC((e <~ ";") ~ e) ^^ app(Seq)

  sealed trait F
  case class FApp(a: Expr) extends F
  case class FSet(e: Expr) extends F
  case object FOpen extends F

  def malloc(sto: Sto): Addr = sto.keys.maxOption.getOrElse(0) + 1

  def interp(e: Expr, env: Env, sto: Sto): (Value, Sto) = e match {
    case Num(n) => (NumV(n), sto)
    case Add(l, r) =>
      val (NumV(n), ls) = interp(l, env, sto)
      val (NumV(m), rs) = interp(r, env, ls)
      (NumV(n + m), rs)
    case Sub(l, r) =>
      val (NumV(n), ls) = interp(l, env, sto)
      val (NumV(m), rs) = interp(r, env, ls)
      (NumV(n - m), rs)
    case Id(x) => (env(x), sto)
    case Val(x, e, b) =>
      val (v, sto1) = interp(e, env, sto)
      interp(b, env + (x -> v), sto1)
    case Fun(x, b) => (CloV(x, b, env), sto)
    case App(f, a) =>
      val (CloV(x, b, fEnv), ls) = interp(f, env, sto)
      val (v, rs) = interp(a, env, ls)
      interp(b, fEnv + (x -> v), rs)
    case NewBox(e) =>
      val (v, s) = interp(e, env, sto)
      val a = malloc(s)
      (BoxV(a), s + (a -> v))
    case SetBox(b, e) =>
      val (BoxV(a), bs) = interp(b, env, sto)
      val (v, es) = interp(e, env, bs)
      (v, es + (a -> v))
    case OpenBox(e) =>
      val (BoxV(a), s) = interp(e, env, sto)
      (s(a), s)
    case Seq(l, r) =>
      val (_, ls) = interp(l, env, sto)
      interp(r, env, ls)
  }

  def interp(e: Expr): Value = interp(e, Map(), Map())._1
}
