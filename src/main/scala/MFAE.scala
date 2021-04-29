class MFAE(cbv: Boolean) extends Language {

  val name = s"MFAE-${if (cbv) "CBV" else "CBR"}"

  sealed trait Expr
  case class Num(n: Int) extends Expr
  case class Add(l: Expr, r: Expr) extends Expr
  case class Sub(l: Expr, r: Expr) extends Expr
  case class Id(x: String) extends Expr
  case class Val(x: String, e: Expr, b: Expr) extends Expr
  case class Fun(x: String, b: Expr) extends Expr
  case class App(f: Expr, a: Expr) extends Expr
  case class Seq(l: Expr, r: Expr) extends Expr
  case class Set(x: String, e: Expr) extends Expr

  sealed trait Value
  case class NumV(n: Int) extends Value
  case class CloV(p: String, b: Expr, e: Env) extends Value

  type Addr = Int
  type Env = Map[String, Addr]
  type Sto = Map[Addr, Value]

  lazy val e: Parser[Expr] =
    e1 ~ rep(wrapR(e)) ^^ {
      case f ~ as => as.foldLeft(f)(App)
    }

  lazy val e1: Parser[Expr] =
    n ^^ Num |
    wrapR((e <~ "+") ~ e) ^^ app(Add) |
    wrapR((e <~ "-") ~ e) ^^ app(Sub) |
    x ^^ Id |
    wrapC(("val" ~> x <~ "=") ~ (e <~ ";") ~ e) ^^ app(Val) |
    wrapC((x <~ "=>") ~ e) ^^ app(Fun) |
    wrapC((e <~ ";") ~ e) ^^ app(Seq) |
    wrapC((x <~ "=") ~ e) ^^ app(Set)

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
    case Id(x) => (sto(env(x)), sto)
    case Val(x, e, b) =>
      val (v, sto1) = interp(e, env, sto)
      val a = malloc(sto1)
      interp(b, env + (x -> a), sto1 + (a -> v))
    case Fun(x, b) => (CloV(x, b, env), sto)
    case App(f, a) =>
      if (cbv || !a.isInstanceOf[Id]) {
        val (CloV(x, b, fEnv), ls) = interp(f, env, sto)
        val (v, rs) = interp(a, env, ls)
        val addr = malloc(rs)
        interp(b, fEnv + (x -> addr), rs + (addr -> v))
      } else {
        val Id(x) = a
        val (CloV(y, b, fEnv), ls) = interp(f, env, sto)
        interp(b, fEnv + (y -> env(x)), ls)
      }
    case Seq(l, r) =>
      val (_, ls) = interp(l, env, sto)
      interp(r, env, ls)
    case Set(x, e) =>
      val (v, sto1) = interp(e, env, sto)
      (v, sto1 + (env(x) -> v))
  }

  def interp(e: Expr): Value = interp(e, Map(), Map())._1
}

object MFAE {
  val CBV = new MFAE(true)
  val CBR = new MFAE(false)
}
