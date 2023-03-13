package u02

object Lab extends App:
  println("Hello, Scala!")

  //Function
  val positive: Int => String = (n:Int) => n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  //method
  def pos(n: Int) : String = n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println(positive(5))
  println(positive(-1))

  println(pos(5))
  println(pos(-1))


  val negVal: (String => Boolean) => String => Boolean = f => !f(_)
  def negDec(pred: String => Boolean): String => Boolean = !pred(_)
  def negWithGenerics[X](pred: X => Boolean): X => Boolean = !pred(_)

  val empty: String => Boolean = _ == ""
  val notEmpty = negVal(empty);

  println(notEmpty("foo"))
  println(notEmpty(""))
  println(notEmpty("foo") && !notEmpty(""))

  val moreThanTen: Int => Boolean = _ > 10
  val lessThanTen = negWithGenerics(moreThanTen)
  println(lessThanTen(5))


  println("\nCurrying")
  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z

  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z

  def p3(x: Int)(y: Int)(z: Int): Boolean = (x, y, z) match
    case (x, y, z) if x <= y && y == z => true
    case _ => false

  def p4(x: Int, y: Int, z: Int): Boolean = x <= y && y == z

  println(p1(5)(6)(7))
  println(p1(5)(6)(6))
  println(p2(5, 6, 7))
  println(p2(5, 6, 6))
  println(p3(5)(6)(7))
  println(p3(5)(6)(6))
  println(p4(5, 6, 7))
  println(p4(5, 6, 6))


  println("\nFunctional compositions")

  def compose(f: Int => Int, g:Int => Int): Int => Int = x => f(g(x))

  println(compose(_ - 1, _ * 2)(5))

  def composeGen[A, B, C](f: B => C, g: A => B): A => C =  x => f(g(x))

  println(composeGen(x => s"$x Hello World!", y => 5)(12.0))