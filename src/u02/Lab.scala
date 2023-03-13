package u02

import scala.annotation.tailrec

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


  println("\nRecursion")
  @tailrec
  def gcd(a: Int, b: Int): Int = (a, b) match
    case (a, b) if a > b && b != 0 => gcd(b, a % b)
    case _ => a

  println(gcd(12, 8))
  println(gcd(14, 7))


  println("\nSum Type, Product Type, Modules")
  enum Shape:
    case Rectangle(weight: Double, height: Double)
    case Circle(radius: Double)
    case Square(l: Double)

  object Shape:
    def perimeter(shape: Shape): Double = shape match
      case Rectangle(w, h) => (w + h) * 2
      case Circle(r) => 2 * 3.14 * r
      case Square(l) => l * l

    def contains(shape: Shape, point: (Double, Double)): Boolean = (shape, point) match
      case (Rectangle(w, h), (x, y)) => x < w && y < h
      case (Circle(r), (x, y)) => x < r && y < r
      case (Square(l), (x, y)) => x < l && y < l

