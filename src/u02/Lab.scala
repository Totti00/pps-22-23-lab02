package u02

import scala.annotation.tailrec

object Lab extends App:
  //Task 1
  println("Hello, Scala!")

  //Task 3, svolto da solo
  val positiveFun: Int => String = _ > 0 match
    case true => "positive"
    case _ => "negative"

  def positiveMeth(n: Int) : String = n > 0 match
    case true => "positive"
    case _ => "negative"

  val negativeVal: (String => Boolean) => String => Boolean = f => !f(_)
  def negativeDec(pred: String => Boolean): String => Boolean = !pred(_)
  def negWithGenerics[X](pred: X => Boolean): X => Boolean = !pred(_)

  println(positiveFun(5))
  println(positiveFun(-1))
  println(positiveMeth(5))
  println(positiveMeth(-1))

  val empty: String => Boolean = _ == ""
  val notEmpty = negativeVal(empty);
  println(notEmpty("foo"))
  println(notEmpty(""))
  println(notEmpty("foo") && !notEmpty(""))

  val moreThanTen: Int => Boolean = _ > 10
  val lessThanTen = negWithGenerics(moreThanTen)
  println(lessThanTen(5))


  //Task 4, svolto da solo
  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z
  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z
  def p4(x: Int, y: Int, z: Int): Boolean = x <= y && y == z

  println(p1(5)(6)(7))
  println(p1(5)(6)(6))
  println(p2(5, 6, 7))
  println(p2(5, 6, 6))
  println(p3(5)(6)(7))
  println(p3(5)(6)(6))
  println(p4(5, 6, 7))
  println(p4(5, 6, 6))

  //Task 5, svolto da solo
  def compose(f: Int => Int, g:Int => Int): Int => Int = x => f(g(x))
  def composeGen[A, B, C](f: B => C, g: A => B): A => C =  x => f(g(x))

  println(compose(_ - 1, _ * 2)(5))
  println(composeGen( x => s"$x Hello World!" ,y => 5.7)(12.0))

  //Task 6, svolto da solo
  @tailrec
  def gcd(a: Int, b: Int): Int = a > b && b != 0 match
    case true => gcd(b, a % b)
    case _ => a

  println(gcd(12, 8))
  println(gcd(14, 7))

  //Task 7, svolto da solo
  enum Shape:
    case Rectangle(weight: Double, height: Double)
    case Circle(radius: Double)
    case Square(l: Double)

  object Shape:
    def perimeter(shape: Shape): Double = shape match
      case Rectangle(w, h) => (w + h) * 2
      case Circle(r) => 2 * Math.PI * r
      case Square(l) => l * l

    def contains(shape: Shape, point: (Double, Double)): Boolean = (shape, point) match
      case (Rectangle(w, h), (x, y)) => x <= w && y <= h
      case (Circle(r), (x, y)) => Math.sqrt(Math.pow(x-0, 2) + Math.pow(y-0,2)) <= r
      case (Square(l), (x, y)) => x <= l && y <= l

  import Shape.*

  val rect: Shape = Rectangle(2, 2)
  val cir: Shape = Circle(2)
  val sq: Shape = Square(2)
  println(perimeter(rect))
  println(perimeter(cir))
  println(perimeter(sq))
  println(contains(rect, (2, 2)))
  println(contains(cir, (1.32, 1.5)))
  println(contains(sq, (4, 4)))

  //Task 8, svolto da solo
  enum Option[A]:
    case Some(a: A)
    case None()

  object Option:
    def isEmpty[A](opt: Option[A]): Boolean = opt match //se None allora è true, altrimenti false
      case None() => true
      case _ => false

    def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match //se opt è = Some(a) allora ritorna a, altrimenti orElse
      case Some(a) => a
      case _ => orElse

    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match
      case Some(a) => f(a)
      case _ => None()

    def filter[A](opt: Option[A])(f: A => Boolean): Option[A] = opt match
      case Some(a) => f(a) match
        case true => Some(a)
        case _ => None()
      case _ => None()

    def map[A](opt: Option[A])(f: A => Boolean): Option[Boolean] = opt match
      case Some(a) => Some(f(a))
      case _ => None()

    def fold[A](opt:Option[A])(x: Int)(f: A => Int): Int = opt match
      case Some(a) => f(a)
      case _ => x

  import Option.*

  val s5: Option[Int] = Some(5)
  val s6: Option[Int] = None[Int]()
  println(filter(s5)(_ > 2))
  println(filter(s5)(_ > 8))
  println(filter(s6)(_ > 2))
  println(map(s5)(_ > 2))
  println(map(s5)(_ > 8))
  println(map(s6)(_ > 2))
  println(fold(s5)(1)(_ + 1))
  println(fold(s6)(1)(_ + 1))
