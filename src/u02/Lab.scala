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

  //Task 4, svolto da solo
  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z
  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z
  def p4(x: Int, y: Int, z: Int): Boolean = x <= y && y == z

  //Task 5, svolto da solo
  def compose(f: Int => Int, g:Int => Int): Int => Int = x => f(g(x))
  def composeGen[A, B, C](f: B => C, g: A => B): A => C =  x => f(g(x))

  //Task 6, svolto da solo
  @tailrec
  def gcd(a: Int, b: Int): Int = a > b && b != 0 match
    case true => gcd(b, a % b)
    case _ => a

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
