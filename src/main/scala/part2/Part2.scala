package part2

object Part2 extends App:
  val signFunction: Int => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  def signMethod(n: Int): String = n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println(signFunction(-7))
  println(signFunction(3))
  println(signFunction(0))

  println(signMethod(-7))
  println(signMethod(3))
  println(signMethod(0))

  val neg: (String => Boolean) => String => Boolean =
    p => s => !p(s)

  def negM(predicate: String => Boolean): String => Boolean =
    s => !predicate(s)

  val empty: String => Boolean = _ == ""
  val notEmpty = negM(empty)
  println(notEmpty("foo"))
  println(notEmpty(""))
  println(notEmpty("foo") && !notEmpty(""))

  def neg[X](predicate: X => Boolean): X => Boolean =
    x => !predicate(x)

  val threeValPredicateFun: (Int, Int, Int) => Boolean =
    (x, y, z) => x <= y && y == z

  val threeValPredicateFunCurried: Int => Int => Int => Boolean =
    x => y => z => x <= y && y == z

  def threeValPredicateMethod(x: Int, y: Int, z: Int): Boolean =
    x <= y && y == z

  def threeValPredicateMethodCurried(x: Int)(y: Int)(z: Int): Boolean =
    x <= y && y == z