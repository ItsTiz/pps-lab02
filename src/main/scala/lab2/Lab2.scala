package lab2

object Lab2 extends App:

  //Task 1 - svolto da solo
  println("Hello, Scala!")

  val value = 5.8
  val value2 = 6.3
  val pendingMult = curriedMult(value2)
  val pendingDiv = divideCurried(value)

  val genericOperation: (Double, Double, (Double, Double) => Double) => Double =
    (a, b, f) => f(a, b)

  def mult(x: Double, y: Double): Double = x * y

  def curriedMult(x: Double)(y: Double): Double = x * y

  def divide(x: Double, y: Double): Double = x / y

  def divideCurried(x: Double)(y: Double): Double = x / y

  println(s"mult: ${mult(value, value2)}")
  println(s"curriedMult - pending call: $pendingMult")
  println(s"curriedMult: ${pendingMult(value2)}")
  println(s"curriedMult: ${curriedMult(value)(value2)}")
  println(s"genericOperation: ${genericOperation(value, value2, _ * _)}")
  println(s"genericOperation: ${genericOperation(value, value2, _ + _)}")
  println(s"genericOperation: ${genericOperation(value, value2, _ - _)}")
  println(s"divide: ${divide(value, value2)}")
  println(s"divide: ${divide(value, 0)}")
  println(s"divide: ${divide(value, 1)}")
  println(s"divideCurried: ${divideCurried(value)(value2)}")
  println(s"divideCurried - pending call: $pendingDiv")
  println(s"divideCurried: ${pendingDiv(value2)}")

  // Task 2 - svolto da solo
  val negativeInt = -7
  val positiveInt = 14

  val signFunction: Int => String =
    case n if n >= 0 => "positive"
    case _ => "negative"

  def signMethod(n: Int): String = n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println(s"signFunction: ${signFunction(negativeInt)}")
  println(s"signFunction: ${signFunction(0)}")
  println(s"signFunction: ${signFunction(positiveInt)}")
  println(s"signMethod: ${signMethod(negativeInt)}")
  println(s"signMethod: ${signMethod(positiveInt)}")
  println(s"signMethod: ${signMethod(0)}")

  val empty: String => Boolean = _ == ""
  val testString = "foo"

  val negS: (String => Boolean) => String => Boolean =
    p => s => !p(s)

  def negM(p: String => Boolean): String => Boolean =
    s => !p(s)

  val notEmpty = negS(empty)
  val notEmptyM = negM(empty)

  println(s"negS: ${notEmpty(testString)}")
  println(s"negS: ${notEmpty("")}")
  println(s"negS: ${notEmpty(testString) && !notEmpty("")}")
  println(s"negM: ${notEmptyM(testString)}")
  println(s"negM: ${notEmptyM("")}")
  println(s"negM: ${notEmptyM(testString) && !notEmptyM("")}")

  val evenInt = 2
  val positiveDouble = 42.42
  val even: Int => Boolean = _ % 2 == 0
  val positive: Double => Boolean = _ > 0

  def neg[X](p: X => Boolean): X => Boolean =
    x => !p(x)

  val notEven = neg(even)
  val notPositive = neg(positive)

  println(s"neg[X]: ${notEven(evenInt)}")
  println(s"neg[X]: ${notEven(evenInt + 1)}")
  println(s"neg[X]: ${notPositive(positiveDouble)}")
  println(s"neg[X]: ${notPositive(-positiveDouble)}")

  val x = 4
  val y = 5
  val z = y
  val invalidZ = z + 1

  val threeValPredicateFun: (Int, Int, Int) => Boolean =
    (x, y, z) => x <= y && y == z

  val threeValPredicateFunCurried: Int => Int => Int => Boolean =
    x => y => z => x <= y && y == z

  def threeValPredicateMethod(x: Int, y: Int, z: Int): Boolean =
    x <= y && y == z

  def threeValPredicateMethodCurried(x: Int)(y: Int)(z: Int): Boolean =
    x <= y && y == z

  println(s"threeValPredicateFun: ${threeValPredicateFun(x, y, z)}")
  println(s"threeValPredicateFun: ${threeValPredicateFun(x, y, invalidZ)}")
  println(s"threeValPredicateFunCurried: ${threeValPredicateFunCurried(x)(y)(z)}")
  println(s"threeValPredicateFunCurried: ${threeValPredicateFunCurried(x)(y)(invalidZ)}")
  println(s"threeValPredicateMethod: ${threeValPredicateMethod(x, y, z)}")
  println(s"threeValPredicateMethod: ${threeValPredicateMethod(x, y, invalidZ)}")
  println(s"threeValPredicateMethodCurried ${threeValPredicateMethodCurried(x)(y)(z)}")
  println(s"threeValPredicateMethodCurried: ${threeValPredicateMethodCurried(x)(y)(invalidZ)}")

  val twice: Int => Int = _ * 2
  val precedent: Int => Int = _ - 1

  def compose(f: Int => Int, g: Int => Int): Int => Int =
    i => f(g(i))

  println(s"compose: ${compose(precedent, twice)(x)}")
  println(s"compose: ${compose(precedent, twice)(y)}")
  println(s"compose: ${compose(precedent, twice)(z + 1)}")

  val half: Int => Double = _ / 2
  val odd: Double => Boolean = _ % 2 == 1

  def composeGeneric[A, B, C](f: B => C, g: A => B): A => C =
    i => f(g(i))

  println(s"composeGeneric: ${composeGeneric(precedent, twice)(x - 2)}")
  println(s"composeGeneric: ${composeGeneric(odd, half)(x + 2)}")

  val stringify: Int => String = _.toString
  val addExclamationMark: String => String = _ + "!"

  def composeThree[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D =
    i => f(g(h(i)))

  println(s"composeThree: ${composeThree(half, precedent, twice)(x)}")
  println(s"composeThree: ${composeThree(addExclamationMark, stringify, twice)(3)}")

  def composeThreeByCompose[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D =
    i => composeGeneric(f, composeGeneric(g, h))(i)

  println(s"composeThreeByCompose: ${composeThreeByCompose(half, precedent, twice)(x)}")
  println(s"composeThreeByCompose: ${composeThreeByCompose(addExclamationMark, stringify, twice)(3)}")

  // Task 3 - svolto da solo
  def power(base: Double, exponent: Int): Double = exponent match
    case 0 => 1
    case _ => base * power(base, exponent - 1)

  println(s"power: ${power(x, y)}")
  println(s"power: ${power(y, z)}")

  def powerTailRec(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _power(base: Double, exponent: Int, acc: Double): Double = exponent match
      case 0 => acc
      case _ => _power(base, exponent - 1, base * acc)
    _power(base, exponent, 1)

  println(s"power: ${power(x - 1, y - 1)}")
  println(s"power: ${power(y + 1, z - 2)}")

  val toBeReversedSingle = 7
  val toBeReversed = 12345

  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _reverse(rem: Int, curr: Int): Int = rem match
      case rem if (rem % 10) == rem => curr * 10 + rem
      case _ => _reverse(rem / 10, curr * 10 + rem % 10)
    _reverse(n, 0)

  println(s"reverseNumber: ${reverseNumber(toBeReversedSingle)}")
  println(s"reverseNumber: ${reverseNumber(toBeReversed)}")
  println(s"reverseNumber: ${reverseNumber(reverseNumber(toBeReversed))}")

  //Task 4 - svolto da solo
  enum Expr:
    case Literal(value: Int)
    case Add(left: Expr, right: Expr)
    case Multiply(left: Expr, right: Expr)

  object Expr:
    def evaluate(expr: Expr): Int = expr match
      case Literal(v) => v
      case Add(l, r) => evaluate(l) + evaluate(r)
      case Multiply(l, r) => evaluate(l) * evaluate(r)

    def show(expr: Expr): String = expr match
      case Literal(v) => v.toString
      case Add(l, r) => "(" + show(l) + " + " + show(r) + ")"
      case Multiply(l, r) => "(" + show(l) + " * " + show(r) + ")"

  //Task 5 - svolto da solo
  //vedi test/task5/OptionalTest e main/scala/task5/Optional