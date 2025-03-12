package lab2

object Lab2 extends App:

  // Task 2 - svolto da solo
  val signFunction: Int => String = _ match
    case n if n >= 0 => "positive"
    case _ => "negative"

  def signMethod(n: Int): String = n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  val negativeInt = -7
  val positiveInt = 14

  println(s"signFunction: ${signFunction(negativeInt)}")
  println(s"signFunction: ${signFunction(0)}")
  println(s"signFunction: ${signFunction(positiveInt)}")
  println(s"signMethod: ${signMethod(negativeInt)}")
  println(s"signMethod: ${signMethod(positiveInt)}")
  println(s"signMethod: ${signMethod(0)}")

  val negS: (String => Boolean) => String => Boolean =
    p => s => !p(s)

  def negM(predicate: String => Boolean): String => Boolean =
    s => !predicate(s)

  val empty: String => Boolean = _ == ""
  val notEmpty = negS(empty)
  val notEmptyM = negM(empty)
  val testString = "foo"

  println(s"negS: ${notEmpty(testString)}")
  println(s"negS: ${notEmpty("")}")
  println(s"negS: ${notEmpty(testString) && !notEmpty("")}")
  println(s"negM: ${notEmptyM(testString)}")
  println(s"negM: ${notEmptyM("")}")
  println(s"negM: ${notEmptyM(testString) && !notEmptyM("")}")

  def neg[X](predicate: X => Boolean): X => Boolean =
    x => !predicate(x)

  val even: Int => Boolean = _ % 2 == 0
  val positive: Double => Boolean = _ > 0
  val evenInt = 2
  val positiveDouble = 42.42
  val notEven = neg(even)
  val notPositive = neg(positive)

  println(s"neg[X]: ${notEven(evenInt)}")
  println(s"neg[X]: ${notEven(evenInt+1)}")
  println(s"neg[X]: ${notPositive(positiveDouble)}")
  println(s"neg[X]: ${notPositive(-positiveDouble)}")

  val threeValPredicateFun: (Int, Int, Int) => Boolean =
    (x, y, z) => x <= y && y == z

  val threeValPredicateFunCurried: Int => Int => Int => Boolean =
    x => y => z => x <= y && y == z

  def threeValPredicateMethod(x: Int, y: Int, z: Int): Boolean =
    x <= y && y == z

  def threeValPredicateMethodCurried(x: Int)(y: Int)(z: Int): Boolean =
    x <= y && y == z

  val x = 4
  val y = 5
  val z = y
  val invalidZ = z + 1

  println(s"threeValPredicateFun: ${threeValPredicateFun(x,y,z)}")
  println(s"threeValPredicateFun: ${threeValPredicateFun(x,y,invalidZ)}")
  println(s"threeValPredicateFunCurried: ${threeValPredicateFunCurried(x)(y)(z)}")
  println(s"threeValPredicateFunCurried: ${threeValPredicateFunCurried(x)(y)(invalidZ)}")
  println(s"threeValPredicateMethod: ${threeValPredicateMethod(x,y,z)}")
  println(s"threeValPredicateMethod: ${threeValPredicateMethod(x,y,invalidZ)}")
  println(s"threeValPredicateMethodCurried ${threeValPredicateMethodCurried(x)(y)(z)}")
  println(s"threeValPredicateMethodCurried: ${threeValPredicateMethodCurried(x)(y)(invalidZ)}")

  def compose(f: Int => Int, g: Int => Int): Int => Int =
    i => f(g(i))

  val twice: Int => Int = _ * 2;
  val precedent: Int => Int = _ - 1;

  println(s"compose: ${compose(precedent, twice)(x)}")
  println(s"compose: ${compose(precedent, twice)(y)}")
  println(s"compose: ${compose(precedent, twice)(z+1)}")

  def composeGeneric[A,B,C](f: B => C, g: A => B): A => C =
    i => f(g(i))

  val half: Int => Double = _ / 2;
  val odd: Double => Boolean = _ % 2 == 1;

  println(s"composeGeneric: ${composeGeneric(precedent, twice)(x-2)}")
  println(s"composeGeneric: ${composeGeneric(odd, half)(x+2)}")

  def composeThree[A,B,C,D](f: C => D, g: B => C, h: A => B): A => D =
    i => f(g(h(i)))

  val stringify: Int => String = _.toString
  val addExclamationMark: String => String = _ + "!"

  println(s"composeThree: ${composeThree(half, precedent, twice)(x)}")
  println(s"composeThree: ${composeThree(addExclamationMark, stringify, twice)(3)}")

  def composeThreeByCompose[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D =
    i => composeGeneric(f, composeGeneric(g, h))(i)

  println(s"composeThreeByCompose: ${composeThreeByCompose(half, precedent, twice)(x)}")
  println(s"composeThreeByCompose: ${composeThreeByCompose(addExclamationMark, stringify, twice)(3)}")

  // Task 3 - svolto da solo
  def power(base: Double, exponent: Int): Double = exponent match
    case 0 => 1
    case _ => base * power(base, exponent-1)

  println(s"power: ${power(x, y)}")
  println(s"power: ${power(y, z)}")

  def powerTailRec(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _power(base: Double, exponent: Int, acc: Double): Double = exponent match
      case 0 => acc
      case _ => _power(base, exponent - 1, base * acc)
    _power(base, exponent, 1)

  println(s"power: ${power(x-1, y-1)}")
  println(s"power: ${power(y+1, z-2)}")

  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _reverse(remainder: Int, currentReverse: Int): Int = remainder match
      case remainder if (remainder % 10) == remainder => currentReverse * 10 + remainder
      case _ => _reverse(remainder / 10, currentReverse * 10 + remainder % 10)
    _reverse(n, 0)

  val toBeReversedSingle = 7
  val toBeReversed = 12345

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


