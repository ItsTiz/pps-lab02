package task4

import org.junit.*
import lab2.Lab2.*
import org.junit.Assert.*

class ExprTest:

  val numericValue: Int = 5
  val numericValue2: Int = 7
  val numericValue3: Int = 13
  val numericValue4: Int = -5

  val literalValue: Expr = Expr.Literal(numericValue)
  val literalValue2: Expr = Expr.Literal(numericValue2)
  val literalValue3: Expr = Expr.Literal(numericValue3)
  val literalValue4: Expr = Expr.Literal(numericValue4)

  @Test def testSimpleAdd(): Unit =
    val sum: Expr = Expr.Add(literalValue, literalValue2)
    assertEquals(numericValue + numericValue2, Expr.evaluate(sum))

  @Test def testComposedAdd(): Unit =
    val leftSum: Expr = Expr.Add(literalValue, literalValue2)
    val rightSum: Expr = Expr.Add(literalValue3, literalValue4)
    val finalSum: Expr = Expr.Add(leftSum, rightSum)
    assertEquals(
      numericValue + numericValue2 + numericValue3 + numericValue4,
      Expr.evaluate(finalSum)
    )

  @Test def testSimpleMultiply(): Unit =
    val product: Expr = Expr.Multiply(literalValue, literalValue2)
    assertEquals(numericValue * numericValue2, Expr.evaluate(product))

  @Test def testOppositeMultiplication(): Unit =
    val product: Expr = Expr.Multiply(literalValue, Expr.Literal(-1))
    assertEquals(-numericValue, Expr.evaluate(product))

  @Test def testComposedMultiply(): Unit =
    val leftProduct: Expr = Expr.Multiply(literalValue, literalValue2)
    val rightProduct: Expr = Expr.Multiply(literalValue3, literalValue4)
    val finalProduct: Expr = Expr.Multiply(leftProduct, rightProduct)
    assertEquals(
      numericValue * numericValue2 * numericValue3 * numericValue4,
      Expr.evaluate(finalProduct)
    )

  @Test def testNestedAdditionAndMultiplication(): Unit =
    val expr: Expr = Expr.Multiply(Expr.Add(literalValue, literalValue2), literalValue3)
    assertEquals(
      (numericValue + numericValue2) * numericValue3,
      Expr.evaluate(expr)
    )

  @Test def testSimpleAddString(): Unit =
    val sum: Expr = Expr.Add(literalValue, literalValue2)
    assertEquals(s"(${numericValue} + ${numericValue2})", Expr.show(sum))

  @Test def testComposedAddString(): Unit =
    val leftSum: Expr = Expr.Add(literalValue, literalValue2)
    val rightSum: Expr = Expr.Add(literalValue3, literalValue4)
    val finalSum: Expr = Expr.Add(leftSum, rightSum)
    assertEquals(
      s"((${numericValue} + ${numericValue2}) + (${numericValue3} + ${numericValue4}))",
      Expr.show(finalSum)
    )

