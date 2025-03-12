package task4

import org.junit.*
import org.junit.Assert.*
import lab2.Lab2.*

class ExprTest:

  val testValue: Expr = Expr.Literal(5)
  val testValue2: Expr = Expr.Literal(7)

  @Test def testSimpleAdd(): Unit =
    val sum: Expr = Expr.Add(testValue, testValue2)
    assertEquals(12, Expr.evaluate(sum))

  @Test def testSimpleMultiply(): Unit =
    val product: Expr = Expr.Multiply(testValue, testValue2)
    assertEquals(35, Expr.evaluate(product))


