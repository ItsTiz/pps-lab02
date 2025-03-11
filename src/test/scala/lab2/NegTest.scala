package lab2

import org.junit.*
import org.junit.Assert.*
import Lab2.*

class NegTest:

  val isEven: Int => Boolean = _ % 2 == 0
  val isEmpty: String => Boolean = _ == ""
  val numberValue = 4

  @Test def testNegIsEvenNumber(): Unit =
    assertFalse(neg(isEven)(numberValue))

  @Test def testNegIsStringEmpty(): Unit =
    assertFalse(neg(isEmpty)(""))

  @Test def testThreeValFun(): Unit =
    assertTrue(threeValPredicateFun(4,5,5))

  @Test def testThreeValFunCurried(): Unit =
    assertTrue(threeValPredicateFun(4, 5, 5))

  @Test def testThreeValMethod(): Unit =
    assertTrue(threeValPredicateFun(4,5,5))

  @Test def testThreeValMethodCurried(): Unit =
    assertTrue(threeValPredicateFun(4,5,5))
