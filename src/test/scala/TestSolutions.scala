import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01"):
    assertResult(1602)(actual = Day01.answer1)
    assertResult(1633)(actual = Day01.answer2)