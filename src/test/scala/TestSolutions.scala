import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01"):
    assertResult(1602)(actual = Day01.answer1)
    assertResult(1633)(actual = Day01.answer2)

  test("Day02"):
    assertResult(2322630)(actual = Day02.answer1)
    assertResult(2105273490)(actual = Day02.answer2)

  test("Day23"):
    assertResult(11417)(actual = Day23.answer1)
    assertResult(49529)(actual = Day23.answer2)