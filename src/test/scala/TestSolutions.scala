import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01"):
    assertResult(1602)(Day01.answer1)
    assertResult(1633)(Day01.answer2)

  test("Day02"):
    assertResult(2322630)(Day02.answer1)
    assertResult(2105273490)(Day02.answer2)

  test("Day03"):
    assertResult(2648450)(Day03.answer1)
    assertResult(2845944)(Day03.answer2)

  test("Day04"):
    assertResult(21607)(Day04.answer1)
    assertResult(19012)(Day04.answer2)

  test("Day05"):
    assertResult(7085)(Day05.answer1)
    assertResult(20271)(Day05.answer2)

  test("Day06"):
    assertResult(380612)(Day06.answer1)
    assertResult(1710166656900L)(Day06.answer2)

  test("Day07"):
    assertResult(328187)(Day07.answer1)
    assertResult(91257582)(Day07.answer2)

  test("Day08"):
    assertResult(521)(Day08.answer1)
    assertResult(1016804)(Day08.answer2)

  test("Day23"):
    assertResult(11417)(Day23.answer1)
    assertResult(49529)(Day23.answer2)
