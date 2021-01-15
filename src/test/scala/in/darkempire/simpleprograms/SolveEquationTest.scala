package in.darkempire.simpleprograms

import in.darkempire.simplepragrams.SolveEquation
import org.scalatest.funsuite.AnyFunSuite

class SolveEquationTest extends AnyFunSuite {

  test("SolveEquation Test:1") {
    val sol = new SolveEquation("X - 100 = 200")
    assert(sol.getValueOfX == 300)
  }

  /*
    Right now the logic is not able to solve the equation correctly
    if the X is in brackets. Need to fix that.
   */
  test("SolveEquation Test:2") {
    val sol = new SolveEquation("2 * ( X + 100 ) = 200")
    assert(sol.getValueOfX == 0)
  }

  test("SolveEquation Test:3") {
    val sol = new SolveEquation("X = 200 + 500 * 2")
    assert(sol.getValueOfX == 1200)
  }

  test("SolveEquation Test:4") {
    val sol = new SolveEquation("X = 100 * 2 / ( 2 + 3 ) ")
    assert(sol.getValueOfX == 40)
  }
}
