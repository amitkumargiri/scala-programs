package in.darkempire

import in.darkempire.simplepragrams.{PostfixOperation, SolveEquation}

/**
 * Portfolio - http://darkempire.in/codesolution/scala/
 *
 * @author Amit Kumar Giri
 */
object RunProgram {

  /** Call the programs from various packages to run a specific program. */
  def main(args: Array[String]): Unit = {
    val exprList = List("(", "1000", "-", "500", "+", "500", ")", "*", "4")
    println(PostfixOperation.solveExpr(exprList))
  }
}
