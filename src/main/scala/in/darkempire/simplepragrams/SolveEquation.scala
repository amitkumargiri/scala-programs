package in.darkempire.simplepragrams

import java.lang
import scala.util.control.Breaks

/**
 * Class to solve the expression. Asked in Interview Question of Knoldus Software from coderbyte.
 * Find the value of X, for any given expression. E.g. 5x + 30 = 55 or 12x0 - 200 = 1000
 *
 * @param expression
 */
class SolveEquation(expression: String) {

  def getValueOfX: Double = ???

  def splitExpr: Array[String] = expression.split("=")
}

object SolveEquation {
  def solve(expr: String): Double = {
    val exprList = expr.split(" ").toList.filter(!_.isEmpty)
    println(exprList)
    if(checkValidExpression(exprList)) {
      println(solveExprList(exprList))
    } else
      println("Expression is NOT valid")
    0.0
  }

  @throws(classOf[NumberFormatException])
  def solveExprList(exprList: List[String]): Double = {
    if (exprList.length < 3)
      exprList(0).toDouble
    else {
      var result = 0.0
      exprList(1) match {
        case "+" => result = exprList(0).toDouble + exprList(2).toDouble
        case "-" => result = exprList(0).toDouble - exprList(2).toDouble
        case "*" => result = exprList(0).toDouble * exprList(2).toDouble
        case "/" => result = exprList(0).toDouble / exprList(2).toDouble
      }
      solveExprList(result.toString()::exprList.drop(3))
    }
  }

  /**
   * Check if the given string a valid expression to solve.
   * @param exprList
   * @return Boolean
   */
  def checkValidExpression(exprList: List[String]): Boolean = {
    var flag = true
    val loop = new Breaks
    loop.breakable({
      for (i <- 0 to exprList.length-1) {
        if (i % 2 != 0) {
          // foreach odd index, it should be operator
          if (!isOperator(exprList(i))) { flag = false; loop.break() }
        } else {
          //foreach even index, it should be a number
          try { exprList(i).toDouble } catch { case ex: NumberFormatException => flag = false; loop.break() }
        }
      }
    })
    flag
  }

  def isOperator(ch: String): Boolean = List("+", "-", "*", "/", "=").contains(ch)
}
