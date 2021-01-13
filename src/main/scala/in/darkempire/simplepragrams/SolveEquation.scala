package in.darkempire.simplepragrams

import java.{lang, util}
import scala.annotation.tailrec
import scala.util.control.Breaks

/**
 * Class to solve the expression. Asked in Interview Question of Knoldus Software from coderbyte.
 * Find the value of X, for any given expression. E.g. 5x + 30 = 55 or 12x0 - 200 = 1000
 *
 * @param expression
 */
class SolveEquation(expression: String) {

  private val space = " "

  def getValueOfX: Double = ???

  def splitExpr: Array[String] = expression.split("=")

  def solve(): Double = {
    var result: Double = 0.0
    // if both LSH and RHS contains X then its unable to solve the equation
    findX match {
      case 0 => println("Unable to solve the equation as both side has X")
      case -1 => // solving for LHS as X
        val solvableExpr = splitExpr.last.split(space).toList.filter(!_.isEmpty)
        result = PostfixOperation.solveExpr(solvableExpr)
        val lhsExpr = splitExpr.head.split(space).toList.filter(!_.isEmpty)
        result = PostfixOperation.solveExprForX(lhsExpr, result)
      case 1 => // solving for RHS as X
        val solvableExpr = splitExpr.head.split(space).toList.filter(!_.isEmpty)
        result = PostfixOperation.solveExpr(solvableExpr)
        val rhsExpr = splitExpr.last.split(space).toList.filter(!_.isEmpty)
        result = PostfixOperation.solveExprForX(rhsExpr, result)
      case _ => println("Invalid value")
    }
    result
  }

  def findX: Int = {
    if ((splitExpr.head.contains("x") || splitExpr.head.contains("X")) &&
      splitExpr.last.contains("x") || splitExpr.last.contains("X")) {
      0
    } else if (splitExpr.head.contains("x") || splitExpr.head.contains("X")) {
      -1 // represents LHS
    } else {
      1 // represents RHS
    }
  }
}

object SolveEquation {

  @tailrec
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

  def solveExprForX: (String, String, Double) = {
    ("X", "+", 0.0)
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
