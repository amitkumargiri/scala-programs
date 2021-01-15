package in.darkempire.simplepragrams

import in.darkempire.simplepragrams.PostfixOperation.isOperator

import java.util.Stack
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

  /**
   * Get the value of X for a given simple equation/expression of X.
   * @return Double
   */
  def getValueOfX: Double = {
    var result: Double = 0.0
    // if both LSH and RHS contains X then its unable to solve the equation
    findX match {
      case 0 => println("Unable to solve the equation as both side has X")
      case -1 => // solving for LHS as X
        val solvableExpr = splitExpr.last.split(space).toList.filter(!_.isEmpty)
        result = solveExpr(solvableExpr)
        val lhsExpr = splitExpr.head.split(space).toList.filter(!_.isEmpty)
        result = solveExprForX(lhsExpr, result)
      case 1 => // solving for RHS as X
        val solvableExpr = splitExpr.head.split(space).toList.filter(!_.isEmpty)
        result = solveExpr(solvableExpr)
        val rhsExpr = splitExpr.last.split(space).toList.filter(!_.isEmpty)
        result = solveExprForX(rhsExpr, result)
      case _ => println("Invalid value")
    }
    result
  }

  def splitExpr: Array[String] = expression.split("=")

  /**
   * Solve the given operation using postfix operation that contains X.
   * E.g. 100 + X * 2 = 500
   *
   * @param list
   * @param rhsResult
   * @throws in.darkempire.simplepragrams.InvalidExpression
   * @return Result of the expression for X
   */
  @throws(classOf[InvalidExpression])
  private def solveExprForX(list: List[String], rhsResult: Double): Double = {
    var result = rhsResult
    val pf = new PostfixOperation(list)
    val exprStack = pf.convertToPostfix
    val opStack = new Stack[String]
    while (!exprStack.isEmpty) {
      val exp = exprStack.pop()
      if (isOperator(exp))
        opStack.push(exp)
      else if (exp == "X") {
        // do nothing
      } else {
        val operator = opStack.pop()
        val value = exp.toDouble
        // reversing the operator as moving from LHS to RHS
        operator match {
          case "+" => result = result - value
          case "-" => result = result + value
          case "*" => result = result / value
          case "/" => result = result * value
        }
      }
    }
    result
  }

  /**
   * Solve the given expression using postfix operation.
   * E.g. 100 + 50 * 2 + ( 40 - 50 )
   *
   * @param list
   * @throws in.darkempire.simplepragrams.InvalidExpression
   * @return Double
   */
  @throws(classOf[InvalidExpression])
  def solveExpr(list: List[String]): Double = {
    val pf = new PostfixOperation(list)
    val exprStack = pf.convertToPostfix
    val tempStack = new Stack[String]
    while (!exprStack.isEmpty) {
      tempStack.push(exprStack.pop())
    }
    val solveStack = new Stack[Double]
    while (!tempStack.isEmpty) {
      val exp = tempStack.pop()
      if (PostfixOperation.isOperator(exp)) {
        val val1 = solveStack.pop()
        val val2 = solveStack.pop()
        exp match {
          case "+" => solveStack.push(val1 + val2)
          case "-" => solveStack.push(val2 - val1)
          case "*" => solveStack.push(val1 * val2)
          case "/" => solveStack.push(val2 / val1)
        }
      } else {
        solveStack.push(exp.toDouble)
      }
    }
    solveStack.pop();
  }

  private def findX: Int = {
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

  /**
   * Tail recursive method to solve the simple mathematical expression.
   * E.g. List("500", "+", "50", "-", "100")
   *
   * @param exprList Expression as String
   * @throws java.lang.NumberFormatException
   * @return Double as Result
   */
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
}
