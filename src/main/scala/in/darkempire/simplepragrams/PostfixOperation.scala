package in.darkempire.simplepragrams

import java.util.Stack

/**
 * Custom Exception for Invalid Expression.
 * @param message
 */
case class InvalidExpression(message: String) extends Exception(message)

class PostfixOperation(expr: List[String]) {

  @throws(classOf[InvalidExpression])
  def convertToPostfix: Stack[String] = {
    val result = new Stack[String]
    val stack = new Stack[String]
    expr.map( ch => {
      if (ch == "(")
        stack.push(ch)
      else if (ch == ")") {
        while (!stack.isEmpty && stack.peek() != "(") result.push(stack.pop())
        stack.pop()
      } else if (! isOperator(ch))
        result.push(ch)
      else {
        while (!stack.isEmpty && precedence(ch) <= precedence(stack.peek()))
          result.push(stack.pop())
        stack.push(ch)
      }
    })
    while (!stack.isEmpty) {
      if (stack.peek() == "(")
        throw InvalidExpression("Invalid Expression")
      result.push(stack.pop())
    }
    result
  }

  implicit def toPostfix: String = {
    val result = convertToPostfix
    val sb = new StringBuilder
    result.forEach(ch => sb.append(ch))
    sb.toString()
  }

  private def isOperator(op: String): Boolean = List("+", "-", "*", "/", "^").contains(op)

  private def precedence(op: String): Int = op match {
    case "+" | "-" => 1
    case "*" | "/" => 2
    case "^" => 3
    case _ => -1 // default
  }
}

object PostfixOperation {

  @throws(classOf[InvalidExpression])
  def solveExpr(list: List[String]): Double = {
    val pf = new PostfixOperation(list)
    val exprStack = pf.convertToPostfix
    println(exprStack.toString)
    val tempStack = new Stack[String]
    while (!exprStack.isEmpty) {
      tempStack.push(exprStack.pop())
    }
    val solveStack = new Stack[Double]
    while (!tempStack.isEmpty) {
      val exp = tempStack.pop()
      if (pf.isOperator(exp)) {
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
}