package in.darkempire.simplepragrams

import java.util.Stack

/**
 * Custom Exception for Invalid Expression.
 * @param message
 */
case class InvalidExpression(message: String) extends Exception(message)

/**
 * The postfix operation class.
 *
 * @param expr Expression
 * @author amitkumargiri #github.com
 * @site http://darkempire.in/codesolution/scala/postfixoperation
 */
class PostfixOperation(expr: List[String]) {

  /**
   * Function to convert the expression to postfix.
   *
   * @throws in.darkempire.simplepragrams.InvalidExpression
   * @return Stack[String]
   */
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
      } else if (! PostfixOperation.isOperator(ch))
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

  /**
   * Implicit function to convert Stack[String] to String.
   * @return String
   */
  implicit def toPostfix: String = {
    val result = convertToPostfix
    val sb = new StringBuilder
    result.forEach(ch => sb.append(ch))
    sb.toString()
  }

  /**
   * Find the precedence of the operator. Solve the expression using BODMASS.
   * @param op Operator
   * @return Integer
   */
  private def precedence(op: String): Int = op match {
    case "+" | "-" => 1
    case "*" | "/" => 2
    case "^" => 3
    case _ => -1 // default
  }
}

object PostfixOperation {

  /**
   * Check if the given string is operator or not.
   *
   * @param op Operator
   * @return Boolean
   */
  def isOperator(op: String): Boolean = List("+", "-", "*", "/", "^").contains(op)
}