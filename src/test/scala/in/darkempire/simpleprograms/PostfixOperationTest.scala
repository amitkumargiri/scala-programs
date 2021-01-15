package in.darkempire.simpleprograms

import in.darkempire.simplepragrams.PostfixOperation
import org.scalatest.funsuite.AnyFunSuite

class PostfixOperationTest extends AnyFunSuite {

  test("Postfix Test:1 a+b") {
    val pf = new PostfixOperation(List("a", "+", "b"))
    assert(pf.toPostfix == "ab+")
  }

  test("Postfix Test:2 a+(b-c)") {
    val pf = new PostfixOperation(List("a","+","(","b","-","c",")"))
    assert(pf.toPostfix == "abc-+")
  }

  test("Postfix Test:3 a+b*(c-d)") {
    val pf = new PostfixOperation(List("a","+","b","*","(","c","-","d",")"))
    assert(pf.toPostfix == "abcd-*+")
  }

  test("Postfix Test:4 a+b*(c-d)^T") {
    val pf = new PostfixOperation(List("a","+","b","*","(","c","-","d",")","^","T"))
    assert(pf.toPostfix == "abcd-T^*+")
  }

  test("Postfix Test:5 a+b^((c-d)*T)") {
    val pf = new PostfixOperation(List("a","+","b","^","(","(","c","-","d",")","*","T",")"))
    assert(pf.toPostfix == "abcd-T*^+")
  }
}
