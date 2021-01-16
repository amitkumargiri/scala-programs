package in.darkempire.datastructure

import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class LinkedListTest extends AnyFunSuite with BeforeAndAfter {

  var list: LinkedList[Int] = LinkedList[Int]
  before { /* executes on every test case */
    list = new LinkedList[Int]
    list.push(5)
    list.push(10)
    list.push(17)
    list.push(23)
    list.push(29)
  }

  test("Linkedlist Head") {
    assert(list.head.get.getData == 29)
  }

  test("Push In Beginning") {
    list.push(67)
    assert(list.head.get.getData == 67)
  }

  test("Push in End") {
    list.pushAtEnd(8)
    assert(list.head.get.getData == 29)
  }

  test("Search Operation") {
    val node = list.search(17)
    assert(node.get.getData == 17)
    assert(list.search(777) == None)
  }

  test("Delete Operation") {
    val node = list.search(23)
    assert(node.get.getData == 23)
    list.delete(23)
    assert(list.search(23) == None)
  }

  test("Update Operation") {
    val node = list.search(23)
    assert(node.get.getData == 23)
    list.update(23, 778)
    assert(list.search(23) == None)
    assert(list.search(778) != None)
  }
}