package in.darkempire.datastructure

/** This is TreeTest File. */

import org.scalatest.funsuite.AnyFunSuite

class BinarySearchTreeTest extends AnyFunSuite {

  private def getSampleData: BSTInt = {
    val bstIntTree = BSTInt() // This is the integer implementation
    bstIntTree.insert(50)
    bstIntTree.insert(30)
    bstIntTree.insert(100)
    bstIntTree.insert(34)
    bstIntTree.insert(25)
    bstIntTree.insert(75)
    bstIntTree.insert(108)
    bstIntTree
  }
  test("Tree Root is 50") {
    val bstIntTree = getSampleData
    assert(bstIntTree.root.get.data == 50)
  }

  test("Tree Height is 3") {
    val bstIntTree = getSampleData
    assert(bstIntTree.root.get.heightOfNode == 3)
  }

  test("Tree Search Node 30") {
    val bstIntTree = getSampleData
    val node = bstIntTree.search(30)
    assert(node != None)
    assert(node.get.data == 30)
  }

  test("Tree Search Node 75") {
    val bstIntTree = getSampleData
    val node = bstIntTree.search(75)
    assert(node != None)
    assert(node.get.data == 75)
  }

  test("Tree Node 999 is NOT found") {
    val bstIntTree = getSampleData
    val node = bstIntTree.search(999)
    assert(node == None)
  }

  test("Deleting LeftNode 30 is successful") {
    val bstIntTreeLocal = BSTInt() // This is the integer implementation
    bstIntTreeLocal.insert(50)
    bstIntTreeLocal.insert(30)
    bstIntTreeLocal.insert(100)
    bstIntTreeLocal.insert(34)
    bstIntTreeLocal.insert(25)
    bstIntTreeLocal.insert(75)
    bstIntTreeLocal.insert(108)
    var node30 = bstIntTreeLocal.search(30)
    assert(node30 != None)
    assert(node30.get.data == 30)
    bstIntTreeLocal.delete(30)
    node30 = bstIntTreeLocal.search(30)
    assert(node30 == None)
  }

  test("Deleting RightNode 100 is successful") {
    val bstIntTreeLocal = BSTInt() // This is the integer implementation
    bstIntTreeLocal.insert(50)
    bstIntTreeLocal.insert(30)
    bstIntTreeLocal.insert(100)
    bstIntTreeLocal.insert(34)
    bstIntTreeLocal.insert(25)
    bstIntTreeLocal.insert(75)
    bstIntTreeLocal.insert(108)
    var node100 = bstIntTreeLocal.search(100)
    assert(node100 != None)
    assert(node100.get.data == 100)
    bstIntTreeLocal.delete(100)
    node100 = bstIntTreeLocal.search(100)
    assert(node100 == None)
  }

  test("Deleting RootNode 50 is successful") {
    val bstIntTreeLocal = BSTInt() // This is the integer implementation
    bstIntTreeLocal.insert(50)
    bstIntTreeLocal.insert(30)
    bstIntTreeLocal.insert(100)
    bstIntTreeLocal.insert(34)
    bstIntTreeLocal.insert(25)
    bstIntTreeLocal.insert(75)
    bstIntTreeLocal.insert(108)
    var node50 = bstIntTreeLocal.search(50)
    assert(node50 != None)
    assert(node50.get.data == 50)
    bstIntTreeLocal.delete(50)
    node50 = bstIntTreeLocal.search(50)
    assert(node50 == None)
    assert(bstIntTreeLocal.root.get.data == 34)
  }

  test("Deleting LeftNode 30 and Then RootNode50 is successful") {
    val bstIntTreeLocal = BSTInt() // This is the integer implementation
    bstIntTreeLocal.insert(50)
    bstIntTreeLocal.insert(30)
    bstIntTreeLocal.insert(100)
    bstIntTreeLocal.insert(34)
    bstIntTreeLocal.insert(25)
    bstIntTreeLocal.insert(75)
    bstIntTreeLocal.insert(108)
    var node30 = bstIntTreeLocal.search(30)
    assert(node30 != None)
    assert(node30.get.data == 30)
    bstIntTreeLocal.delete(30)
    node30 = bstIntTreeLocal.search(30)
    assert(node30 == None)
    var node50 = bstIntTreeLocal.search(50)
    assert(node50 != None)
    assert(node50.get.data == 50)
    bstIntTreeLocal.delete(50)
    node50 = bstIntTreeLocal.search(50)
    assert(node50 == None)
    assert(bstIntTreeLocal.root.get.data == 34)
  }
}