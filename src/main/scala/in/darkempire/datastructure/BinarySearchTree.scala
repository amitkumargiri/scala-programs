package in.darkempire.datastructure

/**
 * @site http://darkempire.in/codesolution/scala/bstimpl
 * @author amitkumargiri #github.com
 */

/**
 * TreeNode to implement the Binary Search Tree. It stores data and left and right nodes.
 * @param data The data value
 * @param left The left subtree
 * @param right The right Subtree
 * @tparam T Data type
 */
class TreeNode[T] (var data: T, var left: Option[TreeNode[T]], var right: Option[TreeNode[T]]) {
  def getData: T = this.data

  def getLeftSubTree: Option[TreeNode[T]] = this.left

  def getRightSubTree: Option[TreeNode[T]] = this.right

  /**
   * Get the max height of the node from the leaf.
   * @return Int
   */
  final def heightOfNode: Int = {
    if(isLeafNode)
      1
    else {
      var leftHeight: Int = 0
      var rightHeight: Int = 0
      if (this.left.nonEmpty) { leftHeight = left.get.heightOfNode }
      if (this.right.nonEmpty) { rightHeight = right.get.heightOfNode }
      if (leftHeight > rightHeight) leftHeight + 1 else rightHeight + 1
    }
  }

  final def isLeafNode: Boolean = left.isEmpty && right.isEmpty

  def printOnlyData(): Unit = {
    print(data)
    if (left.nonEmpty) {
      print(",")
      left.get.printOnlyData()
    }
    if (right.nonEmpty) {
      print(",")
      right.get.printOnlyData()
    }
  }
}

/**
 * Since, the data type is T, you can't perform <, > operation directly. So, compare
 * is the method just like Comparator to compare between data of two nodes.
 *
 * @tparam T Generic Data type
 */
abstract class BinarySearchTree[T]() {

  var root: Option[TreeNode[T]] = None

  def search(data: T): Option[TreeNode[T]] = {
    import scala.util.control.Breaks

    val loop = new Breaks
    var currentNode = root
    var foundFlag = false

    loop.breakable {
      while (currentNode.nonEmpty) {
        compare(data, currentNode.get.data) match {
          case -1 => currentNode = currentNode.get.getLeftSubTree
          case 1 => currentNode = currentNode.get.getRightSubTree
          case 0 =>
            foundFlag = true
            loop.break()
          case _ => // do nothing
        }
      }
    }
    if (foundFlag) currentNode else None
  }

  /** Define this method according to the datatype T. Return 1, -1 or 0. */
  def compare(x: T, y: T): Int

  /**
   * Perform the insert operation. This tree can't have duplicate data.
   * @param data Generic data type
   */
  def insert(data: T): Unit = root match {
    case None => root = Some(new TreeNode(data, None, None))
    case _ =>
      var currentNode = root
      var insertPos: Int = 0
      var prevNode: Option[TreeNode[T]] = None
      while (currentNode.nonEmpty) {
        prevNode = currentNode
        if (compare(data, currentNode.get.data) == -1) {
          currentNode = currentNode.get.getLeftSubTree
          insertPos = -1 // insert at left node
        } else if (compare(data, currentNode.get.data) == 1) {
          currentNode = currentNode.get.getRightSubTree
          insertPos = 1 // insert at right
        } else {
          insertPos = 0 // Do not insert duplicate
        }
      }
      if (insertPos < 0) {
        prevNode.get.left = Some(new TreeNode(data, None, None))
      } else if (insertPos > 0) {
        prevNode.get.right = Some(new TreeNode(data, None, None))
      } else {
        println("Duplicate elements can't be added. This will need different logic to insert, delete and search in whole tree structure.")
      }
  }

  /** Deletes the leaf node. Highly coupled and dependent on the delete method. */
  private def deleteLeafNode(n: Option[TreeNode[T]], branchSide: Int): Unit = branchSide match {
    case 1 => n.get.right = None
    case -1 => n.get.left = None
    case _  => root = None
  }

  /**
   * Deletes the node specified with the data.
   * @param data Generic data type
   */
  def delete(data: T): Unit = {
    import scala.util.control.Breaks

    root match {
      case None => println("The tree is empty.")
      case _ =>
        val loop = new Breaks
        var currentNode = root
        var prevNode: Option[TreeNode[T]] = None
        var branchSide: Int = 0 // -1 for left-side and 1 for right-side
        loop.breakable{
          while (currentNode.nonEmpty) {
            if (compare(data, currentNode.get.data) == -1) {
              prevNode = currentNode
              currentNode = currentNode.get.left
              branchSide = -1 // it traces the prevNodes branch side
            } else if (compare(data, currentNode.get.data) == 1) {
              prevNode = currentNode
              branchSide = 1 // it traces the prevNodes branch side
              currentNode = currentNode.get.right
            } else if (compare(data, currentNode.get.data) == 0) {
              (currentNode.get.left, currentNode.get.right) match {
                /* Case(1) If it is a leaf node, delete it directly */
                case (None, None) => deleteLeafNode(prevNode, branchSide)

                /* Case(2) If it has only right subtree */
                case (None, Some(y)) => if (branchSide < 0) prevNode.get.left = Some(y) else prevNode.get.right = Some(y)

                /* Case(3) If it has only Left subtree */
                case (Some(x), None) => prevNode.get.left = Some(x)

                /* Case(4) If it has Both Left and Right subtrees */
                case (Some(x), Some(_)) => /* go to one left node, then find its rightmost node and replace it with the
                  node you want to delete */
                  (Some(x).get.left, Some(x).get.right) match {
                    /* Case(4.1) If that left node don't have right subtree. */
                    case (_, None) => currentNode.get.left = Some(x).get.left // replace the data and its left node
                      currentNode.get.data = Some(x).get.data
                    /* Case(4.2) If that left node have right subtree. */
                    case (_, Some(k)) => var rightMostNode: Option[TreeNode[T]] = Some(k)
                      var prevTempNode: Option[TreeNode[T]] = Some(x)
                      while (rightMostNode.get.right.isDefined) {
                        prevTempNode = rightMostNode
                        rightMostNode = rightMostNode.get.right
                      }
                      /* Case (4.2.1) The RightMostNode has no left subtree. */
                      if (rightMostNode.get.left.isEmpty) {
                        prevTempNode.get.right = None
                      } else { /* Case (4.2.2) The RightMostNode has left subtree. */
                        prevTempNode.get.right = rightMostNode.get.left
                      }
                      currentNode.get.data = rightMostNode.get.data // replace the data with the node you want to delete
                  }
              }
              loop.break()
            }
          }
        }
    }
  }

  def printData(): Unit = if (root.nonEmpty) root.get.printOnlyData()
}

/** Implementation of the Integer Binary Search Tree. Compare is defined for Int. */
case class BSTInt() extends BinarySearchTree[Int]() {
  override def compare(x: Int, y: Int): Int = if (x == y) 0 else if(x < y) -1 else 1
}

/*
object BSTIntTreeImpl {

  def main(args: Array[String]): Unit = {
    val bstIntTree = BSTInt() // This is the integer implementation
    bstIntTree.insert(50)
    bstIntTree.insert(30)
    bstIntTree.insert(100)
    bstIntTree.insert(34)
    bstIntTree.insert(25)
    bstIntTree.insert(75)
    bstIntTree.insert(108)
    val node = bstIntTree.search(30)
    println(node.get.data)
    // Perform tree operations here...
  }
}
*/