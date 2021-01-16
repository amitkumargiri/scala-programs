package in.darkempire.datastructure

/**
 * @site http://darkempire.in/codesolution/scala/simplelinkedlist
 * @author amitkumargiri #github.com
 */

import scala.util.control.Breaks

/**
 * Node structure to store the data and next pointer.
 * @param data Generic datatype
 * @param next Address to next node
 * @tparam T GenericType
 */
sealed class Node[T] (private var data: T, private var next: Option[Node[T]]) {

  def getData: T = this.data
  def getNext: Option[Node[T]] = this.next

  def setData(d: T) { this.data = d }
  def setNext(n: Option[Node[T]]) { this.next = n}

  def printList(): Unit = {
    print(data)
    if (next.nonEmpty) {
      print(",")
      next.get.printList()
    }
  }
}

/**
 * Implementation of the LinkedList with generic data type.
 * @tparam T Generic datatype
 */
case class LinkedList[T]() {

  var head: Option[Node[T]] = None

  /**
   * Insert operation to be performed in Linked list in the beginning of the list.
   *
   * @param data Generic Data
   */
  def push(data: T): Unit = head match {
    case None => head = Some(new Node(data, None))
    case _ =>
      val node = new Node[T](data, None)
      node.setNext(head)
      head = Some(node)
  }

  /**
   * Insert operation to be performed in Linked list in the end of the list.
   *
   * @param data Generic Data
   */
  def pushAtEnd(data: T): Unit = head match {
    case None => head = Some(new Node(data, None))
    case _ => var currentNode = head
      while (currentNode.get.getNext.nonEmpty) {
        currentNode = currentNode.get.getNext
      }
      currentNode.get.setNext(Some(new Node(data, None)))
  }

  /**
   * Search the element in the linked list. It will return the first occurrence if
   * there are duplicate data in linked list.
   *
   * @param data Generic datatype
   * @return Node with the first occurrence
   */
  def search(data: T): Option[Node[T]] = {
    val loop = new Breaks
    var currentNode: Option[Node[T]] = head
    loop.breakable {
      while (currentNode.nonEmpty) {
        if (currentNode.get.getData == data) {
          loop.break()
        }
        currentNode = currentNode.get.getNext
      }
    }
    if (currentNode.isEmpty) None else currentNode
  }

  def update(oldData: T, newData: T): Boolean = {
    val node = search(oldData)
    node match {
      case None => false
      case _ => node.get.setData(newData)
        true
    }
  }

  /**
   * Delete operation to be performed in Linked list. This will delete the first occurrence of
   * the element only.
   *
   * @param data Generic Data
   */
  def delete(data: T): Unit = {
    val loop = new Breaks
    loop.breakable {
      var prev = head
      var currentNode = head
      while (currentNode.nonEmpty) {
        val node = currentNode.get
        if (node.getData == data) {
          if (prev == currentNode) head = None else prev.get.setNext(node.getNext)
          loop.break
        }
        prev = currentNode
        currentNode = currentNode.get.getNext
      }
    }
  }

  /** Print the linked list */
  def print(): Unit = if (head.nonEmpty) head.get.printList()

  override def toString: String = {
    val sb = new StringBuilder
    var node: Option[Node[T]] = head
    while (node.nonEmpty) {
      sb.append(node.get.getData)
      if (node.get.getNext.nonEmpty) sb.append(", ")
      node = node.get.getNext
    }
    sb.toString()
  }
}