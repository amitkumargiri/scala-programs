package in.darkempire.futures

import scala.concurrent.ExecutionContext.Implicits.global
import java.io.{File, FileInputStream, FileNotFoundException}
import java.nio.charset.StandardCharsets

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

/**
 * Please provide the input string in balanceone.txt
 *
 * @author amitkumargiri #github.com
 * @site http://darkempire.in/codesolution/scala/balancedParenthesesExpUsingThread
 */
object BalancedParenthesesExpression {

  def main(args: Array[String]) {
    val filename = "balanceone.txt"
    var parenthesesCount = 0
    try {
      val file = new File(filename)
      val fis = new FileInputStream(file)
      var i = 0
      val futures = new ListBuffer[Future[Int]]

      println("Program started running...")
      do {
        val buf = new Array[Byte](1024)
        i = fis.read(buf)
        val value = new String(buf, StandardCharsets.UTF_8)
        println("String Value: " + value)
        futures.addOne(getParenthesesCount(value, '(', ')'))
        println("Future added...")
      } while (i != -1)

      val result = Future {
        for(i <- 0 until futures.length-1) {
          val pCount = Await.result(futures(i), Duration.Inf)
          println("Parentheses Count for future(" + i + "): " + pCount)
          parenthesesCount += pCount
        }
      }

      Await.ready(result, Duration.Inf)
      result.onComplete({
        case Success(_) =>
          if (parenthesesCount != 0) {
            println("Not Balanced Expression")
            println("Total Irregular Parentheses count: " + parenthesesCount)
          } else
            println("Balanced Expression")
        case Failure(_) => // don't do anything for now
      })

    } catch {
      case e: FileNotFoundException => println(e)
    }
  }

  def getParenthesesCount(str: String, startChar: Char, endChar: Char): Future[Int] = Future {
    var count = 0
    for(i <- 0 until str.length-1)
      if (str(i) == startChar) count += 1 else if (str(i) == endChar) count -= 1
    count
  }
}