package in.darkempire.simplepragrams

/**
 * Platform: Hacker Earth
 * Problem URL: https://www.hackerearth.com/practice/basic-programming/input-output/basics-of-input-output/practice-problems/algorithm/divisibe-or-2d8e196a/
 * The solution is provided by Amit Kumar Giri.
 *
 * Preferred Coding Language - SCALA
 * Personal-email: allyamit@gmail.com
 *
 * @author amitkumargiri@hotmail.com
 * @site http://darkempire.in/codesolution/scala/divisibleby11
 */

object DivisibleBy11 extends App {

  try {
    val arrSize = scala.io.StdIn.readInt
    val inputArray = scala.io.StdIn.readLine.split(" ")
    val halfSize = arrSize/2
    var oddSum = 0
    var evenSum = 0

    for(i <- 0 to halfSize-1) {
      val firstDigit = inputArray(i)(0) - '0'
      val j = i + halfSize
      val lastDigit = inputArray(j)(inputArray(j).length-1) - '0'

      if (i%2 == 0) oddSum = oddSum + firstDigit else evenSum = evenSum + firstDigit
      if (j%2 == 0) oddSum = oddSum + lastDigit else evenSum = evenSum + lastDigit
    }

    if (math.abs(oddSum-evenSum) % 11 == 0) {
      println("OUI")
    } else {
      println("NON")
    }
  } catch {
    case inputException: Exception => println(inputException.getMessage)
  }
}