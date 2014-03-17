import randomized.Primality._
import scala.io.Source

class Testing {

  def expectedTest = {
    // Create a list of Composite numbers, that are not even.
    val lines = Source.fromFile("src/main/resources/primesBelow1000.txt").getLines()
    val primes = (for(line <- lines) yield line.toInt).toSet
    val numbers = (
      for(i <- 3 to 7919
        if i%2 != 0 // Skip even numbers
      ) yield i).toSet -- primes

    val results : Set[(Int,Int)] = numbers.map((number) => (number, runAlgo(number, solovayStrassen _, 100)))
    println(results)
  }

  def runAlgo(input: Int, algo: Int => Outcomes, runTimes : Int) : Int = {
    (for(i <- 1 to runTimes) yield {
      algo(input)
    }).foldLeft(0) {
      case (primeTimes, ProbablyPrime) => primeTimes + 1
      case (primeTimes, _) => primeTimes
    }
  }

}
