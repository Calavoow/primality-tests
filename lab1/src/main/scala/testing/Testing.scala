import randomized.Primality._
import scala.io.Source

object Testing {

  def expectedTest = {
    // Create a list of Composite numbers, that are not even.
    val lines = Source.fromFile("src/main/resources/primesBelow1000.txt").getLines()
    val primes = (for(line <- lines) yield line.toInt).toSet
    val numbers = (
      for(i <- 3 to 7919
        if i%2 != 0 // Skip even numbers
      ) yield i).toSet -- primes

    def calcResults(primeTester: Int => Outcome, runs: Int) = {
      val results : Set[(Int,Int)] = numbers.map((number) => (number, runAlgo(number, primeTester, runs)))
      val tmp = results.map(_._2)
      (mean(tmp), sample_stddev(tmp))
    }

    val runs = 100
    println(s"Testing $runs times for all composite numbers between 3 and 7919")
    val solovayResults = calcResults(solovayStrassenTest _, runs)
    println(s"Solovay times Prime: mean ${solovayResults._1}, stddev ${solovayResults._2}")
    val millerResults = calcResults(millerRabinTest _, runs)
    println(s"Miller times Prime: mean ${millerResults._1}, stddev ${millerResults._2}")
  }

  def mean(items:Traversable[Int]) = {
    items.sum / items.size.toDouble
  }

  def sample_stddev(items:Traversable[Int]) = {
    val meanCalc = mean(items)
    Math.sqrt(
      (1/(items.size.toDouble-1)) * items.foldLeft(0.0) {
        (sum : Double, item) => sum + Math.pow(item - meanCalc, 2)
      }
    )
  }

  def runAlgo(input: Int, algo: Int => Outcome, runTimes : Int) : Int = {
    (for(i <- 1 to runTimes) yield {
      algo(input)
    }).foldLeft(0) {
      case (primeTimes, ProbablyPrime) => primeTimes + 1
      case (primeTimes, _) => primeTimes
    }
  }

}
