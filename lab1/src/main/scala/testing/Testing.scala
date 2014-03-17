import randomized.Primality._
import scala.io.Source

object Testing {

  def accuracyTest = {
    def calcAccuracy(primeTester: Int => Outcome, runs: Int) = {
      val results : Set[(Int,Int)] = numbers.map((number) => (number, outcomesAlgo(number, primeTester, runs)))
      val tmp = results.map(_._2)
      (mean(tmp), sample_stddev(tmp))
    }

    val runs = 100
    println(s"Testing accuracy $runs times for all composite numbers between 3 and 7919")
    val solovayResults = calcAccuracy(solovayStrassenTest _, runs)
    println(s"Solovay times Prime: mean ${solovayResults._1}, stddev ${solovayResults._2}")
    val millerResults = calcAccuracy(millerRabinTest _, runs)
    println(s"Miller times Prime: mean ${millerResults._1}, stddev ${millerResults._2}")
  }

  def runTimeTest = {
    val logInput = for(i <- 0.5 to 6.0 by 0.1) yield Math.round(Math.pow(10, i)).toInt // They need to be whole numbers

    def calcRunTime(primeTester: Int => Outcome, runs: Int) : Map[Int, (Double, Double)] = {
      // Can you do this better? Create a Map with the input to the results.
      (logInput zip logInput
      .map(timeAlgo( _, primeTester, runs))
      .map {
          (runTimes) => (mean(runTimes), sample_stddev(runTimes))
      }).toMap
    }

    val runs = 100
    println(s"Testing runtime $runs times for logarithmically scaled inputs between 10^(0.5) to 10^6")
    val solovayResults = calcRunTime(solovayStrassenTest _, runs)
    println(s"Solovay runTimes: $solovayResults")
    val millerResults = calcRunTime(millerRabinTest _, runs)
    println(s"Miller runTimes: $millerResults")
  }

  /**
   * Create a list of Composite numbers, that are not even.
   */
  def numbers = {
    val lines = Source.fromFile("src/main/resources/primesBelow1000.txt").getLines()
    val primes = (for(line <- lines) yield line.toInt).toSet
    (
      for(i <- 3 to 7919
          if i%2 != 0 // Skip even numbers
      ) yield i).toSet -- primes
  }

  def mean[T](items:Traversable[T])(implicit num: Numeric[T]) = {
    num.toDouble(items.sum) / items.size
  }

  def sample_stddev[T](items:Traversable[T])(implicit num: Numeric[T]) = {
    val meanCalc = mean(items)
    Math.sqrt(
      (1/(items.size.toDouble-1)) * items.foldLeft(0.0) {
        (sum : Double, item) => sum + Math.pow(num.toDouble(item) - meanCalc, 2)
      }
    )
  }

  def outcomesAlgo(input: Int, algo: Int => Outcome, times : Int) : Int = {
    (for(i <- 1 to times) yield {
      algo(input)
    }).foldLeft(0) {
      case (primeTimes, ProbablyPrime) => primeTimes + 1
      case (primeTimes, _) => primeTimes
    }
  }
  
  def timeAlgo(input: Int, algo: Int => Outcome, times: Int) : Seq[Long] = {
    for(i <- 1 to times) yield {
      val startTime = System.nanoTime
      algo(input)
      System.nanoTime - startTime
    }
  }

}
