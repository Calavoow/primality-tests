package randomized

import scala.util.Random
import scala.annotation.tailrec

object Primality {
  sealed trait Outcomes
  case object ProbablyPrime extends Outcomes
  case class Composite(certificate: Int) extends Outcomes

  /**
   * Try k iterations of a given test with appropriately ranged
   * random values
   */
  def test(n: Int, k: Int = 100, f: Int => Outcomes): Outcomes = {
    assert(k>0, "You should atleast run the primality test once...")

    // see if we can find a certificate of compositeness
    for(i <- Range(0, k)) {
      f(n) match {
        case c:Composite => return c
        case _ =>
      }
    }

    // if the loop did not find any counter examples
    Primality.ProbablyPrime
  }

  /**
   * Using the Solovay-Strassen algorithm, try to find out whether n is a Prime or a Composite.
   *
   */
  def solovayStrassenTest(n : Int) : Outcomes = {
    if(n % 2 == 0) Composite(2)
    else {
      // TODO: fails for n < 3
      val a = Random.nextInt(n-2) + 2 // a in [2,n-1]
      if(gcd(a,n) != 1) Composite(a)
      else{
        val unroundedPow = powMod(a, (n-1)/2, n)
        assert(!unroundedPow.isNaN, "Input too large")
        val pow = unroundedPow.toInt

        val jac = (jacobi(a,n) + n) % n // Make sure to modulo it to a positive number (in case it's -1)
        if(jac == pow) ProbablyPrime
        else Composite(a)
      }
    }
  }

  /**
   * Test if randomly chosen a can be used to find a
   * proof that n is composite
   */
  def millerRabinTest(n: Int): Outcomes = {
    if(n % 2 == 0) Composite(2)
    else {
      val a = Random.nextInt(n-3) + 2 // random in range [2, n-2]

      // factor n as 2^s*d
      val (s, d) = millerRabinFactors(n-1)
      var x = powMod(a, d, n).toInt

      if(x == 1 || x == n - 1) {
        ProbablyPrime
      } else {
        // try s - 1 times
        for(i <- Range(1, s)) {
          x = powMod(x, 2, n).toInt

          // we might find proof
          if(x == 1) return Composite(a)
          // we might run out of tries
          else if(x == n-1) return ProbablyPrime
          // else try next
        }

        Composite(a)
      }
    }
  }

  def millerRabinFactors(n: Int): (Int, Int) = {
    @tailrec
    def rec_factor(k: Int, s: Int): (Int,Int) = {
      // if k is uneven we're done
      if(k % 2 == 1) (s, k)
      // else we factor out a 2, and recurse
      else rec_factor(k/2, s+1)
    }

    rec_factor(n,0)
  }

  /**
   * Calculate the greatest common divisor.
   *
   * @param a
   * @param b
   * @return
   */
  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)

  /**
   * Raise a number to the power, using module every multiplication.
   *
   * This can be used to prevent numbers from becoming too big (NaN for doubles).
   * Example: b^p % m becomes powMod(b,p,m)
   *
   * @param base The base of the power
   * @param pow The power to raise the base to
   * @param mod The modulus to use.
   * @return The number raised to the power, with the given modulus.
   */
  def powMod(base: Double, pow: Double, mod: Int) : Double = {
    @tailrec
    def powMod(base: Double, pow: Double, mod: Int, accum: Double) : Double = {
      pow match {
        case 0 => accum
        case _ => powMod(base, pow-1, mod, (base * accum) % mod)
      }
    }

    powMod(base, pow, mod, 1)
  }

  /**
   * Calculate the Jacobi symbol.
   *    ing time of this algorithm is O(k log3n), where k is the number of different values of a that we test; thus this is an efficient, polynomial-time algorithm. FFT-based multiplication can push the running time down to O(k log2n log log n log log log n) = Õ(k log2n).
   * TODO: Maybe find a better source?
   * http://cryptocode.wordpress.com/2008/08/16/jacobi-symbol/
   *
   * @param a
   * @param n
   * @return
   */
  def jacobi(a: Int, n:Int) : Int = {
    assert(n%2 == 1, "Jacobi is undefined for even n")

    if(a==0){
      if(n==1) 1
      else 0
    } else if(a==2) {
      n%8 match{
        case 1 | 7 => 1
        case 3 | 5 => -1
      }
    } else if( a>= n ) jacobi(a%n, n)
    else if( a%2 == 0 ) jacobi(2,n) * jacobi(a/2, n)
    else {
      if( a%4 == 3 && n%4 == 3) -jacobi(n,a)
      else jacobi(n,a)
    }
  }
}
