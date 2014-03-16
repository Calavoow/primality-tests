package randomized

import scala.util.Random

object Primality {
  sealed trait Outcomes
  case object ProbablyPrime extends Outcomes
  case class Composite(certificate: Int) extends Outcomes

  /**
   * Using the Solovay-Strassen algorithm, try to find out whether n is a Prime or a Composite.
   *
   * @param n
   * @return
   */
  def solovayStrassen(n : Int) : Outcomes = {
    if(n % 2 == 0) Composite(2)
    else {
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
   * Calculate the greatest common divisor.
   *
   * @param a
   * @param b
   * @return
   */
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
  def powMod(base: Double, pow: Double, mod: Int) : Double = powMod(base, pow, mod, 1)

  /**
   * Tail recursive implementation of the powMod function.
   */
  private def powMod(base: Double, pow: Double, mod: Int, accum: Double) : Double = {
    pow match {
      case 0 => accum
      case _ => powMod(base, pow-1, mod, (base * accum) % mod)
    }
  }

  /**
   * Calculate the Jacobi symbol.
   *
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
