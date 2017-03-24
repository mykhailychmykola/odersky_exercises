import scala.annotation.tailrec

object sqrt_newton{
  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x))
        guess else sqrtIter(improveGuess(guess, x), x)


    def isGoodEnough(guess: Double, x: Double): Boolean =
      abs(guess*guess - x) / x < 0.001


    def abs(x: Double): Double = if (x >= 0) x else -x


    def improveGuess(guess: Double, x: Double): Double =
      (guess + x / guess) / 2

    sqrtIter(1.0, x)
  }


  @tailrec
  def gcd(x: Int, y: Int): Int = {
    if (y == 0) x else gcd(y, x % y)
  }
  def factorial(x: Int): Int = {
    if (x == 0) 1 else x * factorial(x - 1)
  }
  def improved_factorial(n: Int): Any = {
    @tailrec
    def loop(aws: Int, n: Int): Int =
      if (n == 0) aws
      else loop(aws * n, n - 1)

    loop(1, n)
  }

  improved_factorial(2)

}