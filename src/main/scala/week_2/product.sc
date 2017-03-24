import scala.annotation.tailrec

object exercise {

  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)


  def factorial(n: Int): Int = if (n == 0 ) 1 else  product(x => x)(0, n)

  factorial(0)
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a>b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1, b))


}