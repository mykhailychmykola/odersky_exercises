import scala.annotation.tailrec

object rational {

  val x = new Rationals(1, 2)
  val y = new Rationals(3, 4)
  val z = new Rationals(3, 2)
  x.number
  x.denom
  (x + y).neg.makeString
  x.subtract(y).subtract(z).makeString
  (y + y).makeString
  x < y
  x.subtract(y).makeString
  x.max(y).makeString


  def Sum(f: Int => Double, a: Int, b: Int): Double = {
    if (a > b) 0
    else f(a) + Sum(f, a + 1, b)

  }
  def Cube(x: Int): Double = math.pow(x, 3)

  Sum(Cube, 1, 3)


  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else  f(a) * product(f)(a+1, b)

  product(x => x)(1, 4)

  def factorial(x: Int): Int = product(x => x)(1, x)

  factorial(6)


  def generalised_func(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), generalised_func(f, combine, zero)(a+1, b))

  class Rationals(x: Int, y: Int) {
    require(y>0, "denominator must be positive!")

    def this(x:Int) = this(x, 1)

    @tailrec
    private def gcd(a: Int, b: Int): Int = if (b==0) a else gcd(b, a%b)
    val number = x
    val denom = y

    def + (that: Rationals) =
      new Rationals(
        number * that.denom + that.number * denom,
        denom * that.denom)

    def makeString = {
      val g = gcd(number, denom)
      number / g + "/" + denom / g
    }

    def neg: Rationals = new Rationals(-number, denom)

    def subtract(that: Rationals) = this + that.neg

    def < (that: Rationals): Boolean = number * that.denom < that.number * denom

    def max(that: Rationals): Rationals = if (this < that) that else this



  }

}


