object abstract_class {
  println("welcome")
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}
class Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x,  new Empty, new Empty)

  def contains(x: Int): Boolean = false
}

case class NonEmpty(x: Int, empty: Empty, empty1: Empty)