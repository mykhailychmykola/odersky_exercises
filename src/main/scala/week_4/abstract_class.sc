import sun.invoke.empty
import sun.invoke.empty.Empty

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class Empty extends IntSet {
  def incl(x: Int): IntSet = {
    new NonEmpty(x,  new Empty, new Empty)
  }
  def contains(x: Int): Boolean = false

  override def toString = {"."}

  override def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left.incl(x), right)
    else if (x > elem) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  override def contains(x: Int): Boolean = {
    if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true
  }

  override def toString = {"{" + left + elem + right + "}"}

  override def union(other: IntSet): IntSet = (left.union(right).union(other)).incl(elem)
}

object abstract_class {
  def main(args: Array[String]): Unit = {
    val empty1 = new NonEmpty(3, new Empty, new Empty)
    val empty2 = empty1.incl(4)
    empty2.incl(1)
    val empty3 = new NonEmpty(9, new Empty, new Empty)
    empty2.union(empty3)
  }

}