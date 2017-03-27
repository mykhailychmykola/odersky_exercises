package week_4

/**
  * Created by nickolay on 24.03.17.
  */
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{

  override def isEmpty: Boolean = false

  def nth[T](list: List[T], n: Int): T = {
    if (list.isEmpty) throw new IndexOutOfBoundsException("Hey Man!!!")
    else if (n == 0) list.head
    else nth(list.tail, n - 1)
  }

}

class Nil[T] extends List[T]{
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException("Nil.head")

  override def tail: List[T] = throw new NoSuchElementException("Nil.tail")
}

