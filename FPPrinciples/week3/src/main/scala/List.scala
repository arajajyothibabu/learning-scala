/**
  * Created by jyothi on 19/9/17.
  */
trait List[T] {

  def isEmpty: Boolean

  def head: T

  def tail: List[T]

}

class Nil[T] extends List[T] {

  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException()

  def tail: Nothing = throw new NoSuchElementException()

}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {

  def isEmpty: Boolean = false

}
