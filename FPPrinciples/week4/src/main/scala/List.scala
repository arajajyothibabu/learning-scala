/**
  * Created by jyothi on 21/9/17.
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

object List {

  def apply[T]: List[T] = new Nil[T] //List()

  def apply[T](x: T): List[T] = new Cons[T](x, new Nil[T]) //List(1)

  def apply[T](x1: T, x2: T): List[T] = new Cons[T](x1, new Cons[T](x2, new Nil[T])) //List(1, 2)

}
