/**
  * Created by jyothi on 19/9/17.
  */
abstract class IntSet {

  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet

}

class Empty extends IntSet {

  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)

  def contains(x: Int): Boolean = false

  def union(other: IntSet): IntSet = other

}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def incl(x: Int): IntSet =
    if(x < elem) new NonEmpty(elem, left incl x, right)
    else if(x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def contains(x: Int): Boolean =
    if(x < elem) left contains  x
    else if(x > elem) right contains  x
    else true

  def union(other: IntSet): IntSet = ((left union right) union other) incl elem

}
