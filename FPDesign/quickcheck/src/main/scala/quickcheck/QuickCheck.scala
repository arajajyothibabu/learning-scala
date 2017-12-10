package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      i <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    findMin(insert(a, empty)) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOfTwoIsMinInTwo") = forAll { (a: Int, b: Int) =>
    findMin(insert(a, insert(b, empty))) == a.min(b)
  }

  property("deletedMinIsEmptyAgain") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("sortedSequenceByDeletingMins") = forAll { (h: H) =>
    def sortedHeap(heap: H): List[Int] =
      if(isEmpty(heap)) List.empty
      else findMin(heap) :: sortedHeap(deleteMin(heap))

    def isSorted(list: List[Int])(implicit ord: Ordering[Int]): Boolean = list match {
      case Seq() => true
      case Seq(_) => true
      case _ => list.sliding(2).forall { case List(x, y) => x <= y }
    }
    isSorted(sortedHeap(h))
  }


  property("minOfMeldedIsMinOfEither") = forAll { (h1: H, h2: H) =>
    val mH = meld(h1, h2)
    if(isEmpty(mH)) true
    else if(isEmpty(h1))
      findMin(mH) == findMin(h2)
    else if(isEmpty(h2))
      findMin(mH) == findMin(h1)
    else
      findMin(mH) == findMin(h1).min(findMin(h2))
  }

  property("minAfterInsertElementGreaterThanMinShouldRetainAsMin") = forAll { (a: Int) =>
    val b = if (a == Int.MaxValue) a + 1 else a
    val h1 = insert(b + 1, insert(b, insert(b + 2, empty)))
    deleteMin(h1) == insert(b + 2, insert(b + 1, empty))
  }

  property("2InsertionsAnd2DeletionsShouldBeEmpty") = forAll { (a: Int) =>
    !isEmpty(deleteMin(insert(a, insert(a, empty))))
  }

}
