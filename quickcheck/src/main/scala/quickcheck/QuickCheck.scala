package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
      v <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a1: Int, a2: Int) =>
    val h = insert(a2, insert(a1, empty))
    findMin(h) == a1.min(a2)
  }

  property("del1") = forAll { a: Int =>
    deleteMin(insert(a, empty)) == empty
  }

  property("arrMin") = forAll { h: H =>
    def arrangeMin(heap: H): List[Int] = {
      if (isEmpty(heap)) Nil
      else findMin(heap) :: arrangeMin(deleteMin(heap))
    }
    val xs = arrangeMin(h)
    xs == xs.sorted
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == findMin(h1).min(findMin(h2))
  }

  property("meld move") = forAll { (h1: H, h2: H) =>
    def arrangeMin(heap: H): List[Int] = {
      if (isEmpty(heap)) Nil
      else findMin(heap) :: arrangeMin(deleteMin(heap))
    }

    val min1 = findMin(h1)
    val meld1 = meld(h1, h2)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    val xs1 = arrangeMin(meld1)
    val xs2 = arrangeMin(meld2)
    xs1 == xs2
  }
}
