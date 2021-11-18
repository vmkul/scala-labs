package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      h <- genHeap
    } yield insert(x, h)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  def checkSorted(prev: Int, h: H): Boolean = {
    if (isEmpty(h)) true
    else {
      val min = findMin(h)
  
      if (min < prev) false
      else
        checkSorted(min, deleteMin(h))
    }
  }

  def heapEqual(h1: H, h2: H): Boolean = {
    if (isEmpty(h1) && isEmpty(h2)) true
    else if (isEmpty(h1) || isEmpty(h2)) false
    else {
      if (findMin(h1) != findMin(h2)) false
      else
        heapEqual(deleteMin(h1), deleteMin(h2))
    }
  }

  def heapContains[T](h: H, elem: T): Boolean = {
    if (isEmpty(h)) false
    else {
      if (findMin(h) == elem) true
      else
        heapContains(deleteMin(h), elem)
    }
  }

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("Min of two") = forAll { (a: Int, b: Int) =>
    val first = insert(a, empty)
    val second = insert(b, first)
    val min = Math.min(a, b)
    findMin(second) == min
  }

  property("Insert then delete is Empty") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("Sorted seq when deleting min") = forAll { (h: H) =>
    if (isEmpty(h)) true
    else
      checkSorted(findMin(h), h)
  }

  property("Min of melding") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) true
    else
      val minElement = Math.min(findMin(h1), findMin(h2))
      findMin(meld(h1, h2)) == minElement
  }

  property("Add 2, delete 2 is empty") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    val e = deleteMin(deleteMin(h2))

    isEmpty(e)
  }

  property("Min value unchanged after adding bigger value") = forAll { (h: H) =>
    if (isEmpty(h)) true
    else {
      val min = findMin(h)
      if (min == Int.MaxValue) true
      else {
        val h1 = insert(min + 1, h)
        findMin(h1) == min
      }
    }
  }

  property("Equal after delete, insert, meld") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) true
    else {
      val m = meld(h1, h2)
      val first = deleteMin(h1)
      val second = insert(findMin(h1), h2)
      val m1 = meld(first, second)

      heapEqual(m, m1)
    }
  }

  property("Element order same for meld") = forAll { (h1: H, h2: H) =>
    val m1 = meld(h1, h2)
    val m2 = meld(h2, h1)

    heapEqual(m1, m2)
  }

  property("Heap contains not minimal element after insert") = forAll { (h: H, a: Int) =>
    val res = insert(a, h)
    heapContains(res, a)
  }
