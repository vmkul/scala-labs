package quickcheck

import org.scalacheck.Properties

import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Test.{check, Result, Failed, PropException}

object Func {
  val functionCi = new PartialFunction[Double, Double] {
    def apply(x: Double) = {
      if (x < 2)
        x
      else
        (1 to 12).toList.map(_ * x).sum
    } 

    def isDefinedAt(x: Double) = x > 1
  }
}


class TotalFunctionSuite extends munit.FunSuite:
  val totalFunction = Func.functionCi.lift

  val propCorrectOption = forAll { (x: Double) =>
    val res = totalFunction(x)
    if (x < 2) {
      res match {
        case Some(b) => false
        case None => true
      }
    } else {
      res match {
        case Some(b) => true
        case None => false
      }
    }
  }

  val propIncreasingFunc = forAll { (a: Int, b: Int) =>
    if (a > b && b > 1) {
      totalFunction(a).get > totalFunction(b).get
    } else true
  }

  test("Total func return value") {
    assert(check(propCorrectOption)(identity).passed)
  }

  test("Total func value grows for bigger argument") {
    assert(check(propIncreasingFunc)(identity).passed)
  }
   