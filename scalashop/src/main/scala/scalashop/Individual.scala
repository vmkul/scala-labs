package scalashop
import scala.concurrent.Future
import scala.util.{Success, Failure}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.parallel.CollectionConverters._

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

  def calcInterval(from: Int, end: Int): List[Double] = {
    var res = List.range(from, end + 1)
    res.map(x => functionCi(x))
  }

  def parCalc(numTasks: Int): List[Double] = {
    val r = -250 until 250 + 1 by (250/ numTasks).ceil.toInt
    val partitionPoints = r.zip(r.tail)

    val tasks = partitionPoints.map(p => task { calcInterval(p._1, p._2) })
    val res = tasks.flatMap { _.join() }

    res.toList
  }
}

object Individual extends App {
  import Func._
  
  val intervalCount: Future[List[Double]] = Future {
    parCalc(10)
  }

  val parCollection: Future[List[Double]] = Future {
    List.range(-250, 250 + 1).par.map(x => functionCi(x)).toList
  }

  val futurePair: Future[(List[Double], List[Double])] = intervalCount.zip(parCollection)

  futurePair onComplete {
    case Success(res) => {
      println("Done computing!")
      println(res._1)
      println(res._2)
    }
    case Failure(t) => println("An error has occurred: " + t.getMessage)
  }
}
