package individual

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

  def toList(range: Seq[Double]): List[Double] = range.toList.map(functionCi orElse (x => -1))
}

object Individual extends App {
  val res = Func.toList((-250 to 250).toSeq.map(_.toDouble))
  println(res)

  println(res.groupBy(_ % 2 == 0))
  println(res.partition(_ > 1000))
  println(res.scanLeft(0.0)((a: Double, b: Double) => (a + b) / 2))
  println(res.segmentLength(_ == -1.0))
  println(res.span(_ < 0))
}
