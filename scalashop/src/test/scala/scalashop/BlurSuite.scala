package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {
  def getRandomInt(start: Int, end: Int): Int = {
    val rnd = new scala.util.Random
    start + rnd.nextInt( (end - start) + 1 )  
  }
  
  def getRandomColor(): RGBA = {
    rgba(getRandomInt(0, 255), getRandomInt(0, 255), getRandomInt(0, 255), getRandomInt(0, 255))
  }

  class MockImg(width: Int, height: Int, val data: Array[RGBA]) extends Img(width, height) {
    def this(w: Int, h: Int) = {
      this(w, h, new Array(w * h))
      for (i <- 0 until data.length)
        data(i) = getRandomColor()
    }

    override def apply(x: Int, y: Int): RGBA = data(y * width + x)

    var updateCount = Array.fill[Int](width * height)(0)
    override def update(x: Int, y: Int, c: RGBA): Unit = {
      data(y * width + x) = c
      updateCount(y * width + x) += 1
    }
  }

  def taintedBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    var sum = 0
    var pointCount = 0

    var i = x - radius
    while (i <= x + radius) {
      var j = y - radius
      while (j <= y + radius) {
        val pixel = src(clamp(i, 0, src.width - 1), clamp(j, 0, src.height - 1))

        sum = sum + pixel
        pointCount = pointCount + 1
        j = j + 1
      }
      i = i + 1
    }
    
    sum / pointCount
  }

  @Test def `HorizontalBoxBlu / VerticalBoxBlur parBlur with 32 tasks should modify each pixel of the destination 32x32 image exactly once`(): Unit = {
    val src = new MockImg(32, 32)
    val dst1 = new MockImg(32, 32)
    val dst2 = new MockImg(32, 32)

    HorizontalBoxBlur.parBlur(src, dst1, 32, 1)
    VerticalBoxBlur.parBlur(src, dst2, 32, 1)

    assert(dst1.updateCount.forall(_ == 1))
    assert(dst2.updateCount.forall(_ == 1))
  }

  @Test def `HorizontalBoxBlu / VerticalBoxBlur with radius 1 and 4 tasks should correctly blur the entire 3x3 image`(): Unit = {
    val src = new MockImg(3, 3)
    val correct = new MockImg(3, 3)
    val dst1 = new MockImg(3, 3)
    val dst2 = new MockImg(3, 3)

    println(boxBlurKernel(src, 0, 0, 1))
    HorizontalBoxBlur.parBlur(src, dst1, 4, 1)
    VerticalBoxBlur.parBlur(src, dst2, 4, 1)
    for (
      x <- 0 until 3;
      y <- 0 until 3
    ) yield correct.update(x, y, boxBlurKernel(src, x, y, 1))

    assert(dst1.data.sameElements(correct.data))
    assert(dst2.data.sameElements(correct.data))
  }

  @Test def `boxBlurKernel should return the correct value on an interior pixel of a 3x4 image with radius 1`(): Unit = {
    val src = new MockImg(3, 4)
    val dst = new MockImg(3, 4)
    val color = getRandomColor

    for (
      x <- 0 until 3;
      y <- 0 until 4
    ) yield src.update(x, y, color)

    for (
      x <- 0 until 3;
      y <- 0 until 4
    ) yield dst.update(x, y, boxBlurKernel(src, x, y, 1))

    assert(dst.data.forall(_ == color))
  }

  @Test def `boxBlurKernel should correctly handle radius 0`(): Unit = {
    val src = new MockImg(3, 4)
    val dst = new MockImg(3, 4)

    for (
      x <- 0 until 3;
      y <- 0 until 4
    ) yield dst.update(x, y, boxBlurKernel(src, x, y, 0))

    assert(dst.data.sameElements(src.data))
  }

  @Test def `boxBlurKernel should compute the averages of red, blue, green and alpha channels separately`(): Unit = {
    val src = new MockImg(3, 4)
    val correct = new MockImg(3, 4)
    val tainted = new MockImg(3, 4)

    for (
      x <- 0 until 3;
      y <- 0 until 4
    ) yield correct.update(x, y, boxBlurKernel(src, x, y, 1))

    for (
      x <- 0 until 3;
      y <- 0 until 4
    ) yield tainted.update(x, y, taintedBlurKernel(src, x, y, 1))

    assert(!correct.data.sameElements(tainted.data))
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
