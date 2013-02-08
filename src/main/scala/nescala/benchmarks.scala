package nescala

import scala.{specialized => spec}
import scala.annotation.tailrec
import scala.util.Random._
import com.google.caliper.Param

case class III(a: Int, b: Int, c: Int)

sealed trait IIIList {
  def a: Int
  def b: Int
  def c: Int
  def tail: IIIList
}

case object IIINil extends IIIList {
  def a = sys.error("die")
  def b = sys.error("die")
  def c = sys.error("die")
  def tail = sys.error("die")
}

case class IIICons(a: Int, b: Int, c: Int, tail: IIIList) extends IIIList

sealed trait I6List {
  def a1: Int
  def a2: Int
  def b1: Int
  def b2: Int
  def c1: Int
  def c2: Int
  def tail: I6List
  def prepend(a: Int, b: Int, c: Int): I6List = I6Cons(a, b, c, this)
}

case object I6Nil extends I6List {
  def a1: Int = sys.error("argh")
  def a2: Int = sys.error("argh")
  def b1: Int = sys.error("argh")
  def b2: Int = sys.error("argh")
  def c1: Int = sys.error("argh")
  def c2: Int = sys.error("argh")
  def tail: I6List = sys.error("argh")
}

case class I6Cons(a1: Int, b1: Int, c1: Int, tail: I6List) extends I6List {
  def a2: Int = sys.error("argh")
  def b2: Int = sys.error("argh")
  def c2: Int = sys.error("argh")
  override def prepend(a: Int, b: Int, c: Int): I6List = I6ConsCons(a, b, c, a1, b1, c1, tail)
}

case class I6ConsCons(a1: Int, b1: Int, c1: Int, a2: Int, b2: Int, c2: Int, tail: I6List) extends I6List

object ConsBenchmarks extends MyRunner(classOf[ConsBenchmarks])

class ConsBenchmarks extends MyBenchmark {
  @Param(Array("8", "11", "14", "17"))
  var pow:Int = 0

  var as: Array[Int] = _
  var bs: Array[Int] = _
  var cs: Array[Int] = _

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt

    as = init(n)(nextInt)
    bs = init(n)(nextInt)
    cs = init(n)(nextInt)
  }

  def timeListOfTuple3(reps: Int) = run(reps) {
    var i = 0
    var d = List.empty[(Int, Int, Int)]
    val n = as.length
    while (i < n) {
      d = (as(i), bs(i), cs(i)) :: d
      i += 1
    }
  }

  def timeListOfIII(reps: Int) = run(reps) {
    var i = 0
    var d = List.empty[III]
    val n = as.length
    while (i < n) {
      d = III(as(i), bs(i), cs(i)) :: d
      i += 1
    }
  }

  def timeIIIList(reps: Int) = run(reps) {
    var i = 0
    var d: IIIList = IIINil
    val n = as.length
    while (i < n) {
      d = IIICons(as(i), bs(i), cs(i), d)
      i += 1
    }
  }

  def timeI6List(reps: Int) = run(reps) {
    var i = 0
    var d: I6List = I6Nil
    val n = as.length
    while (i < n) {
      d = d.prepend(as(i), bs(i), cs(i))
      i += 1
    }
  }

  def timeI6ListFaster(reps: Int) = run(reps) {
    var i = 0
    var d: I6List = I6Nil
    val n = as.length
    while (i < n) {
      d = I6ConsCons(as(i + 1), bs(i + 1), cs(i + 1), as(i), bs(i), cs(i), d)
      i += 2
    }
  }
}

object FoldBenchmarks extends MyRunner(classOf[FoldBenchmarks])

class FoldBenchmarks extends MyBenchmark {
  @Param(Array("8", "11", "14", "17"))
  var pow:Int = 0

  var as: Array[Int] = _

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt
    as = init(n)(nextInt)
  }

  def folder2[@spec(Int) A, @spec(Int) B, @spec(Int) C](arr: Array[A])(b: B, c: C)(f1: (B, A) => B)(f2: (C, A) => C): (B, C) = {
    var state1 = b
    var state2 = c
    var i = 0
    val len = arr.length
    while (i < len) {
      val a = arr(i)
      state1 = f1(state1, a)
      state2 = f2(state2, a)
      i += 1
    }
    (state1, state2)
  }

  import scala.math.{min, max}

  def timeFoldLeft(reps: Int) = run(reps) {
    val a = as(0)
    as.foldLeft((a, a))((tpl, a) => (min(tpl._1, a), max(tpl._2, a)))
  }

  def timeFolder2(reps: Int) = run(reps) {
    val a = as(0)
    folder2(as)(a, a)((amin, a) => min(amin, a))((amax, a) => max(amax, a))
  }

  def timeManual(reps: Int) = run(reps) {
    val a = as(0)
    var amin = a
    var amax = a
    var i = 0
    val len = as.length
    while (i < len) {
      val a = as(i)
      amin = min(amin, a)
      amax = max(amax, a)
      i += 1
    }
    (amin, amax)
  }
}

case class Point(x: Double, y: Double)

case class Points(xs: Array[Double], ys: Array[Double])

case class InterleavedPoints(xs: Array[Double], ys: Array[Double])

object PointBenchmarks extends MyRunner(classOf[PointBenchmarks])

class PointBenchmarks extends MyBenchmark {
  @Param(Array("8", "11", "14", "17", "20"))
  var pow: Int = 0
  var n: Int = 0

  var ps: Array[Point] = _

  var xs: Array[Double] = _
  var ys: Array[Double] = _

  var xys: Array[Double] = _

  override protected def setUp() {
    n = scala.math.pow(2, pow).toInt
    xs = init(n)(nextDouble)
    ys = init(n)(nextDouble)

    ps = new Array[Point](n)
    xys = new Array[Double](n * 2)

    var i = 0
    while (i < n) {
      ps(i) = Point(xs(i), ys(i))
      xys(i * 2) = xs(i)
      xys(i * 2 + 1) = ys(i)
      i += 1
    }
  }

  import scala.math.sqrt

  def timePointArrayDistance(reps: Int) = run(reps) {
    val len = n
    val ps2 = new Array[Point](len)
    var i = 0
    while (i < len) {
      val p = ps(i)
      val x = p.x
      val y = p.y
      val s = sqrt(x * x + y * y)
      ps2(i) = Point(x / s, y / s)
      i += 1
    }
  }

  def timePoints(reps: Int) = run(reps) {
    val len = n
    val xs2 = new Array[Double](len)
    val ys2 = new Array[Double](len)
    var i = 0
    while (i < len) {
      val x = xs(i)
      val y = ys(i)
      val s = sqrt(x * x + y * y)
      xs2(i) = x / s
      ys2(i) = y / s
      i += 1
    }
  }

  def timeInterleavedPoints(reps: Int) = run(reps) {
    val len = n
    val len2 = n * 2
    val xys2 = new Array[Double](len2)
    var i = 0
    while (i < len2) {
      val x = xys(i)
      val y = xys(i + 1)
      val s = sqrt(x * x + y * y)
      xys2(i) = x
      xys2(i + 1) = y
      i += 2
    }
  }
}
