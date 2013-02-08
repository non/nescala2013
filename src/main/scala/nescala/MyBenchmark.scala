package nescala

import com.google.caliper.{Benchmark, Param, SimpleBenchmark, Runner}

import scala.reflect.ClassTag

/**
 * Objects extending Runner will inherit a Caliper-compatible main method.
 */
abstract class MyRunner(cls:java.lang.Class[_ <: Benchmark]) {
  def main(args:Array[String]): Unit = Runner.main(cls, args:_*)
}

/**
 * Extend Benchmark to gain some nice convenience methods.
 */
trait MyBenchmark extends SimpleBenchmark {
  // Sugar for building arrays using a per-cell init function.
  def init[A:ClassTag](size:Int)(init: => A) = {
    val data = Array.ofDim[A](size)
    for (i <- 0 until size) data(i) = init
    data
  }

  // Sugar to run 'f' for 'reps' number of times.
  def run(reps:Int)(f: => Unit) = for(i <- 0 until reps)(f)
}
