import java.lang.instrument.Instrumentation

import scala.util.Random
import sun.instrument.InstrumentationImpl

object Main {
  def main(args: Array[String]): Unit = {
    Test().test()
  }
}

case class Point(name: String, coordinates: List[Int]) {
  override def toString: String = s"$name $coordinates"
}

case class Test() {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println(t1 - t0)
    result
  }

  def test(): Unit = {
    val testScales = List(10, 100, 1000, 10000, 100000, 1000000)
    for (count ← testScales) {
      testRequests(count)
    }
  }

  def testRequests(elemCount: Int): Unit = {
    val dimensions = 3
    val random = new Random()
    val points = for (i ← 0 until elemCount) yield Point(i.toString, List.fill(dimensions)(random.nextInt))
    time {
      testRequestsTimed(points.toList, dimensions, 1000)
    }
  }

  private def testRequestsTimed(points: List[Point], dimensions: Int, requestCount: Int): Unit = {
    val structure = NoStructureRS(points)
    for (i ← 0 until requestCount) {
      val request = 0 until dimensions map (_ ⇒ (Random.nextInt, Random.nextInt))
      structure.processRequest(request.toList)
    }
    println(s"Size of structure ${structure.getSize}")
  }
}

case class NoStructureRS(points: List[Point]) extends RegionSearchStructure {
  private def checkCondition(request: List[(Int, Int)], coordinates: List[Int]): Boolean = {
    val checks = for (i ← coordinates.indices) yield coordinates(i) >= request(i)._1 && coordinates(i) <= request(i)._2
    checks.foldLeft(true)((z, current) ⇒ current && z)
  }

  override def processRequest(request: List[(Int, Int)]): List[Point] = points.filter(point ⇒ checkCondition(request, point.coordinates))

  override def getSize: Int = 0
}

trait RegionSearchStructure {
  def processRequest(request: List[(Int, Int)]): List[Point]

  def getSize: Int
}
