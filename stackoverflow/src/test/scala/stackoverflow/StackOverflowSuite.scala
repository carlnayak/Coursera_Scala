package stackoverflow

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  def getResult(lang: String, scores: String, sc: SparkContext) = {
    def langIdx(lang: String): Int = {
      StackOverflow.langs.indexOf(lang)
    }
    val langCoord = StackOverflow.langSpread * langIdx(lang)
    val iScores = scores.split(",").map(x => x.toInt)

    val vectors = sc.parallelize(iScores.map((langCoord, _)))
    val means = Array((langCoord, iScores.sum / iScores.size))
    StackOverflow.clusterResults(means, vectors)
  }

  test("clusterResults - Groovy-19") {
    val lang = "Groovy"
    val scores = "23,15,13,13,76,26,16,27"
    val sc = StackOverflow.sc
    val results = getResult(lang, scores, sc)
    StackOverflow.printResults(results)
    assert(results.exists(_ == ("Groovy", 100, 8, 19)))
  }

  test("clusterResults - PHP-34") {
    val lang = "PHP"
    val scores = "22,22,57,37,30,30,83,44,28,25,37,26,22,26,24,24,22,39,37,49,75,25,37,46,46,40,133,30,29,40,41,22,24," +
                 "29,33,24,65,55,42,30,22,25,45,61,23,35,67,34,94,24,71,59,50,36,36,36,36,25,39,51,22,49,161,24,54,54," +
                 "24,176,28,36,28,23,27,26,63,137,104,23,44,22,24,26,35,32,175,50,36,25,27,23,27,32,40,46,32,32,61,22," +
                 "78,23,22,28,62,62,36,63,81,38,73,23,27,89,51,24,30,28,22,23,33,27,27,55,23,30,29,59,102,46,96,42,37," +
                 "36,39,194,48,22,24,33,27,48,64,24,28,23,141,31,57,50,23,23,22,108,84,37,22,105,26,29,34,22"
    val sc = StackOverflow.sc
    val results = getResult(lang, scores, sc)
    StackOverflow.printResults(results)
    assert(results.exists(_ == ("PHP", 100, 160, 34)))
  }
}
