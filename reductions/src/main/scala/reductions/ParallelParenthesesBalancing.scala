package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 10, //40,
    Key.exec.maxWarmupRuns -> 20, //80,
    Key.exec.benchRuns -> 20, //120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000 //100000000
    val chars = new Array[Char](length)
    val threshold = 100 //10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceHelper(remainingChars: Array[Char], currentCount: Int): Boolean = remainingChars match {
        case _ if currentCount < 0 => false
        case arr if arr.isEmpty => currentCount == 0
        case arr => balanceHelper(arr.tail, if (arr.head == '(') currentCount + 1 else if (arr.head == ')') currentCount - 1 else currentCount)
      }

    balanceHelper(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx < until) {
        val (left, right) = chars(idx) match {
          case '(' => (arg1 + 1, arg2)
          case ')' => if (arg1 > 0) (arg1 - 1, arg2) else (arg1, arg2 + 1)
          case _ => (arg1, arg2)
        }
        traverse(idx + 1, until, left, right)
      } else {
        (arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      val size = until - from
      if (size <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + size / 2
        val ((l1, l2), (r1, r2)) = parallel(reduce(from, mid), reduce(mid, until))
        if (l1 > r2) (l1 + r1 - r2, l2) else (r1, r2 - l1 + l2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!
}
