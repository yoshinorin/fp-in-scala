package net.yoshinorin.fpinscala.laziness

import net.yoshinorin.fpinscala.laziness.Stream._
import org.scalatest.wordspec.AnyWordSpec

// testOnly net.yoshinorin.fpinscala.laziness.StreamSpec
class StreamSpec extends AnyWordSpec {

  "Option" should {

    "toList" in {
      println("\n-------- toList")
      val s = Stream("a", "b", "c")
      println(s.toList)
    }

    "take" in {
      println("\n-------- take")
      val s = Stream("a", "b", "c", "d", "e")
      println(s.take(2).toList)
    }

    "drop" in {
      println("\n-------- drop")
      val s = Stream("a", "b", "c", "d", "e")
      println(s.drop(3).toList)
    }

    "takeWhile" in {
      println("\n-------- takeWhile")
      val s = Stream("a", "b", "c", "d", "e")
      println(s.takeWhile(x => x != "c").toList)
    }

    "forAll" in {
      println("\n-------- forAll")
      val s = Stream("a", "a", "c", "a", "e")
      println(s.takeWhile(x => x == "a").toList)
    }

    "ones" in {
      println("\n-------- ones")
      val o = Stream.ones.take(5).toList
      println(o)
      val o2 = Stream.ones.forAll(_ != 1)
      println(o2)
      // 終了しない式なのでスタックオーバーフローになる
      // val o2 = Stream.ones.forAll(_ == 1)
    }

    "constant" in {
      println("\n-------- constant")
      val s = Stream()
      println(s.constant("x"))
      println(s.constant("x").take(5).toList)
    }

    "constant unfold" in {
      println("\n-------- constant with unfold")
      val s = Stream()
      println(s.constantWithUnfold("unfold"))
      println(s.constantWithUnfold("unfold").take(5).toList)
    }

    "from" in {
      println("\n-------- from")
      val s = Stream()
      println(s.from(1))
      println(s.from(1).take(10).toList)
    }

    "from unfold" in {
      println("\n-------- from with unfold")
      val s = Stream()
      println(s.fromWithUnfold(3))
      println(s.fromWithUnfold(3).take(10).toList)
    }

    "fibs" in {
      println("\n-------- fibs")
      val s = Stream()
      println(s.fibs().take(10).toList)
    }

    "fibs unfold" in {
      println("\n-------- fibs with unfold")
      val s = Stream()
      println(s.fibsWithUnfold().take(10).toList)
    }

  }

}
