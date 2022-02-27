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
  }

}
