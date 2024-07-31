package net.yoshinorin.fpinscala.errorhandling

import net.yoshinorin.fpinscala.errorhandling.None.{Try, sequece, traverse}
import net.yoshinorin.fpinscala.errorhandling.Option._
import org.scalatest.wordspec.AnyWordSpec

// testOnly net.yoshinorin.fpinscala.errorhandling.OptionSpec
class OptionSpec extends AnyWordSpec {

  "Option" should {

    "mean" in {
      println("\n-------- mean")
      println(mean(Seq(1.2, 1.1, 1.6)))
      println(mean(Seq()))
    }

    "sequence" in {
      println("\n-------- sequence")
      println(sequece(List(Some("aaa"), Some("bbb"), Some("ccc"))))
      println(sequece(List(None)))
    }

    "traverse" in {
      println("\n-------- traverse")
      println(traverse(List(Some("1"), Some("2"), Some("3")))(x => Try(x)))
      // TODO
    }

  }

}
