package net.yoshinorin.fpinscala.errorhandling

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

  }

}
