package dev.matthewbrown

import cats.effect.IO
import cats.effect.testing.specs2.CatsEffect
import org.specs2.mutable.Specification

class ExampleSpec extends Specification with CatsEffect {
  "examples" should {
    "say hello" in {
      IO.pure("").map(_ === "Hello Cats!")
    }
  }
}
