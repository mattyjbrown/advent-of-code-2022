package dev.matthewbrown

import cats.effect.{IO, SyncIO}
import munit.CatsEffectSuite

class HelloWorldSuite extends CatsEffectSuite {

  test("test hello world says hi") {
    IO.pure("").map(it => assertEquals(it, "Hello Cats!"))
  }
}
