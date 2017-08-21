package com.seppomerimaa

import com.seppomerimaa.Automata.{Accept, Reject}
import com.seppomerimaa.TestAutomatonCompanion.{One, Zero}
import org.scalacheck.{Gen, Prop}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.Checkers
import org.scalacheck.Prop.BooleanOperators

/**
  *
  */
class ThirdFromLastSpec extends FlatSpec with Matchers with Checkers {
  val input = Gen.oneOf(Zero, One)
  val suffixes = Gen.listOfN(2, input).suchThat(_.size == 2)
  val prefixes = Gen.listOf(input).suchThat(_.size < 50) // Don't let it get too long b/c we'll OOM

  // These generators can both be slightly flakey in that they themselves will die if they fail to generate something
  // that satisfies that .suchThat(...) conditional after a certain number of attempts. Sigh.
  val acceptable = (for {
    p <- prefixes
    s <- suffixes
  } yield {
    (p :+ One) ++ s
  }).suchThat(_.size >= 3)

  val rejectable = Gen.listOf(input)
    .suchThat { is =>
      is.size < 50 &&
      (is.size < 3 || is(is.size - 3) == Zero)
    }

  it should "accept all inputs where the third to last input is One" in {
    check(Prop.forAll(acceptable) { is =>
      val lastState = ThirdFromLast.run(is)
      val result = ThirdFromLast.result(is)
      (ThirdFromLast.result(is) == Accept) :| s"inputs: $is last states: $lastState result: $result"
    })
  }

  it should "reject all inputs where the third to last input is Zero" in {
    check(Prop.forAll(rejectable) { is =>
      val lastState = ThirdFromLast.run(is)
      val result = ThirdFromLast.result(is)
      (ThirdFromLast.result(is) == Reject) :| s"inputs: $is last states: $lastState result: $result"
    })
  }

  "foo" should "bar" in {
    val lastState = ThirdFromLast.run(List(One, Zero))
    println(s"$lastState")
  }
}
