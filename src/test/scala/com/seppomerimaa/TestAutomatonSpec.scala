package com.seppomerimaa

import com.seppomerimaa.Automata.Accept
import com.seppomerimaa.TestAutomatonCompanion.{One, Zero}
import org.scalacheck.{Gen, Prop}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.Checkers
import org.scalacheck.Prop.BooleanOperators

class TestAutomatonSpec extends FlatSpec with Matchers with Checkers {
  val anyInput = Gen.oneOf(Zero, One)
  val evenZeroes = Gen.listOf(Zero).suchThat(zs => zs.size % 2 == 0)
  val acceptableInputs = for {
    prefix <- Gen.listOf(anyInput)
    zeroes <- evenZeroes
  } yield prefix ++ List(One) ++ zeroes ++ List(One)

  it should "accept any input that ends with a 1, an even number of 0s, and another 1" in {
    check(Prop.forAll(acceptableInputs) { is =>
      (TestAutomaton.result(is) == Accept) :| s"is: $is"
    })
  }
}
