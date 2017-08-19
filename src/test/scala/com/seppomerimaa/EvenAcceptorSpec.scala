package com.seppomerimaa

import com.seppomerimaa.Automata.{Accept, Reject}
import org.scalacheck.{Gen, Prop}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.Checkers
import org.scalacheck.Prop.BooleanOperators

class EvenAcceptorSpec extends FlatSpec with Matchers with Checkers {
  val inputs = Gen.listOf(Gen.oneOf(Zero, One))
  val evenInputs = inputs.suchThat { is =>
    is.map {
      case Zero => 0
      case One => 1
    }.sum % 2 == 0
  }
  val oddInputs = inputs.suchThat { is =>
    is.map {
      case Zero => 0
      case One => 1
    }.sum % 2 == 1
  }

  it should "accept inputs that add up to an even number" in {
    check(Prop.forAll(evenInputs) { is =>
      (EvenAcceptor.result(is) == Accept) :| s"inputs: $is"
    })
  }

  it should "reject inputs that add up to an odd number" in {
    check(Prop.forAll(oddInputs) { is =>
      (EvenAcceptor.result(is) == Reject) :| s"inputs: $is"
    })
  }
}
