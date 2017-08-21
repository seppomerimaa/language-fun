package com.seppomerimaa

import com.seppomerimaa.Automata.{Accepting, Rejecting, State}
import com.seppomerimaa.DeterministicFiniteAutomaton.Transition
import com.seppomerimaa.TestAutomatonCompanion.{Input, One, Zero}

sealed trait OEState extends State
case object Odd extends OEState with Rejecting
case object Even extends OEState with Accepting

/**
  * Accepts inputs that add up to an even number.
  */
object EvenAcceptor extends DeterministicFiniteAutomaton[OEState, Input] {
  val start = Even
  def transition: Transition[OEState, Input] = {
    case (Even, Zero) => Even
    case (Even, One) => Odd
    case (Odd, Zero) => Odd
    case (Odd, One) => Even
  }
}
