package com.seppomerimaa

import com.seppomerimaa.Automata.{Accepting, Rejecting, State}
import com.seppomerimaa.DeterministicFiniteAutomaton.Transition
import TestAutomatonCompanion._

object TestAutomatonCompanion {
  sealed trait MyState extends State
  case object Q1 extends MyState with Rejecting
  case object Q2 extends MyState with Accepting
  case object Q3 extends MyState with Rejecting

  sealed trait Input
  case object Zero extends Input
  case object One extends Input
}

/**
  * Accepts any input that ends with a One, and even numbers of Zeroes, and a final One.
  */
object TestAutomaton extends DeterministicFiniteAutomaton[MyState, Input] {
  val start = Q1

  def transition: Transition[MyState, Input] = {
    case (Q1, Zero) => Q1
    case (Q1, One) => Q2
    case (Q2, Zero) => Q3
    case (Q2, One) => Q2
    case (Q3, Zero) => Q2
    case (Q3, One) => Q2
  }
}
