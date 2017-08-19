package com.seppomerimaa

import com.seppomerimaa.Automata.{Accepting, Rejecting, State}
import com.seppomerimaa.DeterministicFiniteAutomaton.Transition

sealed trait MyState extends State
case object q1 extends MyState with Rejecting
case object q2 extends MyState with Accepting
case object q3 extends MyState with Rejecting

sealed trait Input
case object Zero extends Input
case object One extends Input

/**
  * Accepts any input that ends with a One, and even numbers of Zeroes, and a final One.
  */
object TestAutomaton extends DeterministicFiniteAutomaton[MyState, Input] {
  val start = q1

  def transition: Transition[MyState, Input] = (s, i) => (s, i) match {
    case (q1, Zero) => q1
    case (q1, One) => q2
    case (q2, Zero) => q3
    case (q2, One) => q2
    case (q3, Zero) => q2
    case (q3, One) => q2
  }
}
