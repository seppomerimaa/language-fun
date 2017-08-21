package com.seppomerimaa

import com.seppomerimaa.Automata.{Accepting, Rejecting, State}
import ThirdFromLastCompanion._
import com.seppomerimaa.NonDeterministicFiniteAutomaton.Transition
import com.seppomerimaa.TestAutomatonCompanion.{Input, One, Zero}

/**
  * An NFA that accepts all inputs where the third from last input is a One.
  */
object ThirdFromLast extends NonDeterministicFiniteAutomaton[TFLStates, Input] {

  val start = Q1
  def transition: Transition[TFLStates, Input] = {
    case (Q1, Zero) => Set(Q1)
    case (Q1, One) => Set(Q1, Q2)
    case (Q2, _) => Set(Q3)
    case (Q3, _) => Set(Q4)
    case (Q4, _) => Set(Q5)
    case (Q5, _) => Set(Q5)
  }
  def expand(s: TFLStates): Set[TFLStates] = s match {
    case Q1 => Set()
    case Q2 => Set()
    case Q3 => Set()
    case Q4 => Set()
    case Q5 => Set()
  }
}

object ThirdFromLastCompanion {
  sealed trait TFLStates extends State
  case object Q1 extends TFLStates with Rejecting
  case object Q2 extends TFLStates with Rejecting
  case object Q3 extends TFLStates with Rejecting
  case object Q4 extends TFLStates with Accepting
  case object Q5 extends TFLStates with Rejecting
}
