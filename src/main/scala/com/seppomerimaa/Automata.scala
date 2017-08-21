package com.seppomerimaa

import com.seppomerimaa.Automata.{Accept, Reject, Result, State}

import scala.annotation.tailrec

/**
  *
  */
object Automata {
  sealed trait Result
  case object Accept extends Result
  case object Reject extends Result

  trait State {
    def result: Result
  }
  trait Accepting extends State {
    def result = Accept
  }
  trait Rejecting extends State {
    def result = Reject
  }
}

/**
  * An automaton that accepts an input and either rejects or accepts it. Defined by
  * 1. A finite set of possible states. Here, the type S.
  * 2. A finite set of possible inputs aka an alphabet. Here, the type Input.
  * 3. A transition function from S x Input --> S
  * 4. A starting state.
  * 5. A subset of S that are accepting states. Here, the states of type S whose `result` is `Accept`.
  */
trait DeterministicFiniteAutomaton[S <: State, Input] {
  import DeterministicFiniteAutomaton.Transition

  val start: S

  def transition: Transition[S, Input]

  def run(inputs: List[Input]): S = DeterministicFiniteAutomaton.run(start, inputs, transition)

  def result(inputs: List[Input]): Result = DeterministicFiniteAutomaton.run(start, inputs, transition).result
}

object DeterministicFiniteAutomaton {
  type Transition[S <: State, Input] = (S, Input) => S

  @tailrec
  def run[S <: State, Input](current: S, inputs: List[Input], transition: Transition[S, Input]): S = inputs match {
    case Nil => current
    case hd :: tl => run(transition(current, hd), tl, transition)
  }
}


/**
  * An automaton that accepts an input and either rejects or accepts it. Defined by
  * 1. A finite set of possible states. Here, the type S.
  * 2. A finite set of possible inputs aka an alphabet. Here, the type Input.
  * 3. A transition function from S x Input --> Set[S]
  * 4. A starting state.
  * 5. A subset of S that are accepting states. Here, the states of type S whose `result` is `Accept`.
  */
trait NonDeterministicFiniteAutomaton[S <: State, Input] {
  import NonDeterministicFiniteAutomaton.Transition

  val start: S

  def transition: Transition[S, Input]

  /**
    * What states can we get to from s without consuming an input?
    */
  def expand(s: S): Set[S]

  def run(inputs: List[Input]): Set[S] = NonDeterministicFiniteAutomaton.run(start, inputs, transition, expand)

  def result(inputs: List[Input]): Result =
    NonDeterministicFiniteAutomaton.run(start, inputs, transition, expand)
    .map(_.result)
    .reduce[Result] {
      case (Accept, _) => Accept
      case (_, Accept) => Accept
      case (_, _) => Reject
    }
}
object NonDeterministicFiniteAutomaton {
  type Transition[S <: State, Input] = (S, Input) => Set[S]

  def run[S <: State, Input](
    start: S,
    inputs: List[Input],
    transition: Transition[S, Input],
    branch: S => Set[S]
  ): Set[S] = {
    @tailrec
    def go(currents: Set[S], inputs: List[Input]): Set[S] = inputs match {
      case Nil => currents
      case hd :: tl =>
        println(s"$inputs $currents")
        val news = currents.flatMap(s => transition(s, hd))
        val expandedNews = news.flatMap(branch) union news
        go(expandedNews, tl)
    }
    val expandedStarts = branch(start) + start
    go(expandedStarts, inputs)
  }
}


