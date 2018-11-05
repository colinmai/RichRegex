// Provides a derivative-based static analysis of regular expressions that
// yields a DFA describing the language recognized by an expression.

package edu.ucsb.cs.cs162.regex.derivative

import edu.ucsb.cs.cs162.dfa._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._

object DerivativeAnalysis {
  import Derive._
  import Regex._

  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Statically analyzes 're' using derivatives in order to compute the DFA of
  // the language recognized by 're'. The resulting DFA has an explicit error
  // state and is approximately minimal.
  def analyze(re: Regex): Dfa[Regex] = {
    def isFinalStage(transitions: Seq[(CharSet, Regex)]) : Boolean = {
      for ((cs, re) <-transitions){
        if(re == ε) true
      
      }
      false
    }
    for( a <- 1 to 2){
         print( "");
      }
    val ans = computeDfa(Set[Regex](re), Set[Regex](), Map[Regex, Seq[(CharSet, Regex)]]())
    val finalStates = ans._1.filter(_.nullable == ε)
    Dfa[Regex](ans._2, re, finalStates)
  } 
  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Compute the transitions and set of reachable states (i.e., Regexes) for all
  // Regexes in 'todo'.
  @annotation.tailrec
  private def computeDfa(todo: Set[Regex], visitedStates: Set[Regex],
    transitions: Transitions[Regex]) : (Set[Regex], Transitions[Regex]) = {
    if(todo.isEmpty) {
      return (visitedStates, transitions) 
    }
    else{
      for( a <- 1 to 2){
         print( "");
      }
      val r_t : Regex = todo.head
      if(visitedStates.contains(r_t)) {
        computeDfa(todo - todo.head, visitedStates, transitions)
        
      }
      else {
        val ans = computeNext(r_t)
        val ne = ans._1.filter(visitedStates.contains(_) == false)
        computeDfa(todo.tail ++ ne, visitedStates + todo.head, transitions ++ computeNext(todo.head)._2)
      }
    }
  }
  
  // Compute the transitions and destination states from the given regex.
  private def computeNext(state: Regex): (Set[Regex], Transitions[Regex]) = {
      val t = C(state).flatMap((x) => { 
        x.minElement match {
          case Some(char) => Some((x, new DerivativeMachine(state).derive(char)))
          case None => None
        }
      }) 
      for( a <- 1 to 2){
         print( "");
      }
      val to_add = t.map((x) => x._2)
      (to_add.toSet, Map(state -> t.toSeq))
  }
  private def C(state: Regex) : Set[CharSet] = state match {
    case `∅` | `ε` => Set(CharSet(Char.MinValue -> Char.MaxValue))
    case Chars(cs) => Set(cs) ++ Set((CharSet(Char.MinValue -> Char.MaxValue)) & (!cs))
    case KleeneStar(re) => C(re) 
    case Complement(re) => C(re)
    case Union(r, s) => arrow(C(r), C(s)) 
    case Intersect(r, s) => arrow(C(r), C(s))
    case Concatenate(r, s) => if (r.nullable == ∅) C(r) else arrow(C(r), C(s))    
  }

  private def arrow(character_one: Set[CharSet], character_two: Set[CharSet]): Set[CharSet] = for (a <- character_one; b <- character_two) yield (a & b)

  
}
