package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.util._
import edu.ucsb.cs.cs162.range_set._

class DerivativeAnalysisSpec extends FlatSpec with Matchers with Timeout {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  // The timeout in milliseconds for potentially slow code.
  val timeout = 2000

  // Analyze the given expression subject to a timeout.
  def analyzeWithTimeout(re: Regex) =
    timeoutAfter(timeout) { DerivativeAnalysis.analyze(re) }

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "the analysis"

  it should "should always terminate 1" in {
    val charA = Chars('a')

    // Causes a timeout or stack overflow if expression similarity isn't
    // implemented correctly.
    val dfa = analyzeWithTimeout((charA | (charA ~ charA)).*)
  }

  it should "should always terminate 2" in {
    // This test should test that check if normalization and DFA
    // building work well together. If the regexes are not conflated
    // properly, DFA construction would cause a timeout or stack
    // overflow and this test should fail.
    val charA = Chars('a')

    // Causes a timeout or stack overflow if expression similarity isn't
    // implemented correctly.
    val dfa = analyzeWithTimeout((charA | (charA)).*)
  }

  // more tests...

  it should "produce a DFA that recognizes the strings in language 1" in {
    val charA = Chars('a')

    val dfa = analyzeWithTimeout(ε | charA)

    dfa.matches("") should equal (true)
    dfa.matches("a") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 2" in {
    val charA = Chars('a')
    val dfa = analyzeWithTimeout((charA)*)
    dfa.matches("a") should equal (true)
    dfa.matches("aaa") should equal (true)
    
  }
  
  it should "produce a DFA that recognizes the strings in language 3" in {
    val charA = Chars('a')
    val charb = Chars('b')
    val dfa = analyzeWithTimeout((charA ~ charb)*)
    dfa.matches("ab") should equal (true)
    dfa.matches("ababab") should equal (true)
    
  }
  
  it should "produce a DFA that recognizes the strings in language 4" in {
    val charA = Chars('a')
    val charb = Chars('b')
    val dfa = analyzeWithTimeout((charA | charb) ~ charb)
    dfa.matches("ab") should equal (true)
    dfa.matches("bb") should equal (true)
    
  }
  
  it should "produce a DFA that recognizes the strings in language 5" in {
    val charA = Chars('a')
    val charb = Chars('b')
    val dfa = analyzeWithTimeout((charA ~ charb) ~ (charA | charb).*)
    dfa.matches("abbbbb") should equal (true)
    dfa.matches("abababa") should equal (true)
    
  }


  // more tests...

  it should "produce a DFA that should not recognize strings not in the language 1" in {
    val charA = Chars('a')

    val dfa = analyzeWithTimeout(ε | charA)

    dfa.matches("b") should equal (false)
    dfa.matches("aa") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 2" in {
    val charA = Chars('a')
    val dfa = analyzeWithTimeout((charA)*)
    dfa.matches("b") should equal (false)
    dfa.matches("ba") should equal (false)
  }
  
  it should "produce a DFA that should not recognize strings not in the language 3" in {
    val charA = Chars('a')
    val charb = Chars('b')
    val dfa = analyzeWithTimeout((charA ~ charb)*)
    dfa.matches("a") should equal (false)
    dfa.matches("bababa") should equal (false)
    
  }

  // more tests...

  it should "produce a DFA that has the correct structure 1" in {
      val a = Chars('a')
      val b = Chars('b')
      val Σ = CharSet(Char.MinValue → Char.MaxValue)
      val aSet = CharSet('a')
      val bSet = CharSet('b')
      
      val dfa = DerivativeAnalysis.analyze(a ~ b)
      
      // Check the initial state, and final states
      dfa.init shouldEqual (a ~ b)
      dfa.fin shouldEqual(Set[Regex](ε))
      

  
      
      // Check the transition relation
      dfa.delta(a ~ b) should contain theSameElementsAs Seq((!aSet, Regex.∅), (aSet, b))
      dfa.delta(b) should contain theSameElementsAs Seq((!bSet, ∅), (bSet, ε))
      dfa.delta(ε) should contain theSameElementsAs Seq((Σ, ∅))
      dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))
  }
  
  it should "produce a DFA that has the correct structure 2" in {
      val a = Chars('a')
      val b = Chars('b')
      val Σ = CharSet(Char.MinValue → Char.MaxValue)
      val aSet = CharSet('a')
      val bSet = CharSet('b')
      
      val dfa = DerivativeAnalysis.analyze(a)
      
      // Check the initial state, and final states
      dfa.init shouldEqual (a)
      dfa.fin shouldEqual(Set[Regex](ε))
      

  
      
      // Check the transition relation
      dfa.delta(ε) should contain theSameElementsAs Seq((Σ, ∅))
      dfa.delta(∅) should contain theSameElementsAs Seq((Σ, ∅))
  }

  // more tests...
}
