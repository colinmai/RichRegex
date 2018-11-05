package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.regex._

class DeriveSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._
  
  val test = Chars('a')
  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')
  val c_d = Chars('c', 'd')
  val a_c = Concatenate(test,c)
  val a_c_b = Concatenate(a_c, b)
  val a_or_b = Union(test, b)
  val a_c_or_b = Union(a_c, b)
  val star_a = KleeneStar(test)
  val star_a_c = KleeneStar(a_c)
  val star_a_or_b = KleeneStar(a_or_b)
  val com_a = Complement(test)
  val com_a_c_b = Complement(a_c_b)
  var i_c_d_c = Intersect(c_d, c)
  var xl = Union(com_a_c_b, star_a_c)
  var xll = Intersect(xl, i_c_d_c)
  var xlll = Concatenate(xl, xll)

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  it should "recognize strings in the language 1" in {
    Derive.matches(test, "a") should equal (true)
    Derive.matches(a_c, "ac") should equal (true)
    Derive.matches(a_c_b, "acb") should equal (true)
    Derive.matches(a_or_b, "a") should equal (true)
    Derive.matches(a_or_b, "b") should equal (true)
    Derive.matches(a_c_or_b, "ac") should equal (true)
    Derive.matches(a_c_or_b, "b") should equal (true)
    Derive.matches(star_a_or_b, "aabababbbabbabaababa") should equal (true)
    Derive.matches(com_a, "b") should equal (true)
    Derive.matches(com_a_c_b, "abc") should equal (true)
    Derive.matches(i_c_d_c, "c") should equal (true)
  }

  // more tests...

  it should "not recognize strings not in the language 1" in { 
    Derive.matches(a_c_b, "abc") should equal (false)
    Derive.matches(`∅`, "") should equal (false)
    Derive.matches(`∅`, "asfdhjskdgh") should equal (false)
    Derive.matches(`ε`, "") should equal (true)
    Derive.matches(`ε`, "aSFHsjdh") should equal (false)
    Derive.matches(a_or_b, "ab") should equal (false)
    Derive.matches(a_c_or_b, "acc") should equal (false)
    Derive.matches(star_a_or_b, "aabababbbabbabaababac") should equal (false)
    Derive.matches(com_a, "a") should equal (false)
    Derive.matches(com_a_c_b, "acb") should equal (false)
    Derive.matches(i_c_d_c, "cd") should equal (false)
  }

  // more tests...
  behavior of "eval"
  
  it should "recognize strings in the language" in{
    new DerivativeMachine(test).eval("a") should equal (Derive.matches(test, "a"))
    new DerivativeMachine(test).eval("b") should equal (Derive.matches(test, "b"))
    new DerivativeMachine(a_or_b).eval("b") should equal (Derive.matches(a_or_b, "b"))
    new DerivativeMachine(a_or_b).eval("a") should equal (Derive.matches(a_or_b, "a"))
    new DerivativeMachine(c_d).eval("a") should equal (Derive.matches(c_d, "a"))
    new DerivativeMachine(c_d).eval("c") should equal (Derive.matches(c_d, "c"))
    new DerivativeMachine(c_d).eval("d") should equal (Derive.matches(c_d, "d"))
    new DerivativeMachine(a_c_or_b).eval("b") should equal (Derive.matches(a_c_or_b, "b"))
    new DerivativeMachine(a_c_or_b).eval("a") should equal (Derive.matches(a_c_or_b, "a"))
    new DerivativeMachine(a_c_or_b).eval("c") should equal (Derive.matches(a_c_or_b, "c"))
    new DerivativeMachine(star_a_or_b).eval("a") should equal (Derive.matches(star_a_or_b, "a"))
    new DerivativeMachine(star_a_or_b).eval("e") should equal (Derive.matches(star_a_or_b, "e"))
    new DerivativeMachine(star_a_c).eval("c") should equal (Derive.matches(star_a_c, "c"))
    new DerivativeMachine(star_a_c).eval("a") should equal (Derive.matches(star_a_c, "a"))
    new DerivativeMachine(com_a_c_b).eval("a") should equal (Derive.matches(com_a_c_b, "a"))
    new DerivativeMachine(com_a_c_b).eval("b") should equal (Derive.matches(com_a_c_b, "b"))
    new DerivativeMachine(i_c_d_c).eval("c") should equal (Derive.matches(i_c_d_c, "c"))
    new DerivativeMachine(xl).eval("c") should equal (Derive.matches(xl, "c"))
    new DerivativeMachine(xll).eval("b") should equal (Derive.matches(xll, "b"))
    new DerivativeMachine(xlll).eval("b") should equal (Derive.matches(xlll, "b"))
    new DerivativeMachine(xlll).eval("aa") should equal (Derive.matches(xlll, "aa"))
    new DerivativeMachine(`∅`).eval("") should equal (Derive.matches(`∅`, ""))
    new DerivativeMachine(`∅`).eval("a") should equal (Derive.matches(`∅`, "a"))
    new DerivativeMachine(`ε`).eval("") should equal (Derive.matches(`ε`, ""))
    new DerivativeMachine(`ε`).eval("a") should equal (Derive.matches(`ε`, "a"))
  }
  it should "not recognize strings not in the language" in{
    new DerivativeMachine(test).eval("a") should not equal (Derive.matches(test, "b"))
    new DerivativeMachine(test).eval("a") should not equal (Derive.matches(a_c, "a"))
    new DerivativeMachine(`ε`).eval("a") should not equal (Derive.matches(`ε`, ""))
    new DerivativeMachine(xlll).eval("aa") should not equal (Derive.matches(test, "a"))
  }
  
 //eval calls derive, so eval tests should cover derive
}

