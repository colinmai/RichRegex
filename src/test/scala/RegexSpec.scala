package edu.ucsb.cs.cs162.regex

import org.scalatest._

class RegexSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  val charA = Chars('a')
  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')
  val e = Chars('e')
  val f = Chars('f')

  val r = Chars('a') | Chars('b').+
  val r1 = Chars('x', 'y').* ~ r
  val r2 = Chars('y', 'x').+ ~ r
  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "a regex"

  it should "be buildable using `~`" in {
    (r1 ~ r2) should equal (Chars('x', 'y').* ~ r ~ Chars('y', 'x').+ ~ r)
    // simplifications
    (r ~ ∅) should equal(∅)
    (∅ ~ r) should equal(∅)
    (r ~ ε) should equal(r)
    (ε ~ r) should equal(r)
  }


  it should "be buildable using `|`" in {
    (r1 | r2) should equal(Union(r2, r1)) // also testing normalization due to lengths of r1 and r2
    // simplifications
    (r | ∅) should equal(r)
    (∅ | r) should equal(r)
    (Chars('a' -> 'c') | Chars('c' -> 'f')) should equal(Chars('a'->'f'))
    (r.* |   ε) should equal(r.*)
    (ε   | r.*) should equal(r.*)
    (α.* |   r) should equal(α.*)
    (r |   α.*) should equal(α.*)
    (r | r)     should equal(r)
  }

  it should "be buildable using `*`" in {
    r.* should equal(KleeneStar(r))
    // simplifications
    ∅.* should equal(ε)
    ε.* should equal(ε)
    (r.*).* should equal(r.*)
  }

  it should "be buildable using `!`" in {
    !r should equal(Complement(r))
    // Simplifications
    !(!r) should equal(r)
    !(∅) should equal(α.*)
    !ε should equal(α.+)
  }

  it should "be buildable using `&`" in {
    (r1 & r2) should equal(Intersect(r2, r1)) // also testing normalization due to lengths of r1 and r2
    // Simplifications
    (∅ & r) should equal(∅)
    (r & ∅) should equal(∅)
    (Chars('a'->'d') & Chars('c'->'f')) should equal (Chars('c'->'d'))
    (α.* & r) should equal(r)
    (r & α.*) should equal(r)
    (r & r) should equal(r)
  }

  it should "be buildable using `^`" in {
    (r^5) should equal(r ~ r ~ r ~ r ~ r)
    (b^10) should equal (b~b~b~b~b~b~b~b~b~b)
    ((r1^2)^(3)) should equal ((r1~r1)~(r1~r1)~(r1~r1))
    (c^0) should equal (`ε`)
  }

  it should "be buildable using `>=`" in {
    (r >= 3) should equal(r ~ r ~ r ~ r.*)
    (r1 >= 4) should equal (r1 ~ r1 ~ r1 ~ r1 ~ r1.*)
    (b >= 0) should equal (b.*)
  }

  it should "be buildable using `<=`" in {
    (r <= 3) should equal(ε | r | (r ~ r) | (r ~ r ~ r))
    (r2 <= 0) should equal (`ε`)
    (r1 <= 5) should equal (ε | r1 | (r1 ~ r1) | (r1 ~ r1 ~ r1) | (r1^4) | (r1^5))
  }

  it should "be buildable using `<>`" in {
    (r <>(2, 3)) should equal((r ~ r ~ r.*) & (ε | r | (r ~ r) | (r ~ r ~ r)))
    (c <> (1, 4)) should equal ((c >= 1) & (c <= 4))
    (r2 <> (0, 0)) should equal (`ε`)
    (r1 <> (0, 3)) should equal (`ε` | r1 | (r1 ~ r1) | (r1 ~ r1 ~ r1))
  }


  it should "be buildable using convenience methods 1" in {
    (b ~ c) should equal (Concatenate(b, c))
    ((Chars('c')).* | Chars('z')) should equal (Union(Chars('z'), (Chars('c').*)))
    (Chars('b')~(Chars('c')~Chars('d') | Chars('e'))~Chars('f')) should equal (Chars('b') ~ ((Chars('e') | (Chars('c') ~ Chars('d'))) ~ Chars('f'))) 
  }

  it should "be buildable using convenience methods 2" in {
    (b | (b ~ c)) should equal (Union(b, Concatenate(b, c)))
    (b ~ (c ~ r1)) should equal (Concatenate(b, (Concatenate(c, r1))))
    (r1 | c) should equal (Union(c, r1))
  }

  it should "be buildable using convenience methods 3" in {
    b.* should equal (KleeneStar(b))
    (r1 | c).* should equal (KleeneStar((Union(c, r1))))
  }

  it should "be buildable using convenience methods 4" in {
    !b should equal (Complement(b))
    !(r1 | c) should equal (Complement((Union(c, r1))))
  }

  it should "be buildable using convenience methods 5" in {
    (b & (b ~ c)) should equal (Intersect(b, Concatenate(b, c)))
    (b & r1) should equal (Intersect(b, r1))
  }

  it should "be buildable using convenience methods 6" in {
    b.+ should equal (Concatenate(b, KleeneStar(b)))
  }

  it should "be buildable using convenience methods 7" in {
    b.? should equal (Union(ε, b))
  }

  it should "be buildable using convenience methods 8" in {
    b^3 should equal (Concatenate(b, Concatenate(b, b)))
  }

  it should "be buildable using convenience methods 9" in {
    (b >= 2) should equal (Concatenate(b, Concatenate(b, KleeneStar(b))))
  }

  it should "be buildable using convenience methods 10" in {
    (b <= 2) should equal (Union(ε, Union(b, Concatenate(b, b))))
  }

  it should "be buildable using convenience methods 11" in {
    (b <> (1, 3)) should equal (Intersect(Concatenate(b, KleeneStar(b)), Union(ε, Union(b, Union(Concatenate(b, b), Concatenate(b, Concatenate(b, b)))))))
  }

  it should "be buildable from strings" in {
    "ab".charset ~ "cd".concatenate should equal (Concatenate(Chars('a', 'b'),
      Concatenate(Chars('c'), Chars('d'))))
  }

  it should "pretty-print correctly" in {
    (b.? | (c >= 1)).prettyPrint should equal ("""Union
                                                 |├─ ε
                                                 |└─ Union
                                                 |   ├─ b
                                                 |   └─ Concatenate
                                                 |      ├─ c
                                                 |      └─ KleeneStar
                                                 |         └─ c
                                                 |""".stripMargin)
  }

  it should "normalize correctly 1" in {
    val re = ((charA ~ b) ~ (c ~ d)) ~ (e ~ f)

    val norm = Concatenate(charA, Concatenate(b, Concatenate(c,
      Concatenate(d, Concatenate(e, f)))))

    re should equal (norm)
  }

  it should "normalize correctly 2" in {
    val re = (((b | ε) & charA) | !charA | charA.*) | ((charA ~ b) |
      charA | ε)

    val norm = Union(ε, Union(charA, Union(Concatenate(charA, b),
      Union(KleeneStar(charA), Union(Complement(charA), Intersect(charA,
        Union(ε, b)))))))

    re should equal (norm)
  }

  behavior of "nullable"

  it should "recognize a nullable regex 1" in { 
    (b.*).nullable should equal (`ε`)
    (r1.*).nullable should equal (`ε`)
    ((b.*) ~b).nullable should equal (`∅`)
    ((r.*) ~(r1.*) ~ (r2.*) ~ `∅`).nullable should equal (`∅`)
    (b |c|r|(r1.*)).nullable should equal (`ε`)
    (r2.*).nullable should equal (`ε`)
    (b & c).nullable should equal (`∅`)
  }

  // more tests...

  it should "recognize a non-nullable regex 1" in { 
     b.nullable should equal (`∅`)
    c.nullable should equal (`∅`)
    r1.nullable should equal (`∅`)
  }

  // more tests...
}
