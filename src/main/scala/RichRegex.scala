// This file uses the 'pimp my library' pattern to add builder methods and regex
// operations to Regex.

package edu.ucsb.cs.cs162.regex

import edu.ucsb.cs.cs162.regex.derivative._
import scala.language.postfixOps

object `package` {
  import Regex._

  // Convenient methods to build regular expressions.
  implicit class RegexBuilder(val re: Regex) extends AnyVal {
    //----------------------------------------------------------------------------
    // Public API.
    //----------------------------------------------------------------------------

    // Concatenate 're' with 'other', simplifying if possible (assumes that 're'
    // and 'other' have already been simplified).
    def ~(other: Regex): Regex = (re, other) match {
      // This case should come after all other cases that handle concatenation
      // simplification. It handles the case where the concatenation is not
      // right-associative, and transforms it into right-associative form. There
      // should be a default case after this one that handles anything that this
      // case and the prior simplification cases don't handle.
      case (re, `∅`) => `∅`
      case (`∅`, re) => `∅`
      case (re, `ε`) => re
      case (`ε`, re) => re
      case (_: Concatenate, _) => {
        // Replace the right-most concatenation in 're' with the concatenation
        // to 'other'.
        def replaceRight(re: Regex) : Regex = re match {
          case Concatenate(re1, re2) => Concatenate(re1, replaceRight(re2))
          case _ => Concatenate(re, other)
        }
        replaceRight(re)
      }
      case (re: Regex, other: Regex)  => Concatenate(re, other)
    }

    // Union 're' with 'other', simplifying if possible (assumes that 're' and
    // 'other' have already been simplified).
    def |(other: Regex): Regex = (re, other) match {
      
      // Integrated simplifications from assignment 2
      case (re, `∅`) => re
      case (`∅`, re) => re
      case (Chars(chars1), Chars(chars2)) => Chars(chars1 ++ chars2)
      case (re: KleeneStar, `ε`) => re
      case (`ε`, other: KleeneStar) => other
      case (KleeneStar(`α`), other) => re
      case (re, KleeneStar(`α`)) => other
      case (re: Regex, other: Regex) if re == other => re
      
      // This case should come after all other cases that handle union
      // simplification. It ensures that unions are right-associative and the
      // operands are ordered correctly.
      case _ => {
        // Collect together all immediate non-Union sub-expressions.
        def collect(re: Regex): Set[Regex] = re match {
          case Union(re1, re2) => collect(re1) ++ collect(re2)
          case _ => Set(re)
        }
        val subexpressions = collect(re) ++ collect(other)
        associate(subexpressions.toSeq, (re1, re2) => Union(re1, re2))
      }
    }

    // Apply the Kleene star to 're', simplifying if possible (assumes that 're'
    // has already been simplified).
    def * : Regex = re match {
      case `ε` => `ε`
      case `∅` => `ε`
      case re: KleeneStar => re
      case re: Regex => KleeneStar(re)
      case _ => throw new AssertionError("WRONG")
    }

    // Complement 're', simplifying if possible (assumes that 're' has already
    // been simplified).
    def unary_! : Regex = re match {
      case Complement(re) => re
      case `ε` => (`α`)+
      case `∅` => (`α`)*
      case re: Regex => Complement(re)
      case _ => throw new AssertionError("WRONG")
    }

    // Intersect 're' with 'other', simplifying if possible (assumes that 're'
    // and 'other' have already been simplified).
    def &(other: Regex): Regex = (re, other) match {
      
      // Integrated simplifications from assignment 2
      case (`∅`, other) => `∅`
      case (re, `∅`) => `∅`
      case (Chars(chars1), Chars(chars2)) => Chars(chars1 & chars2)
      case (re: KleeneStar, other) => other
      case (re, other: KleeneStar) => re
      case (re: Regex, other: Regex) if re == other => re
      
      // This case should come after all other cases that handle intersection
      // simplification. It ensures that intersections are right-associative and
      // the operands are ordered correctly.
      case _ => {
        // Collect together all immediate non-Intersect sub-expressions.
        def collect(re: Regex): Set[Regex] = re match {
          case Intersect(re1, re2) => collect(re1) ++ collect(re2)
          case _ => Set(re)
        }
        val subexpressions = collect(re) ++ collect(other)
        associate(subexpressions.toSeq, (re1, re2) => Intersect(re1, re2))
      }
    }

    // Shorthand for 1 or more repetitions of re regex.
    def + : Regex = Concatenate(re, KleeneStar(re))

    // Shorthand for 0 or 1 instances of re regex.
    def ? : Regex = Union(`ε`, re)

    // Shorthand for exactly 'num' repetitions of re regex.
    def ^(num: Int): Regex = {
      assert(num >= 0)
      if (num == 0) `ε` else (re^(num-1)) ~ re
    }

    // Shorthand for at least 'min' repetitions of re regex.
    def >=(min: Int): Regex = {
      assert(min >= 0)
      if (min == 0) re* else (re^(min)) ~ (re*)
    }

    // Shorthand for at most 'max' repetitions of re regex.
    def <=(max: Int): Regex = {
      assert(max >= 0)
      if (max == 0) `ε` else (re <= (max-1)) | (re^max)
    }

    // Shorthand for at least 'min' but at most 'max' repetitions of re regex.
    def <>(min: Int, max: Int): Regex = {
      assert(min >= 0 && max >= min)
      if (max == 0) `ε` else (re >= min) & (re <= max)
    }

    //----------------------------------------------------------------------------
    // Private details.
    //----------------------------------------------------------------------------

    // Sort the subterms of a Regex in lexicographic order.
    private def associate(res: Seq[Regex], join: (Regex, Regex) => Regex): Regex =
      res.sortWith((re1, re2) => re2 lessThanEq re1).reduceLeft(
        (acc, re) => join(re, acc))
  }

  // Add convenient methods to String for building simple regular expressions.
  implicit class StringToRegex(val str: String) extends AnyVal {
    // Builds the concatenation of each character in 'str' in sequence. Example:
    // "abc".concatenate == Chars('a') ~ Chars('b') ~ Chars('c').
    def concatenate: Regex =
      str.foldLeft(ε: Regex)((acc, char) => acc ~ Chars(char))

    // Builds a charset containing each character in 'str'. Example:
    // "abc".charset == Chars('a', 'b', 'c').
    def charset: Regex =
      if (str.isEmpty) ε else Chars(str.toSeq: _*)
  }

  // Operations on regular expressions.
  implicit class RegexOps(val re: Regex) extends AnyVal {
    // Returns ε if 're' is nullable, otherwise returns ∅.
    def nullable: Regex = re match {
      case `ε` | _: KleeneStar => ε
      case `∅` | _: Chars => ∅
      case Concatenate(re1, re2) => re1.nullable ~ re2.nullable
      case Union(re1, re2) => re1.nullable | re2.nullable
      case Complement(re1) => if (re1.nullable == ε) ∅ else ε
      case Intersect(re1, re2) => re1.nullable & re2.nullable
    }

    // Returns true iff the language recognized by 're' is empty.
    def empty: Boolean =
      DerivativeAnalysis.analyze(re).fin.isEmpty

    // Returns true iff the language of 'other' is contained in the language of
    // 're'.
    def contains(other: Regex): Boolean =
      (other & !re).empty

    // Returns true iff the language of 're' is the same as the language of
    // 'other'.
    def equivalent(other: Regex): Boolean =
      re.contains(other) && other.contains(re)

    // Returns true if 're' <= 'other' according to a lexicographic ordering
    // of the regex ASTs.
    def lessThanEq(other: Regex): Boolean = (re, other) match {
      case (`∅`, _) => true
      case (_, `∅`) => false
      case (`ε`, _) => true
      case (_, `ε`) => false
      case (Chars(c1), Chars(c2)) => c1.toString <= c2.toString
      case (_: Chars, _) => true
      case (_, _: Chars) => false
      case (Concatenate(re1, re2), Concatenate(reA, reB)) => {
        if (re1 == reA) re2 lessThanEq reB
        else re1 lessThanEq reA
      }
      case (_: Concatenate, _) => true
      case (_, _: Concatenate) => false
      case (Union(re1, re2), Union(reA, reB)) => {
        if (re1 == reA) re2 lessThanEq reB
        else re1 lessThanEq reA
      }
      case (_: Union, _) => true
      case (_, _: Union) => false
      case (KleeneStar(re1), KleeneStar(reA)) => re1 lessThanEq reA
      case (_: KleeneStar, _) => true
      case (_, _: KleeneStar) => false
      case (Complement(re1), Complement(reA)) => re1 lessThanEq reA
      case (_: Complement, _) => true
      case (_, _: Complement) => false
      case (Intersect(re1, re2), Intersect(reA, reB)) => {
        if (re1 == reA) re2 lessThanEq reB
        else re1 lessThanEq reA
      }
      case (_: Intersect, _) => true
      case (_, _: Intersect) => false
    }
  }
}
