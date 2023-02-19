package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

trait RegularLanguage

case object Empty extends RegularLanguage

case object Epsilon extends RegularLanguage

case class Character(c: Char) extends RegularLanguage

case class Union(lang1: RegularLanguage, lang2: RegularLanguage) extends RegularLanguage

case class Concat(lang1: RegularLanguage, lang2: RegularLanguage) extends RegularLanguage

case class Star(lang: RegularLanguage) extends RegularLanguage

//These need to be case class or case object: Empty, Epsilon, Character, Union, Concat, and Star

/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage = 
  lang match
    case Concat(lang1, lang2) => 
      if lang1 == Epsilon then simplify(lang2)
      else if lang2 == Epsilon then simplify(lang1)
      else if (lang1  == Empty || lang2 == Empty) then Empty
      else Concat(simplify(lang1), simplify(lang2))

    case Union(lang1, lang2) => 
      if lang1 == Empty then simplify(lang2)
      else if lang2 == Empty then simplify(lang1)
      else Union(simplify(lang1), simplify(lang2))

    case Star(x) => 
      x match
        case Epsilon => Epsilon
        case Empty => Empty
        case y => Star(simplify(y))
      
    case otherwise => otherwise

/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = 
  lang match
    case Epsilon => true
    case Empty => false
    case Character(c) => false
    case Union(lang1, lang2) => (nullable(lang1) || nullable(lang2))
    case Concat(lang1, lang2) => (nullable(lang1) && nullable(lang2))
    case Star(lang) => true
  

/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = 
  l match
    case Epsilon => Empty
    case Empty => Empty
    case Character(d) =>
      if c == d then Epsilon
      else Empty
    case Union(lang1, lang2) => Union(derivative(lang1)(c), derivative(lang2)(c))
    case Concat(lang1, lang2) => 
      if !nullable(lang1) then Concat(derivative(lang1)(c), lang2)
      else Union(Concat(derivative(lang1)(c), lang2), derivative(lang2)(c))
    case Star(lang) => Concat(derivative(lang)(c),Star(lang))

/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
