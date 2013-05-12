package com.github.benhutchison.typeclasses

//Unfortunate hiding of an implicit baked into Scala needed to make Numeric syntax "just work" 
import Predef.{any2stringadd => _, _}

import Numeric.Implicits._

object NumericExample extends App {

  //Generally, there's a broad consensus that abstraction is a Good Thing
  //in programming. We expend a great deal of effort abstracting over details
  //so that pieces of software can be swapped out

  //Ironically, while it's not uncommon to see AbstractRequestHandlerStrategyFactory
  //in a Java program, many of the foundation types remain stubbornly concrete

  def addInt(value1: Int, value2: Int) = value1 + value2
  def addFloat(value1: Float, value2: Float) = value1 + value2
  def addBigInt(value1: BigInt, value2: BigInt) = value1 + value2

  //there's obvious commonality here, but it's not supported
  //by the inherited Java class hierarchy

  //Object-orientation is really weak at supporting the retrospective
  //factoring out of commonality. It requires that you 'control' the data
  //classes.
  //
  //Type-classes can get us out of this mess

  def addGeneric[T: Numeric](value1: T, value2: T) = value1 + value2

  //Note the 'Context Bound' syntactic shortcut
  def addGenericDesurgared[T](value1: T, value2: T)
    (implicit n: Numeric[T]) = value1 + value2

  assert(addGeneric(21, 21) == 42)

  assert(addGeneric(BigInt(21000000000000L), BigInt(21000000000000L)) ==
    BigInt(42000000000000L))

  assert(addGeneric(0.0000021, 0.0000021) == 0.0000042)

  //Note the imports to get this to work:
  //import Predef.{any2stringadd => _, _}
  //import Numeric.Implicits._

  //before continuing, I want to point out that that lack of abstraction
  //in the core goes beyond the JVM primitives: binding to a concrete
  //and final String class is just as bad

  //Internally, JVM Strings use an Array[Char]. This is a very poor choice of
  //data structure for many use cases. While it's space efficient,
  //any modification of a String requires a copy of the whole thing.

  //This is often discussed on the web, but rarely is the root cause mentioned:
  //String's concreteness prevents programmers from substituting alternative
  //implementations of String.

  //Generally, scalable immutable data structures have a tree structure, so
  //that copy-modification can approach it's ideal minimum cost of log(N)


  //Numeric supports the sum and product methods in the Scala collections API
  assert(Seq(1, 2, 3).product == 6)
  assert(Seq(-1.0, 1.0).sum == 0.0)

  //..but generic numbers have their difficulties
  case class Vector2[T: Numeric](x: T, y: T) {

    def magnitude: T = {
      def sqrt(a: T): T = ???

      sqrt(x * x + y * y)
    }

  }



  //As well as using typeclasses to unify across existing number types
  //we will want to extend Numeric to new types we invent
  
  //One candidate is Probabilities - number like quantities bounded in the range [0.0 - 1.0]

  class Probability protected[typeclasses] (val value: Double) extends AnyVal

  def asP(p: Double): Probability = {
    toP(p).getOrElse(throw new IllegalArgumentException(p.toString))
  }

  def toP(p: Double): Option[Probability] = {
    if (p >= 0.0 && p <= 1.0) {
      Some(new Probability(p))
    } else {
      None
    }
  }

  /*
   * How to treat Probability as a Numeric type?
   * eg to enable summing over a collection of them...
   *
   * This demonstrate why splitting Numeric into finer grained pieces is needed,
   * (as I advocate here [https://issues.scala-lang.org/browse/SI-5202]).
   *
   * Currently, Numeric is like a restaurant which only offers a 10 course banquet.
   * Oh, well.. lets feast then!
   */

  //Note Fractional is a Numeric subtrait for non-integral numbers
  
  implicit val ProbabilityFractional = new Fractional[Probability] {

    //make sense for Probability
    def plus(x: Probability, y: Probability): Probability =
      new Probability(x.value * y.value)
    def minus(x: Probability, y: Probability): Probability =
      asP(x.value - y.value)
    def times(x: Probability, y: Probability): Probability =
      asP(x.value * y.value)
    def div(x: Probability, y: Probability): Probability =
      asP(x.value / y.value)
    def toDouble(x: Probability): Double = x.value
    def toFloat(x: Probability): Float = x.value.toFloat
    def compare(x: Probability, y: Probability): Int =
      Numeric.DoubleIsFractional.compare(x.value, y.value)

    //dont make sense
    def negate(x: Probability): Probability = ???
    def fromInt(x: Int): Probability = ???
    def toInt(x: Probability): Int = ???
    def toLong(x: Probability): Long = ???

  }


}
