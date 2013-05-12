package com.github.benhutchison.typeclasses

trait Variance {

  //co- and contra- variance is a more general topic than type-classes, but it's
  //both often poorly-understood, and useful for the next topic on subtyping,
  //so I'll briefly cover it here

  //variance arises naturally when these features are both present
  //(a) subtyping
  //(b) types parameterized on other types

  //Note the subtype relation is a _Partial Order_.
  //So for any 2 objects 'A' and 'B', either A <: B, B <: A, or neither is true

  //Variance is a relation between a type and each of it's parameters:
  
  //"how does the subtype relation between A[X1] and A[X2] _vary_ with the subtype relation of X1 and X2?"
  //Either:

  //1 Invariant: no subtype relations exists between A[Supertype] and A[Subtype]
  {
    class A[X]
  }

  //2 Co-variant: the subtype relation in A varies with ('co') the relation in X
  {
    class A[+X]

    def b: A[String] = ???
    val a: A[Any] = b
  }

  //3 Contra-variant: the subtype relation in A varies opposite/against ('contra') the relation in X
  //I find this one quite counter-intuitive
  {
    class A[-X]

    def b: A[Any] = ???
    val a: A[String] = b
  }

  //Note that although we write the notation on the parameter (eg +X),
  //the variance describes the subtype relation on A, relative to the subtype relation of X


  //a more 'operational' way to think about variance is by parameter position
  abstract class A[X] {

    //contra-variant position
    //consumes (or eliminates) values of type X
    def consume(x: X)

    //co-variant position
    //emits (or introduces) values of type X
    def emit: X

  }

  //Let's check our understanding, by looking at variance in the Scala collection API:

  //covariant
  def iterable[A]: scala.collection.Iterable[A]

  //so we can get 'A's out of an Iterable, but not pass them in

  val s1 = iterable[String].head

  //... but take note of this "Jedi mind-trick" to workaround

  def someStrings: Iterable[String]
  def otherStrings: Iterable[String]

  someStrings.sameElements(otherStrings)

  def anys: Iterable[Any]
  someStrings.sameElements(anys)


  //invariant
  def set[A]: scala.collection.Set[A]

  //this is because Set combines the two different ways of defining a set

  //intensionally:  by a characterisitic function (or predicate)
  set[String].contains("hello")

  //extensionally: by enumerating elements. In this role, Set is a duplicate-free Iterable
  val s: String = set[String].head

  //you can have either variance if you're willing to give up the other side:
  //covariant
  val iter1: Iterable[Any] = set[Any]
  val iter2: Iterable[Any] = set[String]

  //contravariant
  val pred1: (String) => Boolean = set[String]
  val pred2: (String) => Boolean = set[Any]

}
