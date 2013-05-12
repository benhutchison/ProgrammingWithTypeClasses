package com.github.benhutchison.typeclasses

object SubtypingExample extends App {

  trait HasId {
    def id: Long
  }

  case class Person(id: Long, name: String) extends HasId

  case class Book(id: Long, title: String) extends HasId

  val HasIdHash = 42
  val PersonHash = 7
  

  {
    //Hash is an example of a contravariant typeclass, because it maps 'A's to Ints
    //Predicate (A => Bool) or Show (A => String) are similar
    
    trait Hash[-A] {
      def hash(a: A): Int
    }
    implicit class RichHash[A: Hash](a: A) {
      def hash: Int = implicitly[Hash[A]].hash(a)
    }

    implicit def hashId = new Hash[HasId] {
      def hash(a: HasId) = HasIdHash
    }

    implicit def hashPerson = new Hash[Person] {
      def hash(a: Person) = PersonHash
    }

    //Surprisingly, not true
    //assert(Person(1, "Ben").hash == PersonHash)
    
    //Hash[HasId] is always chosen by Scala, even when the more specific typeclass Hash[Person] matches
    assert(Person(1, "Ben").hash == HasIdHash)
    assert(Book(2, "The Hobbit").hash == HasIdHash)

    //This feature of Scala is widely disliked and much debated. See also:

    //"Contravariance mucks with implicit resolution"
    //https://issues.scala-lang.org/browse/SI-2509

    //"end the blight of contrarivariance"
    //https://groups.google.com/forum/?fromgroups=#!topic/scala-language/ZE83TvSWpT4

  }

  {
    //when a typeclass consumers and produces 'A', it must be invariant
    //Semigroup is common typeclass representing some 'reduction' or 'accumulation' of As 
    trait Semigroup[A] {
      def ++(a1: A, a2: A): A
    }
    
    //invariant typeclasses match their declared type but not subtypes
    
    implicit class RichSemigroup[A: Semigroup](a: A) {
      def ++(a2: A): A = implicitly[Semigroup[A]].++(a, a2)
    }

    implicit val maxId = new Semigroup[HasId] {
      def ++(a1: HasId, a2: HasId) = if (a1.id > a2.id) a1 else a2 
    }

    //won't compile: "could not find implicit value for evidence parameter of type Semigroup[Person]"
    //Person(1, "Ben") ++ Person(2, "Ken")
  }

  {
    //A nice workaround is to use type bounds on the implicit definition of the typeclass instance 
    //Not sure why this works, but resultant behavior is intuitive
    
    trait Hash[A] {
      def hash(a: A): Int
    }
    implicit class RichHash[A: Hash](a: A) {
      def hash: Int = implicitly[Hash[A]].hash(a)
    }

    implicit def hashId[A <: HasId] = new Hash[A] {
      def hash(a: A) = HasIdHash
    }

    implicit def hashPerson[A <: Person] = new Hash[A] {
      def hash(a: A) = PersonHash
    }

    assert(Person(1, "Ben").hash == PersonHash)
    assert(Book(2, "The Hobbit").hash == HasIdHash)
  }

  {
    //covariant typeclasses yield 'A's from some defined input type
    trait Parse[+A] {
      def parse(s: String): A
    }
    object Parse {
      def parse[A: Parse](s: String): A = implicitly[Parse[A]].parse(s)
    }
    implicit def parseHasId = new Parse[HasId] {
      def parse(s: String) = new HasId {
        def id = 42
      }
    }

    implicit def parseBook = new Parse[Book] {
      def parse(s: String) = Book(1, s)
    }

    //they can be thought of as constructors or factories for A
    //since they introduce an A term where there was none
    
    //since there is no existing A inatnce to enrich, the typeclass needs be imported explictly 
    import Parse.parse
    
    //the most specific typeclass instance is resolved as expected
    val book: Book = parse("The Hobbit")
    assert(book.isInstanceOf[Book])

  }


}
