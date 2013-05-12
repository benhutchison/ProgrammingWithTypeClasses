package com.github.benhutchison.typeclasses

object OrderingExample extends App {

  //Scala has a built in Ordering class
  //Ordering is an easy win for typeclasses vs OO because it common to order
  //the same data differently in different situations

  case class SuburbRating(
    name: String,
    postcode: String,
    state: String,
    rating: Int)

  //the companion object of SuburbRating
  object SuburbRating {
    
    //implicits defined in companions provide a default typeclass 
    
    //Note Recursion here in Ordering.by(), requires another implicit parameter
    //chained implicit parameter search is commonplace when using typeclasses
    //(Side note: Implicit search is itself Turing Complete. 
    //Recursive typeclass search is not present in Agda due to possible non-termination)
    
    implicit val defaultOrder: Ordering[SuburbRating] = Ordering.by(_.rating)
  }

  lazy val myRatings = Vector(
    SuburbRating("Ivanhoe", "3079", "Vic", 9),
    SuburbRating("Kew", "3101", "Vic", 6),
    SuburbRating("Northcote", "3070", "Vic", 7),
    SuburbRating("Coburg", "3056", "Vic", 4),
    SuburbRating("Abbotsford", "3101", "Vic", 5)
  )

  assertEqual("Ivanhoe", myRatings.sorted.last.name)

  //but if we wanted to sort alphabetically by suburb name
  //(eg for printing a table) we can manually override the default

  assertEqual("Abbotsford", myRatings.sorted[SuburbRating](Ordering.by(_.name)).head.name)


  //lets refactor the SuburbRating class to separate Suburbs from Ratings

  object State extends Enumeration {
    type State = Value
    val Vic, Qld, NSW, ACT, SA, WA, Tas, NT = Value
  }
  import State._

  case class Suburb(name: String, postcode: String, state: State)
  
  object Suburb {
    implicit val defaultOrder: Ordering[Suburb] =
      Ordering.by((s) => (s.name, s.postcode, s.state))
  }

  case class SuburbRating2(suburb: Suburb, rating: Int)
  object SuburbRating2 {
    implicit val defaultOrder: Ordering[SuburbRating2] =
      Ordering.by((sr) => (sr.rating, sr.suburb))
  }

  val myRatings2 = myRatings.map((r) =>
    SuburbRating2(Suburb(r.name, r.postcode, State.withName(r.state)), r.rating))

  assertEqual("Ivanhoe", myRatings2.sorted.last.suburb.name)
}
