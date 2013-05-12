package com.github.benhutchison.typeclasses

import java.util.Date

object SerializationExample extends App {

  object State extends Enumeration {
    type State = Value
    val Vic, Qld, NSW, ACT, SA, WA, Tas, NT = Value
  }
  import State._

  case class Suburb(name: String, postcode: String, state: State)

  case class User(email: String, name: String, createdTime: Date)

  case class SuburbRating(suburb: Suburb, user: User, rating: Int)

  val ivanhoe = Suburb("Ivanhoe", "3079", Vic)
  val kew = Suburb("Kew", "3101", Vic)
  val northcote = Suburb("Northcote", "3070", Vic)
  val coburg = Suburb("Coburg", "3056", Vic)
  val abbotsford = Suburb("Abbotsford", "3101", Vic)

  val ben = User("brhutchison@gmail.com", "Ben", new Date(1364000000000L))
  val louise = User("louise@yahoo.com", "Louise", new Date(1345000000000L))


  val allRatings = Seq(
    SuburbRating(kew, ben, 8),
    SuburbRating(kew, louise, 6),
    SuburbRating(coburg, ben, 5),
    SuburbRating(coburg, louise, 7),
    SuburbRating(abbotsford, ben, 3),
    SuburbRating(abbotsford, louise, 6),
    SuburbRating(ivanhoe, louise, 9),
    SuburbRating(ivanhoe, ben, 8),
    SuburbRating(northcote, ben, 8),
    SuburbRating(northcote, louise, 7)
  )
  
  //Serialisation is the easier case, because we are going from a more structured, typed representation to a
  //less structured format.

  trait JsonWriter[T] {
    def write(value: T): String
  }
  implicit class JsonWriteable[T](val value: T) extends AnyVal {
    def write(implicit w: JsonWriter[T]) = w.write(value)
  }

  class SeqWriter[A: JsonWriter] extends JsonWriter[Seq[A]]{
    def write(value: Seq[A]) = value.map(_.write).mkString("[", ",\n", "]")
  }
  implicit def seqWriter[A: JsonWriter] = new SeqWriter[A]


  //more condensed version combines implicit def with anonymous class definition
  implicit def suburbRatingWriter(implicit uw: JsonWriter[User], sw: JsonWriter[Suburb]) =
    new JsonWriter[SuburbRating]  {
      def write(r: SuburbRating) =
        s"""{"Suburb": ${r.suburb.write},\n "User": ${r.user.write},\n "Rating": ${r.rating}}"""
  }

  implicit object UserWriter extends JsonWriter[User] {
    def write(u: User) =
      s"""{"Email": "${u.email}", "Name": "${u.name}", "CreatedTime": ${u.createdTime.getTime}}"""
  }

  implicit object SuburbWriter extends JsonWriter[Suburb] {
    def write(s: Suburb) =
      s"""{"Name": "${s.name}", "Postcode": "${s.postcode}", "State": "${s.state.toString}"}"""
  }

val expected =
"""[{"Suburb": {"Name": "Kew", "Postcode": "3101", "State": "Vic"},
  | "User": {"Email": "brhutchison@gmail.com", "Name": "Ben", "CreatedTime": 1364000000000},
  | "Rating": 8},
  |{"Suburb": {"Name": "Kew", "Postcode": "3101", "State": "Vic"},
  | "User": {"Email": "louise@yahoo.com", "Name": "Louise", "CreatedTime": 1345000000000},
  | "Rating": 6},
  |{"Suburb": {"Name": "Coburg", "Postcode": "3056", "State": "Vic"},
  | "User": {"Email": "brhutchison@gmail.com", "Name": "Ben", "CreatedTime": 1364000000000},
  | "Rating": 5},
  |{"Suburb": {"Name": "Coburg", "Postcode": "3056", "State": "Vic"},
  | "User": {"Email": "louise@yahoo.com", "Name": "Louise", "CreatedTime": 1345000000000},
  | "Rating": 7},
  |{"Suburb": {"Name": "Abbotsford", "Postcode": "3101", "State": "Vic"},
  | "User": {"Email": "brhutchison@gmail.com", "Name": "Ben", "CreatedTime": 1364000000000},
  | "Rating": 3},
  |{"Suburb": {"Name": "Abbotsford", "Postcode": "3101", "State": "Vic"},
  | "User": {"Email": "louise@yahoo.com", "Name": "Louise", "CreatedTime": 1345000000000},
  | "Rating": 6},
  |{"Suburb": {"Name": "Ivanhoe", "Postcode": "3079", "State": "Vic"},
  | "User": {"Email": "louise@yahoo.com", "Name": "Louise", "CreatedTime": 1345000000000},
  | "Rating": 9},
  |{"Suburb": {"Name": "Ivanhoe", "Postcode": "3079", "State": "Vic"},
  | "User": {"Email": "brhutchison@gmail.com", "Name": "Ben", "CreatedTime": 1364000000000},
  | "Rating": 8},
  |{"Suburb": {"Name": "Northcote", "Postcode": "3070", "State": "Vic"},
  | "User": {"Email": "brhutchison@gmail.com", "Name": "Ben", "CreatedTime": 1364000000000},
  | "Rating": 8},
  |{"Suburb": {"Name": "Northcote", "Postcode": "3070", "State": "Vic"},
  | "User": {"Email": "louise@yahoo.com", "Name": "Louise", "CreatedTime": 1345000000000},
  | "Rating": 7}]""".stripMargin

println(allRatings.write)

assertEqual(expected, allRatings.write)

}
