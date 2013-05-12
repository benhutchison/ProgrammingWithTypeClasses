package com.github.benhutchison.typeclasses

import java.util.Date

import dispatch.classic.json._
import Js._

object DeserializationExample extends App {

  object State extends Enumeration {
    type State = Value
    val Vic, Qld, NSW, ACT, SA, WA, Tas, NT = Value
  }
  import State._

  case class Suburb(name: String, postcode: String, state: State)

  case class User(email: String, name: String, createdTime: Date)

  case class SuburbRating(suburb: Suburb, user: User, rating: Int)

  val jsonString =
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

  //type-class parsing  (aka deserialization) is not as a straightforward as
  //serialisation. The asymmetry is because, while in serialization we start with a richly
  //typed object and flatten it to an untyped String, in parsing we 'inflate' a JsValue into
  //a richly typed data structure

  //Note we can only use type-classes if we specify a destination type to parse to.

  //we also need to consider how to model a failure during parsing, if the input doesn't conform
  //to the destination type

  //we could just blow up with a runtime exception if anything goes wrong
  trait SimpleBlowUpJsonReader[T] {
    def read(s: JsValue): T
  }

  //but many people prefer to keep Exceptions for truly "Exception" (ie unforeseen) events 
  
  //alternate version that either returns the read value or information about the errors

  trait JsonReader[T] {
    def read(s: JsValue):  Either[Seq[String], T]

    def success(v: T) = Right(v)
    def fail(message: String) = Left(Seq(message))
  }


  implicit class JsonReadable(val value: JsValue) extends AnyVal {
    def read[T](implicit r: JsonReader[T]): Either[Seq[String], T] = r.read(value)

  }

  implicit def seqReader[T: JsonReader] = new JsonReader[Seq[T]]{

    def read(json: JsValue) = json match {
      case JsArray(ts) => {

        val elemResults = ts.map(_.read[T])

        //splits results into a pair (allFails and allSuccesses) 
        elemResults.partition(_.isLeft) match {

          //allFails empty, so return Seq of successes 
          case (Seq(),  elems) =>
            Right(for(Right(i) <- elems.view) yield i)

          //at least one item failed, collect the failure messages   
          case (allErrors, _) =>
            Left((for(Left(errors) <- allErrors) yield errors).flatten)
        }
      }
      case _ => fail(s"Array expected at '${json.toString().take(10)}'")
    }
  }

  
  //generic reader of a 3-field case class
  //T1/T2/T3 are the types of the fields
  //f1/f2/f3 are the field names
  //apply is the constructor to use once the contained fields have parsed
  
  def product3Reader[C, T1: JsonReader, T2: JsonReader, T3: JsonReader]
    (f1: String, f2: String, f3: String)(apply : (T1, T2, T3) => C) = new JsonReader[C] {


    def read(json: JsValue) = json match {

      case JsObject(map) => {

        //helper method to read one field
        def tryRead[A: JsonReader](field: String) = {
          val jsonOrError = map.get(JsString(field)).toRight(Seq(s"Field not found: ${field}"))
          jsonOrError.right.flatMap(_.read[A])
        }

        (tryRead[T1](f1), tryRead[T2](f2), tryRead[T3](f3)) match {

          //all 3 fields parsed Ok, so construct the object
          case (Right(v1), Right(v2), Right(v3)) => Right(apply(v1, v2, v3))

          //for anything else, extract all errors
          //(note: pattern matching used on LHS of for expression below 'Left(msgs)')
          case (a, b, c) => Left((for (Left(msgs) <- Seq(a, b, c)) yield msgs).flatten)
        }
      }
      case _ => fail(s"Object expected at '${json.toString().take(10)}'")
    }

  }

  //Each typeclass instance configures the generic reader for that class 
  
  implicit lazy val suburbReader: JsonReader[Suburb] = product3Reader("Name", "Postcode", "State")(Suburb)

  implicit lazy val userReader: JsonReader[User] = product3Reader("Email", "Name", "CreatedTime")(User)

  implicit lazy val suburbRatingReader: JsonReader[SuburbRating] = product3Reader("Suburb", "User", "Rating")(SuburbRating)

  implicit lazy val stateReader: JsonReader[State] = new JsonReader[State] {
    def read(json: JsValue) = json match {
      case JsString(s) => values.find(_.toString == s).toRight(Seq(s"No such State '${s}'"))
      case _ => fail("Expected State")
    }
  }

  implicit lazy val intReader: JsonReader[Int] = new JsonReader[Int] {
    def read(json: JsValue) = json match {
      case JsNumber(n) => success(n.intValue)
      case _ => fail("Expected Int")
    }
  }

  implicit lazy val stringReader: JsonReader[String] = new JsonReader[String] {
    def read(json: JsValue) = json match {
      case JsString(s) => success(s)
      case _ => fail("Expected String")
    }
  }

  implicit lazy val dateReader: JsonReader[Date] = new JsonReader[Date] {
    def read(json: JsValue) = json match {
      case JsNumber(n) => success(new Date(n.longValue))
      case _ => fail("Expected Date")
    }
  }

  val js = Js(jsonString)

  println( js.read[Seq[SuburbRating]] )

}
