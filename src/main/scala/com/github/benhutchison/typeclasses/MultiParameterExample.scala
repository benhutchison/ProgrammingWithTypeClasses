package com.github.benhutchison.typeclasses

import java.util.Date

class MultiParameterExample extends App {

  //type classes can mention more than one type, in which case they are called 'multiparameter'

  //curiously, genuine use cases for 2-parameter type classes are hard to find

  //here's an slightly contrived example
  trait Coerce[A, B]

  object State extends Enumeration {
    type State = Value
    val Vic, Qld, NSW, ACT, SA, WA, Tas, NT = Value
  }
  import State._

  case class Suburb(name: String, postcode: String, state: State)

  case class User(email: String, name: String, createdTime: Date)

  //imagine we want to interface our dmoain to another mapping library that has a type representing a
  //label of a world map match some position
  trait MapLabel {
    def lat: Double
    def lon: Double
    def label: String
  }

  //we can convert States and Suburbs to Map

}
