package com.github.benhutchison.typeclasses

import java.util.Date

object EquivalenceExample extends App {

  //A comment on Equality vs Equivalence before we get started
  //In software, we often talk about the Equality but mean Equivalence
  //From java.lang.Object#equals
  // "The equals method implements an equivalence relation on non-null object references"
  //Informally, if 2 objects are equal then they are /the same object/,
  //in every way and in all contexts

  //...whereas equivalence means roughly "equal in a given context"

  case class User(
    id: Long,
    email: String,
    firstName: String,
    lastName: String,
    createdTime: Date)

  val benUser = User(1L, "brhutchison@gmail.com", "Ben", "Hutchison", new Date())

  //Built in "universal equality", by overriding java.lang.Object.equals()
  //Case classes automatically implement equals() based upon the values of all their fields

  //proof it's not reference equality
  assert(benUser == benUser.copy())

  val modifiedUser = benUser.copy(lastName = "Hutchinson")
  assert(benUser != modifiedUser)


  //let's start by defining User equivalence outside of User itself
  //we'll use the Scala library class Equiv
  val userEq = new Equiv[User] {
    def equiv(x: User, y: User): Boolean = x.id == y.id &&
      x.email == y.email && x.firstName == y.firstName &&
      x.lastName == y.lastName && x.createdTime == y.createdTime
  }

  //we can get the same results from our equiv, although it clunky and manual
  assert(userEq.equiv(benUser, benUser.copy()))
  assert(!userEq.equiv(benUser, modifiedUser))

  //lets convert 'userEq' to a typeclass and pull in some syntatic
  //sugar for an infix equivalence operator
  implicit class EquivOp[A](a1: A)(implicit e: Equiv[A]) {

    def ===(a2: A) = e.equiv(a1, a2)
    def !==(a2: A) = !e.equiv(a1, a2)
  }

  {
    implicit val userEqImplicit = userEq

    assert(benUser === benUser.copy())
    assert(benUser !== modifiedUser)
  }
  //before moving on, notice how we had to name our equivalence methods
  //differently from Scala's built in 00- equality methods '==' and '!='
  //In Scala, OO- methods always bind tighter than typeclass methods
  //which is a recurring obstacle to typeclass usage


  //So far, we've done alot work to re-implement a built in operation...
  //Where's the payoff?
  //
  //The single biggest benefit of the typeclass approach comes when we want to
  //change or re-interpret an operation.
  //The difficulty of doing this is object-orientation's 'Achilles Heel'
  //because data and behavior are irreversibly coupled

  //let's look at some plausible reasons why we might want to re-interpret
  //'equivalence' for a user

/* Scenario: if we are certain that our Id value uniquely identifies
a User, then we can optimize equality checking by comparing only
this 'primary key'.
 */
  {
    implicit val userIdEq = new Equiv[User] {
      def equiv(x: User, y: User): Boolean = x.id == y.id
    }

    assert(benUser === modifiedUser)
  }

/* Scenario: different sub-systems use differing primary keys
that almost but don't quite match up. For example, identifying
users by Id vs Email address

  +---------------+            +-------------------+
  |   Business    |            | Business System B |
  |   System A    |<----------+|  Email Keyed      |
  |   Id Keyed    |+---------->|                   |
  +---------------+            +-------------------+
             99% correspondence between
                 data in 2 systems
*/
  {
    val sameEmailDifferentIdUser = benUser.copy(id = 123)

    implicit val userEmailEq = new Equiv[User] {
      def equiv(x: User, y: User): Boolean = x.email == y.email
    }

    assert(benUser === sameEmailDifferentIdUser)
  }

  {
    //Using multiple Equiv typeclasses together
    //dispatching on type
    
    implicit val userEqImplicit = userEq

    case class SuburbRating(
      name: String,
      postcode: String,
      state: String,
      rating: Int)

    implicit val suburbEq = new Equiv[SuburbRating] {
      def equiv(x: SuburbRating, y: SuburbRating): Boolean =
        x.name == y.name && x.postcode == y.postcode && x.state == y.state
    }

    def findUsersWithSameFavoriteSuburb(
      current: User,
      allUsers: Iterable[User],
      ratings: (User) => Seq[SuburbRating]): Iterable[User] = {

    	//nested functions can refer to variables defined in containing scopes
        def favoriteSuburb(u: User) = ratings(u).max[SuburbRating](Ordering.by(_.rating))

        val myFavorite = favoriteSuburb(current)
        val otherUsers = allUsers.filter(_ !== current)
        otherUsers.filter((u) => favoriteSuburb(u) === myFavorite)
    }
  }
}
