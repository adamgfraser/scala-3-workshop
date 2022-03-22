/**
 * EXTENSION METHODS
 * 
 * Scala 3 brings first-class support for "extension methods", which allow adding methods to 
 * classes after their definition. Previously, this feature was emulated using implicits.
 */

final case class StatsCounter(count: Int, sum: Int) {
  def increment(value: Int): StatsCounter =
    StatsCounter(count + 1, sum + value)
}

extension (counter: StatsCounter) def increment(): StatsCounter =
  counter.increment(1)

object Example:
  val x = StatsCounter(0, 0)
  x.increment()

object ext_methods:
  final case class Email(value: String)

  /**
   * EXERCISE 1
   * 
   * Add an extension method to `Email` to retrieve the username of the email address (the part 
   * of the string before the `@` symbol).
   */
  extension (e: Email) def username: String =
    e.value.split('@').head

  val sherlock = Email("sherlock@holmes.com").username

  /**
   * EXERCISE 2
   * 
   * Add an extension method to `Email` to retrieve the server of the email address (the part of 
   * the string after the `@` symbol).
   */
  extension (e: Email) def server: String =
    e.value.split('@').last

  /**
   * EXERCISE 3
   * 
   * Add an extension method to `Int` called `split` that can package up the high 16 bits and 
   * low 16 bits into a tuple of two ints, containing the two parts.
   */
  extension (n: Int) def split: (Int, Int) =
    (n >> 16, n & 0xFFFF)

  /**
   * A rational number is one in the form n/m, where n and m are integers.
   */
  final case class Rational(numerator: BigInt, denominator: BigInt):
    def +(that: Rational): Rational =
      ???

  /**
   * EXERCISE 4
   * 
   * Extension methods may be operators, such as `+` or `-`. Add a collection of extension methods 
   * to `Rational`, including `+`, to add two rational numbers, `*`, to multiply two rational 
   * numbers, and `-`, to subtract one rational number from another rational number.
   */
  extension (r: Rational) def +(that: Rational): Rational =
    ???

  val rational1 = Rational(1, 2)
  val rational2 = Rational(2, 3)
  val rational3 = rational1 + rational2

  /**
   * EXERCISE 5
   * 
   * Convert this implicit syntax class to use extension methods.
   */
  extension (self: String) def equalsIgnoreCase(that: String) =
    self.toLowerCase == that.toLowerCase

  /**
   * EXERCISE 6
   * 
   * Import the extension method `isSherlock` into the following object so the code will compile.
   */
  object test:
    import string_extensions.isSherlock

    val test: Boolean = "John Watson".isSherlock

  object string_extensions:
    extension (s: String) def isSherlock: Boolean = s.startsWith("Sherlock")

  /**
   * EXERCISE 7
   * 
   * Extension methods may be generic. Define a generic extension method called `uncons`, which 
   * works on any `List[A]`, and which returns an `Option[(A, List[A])]` (either `None` if the list 
   * is empty, or otherwise the head and tail in a tuple).
   */

  // cons (A, List[A]) => List[A]  1 :: List(2, 3)
  // uncons List[A] => (A, List[A])
  // xs match { case h :: t => ??? }
  object list_extensions:
    val test: Option[(String, List[String])] = List("foo", "bar").uncons

    extension [A] (xs: List[A]) def uncons: Option[(A, List[A])] =
      xs match
        case Nil => None
        case x :: xs => Some(x, xs)

  /**
   * EXERCISE 8
   * 
   * Add another generic extension method called `zip` to `Option[A]`, which takes an `Option[B]`, 
   * and returns an `Option[(A, B)]`.
   */
  object option_extensions:
    val test: Option[(Int, String)] = Some(123).zip(Some("foo"))

    extension[A] (x: Option[A]) def zip[B](y: Option[B]): Option[(A, B)] =
      (x,y) match {
        case (Some(x), Some(y)) => Some((x,y))
        case _    => None
      }

  /**
   * EXERCISE 9
   * 
   * One possible application of extension methods is adding methods to generic types of a certain
   * shape. For example, adding `flatten` to a `List[List[A]]`, but not to other types.
   * Add `mapInside` method to `List[Option[A]]` to map on the `A` inside the futures.
   */
  object list_future_extensions:
    val digits: List[Option[Int]] = List(Some("12"), None, Some("321")).mapInside(_.length)

    extension[A] (xs: List[Option[A]]) def mapInside[B](f: A => B): List[Option[B]] =
      xs.map(_.map(f))

    def mapInside0[A, B](xs: List[Option[A]])(f: A => B): List[Option[B]] =
      xs.map(_.map(f))

      mapInside(???)(f)

  trait MyList[+A] {
    // def mapInside[B, C](f: B => C)(implicit ev: A <:< Option[B]): MyList[Option[C]] =
    //   map { a =>
    //     val optionB = ev(a)
    //     optionB.map(f)
    //   }
    def map[B](f: A => B): MyList[B] =
      ???
  }

  object MyList {

    // Scala 2
    // implicit final class MapInsideOps[A](private val self: MyList[Option[A]]) extends AnyVal {
    //   def mapInside[B](f: A => B): MyList[Option[B]] =
    //     self.map(_.map(f))
    // }

    // Scala 3
    extension[A] (xs: MyList[Option[A]]) def mapInside[B](f: A => B): MyList[Option[B]] =
      xs.map(_.map(f))
  }