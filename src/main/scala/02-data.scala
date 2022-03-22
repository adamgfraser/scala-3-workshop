/**
 * ENUMS
 * 
 * Scala 3 adds support for "enums", which are to sealed traits like case classes 
 * were to classes. That is, enums cut down on the boilerplate required to use 
 * the "sealed trait" pattern for modeling so-called sum types, in a fashion very 
 * similar to how case classes cut down on the boilerplate required to use 
 * classes to model so-called product types.
 * 
 * Strictly speaking, Scala 3 enums are not the same as Java enums: while the 
 * constructors of enums are finite, and defined statically at compile-time in the 
 * same file, these constructors may have parameters, and therefore, the total 
 * number of values of any enum type could be large or infinite.
 * 
 * Enums and case classes provide first-class support for "algebraic data types" 
 * in Scala 3.
 */

// product types (A and B) final case class Person(name: String, age: Int)
// sum types (A or B) enum

package enums: 
  /**
   * EXERCISE 1
   * 
   * Convert this "sealed trait" to an enum.
   */
  enum DayOfWeek extends Enum[DayOfWeek]:
    case Sunday
    case Monday
    case Tuesday
    case Wednesday
    case Thursday
    case Friday
    case Saturday

  /**
   * EXERCISE 2
   * 
   * Augment `DayOfWeek` with Java enum compatibility, by extending `Enum[DayOfWeek]`. Then explore 
   * this interop with Java enums by finding all values of `DayOfWeek`, and by finding the value 
   * corresponding to the string "Sunday".
   */
  def daysOfWeek: Array[DayOfWeek] = DayOfWeek.values
  def sunday: DayOfWeek = DayOfWeek.valueOf("Sunday")

  /**
   * EXERCISE 3
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * Take special note of the inferred type of any of the case constructors!
   */
  enum Color:
    case Red
    case Green
    case Blue
    case Custom(red: Int, green: Int, blue: Int)

  val myColor: Color = Color.Custom(123, 23, 5)

  /**
   * EXERCISE 4
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * Take special note of the inferred type parameters in the case constructors!
   */
  enum Result[+Error, +Value]:
    case Succeed[+Value](value: Value) extends Result[Nothing, Value]
    case Fail[+Error](error: Error) extends Result[Error, Nothing]

  /**
   * EXERCISE 5
   * 
   * Convert this "sealed trait" to an enum.
   * 
   * Take special note of the inferred type parameters in the case constructors!
   */
  enum Workflow[-Input, +Output]:
    case End[Output](value: Output) extends Workflow[Any, Output]

  /**
   * EXERCISE 6
   * 
   * Convert this "sealed trait" to an enum.
   */
  enum Conversion[-From, +To]:
    case AnyToString extends Conversion[Any, String]
    case StringToInt extends Conversion[String, Option[Int]]

/**
 * CASE CLASSES
 * 
 * Scala 3 makes a number of improvements to case classes.
 */
package case_classes:
  /**
   * EXERCISE 1
   * 
   * By making the public constructor private, make a smart constructor for `Email` so that only 
   * valid emails may be created.
   */
  final case class Email private (value: String)
  object Email:
    def fromString(v: String): Option[Email] =
      if (isValidEmail(v)) Some(Email(v)) else None

    def isValidEmail(v: String): Boolean = v.matches("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$")

  /**
   * EXERCISE 2
   * 
   * Try to make a copy of an existing `Email` using `Email#copy` and note what happens.
   * 
   */
  def changeEmail(email: Email): Email =
    ???

  /**
   * EXERCISE 3
   * 
   * Try to create an Email directly by using the generated constructor in the companion object.
   * 
   */
  def caseClassApply(value: String): Email = ???

/**
 * PATTERN MATCHING
 * 
 * Scala 3 provides upgrades to the power and flexibility of pattern matching.
 */  
object pattern_matching:
  /**
   * EXERCISE 1
   * 
   * Remove the unsound usage of `ClassTag` from this pattern match, replacing it with `Typeable`.
   */
  // def getT[T <: Matchable](list: List[Any])(implicit ev: scala.reflect.TypeTest[T, Any]): Option[T] = 
  //   list match 
  //     case (head : T) :: _ => Some(head)
  //     case _ => None 

  val h :: t = ::("foo", Nil)

  val x = 1
  val y = 2

  /**
   * EXERCISE 2
   * 
   * Scala 3 more strongly checks patterns. Add some more irrefutable patterns to this `for` 
   * comprehension.
   */
  for
    (l, r) <- Some((x, y))
  yield l + r