/**
 * TYPECLASSES
 * 
 * Scala 3 introduces direct support for typeclasses using contextual features of the language.
 * Typeclasses provide a way to abstract over similar data types, without having to change the 
 * inheritance hierarchy of those data types, providing the power of "mixin" interfaces, but 
 * with additional flexibility that plays well with third-party data types.
 */


object TypeclassIntroduction:

  // type classes are an alternative for descibing common behavior through
  // inheritance

  // abstract over things that can be encoded to JSON
  // Strings, Numbers, Domain objects, that can all be encoded to JSON

  trait Json

  object ObjectOriented:

    // A => Json
    trait JsonEncodeable:
      def toJson: Json

    trait MyDomainObject extends JsonEncodeable:
      def toJson: Json =
        ???

    object MyDomainObject extends JsonDecodable[MyDomainObject]:
      def fromJson(json: Json): MyDomainObject =
        ???

    trait JsonDecodable[A]:
      def fromJson(json: Json): A

    // Strings can be encoded to JSON???

    // String is defined in Java / Scala so I can't just extend it with new traits
    // trait String extends JsonEncodeable

  object UsingTypeClassesScala2:
    import JsonEncoder.*

    trait MyDomainObject

    trait JsonEncoder[-A]:
      def encode(value: A): Json

    object JsonEncoder:

      def apply[A](implicit encoder: JsonEncoder[A]): JsonEncoder[A] =
        encoder

      implicit val gobblediegook: JsonEncoder[String] =
        ???
      implicit val myDomainObjectEncoder: JsonEncoder[MyDomainObject] =
        ???

      implicit final class JsonEncoderOps[A](private val value: A) extends AnyVal:
        def toJson(implicit encoder: JsonEncoder[A]): Json =
          encoder.encode(value)

    trait JsonDecoder[+A]:
      def decode(value: Json): A    

    object JsonDecoder:

      def apply[A](implicit decoder: JsonDecoder[A]): JsonDecoder[A] =
        decoder

    val string = "hello"
    string.toJson

    val stringEncoder = JsonEncoder[String]
    stringEncoder.encode("world")

  object UsingTypeClassesScala3:
    import JsonEncoder.given_JsonEncoder_String

    trait MyDomainObject

    trait JsonEncoder[-A]:
      extension (value: A) def encode: Json
      extension (value: A) def toJson: Json =
        value.encode

    object JsonEncoder:

      given JsonEncoder[String] with
        extension (value: String) def encode: Json =
          ???

      given JsonEncoder[MyDomainObject] with
        extension (value: MyDomainObject) def encode: Json =
          ???

    trait JsonDecoder[+A]:
      extension (value: Json) def decode: A    

    object JsonDecoder

    val string = "hello"
    string.toJson

    val stringEncoder = summon[JsonEncoder[String]]
    stringEncoder.encode("world")


end TypeclassIntroduction

// "extension" - defining extension methods
// "given" - defining instances that are available for the compiler to look up based on type
// "using" - asking the scala compiler for a value of a certain type

object typeclass_basics:
  trait PrettyPrint[-A]:
    extension (a: A) def prettyPrint: String

  // implicit val stringPrettyPrint: PrettyPrint[String] =
  // new PrettyPrint[String] {
  //  def prettyPrint(a: String): String = ???
  // }
  // anonymous given instance

  given PrettyPrint[String] with
    extension (a: String) def prettyPrint: String = a

  "foo".prettyPrint

  final case class Person(name: String, age: Int)

  /**
   * EXERCISE 1
   * 
   * With the help of the `given` keyword, create an instance of the `PrettyPrint` typeclass for the 
   * data type `Person` that renders the person in a pretty way.
   */
  given PrettyPrint[Person] with
    extension (a: Person) def prettyPrint: String =
      s"Person(${a.name}, ${a.age})"

  /**
   * EXERCISE 2
   * 
   * With the help of the `given` keyword, create a **named* instance of the `PrettyPrint` typeclass 
   * for the data type `Int` that renders the integer in a pretty way.
   */
  given PrettyPrint[Int] with
    extension (a: Int) def prettyPrint: String =
      a.toString

  /**
   * EXERCISE 3
   * 
   * Using the `summon` function, summon an instance of `PrettyPrint` for `String`.
   */
  val stringPrettyPrint: PrettyPrint[String] = summon[PrettyPrint[String]]

  /**
   * EXERCISE 4
   * 
   * Using the `summon` function, summon an instance of `PrettyPrint` for `Int`.
   */
  val intPrettyPrint: PrettyPrint[Int] = summon[PrettyPrint[Int]]

  /**
   * EXERCISE 5
   * 
   * With the help of the `using` keyword, create a method called `prettyPrintIt` that, for any type 
   * `A` for which a `PrettyPrint` instance exists, can both generate a pretty-print string, and 
   * print it out to the console using `println`.
   */
  extension[A] (a: A) def prettyPrintIt(using PrettyPrint[A]): Unit = {
    val string = a.prettyPrint
    println(string)
  }

  "Adam".prettyPrintIt

  // inductive typeclass instances
  // Given an instance for the parts of something we can "derive" an instance
  // for the whole thing

  // given ==> insert an entry into Scala compiler global map from type to typeclass instance
  // using ==> lookup an entry in the Scala compiler global map

  /**
   * EXERCISE 6
   * 
   * With the help of both `given` and `using`, create an instance of the `PrettyPrint` type class
   * for a generic `List[A]`, given an instance of `PrettyPrint` for the type `A`.
   */
  given [A] (using PrettyPrint[A]): PrettyPrint[List[A]] with
    extension (a: List[A]) def prettyPrint: String =
      a.map(_.prettyPrint).mkString("[", ", ", "]")

  // Scala 2 equivalent
  // implicit def listPrettyPrint[A](implicit prettyPrint: PrettyPrint[A]): PrettyPrint[List[A]] =
  //   new PrettyPrint[List[A]] {
  //     def prettyPrint(a: List[A]): String =
  //       a.map(_.prettyPrint).mkString("[", ", ", "]")
  //   }

  /**
   * EXERCISE 7
   * 
   * With the help of both `given` and `using`, create a **named** instance of the `PrettyPrint` 
   * type class for a generic `Vector[A]`, given an instance of `PrettyPrint` for the type `A`.
   */
  given [A] (using PrettyPrint[A]): PrettyPrint[Vector[A]] with
    extension (a: Vector[A]) def prettyPrint: String =
      a.map(_.prettyPrint).mkString("[",",","]")

  import scala.CanEqual.*

object given_scopes:
  trait Hash[-A]:
    extension (a: A) def hash: Int

  // single abstract method syntax

  object Hash:
    given Hash[Int] = _.hashCode
    given Hash[Long] = _.hashCode
    given Hash[Float] = _.hashCode
    given Hash[Double] = _.hashCode

  object givens {
    import given_scopes.Hash.{given_Hash_Int, given_Hash_Double}

    /**
     * EXERCISE 1
     * 
     * Import the right given into the scope (but ONLY this given) so the following code will compile.
     */
    12.hash 

    /**
     * EXERCISE 2
     * 
     * Import the right given into the scope (but ONLY this given) so the following code will compile.
     */
     12.123.hash   
  }

  object more_typeclasses:
    /**
     * EXERCISE 1
     * 
     * Define a typeclass called `Enumerable[A]` that can list all of the `A` values of a type with 
     * a finite number of values. One way to do this is to add a single operation called 
     * `enumerate` that returns a `List[A]`.
     */
    trait Enumerable[+A]:
      def enumerate: List[A]

    object Enumerable:

      def enumerate[A](using Enumerable[A]): List[A] =
        summon[Enumerable[A]].enumerate

      /**
       * EXERCISE 2
       * 
       * Define an instance of the typeclass Enumerable for `Boolean`.
       */
      given Enumerable[Boolean] with
        def enumerate: List[Boolean] =
          List(true, false)


      // Option[Boolean] // list all the types of that value
      // Boolean possible values are true, false
      // What are the possible values of Option[Boolean]?
      // None, Some(true), Some(false)

      // stuff all values of A inside Some and then add None
    
      /**
       * EXERCISE 3
       * 
       * Instances for generic data types often depend on instances for their element types.
       * With `using`, this relationship can be captured precisely. Define an `Enumerable` instance
       * for `Option[A]` given an instance for `A`.
       */
      given [A](using Enumerable[A]): Enumerable[Option[A]] with 
        def enumerate: List[Option[A]] =
          None :: Enumerable.enumerate[A].map(Some(_))

    end Enumerable

    /**
     * EXERCISE 4
     * 
     * By adding a `using`, implement this polymorphic function to return the "first" enumerable
     * value.
     */
    def first[A](using Enumerable[A]): A =
      Enumerable.enumerate[A].head

    /**
     * EXERCISE 5
     * 
     * By adding a `using`, implement this polymorphic function to return the "last" enumerable
     * value.
     */
    def last[A](using Enumerable[A]): A =
      Enumerable.enumerate[A].last

    /**
     * EXERCISE 6
     * 
     * By adding a `using`, implement this polymorphic function to return the "ordinal" of the 
     * specified enumerable value.
     */
    def ordinalOf[A](value: A)(using Enumerable[A]): Int =
      Enumerable.enumerate[A].indexOf(value)

    /**
     * EXERCISE 7
     * 
     * Make an extension method called `ordinal` that is added to any data type that has an 
     * `Enumerable` instance. You will have to use `using`.
     */
    extension [A](value: A)(using Enumerable[A]) def ordinal: Int =
      ordinalOf(value)

    Option(true).ordinal
    type Dummy

    /**
     * EXERCISE 8
     * 
     * Givens can be introduced with an expression by using a so-called "alias given". The 
     * difference is that instead of using `with` and defining the body of the given, you 
     * instead use `=` and set the instance equal to a specified value.
     * 
     * Define a `Show` for `Email` by using a given alias and the `contramap` function on 
     * the `Show` instance for `String`.
     */
    given Show[Email] =
      summon[Show[String]].contramap {
        case Email(value) => s"Email($value)"
      }

    trait Show[-A]: 
      self =>
        extension (value: A) def show: String

        def contramap[B](f: B => A): Show[B] = 
          (b: B) => self.show(f(b))

    final case class Email(value: String)

    object Show:
      given Show[String] with 
        extension (value: String) def show: String = value 

    val showString = Show.given_Show_String

    // "Adam".show

    /**
     * EXERCISE 9
     * 
     * You can introduce givens inside patterns. These are called "pattern-bound" given instances.
     * Using some example of destructuring assignment, introduce a given with a pattern.
     */
    Some(showString) match 
      case Some(given Show[String]) => "Adam".show

    /**
     * EXERCISE 10
     * 
     * Using `NotGiven`, you can create given instances that will be used only if the specified
     * instance is not available.
     * 
     * Using `NotGiven`, create an instance of `Show` for `List[A]` only when there is not already 
     * a given instance of `Show` for `A`.
     */
    import scala.util.NotGiven 
    given [A](using NotGiven[A]): Show[List[A]] = 
      (list: List[A]) => list.map(_.toString).mkString("[",",","]")


    /**
     * EXERCISE 11
     * 
     * Adding the right `using` clause to this function so that it compiles.
     */
    def hashing[T](value: T)(using Hash[T]) = value.hash 

    /**
     * EXERCISE 12
     * 
     * Adding the right `using` clause to this function so that it compiles.
     */
    def hashingDoubles(using Hash[Double]) = 12.123.hash   

    /**
     * EXERCISE 13
     * 
     * Intersperse normal parameters with their `using Hash[X]` for those parameters.
     */
    def intersperse[A, B, C, D] = ???
  
object typeclass_derives:

  // JsonEncoder[String]
  // JsonEncoder[Int]

  // JsonEncoder[Person] <== would like compiler to automatically derive

  // @derive(JsonEncoder)
  // final case class Person(name: String, age: Int)

  trait JsonEncoder[A]

  object JsonEncoder

  /**
   * EXERCISE 1
   * 
   * Using the `derives` clause, derive an instance of the type class `CanEqual` for 
   * `Color`.
   */
  enum Color derives CanEqual:
    case Red 
    case Green 
    case Blue

  /**
   * EXERCISE 2
   * 
   * Using the `derives` clause, derive an instance of the type class `CanEqual` for 
   * `Person`.
   */
  final case class Person(name: String, age: Int)

  implicit def toOption[A](value: A): Option[A] = Some(value)

  val x: Option[Person] = Person("Adam", 23)

  Person("Adam", 23).zip(Person("Bob", 42))

/**
 * IMPLICIT CONVERSIONS
 * 
 * Scala 3 introduces a new type class called `Conversion` to perform "implicit 
 * conversions"--the act of automatically converting one type to another.
 */
object conversions:
  final case class Rational(n: Int, d: Int)

  /**
   * EXERCISE 1
   * 
   * Create an instance of the type class `Conversion` for the combination of types
   * `Rational` (from) and `Double` (to).
   */
  given Conversion[Rational, Double] =
    (r: Rational) => r.n.toDouble / r.d.toDouble

  /**
   * EXERCISE 2
   * 
   * Multiply a rational number by 2.0 (a double) to verify your automatic
   * conversion works as intended.
   */
  Rational(1, 2) * 2.0

object typeclass_graduation extends App:
  /**
   * EXERCISE 1
   * 
   * Add cases to this enum for every primitive type in Scala.
   */
  enum PrimType[A]:
    case Boolean extends PrimType[Boolean]
    case Byte extends PrimType[Byte]
    case Char extends PrimType[Char]
    case Double extends PrimType[Double]
    case Float extends PrimType[Float]
    case Int extends PrimType[Int]
    case Long extends PrimType[Long]
    case Short extends PrimType[Short]
    case String extends PrimType[String]
    case Unit extends PrimType[Unit]
  
  /**
   * EXERCISE 2
   * 
   * Add another case to `Data` to model enumerations, like `Either`.
   */
  enum Data:
    case Record(fields: Map[String, Data])
    case Primitive[A](primitive: A, primType: PrimType[A])
    case Collection(elements: Vector[Data])
    case Enumeration(caseValue: Data, index: Int)

  /**
   * EXERCISE 3
   * 
   * Develop a type class called `EncodeData[A]`, that can encode an `A` into `Data`.
   */
  trait EncodeData[A]:
    extension (a: A) def encode: Data

  /**
   * EXERCISE 4
   * 
   * In the companion object of `Data`, write encoders for different primitive types in Scala,
   * including lists and collections.
   */
  object EncodeData:

    given EncodeData[Boolean] = Data.Primitive(_, PrimType.Boolean)
    given EncodeData[Byte] = Data.Primitive(_, PrimType.Byte)
    given EncodeData[Char] = Data.Primitive(_, PrimType.Char)
    given EncodeData[Double] = Data.Primitive(_, PrimType.Double)
    given EncodeData[Float] = Data.Primitive(_, PrimType.Float)
    given EncodeData[Int] = Data.Primitive(_, PrimType.Int)
    given EncodeData[Long] = Data.Primitive(_, PrimType.Long)
    given EncodeData[Short] = Data.Primitive(_, PrimType.Short)
    given EncodeData[String] = Data.Primitive(_, PrimType.String)
    given EncodeData[Unit] = Data.Primitive(_, PrimType.Unit)

    given [A, B](using EncodeData[A], EncodeData[B]): EncodeData[(A, B)] = 
      (a: A, b: B) => Data.Record(Map("_1" -> a.encode, "_2" -> b.encode))

    given [A](using EncodeData[A]): EncodeData[List[A]] =
      as => Data.Collection(as.map(_.encode).toVector)

    given [K, V](using EncodeData[K], EncodeData[V]): EncodeData[Map[K, V]] =
      map => Data.Collection(map.toVector.map(_.encode))

    given [A, B](using EncodeData[A], EncodeData[B]): EncodeData[Either[A, B]] = {
      case Left(a) => Data.Enumeration(a.encode, 0)
      case Right(b) => Data.Enumeration(b.encode, 1)
    }

  import EncodeData.{given_EncodeData_Int, given_EncodeData_String, given_EncodeData_List}

  /**
   * EXERCISE 5
   * 
   * Create an instance of `EncodeData` for `Person`.
   */
  final case class Person(name: String, age: Int)
  object Person {
    given EncodeData[Person] =
      p => Data.Record(Map("name" -> p.name.encode, "age" -> p.age.encode))
  }

  println(Person("Adam", 23).encode)

  // Record(Map(name -> Primitive(Adam,String), age -> Primitive(23,Int)))

  final case class Criminal(name: String, age: Int, knownAliases: List[Person])

  object Criminal:
    given EncodeData[Criminal] =
      c => Data.Record(Map(
        "name" -> c.name.encode,
        "age" -> c.age.encode,
        "knownAliases" -> c.knownAliases.encode
      ))