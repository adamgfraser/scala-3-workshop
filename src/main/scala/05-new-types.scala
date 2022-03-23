import javax.sql.DataSource
import UnionTypeExample.SubscriptionService
import UnionTypeExample.PersistenceService
import UnionTypeExample.ZIO
import UnionTypeExample.PersistenceError
import UnionTypeExample.SubscriptionError
import OpaqueTypesGraduation.Definition.ViewedPercentage

/**
 * Scala 3 introduces several new types that increase the power of the Scala type system.
 */

object Scala2Example {

  trait DataSource

  trait Closeable

  type MyDataSource = DataSource with Closeable
}

object WithExamples extends App {

  trait Greeter {
    def hi: String
    def greet(name: String): String
  }

  trait Welcomer {
    def welcome(name: String): String
    def greet(name: String): String
  }

  trait DefaultGreeter extends Greeter {
    def hi: String = "Hello"
    def greet(name: String): String = s"Hi, $name"
  }

  trait DefaultWelcomer extends Welcomer {
    def welcome(name: String): String = s"Welcome, $name"
    def greet(name: String): String = s"Welcome, $name"
  }

  trait DefaultGreeterWithWelcomer extends DefaultGreeter with DefaultWelcomer {
    override def greet(name: String): String = super.greet(name)
  }

  trait DefaultWelcomerWithGreeter extends DefaultWelcomer with DefaultGreeter {
    override def greet(name: String): String = super.greet(name)
  }

  val greeter1 = new DefaultGreeterWithWelcomer {}
  val greeter2 = new DefaultWelcomerWithGreeter {}

  println(greeter1.greet("Adam"))
  println(greeter2.greet("Adam"))

  def IsEqual[A, B](using ev: A =:= B) = ()

  IsEqual[Welcomer & Greeter, Greeter & Welcomer]

}

/**
 * INTERSECTION TYPES
 * 
 * Scala 3 introduces intersection types, which are a commutative version of the `with` operator.
 * In Scala 3, `A & B` is the same type as `B & A`. Whereas, `A with B` is only the same as 
 * `B with A` in the event there are no overlaps between `A` and `B`.
 * 
 * Intersection types are useful to describe types having all the members of other types.
 * 
 * Commutativity:  A & B == B & A
 * Associativity:  (A & B) & C == A & (B & C)
 * Annihilation:   A & Nothing == Nothing
 * Identity:       A & Any == Any 
 * Distributivity: A & (B | C) == A & B | A & C
 */
object intersection_types:
  final case class User(name: String, id: String, email: String)
  
  trait HasLogging:
    def logging: Logging

  final case class Logging(log: String => Unit)
  val TestLogging = Logging(println(_))

  trait HasUserRepo:
    def userRepo: UserRepo

  final case class UserRepo(getUserById: String => User)
  val TestUserRepo = UserRepo(_ => User("Sherlock Holmes", "sholmes", "sherlock@holmes.com"))

  /**
   * EXERCISE 1
   * 
   * Form the intersection of the types `HasLogging` and `HasUserRepo` by using the type 
   * intersection operator `&`.
   */
  type HasLoggingAndUserRepo = HasLogging & HasUserRepo

  /**
   * EXERCISE 2
   * 
   * Using the `IsEqual` helper method, test to see if the type `HasLogging & HasUserRepo` is the 
   * same as the type `HasUserRepo & HasLogging`.
   */
  def IsEqual[A, B](using ev: A =:= B) = ()

  IsEqual[HasLogging & HasUserRepo, HasUserRepo & HasLogging]

  /**
   * EXERCISE 3
   * 
   * To create a class with a given intersection type, the `with` operator may be used.
   * 
   * Create class that has the type `HasUserRepo & HasLogging`.
   */
  class BothUserRepoAndLogging extends HasUserRepo with HasLogging:
    def logging: Logging = TestLogging
    def userRepo: UserRepo = TestUserRepo

object UnionTypeExample {

  // Persistence service
  // Subscription service

  trait PersistenceService
  trait SubscriptionService

  sealed trait PersistenceError // lots of subtypes of this
  sealed trait SubscriptionError // lots of subtypes of this

  trait ZIO[-R, +E, +A] {
    def map[B](f: A => B): ZIO[R, E, B] = ???
    def flatMap[R1, E1, B](f: A => ZIO[R1, E1, B]): ZIO[R & R1, E | E1, B] = ???
  }

  extension [R, E1, E2, A] (zio: ZIO[R, E1 | E2, A]) def handleSome[A1 >: A](f: E1 => A): ZIO[R, E2, A] =
    ???

  type X = PersistenceError | SubscriptionError

  val persistSomething: ZIO[PersistenceService, PersistenceError, Unit] = ???
  val subscribeToSomething: ZIO[SubscriptionService, SubscriptionError, Unit] = ???

  val doSomething: ZIO[PersistenceService & SubscriptionService, PersistenceError | SubscriptionError, Unit] =
    for {
      _ <- persistSomething
      _ <- subscribeToSomething
    } yield ()

  // val doSomething2 = doSomething.handleSome((e: PersistenceError) => println(e))
}

/**
 * UNION TYPES
 * 
 * Scala 3 introduces union types, which have no direct analogue in Scala 2.x. The union of two 
 * types `A` and `B`, written `A | B`, describes the type of values that have either type `A` or 
 * type `B`. For example, `Int | String` is the type of values that have either type `Int` or 
 * type `String`. Union types are powerful but do have limitations stemming from type erasure.
 * 
 * Commutativity: A | B == B | A
 * Associativity: A | (B | C) == (A | B) | C
 * Identity:      A | Nothing == A 
 * Annihilation:  A | Any = Any
 * Subsumption:   forall B >: A: A | B == B
 */
object union_types:
  final case class PaymentDenied(message: String)
  final case class MissingAddress(message: String)
  final case class NetworkError(message: String)

  /**
   * EXERCISE 1
   * 
   * Form the union of the types `PaymentDenied` and `MissingAddress` using the type union 
   * operator `|`.
   */
  type PaymentDeniedOrMissingAddress = PaymentDenied | MissingAddress

  /**
   * EXERCISE 2
   * 
   * Create a value of type `PaymentDeniedOrMissingAddress` by assigning the following variable to 
   * a `PaymentDenied` error.
   */
  val example1: PaymentDeniedOrMissingAddress =
    PaymentDenied("Payment denied")

  /**
   * EXERCISE 3
   * 
   * Create a value of type `PaymentDeniedOrMissingAddress` by assigning the following variable to 
   * a `MissingAddress` error.
   */
  val example2: PaymentDeniedOrMissingAddress =
    MissingAddress("ah!")

  /**
   * EXERCISE 4
   * 
   * Perform a pattern match on `example2`, covering each possibility and printing out the 
   * error messages to the console.
   */
  example2 match
    case PaymentDenied(message) => println(s"Payment denied: $message")
    case MissingAddress(message) => println(s"Missing address: $message") 

  /**
   * EXERCISE 5
   * 
   * Try to pattern match on `SomeList` and handle both cases. Explain 
   * your findings and what this implies about union types.
   */
  def whatList(l: SomeList) = ???

  type SomeList = List[String] | List[Int]


object MatchingExample {

  List(1, 2, 3) match {
    case h :: t => ???
    case Nil    => ???
  }

  // type Weird[A] = A match {
  //   case Int => String
  //   case String => Int
  // }
}

/**
 * MATCH TYPES
 * 
 * Match types bring the `match` construct to the type level, allowing the creation of type-level 
 * functions that return different types depending on the (statically known) input types.
 */

// def combine[L, R](L, R): ???

object match_types:
  type Combine[Left, Right] = Left match
    case Unit => Right 
    case ? => 
      Right match
        case Unit => Left 
        case ? => (Left, Right)


  type Result1 = Combine[Unit, Unit]
  type Result2 = Combine[Unit, Int]
  type Result3 = Combine[Int, Unit]
  type Result4 = Combine[Int, Int]

  def isSameType[A, B](using A =:= B) = ()

  isSameType[Result1, Unit]
  isSameType[Result2, Int]
  isSameType[Result3, Int]
  isSameType[Result4, (Int, Int)]

  /**
   * EXERCISE 1
   * 
   * Construct a value of the appropriate type, which is computed using the match type `Combine`.
   */
  val unitAndString: Combine[Unit, String] =
    "foo"

  /**
   * EXERCISE 2
   * 
   * Construct a value of the appropriate type, which is computed using the match type `Combine`.
   */
  val stringAndUnit: Combine[String, Unit] =
    "bar"

  /**
   * EXERCISE 3
   * 
   * Construct a value of the appropriate type, which is computed using the match type `Combine`.
   */
  val stringAndString: Combine[String, String] =
    ("foo", "bar")

  /**
   * EXERCISE 4
   * 
   * On the JVM, collection types generally "box" primitive values like `Int`, creating wrapper
   * values around these primitives. One of the exceptions is `Array`, which does not box primitive 
   * values.
   * 
   * Create a match type that will return Scala's `Vector` for all types except primitive types,
   * but for primitive types, will return Scala's `Array`.
   */
  type Collection[X] = X match
    case Boolean => Array[Boolean]
    case Byte    => Array[Byte]
    case Long    => Array[Long]
    case Short   => Array[Short]
    case Unit    => Array[Unit]
    case Double  => Array[Double]
    case Char    => Array[Char]
    case Int => Array[Int]
    case ?   => Vector[X]

  def loadData[X]: Collection[X] =
    ???

  final case class Person(name: String, age: Int)

  val x = loadData[Int]
  val y = loadData[Person]

  type MyList = List[Int]
  type MySuperNestedList = List[List[List[List[List[Int]]]]]

  val z: ElementType[MyList] = 7
  val a: ElementType[MySuperNestedList] = 7

  /**
   * EXERCISE 5
   * 
   * Match types can be recursive. Write a match type that determines the "atom" type of a string 
   * or array or iterable.
   */
  type ElementType[X] = X match
    case String => Char
    case Array[t] => ElementType[t]
    case Iterable[t] => ElementType[t]
    case AnyVal => X

  // headOf(1)
  // headOf(List(1, 2, 3)) == 1
  // headOf(List(List(1, 2, 3))) == 1
  // headOf("Adam") == 'A'

  /**
   * EXERCISE 6
   * 
   * Match types can be used to define dependently typed methods. Implement the following 
   * `head` function which returns the head of the specified value (a character of a string, 
   * or the first element of an array or iterable, or the passed in value, otherwise).
   */
  def headOf[X](x: X): ElementType[X] =
    x.asInstanceOf[Matchable] match
      case string: String => string.head.asInstanceOf[ElementType[X]]
      case array: Array[_] => headOf(array.head).asInstanceOf[ElementType[X]]
      case iterable: Iterable[_] => headOf(iterable.head).asInstanceOf[ElementType[X]]
      case anyVal: AnyVal => anyVal.asInstanceOf[ElementType[X]]

  val example1 = headOf(1)
  val example2 = headOf(List(1, 2, 3))
  val example3 = headOf(List(List(1, 2, 3)))
  val example4 = headOf("Adam")

object Definitions:
  opaque type Password <: String = String
  object Password:
    def apply(string: String): Password = string
  opaque type FirstName <: String = String
  object FirstName:
    def apply(string: String): FirstName = string
  opaque type LastName <: String = String
  object LastName:
    def apply(string: String): LastName = string

object Newtypes {
  import Definitions.*

  // new types problem

  // how do we disambiguiate different domain types that have the same scala type

  // String
  // First name, Last name, password, welcome message

  def saveUserToSecureDatabse(firstName: FirstName, lastName: LastName, password: Password): Unit =
    ???

  val password = Password("password")
  val firstName = FirstName("John")
  val lastName = LastName("Doe")

  // final class FirstName(val value: String) extends AnyVal {
  //   def toUpperCase: FirstName =
  //     new FirstName(value.toUpperCase.asInstanceOf[String])
  // }
  // final class LastName(val value: String) extends AnyVal
  // final class Password(val value: String) extends AnyVal

  // final case class FirstName(value: String)

  // val x: String = firstName.toUpperCase

  saveUserToSecureDatabse(firstName, lastName, password)
}

// opaque types are different types at compile time
// they don't exist at all at runtime

implicit def isSameType[A, B](using A =:= B): Unit = ()

/**
 * OPAQUE TYPES
 * 
 * Opaque types are a new variant of a type alias that hides information on the underlying type.
 * This can be useful to create novel types that are backed by other types, without any runtime 
 * overhead. An example might be an "Email" type that is really backed by a "String", but which is 
 * treated as a unique (opaque) type by the Scala compiler.
 */
object opaque_types:
  object email_example:
    opaque type Email = String
    object Email:
      /**
       * EXERCISE 1
       * 
       * The scope of an opaque type has special privileges. Create a constructor for email that
       * takes a string, and returns an `Email`.
       */
      // def apply(string: String): Email =
      //   string

      def fromString(string: String): Option[Email] =
        if string.contains("@") then Some(string) else None

    end Email

    /**
     * EXERCISE 2
     * 
     * Define an extension method to retrieve the username of an email (the part before the '@' 
     * character).
     */
    extension (e: Email) def username: String =
      e.split('@').head
  end email_example

  import email_example.*

  /**
   * EXERCISE 3
   * 
   * Use the constructor you made to build an `Email` value given a `String`.
   */
  lazy val exampleEmail: Option[Email] =
    Email.fromString("adam@ziverge.com")

  // Inside the scope we know that Email === String
  // Outside scope we think that Email <: String

  

  /**
   * EXERCISE 4
   * 
   * Try to pass the email you constructed to the function `printString` and note your findings.
   */
  printString(???)

  def printString(string: String): Unit = println(string)

  object natural_example:
    /**
     * EXERCISE 5
     * 
     * Add a subtype bound to `Natural` (on the left-hand side of the equals sign). This subtype 
     * relationship must be true and it will be "exported" outside the scope in which the opaque
     * type is defined.
     */
    opaque type Natural <: Int = Int

    object Natural:
      /**
       * EXERCISE 6
       * 
       * Define a smart constructor that, given an `Int`, may or may not return a `Natural`, 
       * depending on whether the number is a natural number (non-negative) or not.
       */
      def fromInt(i: Int): Option[Natural] =
        if i >= 0 then Some(i) else None
    end Natural
  end natural_example

  import natural_example.*

  /**
   * EXERCISE 7
   * 
   * Construct an example natural number from the number 5, and call `get` on the `Option` because
   * you know it is a natural number.
   */
  lazy val exampleNatural: Natural =
    Natural.fromInt(5).get

  /**
   * EXERCISE 8
   * 
   * Try to pass the natural number to the function `printInt` and note your findings.
   */
  printInt(exampleNatural)

  def printInt(v: Int): Unit = println(v.toString())

object OpaqueTypesGraduation:

  object TypeClassDefinition:

    type Json = String

    trait JsonEncoder[-A]:
      extension (a: A) def toJson: Json

  end TypeClassDefinition

  object Definition:
    import TypeClassDefinition.*

    opaque type ViewedPercentage <: Double = Double

    object ViewedPercentage:

      def fromDouble(d: Double): Option[ViewedPercentage] =
        if d >= 0 && d <= 100.0 then Some(d) else None

      // rounding would be nice to add later...    
      extension (vp: ViewedPercentage) def prettyPrint: String =
        s"${vp * 100}%"

      given JsonEncoder[ViewedPercentage] with
        extension (vp: ViewedPercentage) def toJson: Json =
          vp.prettyPrint

    end ViewedPercentage

  end Definition

  object Use:
    import Definition.*

    val viewedPercentage: ViewedPercentage =
      ViewedPercentage.fromDouble(0.55).get

    // Do something with a viewed percentage that treats it as a double
    val total = 1000
    val viewed = total * viewedPercentage / 100

    // Define and use a extension method that is specified to the viewed
    // percentage type
    // define and use a prettyPrint extension method that prints the viewed
    // percentage in an appropriate way
    viewedPercentage.prettyPrint

    viewedPercentage.toJson

  end Use

end OpaqueTypesGraduation

object Polymorphic {

  // universally quantified function
  // for any A I can apply this function

  trait Identity {
    def apply[A, B](a: A, b: B): (B, A) = (b, a)
  }

  // for a particular A I can apply this function
  def swap[A, B](a: A, b: B): (B, A) = (b, a)
}

/**
 * POLYMORPHIC FUNCTION TYPES
 * 
 * Scala 3 introduces polymorphic function types, which gives functions the ability to be 
 * parametrically polymorphic. In Scala 2.x, only methods may be parametrically polymorphic.
 */
object polymorphic_functions:
  def identityMethod[A](a: A): A = a 
  val identityFn: [X] => X => X = [A] => (a: A) => a

  /**
   * EXERCISE 1
   * 
   * Define a polymorphic function `firstFn` that does exactly what the method `firstMethod` does.
   */
  lazy val firstFn: [A, B] => (A, B) => A = [A, B] => (a: A, b: B) => a
  def firstMethod[A, B](tuple: (A, B)): A = tuple._1

  val first1 = firstFn(1, "Hello")
  val first2 = firstFn("Hello", 1)

  /**
   * EXERCISE 2
   * 
   * Define a polymorphic function `secondFn` that does exactly what the method `secondMethod` does.
   */
  lazy val secondFn: [A, B] => (A, B) => B = [A, B] => (a: A, b: B) => b
  def secondMethod[A, B](tuple: (A, B)): B = tuple._2

/**
 * DEPENDENT FUNCTION TYPES
 * 
 * Scala 3 introduces dependent function types, which give function types the ability to model 
 * path-dependent functions that were previously only possible using methods.
 */
object dependent_functions:
  trait Entry:
    type Out

  trait StringEntry extends Entry:
    type Out = String

  trait IntEntry extends Entry:
    type Out = Int

  val x = new StringEntry {}
  val y = new IntEntry {}

  def getMethod(entry: Entry): entry.Out = ???

  val out1: String = getMethod(x)
  val out2: Int = getMethod(y)

  /**
   * EXERCISE 1
   * 
   * Explicitly provide a type signature for `getFn`.
   */
  lazy val getFn: (entry: Entry) => entry.Out = (entry: Entry) => getMethod(entry)

  trait Combine[L, R]:
    type Out

    def combine[L, R](l: L, r: R): Out

  /**
   * EXERCISE 2
   * 
   * Define a polymorphic function `combineFn` that does exactly what the method 
   * `combineMethod` does.
   */
  lazy val combineFn: [L, R] => (l: L, r: R, c: Combine[L, R]) => c.Out =
    [L, R] => (l: L, r: R, c: Combine[L, R]) => c.combine(l, r)
  def combineMethod[L, R](l: L, r: R, c: Combine[L, R]): c.Out = c.combine(l, r)

object TypeLambdasIntro {

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case h :: t => foldLeft(t)(f(z, h))(f)
    }

  val foldLeftInt: List[Int] => Int => ((Int, Int) => Int) => Int =
    foldLeft[Int, Int]

  val sum: List[Int] => Int = foldLeft(_)(0)(_ + _)

  // Collection[_] List[_] Vector[_]
  // Map[_, _]

  object Scala2Example {
    trait Sized[Collection[_]] {
      def size[A](collection: Collection[A]): Int
    }

    object Sized {
      implicit val ListSized: Sized[List] =
        new Sized[List] {
          def size[A](collection: List[A]): Int =
            collection.length
        }

      implicit val VectorSized: Sized[Vector] =
        new Sized[Vector] {
          def size[A](collection: Vector[A]): Int =
            collection.length
        }

      // Scala 2
      implicit def MapSized[K]: Sized[( { type MapPartiallyApplied[V] = Map[K, V]})#MapPartiallyApplied ] =
        new Sized[( { type MapPartiallyApplied[V] = Map[K, V]})#MapPartiallyApplied ] {
          def size[V](collection: Map[K, V]): Int =
            collection.size
        }
    }
  }

  object Scala3Example:
    trait Sized[Collection[_]] {
      def size[A](collection: Collection[A]): Int
    }

    object Sized {
      given Sized[List] =
        new Sized[List] {
          def size[A](collection: List[A]): Int =
            collection.length
        }

      given Sized[Vector] =
        new Sized[Vector] {
          def size[A](collection: Vector[A]): Int =
            collection.length
        }
      
      type MapPartiallyApplied[K] = [V] =>> Map[K, V]

      // Scala 2
      implicit def MapSized[K]: Sized[MapPartiallyApplied[K]] =
        new Sized[MapPartiallyApplied[K]] {
          def size[V](collection: Map[K, V]): Int =
            collection.size
        }
    }

}

/**
 * Scala 3 introduces first-class support for "type lambdas", which previously had to 
 * be emulated using structural types and type projection, and gave rise to the popular 
 * "kind-projector" plug-in as a way of simplifying their expression.
 */
object type_lambdas:
  type MapK[K] = [V] =>> Map[K, V]
  type MapPartiallyApplied[Key] = [Value] =>> Map[Key, Value]

  type MapWithStringKey[Value] = MapPartiallyApplied[String][Value]

  type MapStringInt = MapWithStringKey[Int]

  type MapString[V] = MapK[String][V]

  trait Sizable[F[_]]:
    def size[A](fa: F[A]): Int

  val sizableList = new Sizable[List]:
    def size[A](fa: List[A]): Int = fa.length
  /**
   * EXERCISE 1
   * 
   * Define a `Sizable` for `Map` for the given key type `K`. You will have to 
   * use a type lambda.
   */
  def sizableMap[K] = new Sizable[MapK[K]]:
    def size[V](fa: Map[K, V]): Int = fa.size

  /**
   * EXERCISE 2
   * 
   * Define a type-level function `Flip` that accepts a type constructor (`[*, *] => *`), and 
   * returns another type constructor that merely flips the order of type parameters to the first 
   * type constructor.
   */
  type Flip[F[_, _]] = [A, B] =>> F[B, A]

  val example: Flip[Tuple2][Int, String] =
    ("Adam", 42)

  // Flip[Map[K, V]] == Map[V, K]
  // Flip((Int, String)) == (String, Int)

  /**
   * EXERCISE 3
   * 
   * Use the `Flip` type constructor you defined to flip the order of type parameters to `Map`.
   */
  type FlippedMap[K, V] = Flip[Map][K, V]

  /**
   * EXERCISE 4
   * 
   * Define a type-level function `Curry` that accepts a type constructor (`[*, *] => *`), and 
   * returns another type constructor that takes one type parameter, and returns another type 
   * constructor which takes one type parameter, returning the type constructed by the original 
   * type constructor, fully applied with both type parameters.
   */
  type Curry[F[_, _]] = [A] =>> [B] =>> F[A, B]

  // Tuple2[Int, String]

  val example2: Curry[Tuple2][Int][String] =
    (42, "Adam")

  // Not supported... 
  // type Uncurry[F[_][_]] = [A, B] =>> F[A][B]

  /**
   * EXERCISE 5
   * 
   * When `-Ykind-projector` is specified, Dotty will emulate kind-projector 
   * syntax. Partially apply `Map` to the key type parameter with `K`, using the 
   * placeholder `*` for the value type parameter.
   */
  def sizableMap2[K] =  new Sizable[Map[K, *]]:
    def size[V](fa: Map[K, V]): Int = fa.size

object ContextFunctionsIntro {
  import scala.concurrent.*

  val myFutureFunction2: ExecutionContext ?=> Int => Future[Int] =
    ???

  def myFutureFunction(a: Int)(implicit ec: ExecutionContext): Future[Int] =
    ???

}

/**
 * CONTEXT FUNCTIONS
 * 
 * Scala 3 introduces context functions, which are functions that depend on some context.
 */
object context_functions extends App:
  trait Program:
    def addOp(op: Op): Unit 
  object Program:
    def make(): Program = 
      var ops = List.empty[Op]
      new Program:
        def addOp(op: Op): Unit = 
          ops = op :: ops


  def addOp(op: Op)(using p: Program) = 
    p.addOp(op)

  enum Op:
    case PushInt(v: Int)
    case Pop
    case Mul 
    case Sub
    case Add

  def op(o: Op): Program ?=> Unit = addOp(o)

  def pushInt(i: Int): Program ?=> Unit = op(Op.PushInt(i))
  val mul: Program ?=> Unit = op(Op.Mul)

  def program[A](f: Program ?=> A): A = 
    given Program = Program.make()
    f 

  program {
    pushInt(12)
    pushInt(23)
    mul
  }

  /**
   * EXERCISE 1
   * 
   * Define a small DSL for building HTML by adding a few functions like `p`, `h1`, etc., 
   * which use context functions to pass around a string builder that is used for printing the 
   * HTML fragments.
   */
  def text(string: String): HTML[Unit] =
    append(s"$string")
  def p[A](inner: HTML[A]): HTML[A] = {
    append("<p>")
    val a = inner
    append("</p>")
    a
  }

  type HTML[+A] = StringBuilder ?=> A

  def append(text: String)(using sb: StringBuilder): Unit =
    sb.append(text)

  def makeHtml(html: HTML[Any]): String = {
    given sb: StringBuilder = new StringBuilder()
    html
    sb.result
  }

  val example = 
    makeHtml {
      p(text("Hello World!"))
    }

  println(example)
  // <p>Hello World!</p>
  
  import scala.concurrent.* 
  type Task[+A] = ExecutionContext ?=> A

  /**
   * EXERCISE 2
   * 
   * In any "multi-parameter" context function `A ?=> B ?=> ... Z`, Scala can adapt the order of 
   * the context parameters. Try this for yourself by passing `permutation2` into the function 
   * `acceptsPermutation1`.
   */
  def permutation1: HTML[Task[Unit]] = ???
  def permutation2: Task[HTML[Unit]] = permutation1
  def acceptsPermutation1(p: HTML[Task[Unit]]): Unit = () 
  acceptsPermutation1(permutation2)

  /**
   * EXERCISE 3
   * 
   * Unlike contravariant reader effects (e.g. environment in ZIO), context functions do not infer.
   * Add type ascriptions to make this code compile.
   */
  // def composed = compose(task, html)

  def compose[A, B, C, D](left: A ?=> B, right: C ?=> D): A ?=> C ?=> (B, D) = 
    (left, right)

  def task: Task[String] = ??? 
  def html: HTML[Int] = ???

  def x: Task[HTML[(String, Int)]] = compose(task, html)

/**
 * SINGLETON TYPES
 * 
 * Literals in Scala now have their own singleton types, which are subtypes of their broader types.
 * For example, the value `true` has a subtype of `Boolean`, namely, `true`. Singleton types
 * provide additional precision and are a relatively simple change to the language that is useful 
 * in conjunction with type-level and metaprogramming.
 */
object singleton_types:
  /**
   * EXERCISE 1
   * 
   * Explicitly ascribe this literal value a singleton type.
   */
  val trueValue: true = true

  /**
   * EXERCISE 2
   * 
   * Test to see if `true` is a subtype of `Boolean` by using the helper type function 
   * `IsSubtypeOf`.
   */
  type trueSubtypeBoolean = IsSubtypeOf[true, Boolean]

  /**
   * EXERCISE 3
   * 
   * Explicitly ascribe this literal value a singleton type.
   */
  val stringValue: "name" = "name"

  /**
   * EXERCISE 4
   * 
   * Explicitly ascribe this literal value a singleton type.
   */
  val floatValue: 3.1415f = 3.1415f  

  infix type IsSubtypeOf[A, B >: A]

/**
 * TRANSPARENT TRAITS
 * 
 * Transparent traits can be suppressed in type inference to eliminate noise in type errors.
 */
object transparent_traits:
  /**
   * EXERCISE 1
   * 
   * Mark the following trait as transparent by using the `transparent` keyword.
   */
  transparent trait KryoSerialize
  
  sealed trait Result

  // Result with Product with Serializable

  case object Success extends Result with KryoSerialize
  case object Failure extends Result with KryoSerialize

  val xs = List(Success, Failure)

