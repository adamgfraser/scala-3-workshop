/**
 * Scala 3 drops a number of features. Most have equivalents you can use instead.
 */
object dropped:
  trait QuickApp extends DelayedInit:
    var fn: () => Unit = () => ()

    def delayedInit(body: => Unit) =
      fn = () => body

    def main(args: Array[String]): Unit = 
      println("App starting")      
      fn()
      println("App ending")

  /**
   * EXERCISE 1
   * 
   * Rewrite the following code to not rely on `DelayedInit`.
   */
  object MyApp:

    @main
    def run =
      println("Hello, World!")

  /**
   * EXERCISE 2
   * 
   * Rewrite the following code to not rely on existential types.
   */
  def printFirst[A](x : List[A]) = x.headOption.foreach(println(_))

  /**
   * EXERCISE 3
   * 
   * Rewrite the type signature of `MappableMapK` to not rely on type projections.
   */
  def MappableMapK[K]: Mappable[Map[K, *]] = ???
  trait Mappable[F[_]]:
    def map[A, B](fa: F[A], f: A => B): F[B]

  /**
   * EXERCISE 4
   * 
   * Rewrite the do/while loop to an equivalent while loop.
   */
  var i = 0
  // do { println("Hello!"); i = i + 1; } while (i < 10)

  while i <= 10 do
    println("Hello!")
    i = i + 1

  /**
   * EXERCISE 5
   * 
   * Rewrite the following procedural method into an expression-oriented method.
   */
  def runIt() = { println("Running it!") }

  /**
   * EXERCISE 6
   * 
   * Rewrite the following package object to be just a normal package.
   */
// package email { 
//   type Email = String

//   def makeEmail(s: String): Email = 
//     if (isValidEmail(s)) s else throw new IllegalStateException(s"${s} is not an email")

//   def isValidEmail(v: String): Boolean = v.matches("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$")
// }

  /**
   * EXERCISE 7
   * 
   * Rewrite the following code to not use early initializers.
   */
  abstract class MapEntry {
    def key: String
    final def hash = key.hashCode
  }

  class IntMapEntry extends MapEntry:
    val key = "Int"

  /**
   * EXERCISE 8
   *
   * Rewrite the following code to not use class shadowing.
   */
  class UserRepo {
    type User 

    class UserOps { }
  }
  class UserAndProfileRepo extends UserRepo {
    type Profile 
    class UserAndProfileOps { }
  }

  /**
   * EXERCISE 9
   * 
   * Rewrite the following code to not use XML literals, using XML string
   * interpolation instead.
   */
  val html = xml"<html><head></head><body><h1>Hello World</h1></body></html>"

  extension (sc: StringContext) def xml(args: Any*) = ???

  /**
   * EXERCISE 10
   * 
   * Rewrite the following code to not use symbol literals.
   */
  val x = Map("int" -> classOf[Int], "string" -> classOf[String])

  /**
   * EXERCISE 11
   * 
   * Rewrite the following code to not use auto-application of the nullary
   * method `printIt()`.
   */
  printIt()

  def printIt(): Unit = println("Parens not optional!")

  /**
   * EXERCISE 12
   * 
   * Rewrite the following code to not assume weak conformance.
   */
  val list = List(1d, 3.1415)
  acceptListDouble(list)

  def acceptListDouble(l: List[Double]): Unit = println(l.mkString(", "))

  import scala.util.control.NonLocalReturns.*

  /**
   * EXERCISE 13
   * 
   * Rewrite the following code to not use non-local returns, using
   * scala.util.control.NonLocalReturns or otherwise.
   */
  def shortCircuitingForeach(nums: Iterable[Int]): Int = 
    returning {
      nums.foreach(num => if (num > 50) throwReturn(num))
      throwReturn(-1)
    }

  /**
   * EXERCISE 14
   * 
   * Rewrite the following code to not use `private[this]`.
   */
  class Counter {
    private var counter = 0 
  
    def increment(): Unit = counter += 1
  
    def decrement(): Unit = counter -= 1 
  
    def get(): Int = counter
  }

  

  

