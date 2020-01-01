import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import bobsrockets.nothingIntersting
import java.io.BufferedReader
import java.io.Closeable
import java.io.FileReader
import java.nio.file.{Files, Paths}
import org.stairwaybook.expr._
import org.stairwaybook.expr.Expr._
import org.stairwaybook.expr.ExprFormatter
import org.stairwaybook.simulation._
import MySimulation._
import scala.collection.mutable
import scala.math

object Main extends App {
  val firstArg = if (args.length > 0) args(0) else ""
  firstArg match {
    case "salt"  => println("pepper")
    case "chips" => println("salsa")
    case "eggs"  => println("bacon")
    case _ => {
      println("huh?")
    }
  }

  def max(x: Int, y: Int) = if (x > y) x else y

  var capitals = Map("Us" -> "DC", "France" -> "Paris")
  var franceCapital = capitals("France")
  val xs = 1 to 3
  xs.foreach(println)

  val it = xs.iterator
  it.foreach(x => println(x))

  println(franceCapital)
  println("Hello, 2!")

  // val system = ActorSystem("HelloSystem")
  // val helloActor = system.actorOf(Props[HelloActor], name = "helloactor")
  // helloActor ! "hello"
  // helloActor ! "buenos dias"
  // helloActor ! "dupa"

  val res = max(12, 3)
  println(res)

  val greetStrings = Array("Adam", "Małysz", "Pudzian")
  for (i <- 0 to 2)
    println(greetStrings(i))

  val oneTwoThree = List(1, 2, "3")
  val nextItems = List(4, 5, 6, 7)
  val whole = oneTwoThree ::: nextItems
  val whole2 = 0 :: whole

  whole2.foreach(println)

  val item = whole2(1)
  println(item)

  val pair = (99, "Luftballons")
  println(pair._1)
  println(pair._2)

  val funnyResult = (1).+(-2)
  println(funnyResult)

  val treasureMap = Map(
    1 -> "A",
    2 -> "B"
  )

  println(treasureMap)
  println(treasureMap(2))

  (0 to 100)
    .map(i => {
      if ((i % 3 == 0) && (i % 5 == 0))
        "Fizz Buzz"
      else if (i % 5 == 0)
        "Fizz"
      else if (i % 3 == 0)
        "Buzz"
      else
        i

    })
    .foreach(println)

  val sum1 = new ChecksumAccumulator()
  val sum2 = new ChecksumAccumulator()
  val result = sum1.add(2)
  println(s"Res ${result}")
  println(s"Sum ${sum1.checksum()}")

  val s1 = Singleton
  println(s1.dupa)

  println("""|Welcome to Ultamix 3000.
             |Type "HELP" for help.""".stripMargin)

  val s = 'aSymbol
  println(s.name)

  //println(f"$pi is approximately ${math.Pi}%.8f.")

  val oneHalf = new Rational(1, 2)
  val twoThirds = new Rational(2, 3)
  println(oneHalf + twoThirds)

  val aBiggerOne = new Rational(66, 42)
  println(aBiggerOne)
  println(aBiggerOne + 1)

  implicit def intToRational(x: Int) = new Rational(x)

  println(2 * aBiggerOne)

  var test = 1;
  var unitIGuess = (test = 2)
  println(unitIGuess)

  val currentPath = Paths.get(System.getProperty("user.dir"))
  val scalaFilesPath = Paths.get(currentPath.toString, "src", "main", "scala")

  val filesHere = (new java.io.File(scalaFilesPath.toString)).listFiles
  for (file <- filesHere if file.getName.endsWith(".scala"))
    println(file)

  val func = (x: Int, y: Int) => x * y
  println(func)
  val func2 = func(-1, _)
  println(func2(4))

  var more = 13
  val moreFunc = (x: Int) => x + more

  println(moreFunc(2))
  more = 15
  println(moreFunc(2))

  val someNumbers = List(-6, 14, -2, 34567, -21312)
  someNumbers
    .filter(_ > 0)
    .foreach(println)

  def echo(args: Any*) {
    for (arg <- args)
      print(s"$arg ")
    println()
  }

  echo("1")
  echo("1", "2")
  echo(someNumbers: _*)

  def bang(x: Int): Int =
    if (x == 0) throw new Exception("bang!")
    else bang(x - 1)

  //bang(5)

  def curriedSum(x: Int)(y: Int) = x + y

  println(s"CurriedSum: ${curriedSum(1)(2)}")

  def first(x: Int) = (y: Int) => x + y

  val second = first(1)
  println(s"More verbose function curring ${second(2)}")

  def twice(op: Double => Double, x: Double) = op(op(x))
  println(twice(_ + 1, 5))

  def repeatNTimes(
      operation: Double => Double,
      seed: Double,
      n: Int
  ): Double = {
    if (n == 0)
      seed
    else
      repeatNTimes(operation, operation(seed), n - 1)
  }

  println(s"repeatNTimes ${repeatNTimes(_ * 2, 1, 10)}")

  def use[T <: Closeable](resourceFactory: => T)(fun: (T) => Unit) = {
    var resource: Option[T] = Option.empty;
    try {
      resource = Option(resourceFactory)
      fun(resource.get)
    } catch {
      case e: Exception => println(e)
    } finally {
      resource match {
        case Some(r) => r.close()
        case None    => ()
      }
    }
  }

  println("use")
  //val inFile = new FileReader("not a file")

  use(new FileReader("/home/pk/GIT/Scala-Fun/hello-world/resources/test.txt")) {
    fr =>
      use(new BufferedReader(fr)) { bufferReader =>
        {
          val line = bufferReader.readLine()
          println(line)
        }
      }
  }

  val someFunc = (() => { println("test") })()

  println(Spiral.spiral(7, 0))
  println(Spiral.spiral(13, 0))

  println(new Rational(1) > new Rational(2))

  val queue = new BasicIntQueue with Filtering with Doubling
  queue.put(1)
  println(queue.get())
  queue.put(51)
  println(queue.get())
  println(queue.get())

  nothingIntersting()

  val expr = UnOp("-", UnOp("-", Var("1")))
  val expr2 = UnOp("abs", UnOp("abs", Var("123")))
  println(simplifyTop(expr))
  println(simplifyTop(expr2))

  val complexExpr1 =
    BinOp("+", BinOp("*", BinOp("+", Var("x"), Var("y")), Var("z")), Number(1));

  println(complexExpr1)
  val f = new ExprFormatter
  val e1 = BinOp(
    "*",
    BinOp("/", Number(1), Number(2)),
    BinOp("+", Var("x"), Number(1))
  )
  val e2 = BinOp(
    "+",
    BinOp("/", Var("x"), Number(2)),
    BinOp("/", Number(1.5), Var("x"))
  )
  val e3 = BinOp("/", e1, e2)
  def show(e: Expr) = println(f.format(e) + "\n\n")

  for (e <- Array(e1, e2, e3, complexExpr1)) show(e)

  import Sort._
  val sorted = quickSort(List(4, 5, 156, 2, 765, 23, 976, 2, 6))
  println(sorted)

  println("Circut simulation")
  val input1, input2, sum, carry = new Wire
  probe("sum", sum)
  probe("carry", carry)
  halfAdder(input1, input2, sum, carry)
  input1 setSignal true
  run()
  input2 setSignal true
  run()

  val aTrait = new {
    val denomArg = 1
    val numerArg = 2
  } with RationalTrait

  aTrait.denom
  LazyDemo
  LazyDemo.x
  LazyDemo.x
  LazyDemo.y

  val frog = new Frog()
  frog.philosophize()
  frog.eat(new Grass())
  println("End")
}

object Sort {
  def quickSort[T <% Ordered[T]](orderable: List[T]): List[T] = {
    orderable match {
      case Nil         => Nil
      case head :: Nil => List(head)
      case head :: tail =>
        quickSort(tail.filter(_ <= head)) ::: List(head) ::: quickSort(
          tail.filter(_ > head)
        )
    }
  }
}

trait RationalTrait {
  val numerArg: Int
  val denomArg: Int
  private lazy val g = {
    require(denomArg != 0)
    gcd(numerArg, denomArg)
  }
  val numer = numerArg / g
  val denom = denomArg / g
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
  override def toString = numer + "/" + denom
}

object LazyDemo {
  lazy val x = { println("initializing x"); "done x" }
  val y = { println("initializing y"); "done y" }
}

class ChecksumAccumulator {
  private var sum = 0
  def add(b: Byte) = sum += b
  def checksum() = ~(sum & 0xFF) + 1
}

object ChecksumAccumulator {
  private val cache = mutable.Map.empty[String, Int]

  def calculate(s: String): Int =
    if (cache.contains(s))
      cache(s)
    else {
      val acc = new ChecksumAccumulator
      for (c <- s)
        acc.add(c.toByte)

      val cs = acc.checksum()
      cache += (s -> cs)
      cs
    }
}

object Singleton {
  var dupa = "dupa"
}

trait Philosophical {
  def philosophize() = {
    println("I consume memory, therefore I am!")
  }
}

class Food
class Grass extends Food
abstract class Animal {
  type SuitableFood <: Food
  def eat(food: SuitableFood)
}
class Frog extends Animal with Philosophical {
  type SuitableFood = Grass

  override def toString = "green"
  override def philosophize() = {
    println("It ain't easy being " + toString + "!")
  }

  override def eat(food: Grass): Unit = println(s"Eating ${food.getClass()}")
}

class Pasture {
  var animals: List[Animal { type SuitableFood = Grass }] = Nil
}
