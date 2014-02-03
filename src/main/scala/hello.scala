import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import scala.concurrent.duration._
import scala.collection.JavaConversions._
import org.apache.http.client.methods._
import org.apache.http.impl.client.HttpClients

abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length

  def above(that: Element): Element = 
    new ArrayElement(this.contents ++ that.contents)

  def beside(that: Element): Element = {
    new ArrayElement(for ((line1, line2) <- 
                          this.contents zip that.contents) yield line1 + line2)
  }

  override def toString = contents mkString "\n"
}

object Element {
  def apply(contents: Array[String]): Element =
      new ArrayElement(contents)

  def apply(chr: Char, width: Int, height: Int): Element = 
      new UniformElement(chr, width, height)

  def apply(line: String): Element = 
      new LineElement(line)
}

class ArrayElement(val contents: Array[String]) extends Element {
}

class LineElement(s: String) extends Element {
  val contents = Array(s)
  override def width = s.length
  override def height = 1
}

class UniformElement(ch: Char,
                     override val width: Int,
                     override val height: Int) extends Element {
  private val line = ch.toString * width
  def contents = Array.fill(height)(line)
}


abstract class Expr
case class Var(name: String) extends Expr
case class Num(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

object Hello {
  def simplifyTop(expr: Expr): Expr = expr match {
      case UnOp("-", UnOp("-", e)) => e
      case BinOp("+", e, Num(0)) => e
      case BinOp("*", e, Num(1)) => e
      case _ => expr
    }

  def main(args: Array[String]) {

    println("hello world");
    val client = HttpClients.createDefault
    val req = new HttpGet("http://www.cinsk.org/")

    //val resp = client.execute(req)

    // for (hdr <- resp.headerIterator) {
    //   println(hdr)
    // }


    val op = future {
      client.execute(req)
    }

    op onComplete {
      case Success(resp) => for (hdr <- resp.headerIterator) {
        println(hdr)
      }
      case Failure(v) => println(v)
    }


    Await.ready(op, Duration(1000, MILLISECONDS))
    println("done")
  }
}
