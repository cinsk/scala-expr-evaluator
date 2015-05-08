// Simple Expression Parser
//
// Copyright (C) 2015  Seong-Kook Shin <cinsky@gmail.com>
//
// DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
// Version 2, December 2004
// 
// Copyright (C) 2015 Seong-Kook Shin <cinsky@gmail.com>
// 
// Everyone is permitted to copy and distribute verbatim or modified
// copies of this license document, and changing it is allowed as long
// as the name is changed.
// 
//            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
//   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
// 
//  0. You just DO WHAT THE FUCK YOU WANT TO.
// 
// This program is free software. It comes without any warranty, to the
// extent permitted by applicable law. You can redistribute it and/or
// modify it under the terms of the Do What The Fuck You Want To Public
// License, Version 2, as published by Sam Hocevar. See
// http://www.wtfpl.net/ for more details.

package cinsk

import scala.util.parsing.combinator._
import scala.util.{Either, Right, Left}

class ParseError(msg: String) extends RuntimeException(msg)

sealed class Expr {
  type ExprResult = Either[Boolean, Double]

  def asNum(x: Either[Boolean, Double]): Double =
    x.fold(l => if (l) 1.0 else 0.0, r => r)

  def asBool(x: Either[Boolean, Double]): Boolean =
    x.fold(l => l, r => if (r == 0.0) false else true)

  def toDouble(x: AnyVal): Either[Boolean, Double] = x match {
      // TODO: use structural type (e.g. { def toDouble: Double }
      case b: Boolean => Left(b)
      case d: Double => Right(d)
      case s: Short => Right(s.toDouble)
      case c: Char => Right(c.toDouble)
      case b: Byte => Right(b.toDouble)
      case l: Long => Right(l.toDouble)
      case f: Float => Right(f.toDouble)
      case i: Int => Right(i.toDouble)
      case a@_ => throw new ParseError(s"unsupported type: ${x.getClass}")
  }

  def eval(bnd: Map[String, AnyVal]): Either[Boolean, Double] = {
    this match {
      case ExNumber(x) => Right(x)
      case ExVar(name) => bnd.get(name) match {
        case Some(x) => toDouble(x)
        case None => throw new ParseError(s"unbounded symbol, ${name}")
      }

      case ExBool(v) => Left(v)
      case ExUnOp("-", arg) => Right(-asNum(arg.eval(bnd)))
      case ExUnOp("!", arg) => Left(!asBool(arg.eval(bnd)))

      case ExBinOp("+", l, r) => Right(asNum(l.eval(bnd)) + asNum(r.eval(bnd)))
      case ExBinOp("-", l, r) => Right(asNum(l.eval(bnd)) - asNum(r.eval(bnd)))
      case ExBinOp("*", l, r) => Right(asNum(l.eval(bnd)) * asNum(r.eval(bnd)))
      // TODO: check if r is zero??
      case ExBinOp("/", l, r) => Right(asNum(l.eval(bnd)) / asNum(r.eval(bnd)))

      case ExBinOp(">=", l, r) => Left(asNum(l.eval(bnd)) >= asNum(r.eval(bnd)))
      case ExBinOp("<=", l, r) => Left(asNum(l.eval(bnd)) <= asNum(r.eval(bnd)))
      case ExBinOp("==", l, r) => Left(asNum(l.eval(bnd)) == asNum(r.eval(bnd)))

      case ExBinOp("!=", l, r) => Left(asNum(l.eval(bnd)) != asNum(r.eval(bnd)))
      case ExBinOp(">", l, r) => Left(asNum(l.eval(bnd)) > asNum(r.eval(bnd)))
      case ExBinOp("<", l, r) => Left(asNum(l.eval(bnd)) < asNum(r.eval(bnd)))
      case ExBinOp("&&", l, r) => Left(asBool(l.eval(bnd)) && asBool(r.eval(bnd)))
      case ExBinOp("||", l, r) => Left(asBool(l.eval(bnd)) || asBool(r.eval(bnd)))

      case ExUnOp(o, _) =>
        throw new ParseError(s"unrecognized unary operator, '${o}'")
      case ExBinOp(o, _, _) =>
        throw new ParseError(s"unrecognized binary operator, '${o}'")
      case e: Expr =>
        throw new ParseError(s"unrecognized expression, '${e}'")
    }
  }
  def eval: Either[Boolean, Double] = eval(Map())

  import scala.util.matching.Regex.Match
  import scala.util._

  def evalWithNumericMatch(m: Match,
                           bnd: Map[String, AnyVal] = Map(),
                           varName: String = "m"): Either[Boolean, Double] = {
    eval(bnd ++ (for ((s, i) <- m.subgroups.zipWithIndex;
                      d = Try(s.toDouble);
                      if d.isSuccess)
                 yield (s"${varName}[${i+1}]" -> d.get)).toMap)
  }
}

case class ExBool(value: Boolean) extends Expr
case class ExVar(name: String) extends Expr
case class ExNumber(value: Double) extends Expr
case class ExUnOp(op: String, arg: Expr) extends Expr
case class ExBinOp(op: String, left: Expr, right: Expr) extends Expr

object ExprEvaluator extends JavaTokenParsers {
  def bool: Parser[Expr] = """(false|true)""".r ^^ { b =>
      b match {
        case "false" => ExBool(false)
        case "true" => ExBool(true)
      } }

  def intNumber: Parser[Expr] = decimalNumber ^^ { a => ExNumber(a.toDouble) }
  def doubleNumber: Parser[Expr] = floatingPointNumber ^^ {
      a => ExNumber(a.toDouble) }
  def number: Parser[Expr] = intNumber | doubleNumber

  def unopFactor: Parser[Expr] = """[-+!]""".r ~ factor ^^ {
      u => u match {
        case ("-" ~ f) => ExUnOp("-", f)
        case ("+" ~ f) => f
        case ("!" ~ f) => ExUnOp("!", f)
      }
    }

  def variable: Parser[Expr] = """[a-zA-Z_][a-zA-Z_0-9]*(\[[0-9]+\])?""".r ^^ {
      a => ExVar(a.toString) }

  def factor: Parser[Expr] =
    number | bool | variable | unopFactor | "(" ~> logicalExpr <~ ")"

  def term: Parser[Expr] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
      case flhs ~ lst => (flhs /: lst) {
        case (x, "*" ~ y) => ExBinOp("*", x, y)
        case (x, "/" ~ y) => ExBinOp("/", x, y)
      }
    }

  def expr: Parser[Expr] =
    term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
      case flhs ~ lst => lst.foldLeft(flhs) {
        case (x, "+" ~ y) => ExBinOp("+", x, y)
        case (x, "-" ~ y) => ExBinOp("-", x, y)
      }
    }

  def compareExpr: Parser[Expr] = expr ~ rep(">=" ~ log(expr)("ge expr") |
                                             "<=" ~ log(expr)("le expr") |
                                             "==" ~ log(expr)("eq expr") |
                                             "!=" ~ log(expr)("ne expr") |
                                             ">" ~ log(expr)("gt expr") |
                                             "<" ~ log(expr)("lt expr")) ^^ {
      case number ~ list => list.foldLeft(number) {
        case (x, ">=" ~ y) => ExBinOp(">=", x, y)
        case (x, "<=" ~ y) => ExBinOp("<=", x, y)
        case (x, "==" ~ y) => ExBinOp("==", x, y)
        case (x, "!=" ~ y) => ExBinOp("!=", x, y)
        case (x, ">" ~ y) => ExBinOp(">", x, y)
        case (x, "<" ~ y) => ExBinOp("<", x, y)
      }
    }

  def logicalExpr : Parser[Expr] = compareExpr ~ rep("&&" ~ log(compareExpr)("and expr") |
                                                "||" ~ log(compareExpr)("or expr")) ^^ {
      case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
        case (x, "&&" ~ y) => ExBinOp("&&", x, y)
        case (x, "||" ~ y) => ExBinOp("||", x, y)
      }
    }

  def apply(input: String): Expr = parseAll(logicalExpr, input) match {
      case Success(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.msg)
    }
}


object Main extends App {
  ExprEvaluator("-3 + 4 * 2").eval match {
    case Right(x) => println(s"numeric value: $x")
    case Left(x) => println(s"boolean value: $x")
  }

  ExprEvaluator("-3 + 4 * 2 >= 10").eval match {
    case Right(x) => println(s"numeric value: $x")
    case Left(x) => println(s"boolean value: $x")
  }

  val constraints = "m[1]>=100 && m[2]<100"
  val patterns = "^[sh]dd-([0-9]+)-([0-9]+)$".r

  patterns.findFirstMatchIn("sdd-250-40") match {
    case Some(m) => ExprEvaluator(constraints).evalWithNumericMatch(m) match {
      case Left(x) => println(s"constraint match: $x")
      case _ => println("constraint error")
    }
    case None => println("NOT MATCHED")
  }
}
