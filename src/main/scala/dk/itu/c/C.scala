/*
----------------------------------------------------------------------
Copyright (c) 2012 Christian Harrington and Nicolai Dahl

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

 * The software is provided "as is", without warranty of any kind,
  express or implied, including but not limited to the warranties of
  merchantability, fitness for a particular purpose and
  noninfringement.  In no event shall the authors or copyright
  holders be liable for any claim, damages or other liability,
  whether in an action of contract, tort or otherwise, arising from,
  out of or in connection with the software or the use or other
  dealings in the software.
----------------------------------------------------------------------
*/

package dk.itu.c

trait C {
  sealed abstract class CAST
  case class Program (contents: List[TopDec]) extends CAST
  
  sealed abstract class TopDec
  case class FunctionDec (returnType: Option[Type], identifier: String, parameters: List[Tuple2[Type, String]], statement: Statement) extends TopDec
  case class VariableDec (variableType: Type, identifier: String) extends TopDec
  
  sealed abstract class BlockDec
  case class BlockStatement (statement: Statement) extends BlockDec
  case class LocalVariable (variableType: Type, identifier: String) extends BlockDec
  
  sealed abstract class Statement
  case class Block (contents: List[Statement]) extends Statement
  case class ExpressionStatement (contents: Expression) extends Statement
  case class If (condition: Expression, trueBranch: Statement, falseBranch: Statement) extends Statement
  case class While (condition: Expression, contents: Statement) extends Statement
  case class Return (returnExpression: Option[Expression]) extends Statement
  
  sealed abstract class Type
  case class TypeInteger extends Type
  case class TypeChar extends Type
  case class TypeArray (arrayType: Type, length: Option[Integer]) extends Type
  case class TypePointer (pointerType: Type) extends Type
  
  sealed abstract class Expression
  case class ConstantInteger (contents: Integer) extends Expression
  case class BinaryOperator (operator: String, expression1: Expression, expression2: Expression) extends Expression
}

trait Generator extends C {
  def generate (c: CAST): String =
    c match {
      case Program (contents) => contents.map(
          e=>e match{
            case function: FunctionDec => generateFunctionDec(function) 
            case variable: VariableDec => generateVariableDec(variable) 
       }).mkString("\n")
  	}
  
  def evalType(t: Type) : String = 
    t match {
      case t1: TypeInteger => "int"
      case t2: TypeChar => "char"
      case t3: TypePointer => evalType(t3.pointerType) + "*"
      case t4: TypeArray => t4.length match {
        case None => evalType(t4.arrayType) + "[]"
        case Some(l) => evalType(t4.arrayType) + "[" + l + "]"
      }
    }
    
  def generateFunctionDec(functionDec: FunctionDec): String = {
    val ts = 
      functionDec.returnType match {
        case None => "void"
        case Some(t) => evalType(t)
      }
  
    ts + " " + functionDec.identifier + "(" + functionDec.parameters.map(tuple => tuple._1 + " " + tuple._2 + ",") + ")" + "{" + functionDec.statement + "}"
  }
  
  def generateVariableDec(variableDec: VariableDec): String =
    evalType(variableDec.variableType) + " " + variableDec.identifier 
  
  
  /*s match {
      case Block (ss) => ss.map(generate).mkString("{",";","}")
    }*/
}

object Test extends Generator with App {
  //println(generate(Program(List(VariableDec(TypeInteger(), "kage"), FunctionDec(TypeInteger, "jens", List(Tuple2(TypeInterger, "antal")), )))))
}