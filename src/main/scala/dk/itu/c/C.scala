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
  
  
  case class Program (contents: List[TopDec])
  
  //Top level declaration
  sealed abstract class TopDec
  case class FunctionDec (returnType: Option[Type], identifier: String, parameters: List[(Type, String)], statement: Statement) extends TopDec
  case class VariableDec (variableType: Type, identifier: String) extends TopDec
  
  //Declarations inside a block
  sealed abstract class BlockDec
  case class BlockStatement (statement: Statement) extends BlockDec
  case class LocalVariable (variableType: Type, identifier: String) extends BlockDec
  
  //C statements
  sealed abstract class Statement
  case class Block (contents: List[BlockDec]) extends Statement
  case class ExpressionStatement (expr: Expression) extends Statement
  case class If (condition: Expression, trueBranch: Statement, falseBranch: Statement) extends Statement
  case class While (condition: Expression, contents: Statement) extends Statement
  case class Return (returnExpression: Option[Expression]) extends Statement
  
  //C types
  sealed abstract class Type
  case object TypeInteger extends Type
  case object TypeChar extends Type
  case class TypeArray (arrayType: Type, length: Option[Integer]) extends Type
  case class TypePointer (pointerType: Type) extends Type
  
  //C Unary operators
  sealed abstract class UnaryOp
  case object UnaryDecrement extends UnaryOp
  case object UnaryIncrement extends UnaryOp

  //C Binary Operators
  sealed abstract class BinaryOp
  case object BinaryPlus extends BinaryOp
  case object BinaryMinus extends BinaryOp
  case object BinaryTimes extends BinaryOp
  case object BinaryDivide extends BinaryOp
  
  //C Expressions
  sealed abstract class Expression
  case class AccessExpr (access: Access) extends Expression //x    or  *p    or  a[e]
  case class Assign (access: Access, expr: Expression) extends Expression  //x=e  or  *p=e  or  a[e]=e 
  case class Address (access: Access) extends Expression //&x   or  &*p   or  &a[e] 
  case class ConstantInteger (contents: Integer) extends Expression
  case class UnaryPrim (operator: UnaryOp, expression: Expression) extends Expression //Unary primitive operator
  case class BinaryPrim (operator: BinaryOp, expression1: Expression, expression2: Expression) extends Expression //Binary primitive operator
  case class SeqAnd (expr1: Expression, expr2: Expression) extends Expression //Sequential and &&
  case class SeqOr (expr1: Expression, expr2: Expression) extends Expression //Sequential or ||
  case class Call (ident: String, args: List[Expression]) extends Expression //Function call f(...)
  case class ConditionExpression (expr1: Expression, expr2: Expression, expr3: Expression) extends Expression //e1 ? e2 : e3
  case class Cast(expression: Expression, newType: Type) extends Expression //(int) a;

  //C variable access
  sealed abstract class Access
  case class AccessVariable (ident: String) extends Access //Variable access  ident
  case class AccessDeref (expr: Expression) extends Access //Pointer dereferencing  *p
  case class AccessIndex (access: Access, expr: Expression) extends Access //Access Array indexing  a[e]  
}

trait Generator extends C {
  
  //Main generate function
  def generate (prog: Program): String =
    prog.contents.map{
      case function: FunctionDec => generateFunctionDec(function) 
      case variable: VariableDec => generateVariableDec(variable) 
    } mkString "\n"
  
  def generateFunctionDec(functionDec: FunctionDec): String = {
    val ts = 
      functionDec.returnType match {
        case None => "void"
        case Some(t) => generateType(t)
      }
      
    val returnType = ts
    val funcName = functionDec.identifier

    val params = (functionDec.parameters.map({
      case (typ, name) => typ + " " + name
    }).mkString("(", ", ", ")"))

    val body = "{" + functionDec.statement + "}"

    returnType + " " + funcName + params + body
  }
  
  def generateVariableDec(variableDec: VariableDec): String =
    generateType(variableDec.variableType) + " " + variableDec.identifier 
  
    
  def genereateBlockDec(bd: BlockDec): String =
    bd match {
      case BlockStatement(statement) => generateStatement(statement)
      case LocalVariable(varType, identifier) => generateType(varType) + " " + identifier
    }
    
  def generateStatement(stmt: Statement): String =
    stmt match {
      case Block (contents) => ""
      case ExpressionStatement (expr) => generateExpr(expr) + ";"
      case If (condition, trueBranch, falseBranch) => 
        "if(" + generateExpr(condition) + ")\n" +
        generateStatement(trueBranch) + "\n" +
        "else\n" +
        generateStatement(falseBranch) + "\n"
      case While (condition, contents) => 
        "while(" + generateExpr(condition) + ")" +
        generateStatement(contents)
      case Return (returnExpression) => returnExpression.map(generateExpr).getOrElse("") + ";"
    }
    
  def generateType(t: Type) : String = 
    t match {
      case TypeInteger => "int"
      case TypeChar => "char"
      case TypePointer(t) => generateType(t) + "*"
      case TypeArray(t, None) => generateType(t) + "[]"
      case TypeArray(t, Some(l)) => generateType(t) + "[" + l + "]"
    }
  
  def generateUnaryOp(ope: UnaryOp): String =
    ope match {
      case UnaryDecrement => "--"
      case UnaryIncrement => "++"
    }
  
  def generateBinaryOp(ope: BinaryOp): String =
    ope match {
      case BinaryPlus => "+"
      case BinaryMinus => "-"
      case BinaryTimes => "*"
      case BinaryDivide => "/"
    }
    
    
  def generateExpr(e: Expression): String =
    e match {
      case AccessExpr(access) => generateAccess(access)
      case Assign(access, expr) => generateAccess(access) + "=" + generateExpr(expr)
      case Address(access) => ""
      case ConstantInteger(i) => i.toString()
      case UnaryPrim(operator, expr) => generateUnaryOp(operator) + generateExpr(expr)
      case BinaryPrim(operator, expr1, expr2) => generateExpr(expr1) + " " + generateBinaryOp(operator) + " " + generateExpr(expr2)
      case SeqAnd(expr1, expr2) => generateExpr(expr1) + " && " + generateExpr(expr2)
      case SeqOr(expr1, expr2) => generateExpr(expr1) + " || " + generateExpr(expr2)
      case Call(identifier, args) => identifier + args.mkString("(", ", ", ")")
      case ConditionExpression(expr1, expr2, expr3) => generateExpr(expr1) + " ? " + generateExpr(expr2) + " : " + generateExpr(expr3)
      case Cast(expr, newType) => "(" + generateType(newType) + ") " + generateExpr(expr)
    }
  
  
  def generateAccess(a: Access): String =
    a match {
      case AccessVariable(identifier) => ""
      case AccessDeref(expr) => "*" + generateExpr(expr)
      case AccessIndex(access, expr) => generateAccess(access) + "[" + generateExpr(expr) + "]"
    }
    
  
  
  /*s match {
      case Block (ss) => ss.map(generate).mkString("{",";","}")
    }*/
}

object Test extends Generator with App {
  //println(generate(Program(List(VariableDec(TypeInteger(), "kage"), FunctionDec(TypeInteger, "jens", List(Tuple2(TypeInterger, "antal")), )))))
}




