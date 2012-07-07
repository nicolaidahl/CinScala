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
import scala.collection.mutable.HashMap
import scala.dbc.syntax.StatementExpression


trait C {
  
  case class CASTException(smth:String) extends Exception(smth)
  case class UnknownVariableException(smth1:String)  extends CASTException(smth1)
  case class VariableRedefinitionException(smth1:String) extends CASTException(smth1)
  case class FunctionRedefinitionException(smth1: String) extends CASTException(smth1)
  
  //Variable environment: Map from identifier to variable type
  type VarEnv = Map[String, Type] 
  //Function environment: Map from identifier to tuple of return type and argument list
  type FunEnv = Map[String, (Option[Type], ArgList)]
  //Argument list
  type ArgList = List[(Type, String)]  
  
  
  case class Program (contents: List[TopDec])
  
  //Top level declaration
  sealed abstract class TopDec
  case class FunctionDec (returnType: Option[Type], identifier: String, parameters: ArgList, statements: List[StmtOrDec]) extends TopDec
  case class VariableDec (variableType: Type, identifier: String) extends TopDec
  
  //Declarations inside a block
  sealed abstract class StmtOrDec
  case class Stmt (statement: Statement) extends StmtOrDec
  case class LocalVariable (variableType: Type, identifier: String) extends StmtOrDec //int x;
  case class LocalVariableWithAssign (variableType: Type, identifier: String, expr: Expression) extends StmtOrDec //int x = e;
  
  //C statements
  sealed abstract class Statement
  case class Block (contents: List[StmtOrDec]) extends Statement
  case class ExpressionStatement (expr: Expression) extends Statement
  case class If (condition: Expression, trueBranch: Statement, falseBranch: Statement) extends Statement
  case class While (condition: Expression, contents: Statement) extends Statement
  case class Return (returnExpression: Option[Expression]) extends Statement
  case class Switch (condition: Expression)
  
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
  def generate (prog: Program, varEnv: VarEnv, funEnv: FunEnv): String =
    prog.contents.map{
      case function: FunctionDec => generateFunctionDec(function, varEnv, funEnv) 
      case variable: VariableDec => generateVariableDec(variable, varEnv, funEnv) 
    } mkString "\n\n"
  
  def generateFunctionDec(functionDec: FunctionDec, varEnv: VarEnv, funEnv: FunEnv): String = {
      
    //Fail if already defined else add to environment  
    if(funEnv.exists(_._1.equals(functionDec.identifier)))
      throw new FunctionRedefinitionException("Redifinition of function " + functionDec.identifier)
    
    val funEnv1 = funEnv + (functionDec.identifier -> (functionDec.returnType, functionDec.parameters))
      
    val ts = 
      functionDec.returnType match {
        case None => "void"
        case Some(t) => generateType(t, varEnv, funEnv1)
      }
      
    val returnType = ts
    val funcName = functionDec.identifier

    val params = (functionDec.parameters.map({
      case (typ, name) => generateType(typ, varEnv, funEnv1) + " " + name
    }).mkString("(", ", ", ")"))

    
    
    val body = "{\n" + stmtOrDecLoop(varEnv, funEnv)(functionDec.statements)._1 + "\n}"

    returnType + " " + funcName + params + body
  }
  
  def generateVariableDec(variableDec: VariableDec, varEnv: VarEnv, funEnv: FunEnv): String = {
    val varEnv1 = varEnv + (variableDec.identifier -> variableDec.variableType)
    generateType(variableDec.variableType, varEnv1, funEnv) + " " + variableDec.identifier 
  }
  
  
  def stmtOrDecLoop(varEnv: VarEnv, funEnv: FunEnv)(stmtsordecs: List[StmtOrDec]): (String, VarEnv) = {
      stmtsordecs match {
        case Nil => ("", varEnv)
      	case head :: tail =>
          val (retString, varEnv1) = generateStmtOrDec(varEnv, funEnv)(head)
          val (resStringFinal, varEnvFinal) = stmtOrDecLoop(varEnv1, funEnv)(tail)
          (retString + "\n" + resStringFinal, varEnvFinal)
      }
    }
    
  def generateStmtOrDec(varEnv: VarEnv, funEnv: FunEnv)(sord: StmtOrDec): (String, VarEnv) =
    sord match {
      case Stmt(statement) => (generateStatement(varEnv, funEnv)(statement), varEnv)
      case LocalVariable(varType, identifier) => 
        if(varEnv.exists(_._1.equals(identifier)))
          throw new VariableRedefinitionException("Variable " + identifier + " has already been defined")
        else {
          val varEnv1 = varEnv + (identifier -> varType)
          val retString = generateType(varType, varEnv1, funEnv) + " " + identifier + ";"
          (retString, varEnv1)
        }
      case LocalVariableWithAssign(varType, ident, expr) =>
        if(varEnv.exists(_._1.equals(ident)))
          throw new VariableRedefinitionException("Variable " + ident + " has already been defined")
        else {
          val varEnv1 = varEnv + (ident -> varType)
          val retString = generateType(varType, varEnv1, funEnv) + " " + ident + " = " + generateExpr(varEnv1, funEnv)(expr) + ";"
          (retString, varEnv1)
        }
    }
    
  def generateStatement(varEnv: VarEnv, funEnv: FunEnv)(stmt: Statement): String =
    stmt match {
      case Block (contents) => 
        "{\n" + stmtOrDecLoop(varEnv, funEnv)(contents)._1 + "\n}"
      case ExpressionStatement (expr) => generateExpr(varEnv, funEnv)(expr) + ";"
      case If (condition, trueBranch, falseBranch) => 
        "if(" + generateExpr(varEnv, funEnv)(condition) + ")\n" +
        generateStatement(varEnv, funEnv)(trueBranch) + "\n" +
        "else\n" +
        generateStatement(varEnv, funEnv)(falseBranch) + "\n"
      case While (condition, contents) => 
        "while(" + generateExpr(varEnv, funEnv)(condition) + ")" +
        generateStatement(varEnv, funEnv)(contents)
      case Return (returnExpression) => "return " + returnExpression.map(generateExpr(varEnv, funEnv)).getOrElse("") + ";"
    }
    
  def generateType(t: Type, varEnv: VarEnv, funEnv: FunEnv) : String = 
    t match {
      case TypeInteger => "int"
      case TypeChar => "char"
      case TypePointer(t) => generateType(t, varEnv, funEnv) + "*"
      case TypeArray(t, None) => generateType(t, varEnv, funEnv) + "[]"
      case TypeArray(t, Some(l)) => generateType(t, varEnv, funEnv) + "[" + l + "]"
    }
  
  def generateUnaryOp(ope: UnaryOp, varEnv: VarEnv, funEnv: FunEnv): String =
    ope match {
      case UnaryDecrement => "--"
      case UnaryIncrement => "++"
    }
  
  def generateBinaryOp(ope: BinaryOp, varEnv: VarEnv, funEnv: FunEnv): String =
    ope match {
      case BinaryPlus => "+"
      case BinaryMinus => "-"
      case BinaryTimes => "*"
      case BinaryDivide => "/"
    }
    
    
  def generateExpr(varEnv: VarEnv, funEnv: FunEnv)(e: Expression): String =
    e match {
      case AccessExpr(access) => generateAccess(access, varEnv, funEnv)
      case Assign(access, expr) => 
        generateAccess(access, varEnv, funEnv) + " = " + generateExpr(varEnv, funEnv)(expr)
      case Address(access) => ""
      case ConstantInteger(i) => i.toString()
      case UnaryPrim(operator, expr) => 
        generateUnaryOp(operator, varEnv, funEnv) + generateExpr(varEnv, funEnv)(expr)
      case BinaryPrim(operator, expr1, expr2) => 
        generateExpr(varEnv, funEnv)(expr1) + " " + generateBinaryOp(operator, varEnv, funEnv) + " " + generateExpr(varEnv, funEnv)(expr2)
      case SeqAnd(expr1, expr2) => 
        generateExpr(varEnv, funEnv)(expr1) + " && " + generateExpr(varEnv, funEnv)(expr2)
      case SeqOr(expr1, expr2) => 
        generateExpr(varEnv, funEnv)(expr1) + " || " + generateExpr(varEnv, funEnv)(expr2)
      case Call(identifier, args) => identifier + args.mkString("(", ", ", ")")
      case ConditionExpression(expr1, expr2, expr3) => 
        generateExpr(varEnv, funEnv)(expr1) + " ? " + generateExpr(varEnv, funEnv)(expr2) + " : " + generateExpr(varEnv, funEnv)(expr3)
      case Cast(expr, newType) => 
        "(" + generateType(newType, varEnv, funEnv) + ") " + generateExpr(varEnv, funEnv)(expr)
    }
  
  
  def generateAccess(a: Access, varEnv: VarEnv, funEnv: FunEnv): String =
    a match {
      case AccessVariable(identifier) => 
        if(varEnv.exists(_._1.equals(identifier)))
          identifier
        else
          throw new UnknownVariableException("The variable " + identifier + " does not exist in the current scope.")
      case AccessDeref(expr) => "*" + generateExpr(varEnv, funEnv)(expr)
      case AccessIndex(access, expr) => generateAccess(access, varEnv, funEnv) + "[" + generateExpr(varEnv, funEnv)(expr) + "]"
    }
    
  
}





