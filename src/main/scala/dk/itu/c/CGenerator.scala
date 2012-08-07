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

trait CGenerator extends CAbstractSyntax {
  
  def getEmptyVarEnv: Map[String, Type] = Map.empty[String, Type]
  def getEmptyFunEnv: Map[String, (Option[Type], ArgList)] = Map.empty[String, (Option[Type], ArgList)]
  
  case class CASTException(smth:String) extends Exception(smth)
  case class UnknownVariableException(smth1:String)  extends CASTException(smth1)
  case class VariableRedefinitionException(smth1:String) extends CASTException(smth1)
  case class FunctionRedefinitionException(smth1: String) extends CASTException(smth1)
  
  
  def lookupVar(varEnv: VarEnv, identifier: String): Boolean =
    varEnv.exists(_._1.equals(identifier))
  
  def lookupFunc(funEnv: FunEnv, identifier: String): Boolean =
    funEnv.exists(_._1.equals(identifier))
  
    
  //Main generate function
  def generate (prog: Program, varEnv: VarEnv, funEnv: FunEnv): String = {
    
    
    generateTopDecs(varEnv, funEnv)(prog.contents)._3
  }
  
  def generatePrecompileInstruction(instr: PrecompileInstruction, varEnv: VarEnv, funEnv: FunEnv) = {
    instr match {
      case IncludeLoc(s) => "#include \"" + s + "\" \n"
      case IncludeStd(s) => "#include <" + s + "> \n"
    }
  }
  
  def generateTopDecs (varEnv: VarEnv, funEnv: FunEnv)(topDecs: List[TopDec]): (VarEnv, FunEnv, String) =
	topDecs match {
	  case Nil => (varEnv, funEnv, "")
	  case head :: tail =>
	    head match {
	      case variable: VariableDec =>
	        val (varEnv1, str) = generateVariableDec(varEnv, funEnv, variable)
	        val (varEnv2, funEnv1, str1) = generateTopDecs(varEnv1, funEnv)(tail)
	        (varEnv2, funEnv1, str + str1)
	      case function: FunctionDec => 
	        val (funEnv1, str) = generateFunctionDec(varEnv, funEnv, function)
	        val (varEnv1, funEnv2, str1) = generateTopDecs(varEnv, funEnv1)(tail)
	        (varEnv1, funEnv2, str + str1)
	      case PrecompileInstr(precompInstr) =>
	        val result = generatePrecompileInstruction(precompInstr, varEnv, funEnv)
	        val (varEnv1, funEnv1, str1) = generateTopDecs(varEnv, funEnv)(tail)
	        (varEnv1, funEnv1, result + str1)
	    }
	    
	}
  
  /**
   * Generate a function declaration
   */
  def generateFunctionDec(varEnv: VarEnv, funEnv: FunEnv, functionDec: FunctionDec): (FunEnv, String) = {
      
    //Fail if already defined else add to environment  
    if(lookupFunc(funEnv, functionDec.identifier))
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
    
    val body = "{\n" + stmtOrDecLoop(varEnv, funEnv1)(functionDec.stmtOrDecs)._1 + "\n}"

    (funEnv1, returnType + " " + funcName + params + body)
  }  

  /**
   * Generate a variable declaration
   */
  def generateVariableDec(varEnv: VarEnv, funEnv: FunEnv, variableDec: VariableDec): (VarEnv, String) = {
    val varEnv1 = varEnv + (variableDec.identifier -> variableDec.variableType)
    (varEnv1, generateType(variableDec.variableType, varEnv1, funEnv) + " " + variableDec.identifier) 
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
      case Dec(declaration) => generateDeclaration(varEnv, funEnv)(declaration)
    }
  
  def generateDeclaration(varEnv: VarEnv, funEnv: FunEnv)(dec: Declaration): (String, VarEnv) =
    dec match {
    case LocalVariable(varType, identifier) => 
      if(lookupVar(varEnv, identifier))
        throw new VariableRedefinitionException("Variable " + identifier + " has already been defined")
      else {
        val varEnv1 = varEnv + (identifier -> varType)
        val retString = generateType(varType, varEnv1, funEnv) + " " + identifier + ";"
        (retString, varEnv1)
      }
    case LocalVariableWithAssign(varType, ident, expr) =>
      if(lookupVar(varEnv, ident))
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
      case If (condition, ifBranch, elseIfBranches, elseBranch) => 
        val ifStr = 
          "if(" + generateExpr(varEnv, funEnv)(condition) + ")\n{\n" +
          stmtOrDecLoop(varEnv, funEnv)(ifBranch)._1 + "}\n" 
        val elseBranchesStr = 
          elseIfBranches match {
            case None => ""
            case Some(bs) => bs.map(b => "else if(" + generateExpr(varEnv, funEnv)(b._1) + ")\n{\n" + stmtOrDecLoop(varEnv, funEnv)(b._2)._1) + "\n}\n"
          }
        
        val elseStr = 
          elseBranch match {
            case None => ""
            case Some(s) => "else\n{\n" + stmtOrDecLoop(varEnv, funEnv)(s)._1 + "}\n"
          }
          
        ifStr + elseBranchesStr + elseStr
      case Switch (expr, cases, default) =>
        def generateCase(aCase: (Expression, List[StmtOrDec])): String = 
          "case " + generateExpr(varEnv, funEnv)(aCase._1) + ":\n" + stmtOrDecLoop(varEnv, funEnv)(aCase._2)._1 + "break;\n"
        
        val switchStr = "switch(" + generateExpr(varEnv, funEnv)(expr) + "){\n"
        val casesStr = cases.map(generateCase).mkString
        val defaultStr = 
          default match {
            case None => ""
            case Some(l) => "default: \n" + stmtOrDecLoop(varEnv, funEnv)(l)._1 +"\nbreak;\n"
          }
        
        switchStr + casesStr + defaultStr + "}\n"
      case While (condition, contents) => 
        "while(" + generateExpr(varEnv, funEnv)(condition) + ")" +
        generateStatement(varEnv, funEnv)(contents)
      case For (initialization, condition, counter, contents) =>
        "for(" + generateExpr(varEnv, funEnv)(initialization) + ";" + generateExpr(varEnv, funEnv)(condition) + ";" + generateExpr(varEnv, funEnv)(counter) + ")" + 
        generateStatement(varEnv, funEnv)(contents)
      case DoWhile (contents, condition) =>
        "do " + generateStatement(varEnv, funEnv)(contents) + "\n" +
        "while(" + generateExpr(varEnv, funEnv)(condition) + ");"
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
      case BinaryEquals => "="
      case BinaryLessThan => "<"
      case BinaryLessThanOrEquals => "<="
      case BinaryGreaterThan => ">"
      case inaryGreaterThanOrEquals => ">="
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
      case Call(identifier, args) => 
        if(!lookupFunc(funEnv, identifier))
          printf("Warning: Function " + identifier + " is unknown.\n\n")
        identifier + args.map(generateExpr(varEnv, funEnv)).mkString("(", ", ", ")")
      case ConditionExpression(expr1, expr2, expr3) => 
        generateExpr(varEnv, funEnv)(expr1) + " ? " + generateExpr(varEnv, funEnv)(expr2) + " : " + generateExpr(varEnv, funEnv)(expr3)
      case Cast(expr, newType) => 
        "(" + generateType(newType, varEnv, funEnv) + ") " + generateExpr(varEnv, funEnv)(expr)
    }
  
  
  def generateAccess(a: Access, varEnv: VarEnv, funEnv: FunEnv): String =
    a match {
      case AccessVariable(identifier) => 
        if(lookupVar(varEnv, identifier))
          identifier
        else
          throw new UnknownVariableException("The variable " + identifier + " does not exist in the current scope.")
      case AccessDeref(expr) => "*" + generateExpr(varEnv, funEnv)(expr)
      case AccessIndex(access, expr) => generateAccess(access, varEnv, funEnv) + "[" + generateExpr(varEnv, funEnv)(expr) + "]"
    }
    
  
}





