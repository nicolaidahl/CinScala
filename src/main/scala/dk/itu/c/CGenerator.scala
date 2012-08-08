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
  
  def getEmptyVarEnv: Map[String, TypeSpecifier] = Map.empty[String, TypeSpecifier]
  def getEmptyFunEnv: Map[String, List[Declaration]] = Map.empty[String, List[Declaration]]
  
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
  
  /*def generatePrecompileInstruction(instr: PrecompileInstruction, varEnv: VarEnv, funEnv: FunEnv) = {
    instr match {
      case IncludeLoc(s) => "#include \"" + s + "\" \n"
      case IncludeStd(s) => "#include <" + s + "> \n"
    }
  }*/
  
  def generateTopDecs (varEnv: VarEnv, funEnv: FunEnv)(topDecs: List[ExternalDeclaration]): (VarEnv, FunEnv, String) =
	topDecs match {
	  case Nil => (varEnv, funEnv, "")
	  case head :: tail =>
	    head match {
	      case variable: Declaration =>
	        val (varEnv1, str) = generateDeclaration(varEnv, funEnv)(variable)
	        val (varEnv2, funEnv1, str1) = generateTopDecs(varEnv1, funEnv)(tail)
	        (varEnv2, funEnv1, str + str1)
	      case function: FunctionDec => 
	        val (funEnv1, str) = generateFunctionDec(varEnv, funEnv, function)
	        val (varEnv1, funEnv2, str1) = generateTopDecs(varEnv, funEnv1)(tail)
	        (varEnv1, funEnv2, str + str1)
	      /*case PrecompileInstr(precompInstr) =>
	        val result = generatePrecompileInstruction(precompInstr, varEnv, funEnv)
	        val (varEnv1, funEnv1, str1) = generateTopDecs(varEnv, funEnv)(tail)
	        (varEnv1, funEnv1, result + str1)*/
	    }
	    
	}
  
  /**
   * Generate a function declaration
   */
  def generateFunctionDec(varEnv: VarEnv, funEnv: FunEnv, functionDec: FunctionDec): (FunEnv, String) = {
    val (ident, str) = generateDeclarator(varEnv, funEnv)(functionDec.declarator)
    val returnType = functionDec.declarationSpecifiers match {
      case Some(ds) => ds.typeSpec
      case None => TypeInteger
    }
    val parameters = functionDec.declarationList match {
      case Some(p) => p
      case None => List()
    }
    
    val funEnv1 = funEnv + (ident -> parameters)
      
    val returnTypeStr = generateType(returnType, varEnv, funEnv1)

    val parametersStr = (parameters.map({
      case d => generateDeclaration(varEnv, funEnv)(d)
    }).mkString("(", ", ", ")"))
    
    val body = "{\n" + generateStmt(varEnv, funEnv1)(functionDec.compoundStmt) + "\n}"

    (funEnv1, returnType + " " + ident + parametersStr + body)
  }  

  /**
   * Generate a variable declaration
   */
  /*def generateVariableDec(varEnv: VarEnv, funEnv: FunEnv, variableDec: Declaration): (VarEnv, String) = {
    val varEnv1 = varEnv + (variableDec.identifier -> variableDec.variableType)
    (varEnv1, generateType(variableDec.variableType, varEnv1, funEnv) + " " + variableDec.identifier) 
  }*/
  
  def generateDeclaration(varEnv: VarEnv, funEnv: FunEnv)(dec: Declaration): (VarEnv, String) = {
    var tenv = varEnv
    var s = List()
    val decSpecs = generateDeclarationSpecifiers(dec.decSpecs)
    
    for (id <- dec.declarators) {
      val (ident, str) = generateInitDeclarator(varEnv, funEnv)(id)
      tenv = tenv + (ident -> dec.decSpecs.typeSpec)
      s ::: List(str)
    }
    
    (tenv, decSpecs + s.mkString(", "))
  }
  
  def generateInitDeclarator(varEnv: VarEnv, funEnv: FunEnv)(dec: InitDeclarator): (String, String) = {
    dec match {
      case DeclaratorWrap(d) => generateDeclarator(varEnv, funEnv)(d)
      case DeclaratorWithAssign(d, a) => {
        val (ident, str) = generateDeclarator(varEnv, funEnv)(d)
        (ident, str + " = " + generateInitializer(varEnv, funEnv)(a))
      }
    }
  }
  
  def generateInitializer(varEnv: VarEnv, funEnv: FunEnv)(init: Initializer): String = {
    init match {
      case ExpressionInitializer(expr) => generateExpression(varEnv, funEnv)(expr)
      case Scalar(initializers) => "{" + initializers.map(i => generateInitializer(varEnv, funEnv)(i)).mkString(", ") + "}"
    }
  }
  
  def generateDeclarator(varEnv: VarEnv, funEnv: FunEnv)(dec: Declarator): (String, String) = {
    dec.pointer match {
      case Some(p) => {
        val (ident, str) = generateDirectDeclarator(varEnv, funEnv)(dec.directDeclarator)
        (ident, "*" + str) //FIXME
      }
      case None => generateDirectDeclarator(varEnv, funEnv)(dec.directDeclarator)
    }  
  }
  
  def generateDirectDeclarator(varEnv: VarEnv, funEnv: FunEnv)(dec: DirectDeclarator): (String, String) = {
    dec match {
      case DeclareIdentifier(name) => (name, name)
      case ParenthesiseDeclarator(declarator) => 
        val (ident, str) = generateDeclarator(varEnv, funEnv)(declarator)
        (ident, "(" + str + ")")
      case DeclareArray(dirDecl, expr) => {
    	val exprVal = expr match {
    	  case Some(e) => generateExpression(varEnv, funEnv)(e)
    	  case None => ""
    	}
    	val (ident, str) = generateDirectDeclarator(varEnv, funEnv)(dirDecl)
    	(ident, str + "[" + exprVal + "]")
      }
    }
  }
  
  def generateDeclarationSpecifiers(decSpecs: DeclarationSpecifiers) = {
    val storageSpecifier = decSpecs.storage match {
      case Some(s) => generateStorageSpecifier(s) + " "
      case None => ""
    }
    val typeSpecifier = generateTypeSpecifier(decSpecs.typeSpec) + " "
    val typeQualifier = decSpecs.qualifier match {
      case Some(q) => generateTypeQualifier(q) + " "
      case None => ""
    }
    
    storageSpecifier + typeSpecifier + typeQualifier
  }
  
  def generateStorageSpecifier(storageSpecs: StorageClassSpecifier) = {
    storageSpecs match {
      case Auto => "auto"
      case Register => "register"
      case Static => "static"
      case Extern => "extern"
      case Typedef => "typedef"
    }
  }
  
  def generateTypeSpecifier(typeSpec: TypeSpecifier) = {
    typeSpec match {
      case TypeVoid => "void"
      case TypeChar => "char"
      case TypeShort => "short"
      case TypeInteger => "int"
      case TypeLong => "long"
      case TypeFloat => "float"
      case TypeDouble => "double"
      case TypeSigned => "signed"
      case TypeUnsigned => "unsigned"
    }
  }
  
  def generateTypeQualifier(typeQual: TypeQualifier) = {
    typeQual match {
      case Const => "const"
      case Volatile => "volatile"
    }
  }
  
  /*def stmtOrDecLoop(varEnv: VarEnv, funEnv: FunEnv)(stmtsordecs: List[StmtOrDec]): (VarEnv, String) = {
      stmtsordecs match {
        case Nil => (varEnv, "")
      	case head :: tail =>
          val (varEnv1, retString) = generateStmtOrDec(varEnv, funEnv)(head)
          val (varEnvFinal, resStringFinal) = stmtOrDecLoop(varEnv1, funEnv)(tail)
          (varEnvFinal, retString + "\n" + resStringFinal)
      }
    }*/
    
  
  /*def generateDeclaration(varEnv: VarEnv, funEnv: FunEnv)(dec: Declaration): (String, VarEnv) =
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
    }*/
    
  /*def generateStatement(varEnv: VarEnv, funEnv: FunEnv)(stmt: Statement): String =
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
    }*/
  
  def generateStmt(varEnv: VarEnv, funEnv: FunEnv)(stmt: Statement): String = {
    stmt match {
      case LabeledStmt(ls) => generateLabeledStmt(varEnv, funEnv)(ls)
      case ExpressionStmt(expr) => expr match {
          case Some(es) => generateExpression(varEnv, funEnv)(es) + ";"
          case None => ";"
        }
      case CompoundStmt(cs) => generateCompoundStmt(varEnv, funEnv)(cs)
      case SelectionStmt(ss) => generateSelectionStmt(varEnv, funEnv)(ss)
      case IterationStmt(is) => generateIterationStmt(varEnv, funEnv)(is)
      case JumpStmt(js) => generateJumpStmt(varEnv, funEnv)(js)
    }
  }
  
  def generateLabeledStmt(varEnv: VarEnv, funEnv: FunEnv)(stmt: LabeledStatement): String = {
    stmt match {
      case LabelStmt(i, s) => i + ": " + generateStmt(varEnv, funEnv)(s) + "\n"
      case CaseStmt(e, s) => "case " + generateConstantExpression(varEnv, funEnv)(e) + "\n" + generateStmt(varEnv, funEnv)(s)
      case DefaultCaseStmt(s) => "default: \n" + generateStmt(varEnv, funEnv)(s)
    }
  }
  
  def generateStmtOrDec(varEnv: VarEnv, funEnv: FunEnv)(sord: StmtOrDec): (VarEnv, String) =
    sord match {
      case Stmt(statement) => (varEnv, generateStmt(varEnv, funEnv)(statement))
      case Dec(declaration) => generateDeclaration(varEnv, funEnv)(declaration)
    }

  
  def generateCompoundStmt(varEnv: VarEnv, funEnv: FunEnv)(stmts: List[StmtOrDec]): String = {
    stmts match {
      case Nil => ""
      case head :: tail => {
        val (varEnv1, str1) = generateStmtOrDec(varEnv, funEnv)(head)
        val str2 = generateCompoundStmt(varEnv1, funEnv)(tail)
        str1 + "\n" + str2
      }
    }
  }
  
  def generateSelectionStmt(varEnv: VarEnv, funEnv: FunEnv)(stmt: SelectionStatement): String = {
    stmt match {
      case If(e, s) => "if(" + generateExpression(varEnv, funEnv)(e) + ")" + "{\n" + generateStmt(varEnv, funEnv)(s) + "}\n"
      case IfElse(e, s1, s2) => "if(" + generateExpression(varEnv, funEnv)(e) + ") {\n" + generateStmt(varEnv, funEnv)(s1) + "}\n else {\n" + generateStmt(varEnv, funEnv)(s2) + "}\n"
      case Switch(e, s) => "switch(" + generateExpression(varEnv, funEnv)(e) + ") {\n" + generateStmt(varEnv, funEnv)(s) + "}\n"
    }
  }
  
  def generateIterationStmt(varEnv: VarEnv, funEnv: FunEnv)(stmt: IterationStatement): String = {
    stmt match {
      case While(e, s) => "while(" + generateExpression(varEnv, funEnv)(e) + ") {\n" + generateStmt(varEnv, funEnv)(s) + "}\n"
      case For(i, e, c, s) => {
        val ss = List(i, e, c).map({
          case expr => expr match {
            case Some(e) => generateExpression(varEnv, funEnv)(e)
            case None => ""
          }
        })
        "for(" + ss.mkString("; ") + ") {\n" + generateStmt(varEnv, funEnv)(s) + "}"
      }
      case DoWhile(s, e) => "do {\n" + generateStmt(varEnv, funEnv)(s) + "} while (" + generateExpression(varEnv, funEnv)(e) + ")\n"
    }
  }
  
  def generateJumpStmt(varEnv: VarEnv, funEnv: FunEnv)(stmt: JumpStatement): String = {
    stmt match {
      case Goto(i) => "goto " + i + ";"
      case Continue => "continue;"
      case Break => "break;"
      case Return(e) => {
        val es = e match {
          case Some(expr) => generateExpression(varEnv, funEnv)(expr)
          case None => ""
        }
        "return " + es
      }
    }
  }
    
  def generateType(t: TypeSpecifier, varEnv: VarEnv, funEnv: FunEnv): String = 
    t match {
      case TypeInteger => "int"
      case TypeChar => "char"
      /*case TypePointer(t) => generateType(t, varEnv, funEnv) + "*"
      case TypeArray(t, None) => generateType(t, varEnv, funEnv) + "[]"
      case TypeArray(t, Some(l)) => generateType(t, varEnv, funEnv) + "[" + l + "]"*/
    }
  
  def generateUnaryOp(ope: UnaryOp, varEnv: VarEnv, funEnv: FunEnv): String =
    ope match {
      case Address => "&"
      case Deref => "*"
      case Positive => "+"
      case Negative => "-"
      case OnesCompliment => "~"
      case Negation => "!"
    }
  
  def generateBinaryOp(ope: BinaryOp, varEnv: VarEnv, funEnv: FunEnv): String =
    ope match {
      case BinaryPlus => "+"
      case BinaryMinus => "-"
      case BinaryTimes => "*"
      case BinaryDivide => "/"
      case BinaryModulo => "%"
      case BinaryEquality => "=="
      case BinaryLessThan => "<"
      case BinaryLessThanOrEquals => "<="
      case BinaryGreaterThan => ">"
      case BinaryGreaterThanOrEquals => ">="
      case BinaryBitwiseOr => "|"
      case BinaryBitwiseAnd => "&"
      case BinaryBitwiseXOR => "^"
      case BinaryLogicalAnd => "&&"
      case BinaryLogicalOr => "||"
      case BinaryShiftRight => ">>"
      case BinaryShiftLeft => "<<"
    }
    
    
  def generateExpression(varEnv: VarEnv, funEnv: FunEnv)(e: Expression): String =
    /*e match {
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
    }*/
    ""
  
  def generateConstantExpression(varEnv: VarEnv, funEnv: FunEnv)(e: ConstantExpression): String =
    ""
  
  /*def generateAccess(a: Access, varEnv: VarEnv, funEnv: FunEnv): String =
    a match {
      case AccessVariable(identifier) => 
        if(lookupVar(varEnv, identifier))
          identifier
        else
          throw new UnknownVariableException("The variable " + identifier + " does not exist in the current scope.")
      case AccessDeref(expr) => "*" + generateExpr(varEnv, funEnv)(expr)
      case AccessIndex(access, expr) => generateAccess(access, varEnv, funEnv) + "[" + generateExpr(varEnv, funEnv)(expr) + "]"
    }*/
    
  
}





