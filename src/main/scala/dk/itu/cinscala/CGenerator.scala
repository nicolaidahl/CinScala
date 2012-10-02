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

package dk.itu.cinscala
import scala.collection.mutable.HashMap
import CAbstractSyntax._

trait CGenerator {
  
  def getEmptyVarEnv: Map[String, CTypeSpecifier] = Map.empty[String, CTypeSpecifier]
  def getEmptyFunEnv: Map[String, List[CDeclaration]] = Map.empty[String, List[CDeclaration]]
  
  class CASTException(val smth:String) extends Exception(smth)
  case class UnknownVariableException(smth1:String)  extends CASTException(smth1)
  case class VariableRedefinitionException(smth1:String) extends CASTException(smth1)
  case class FunctionRedefinitionException(smth1: String) extends CASTException(smth1)
  
  
  def lookupVar(varEnv: VarEnv, identifier: String): Boolean =
    varEnv.exists(_._1.equals(identifier))
  
  def lookupFunc(funEnv: FunEnv, identifier: String): Boolean =
    funEnv.exists(_._1.equals(identifier))
  
    
  //Main generate function
  def generate (prog: Program, varEnv: VarEnv, funEnv: FunEnv): String = {
    generateExternalDeclarations(varEnv, funEnv)(prog.contents)._3
  }
  
  def generateControlLine(instr: CControlLine, varEnv: VarEnv, funEnv: FunEnv) = {
    instr match {
      case CIncludeLocal(s) => "#include \"" + s + "\" \n"
      case CIncludeGlobal(s) => "#include <" + s + "> \n"
    }
  }
  
  def generateExternalDeclarations (varEnv: VarEnv, funEnv: FunEnv)(topDecs: List[CExternalDeclaration]): (VarEnv, FunEnv, String) =
	topDecs match {
	  case Nil => (varEnv, funEnv, "")
	  case head :: tail =>
	    head match {
	      case CGlobalDeclaration(decSpecs, declarators) =>
	        val (varEnv1, str) = generateDeclaration(varEnv, funEnv)(CDeclaration(decSpecs, declarators))
	        val (varEnv2, funEnv1, str1) = generateExternalDeclarations(varEnv1, funEnv)(tail)
	        (varEnv2, funEnv1, str + "\n\n" + str1)
	      case function: CFunctionDec => 
	        val (funEnv1, str) = generateFunctionDec(varEnv, funEnv, function)
	        val (varEnv1, funEnv2, str1) = generateExternalDeclarations(varEnv, funEnv1)(tail)
	        (varEnv1, funEnv2, str + "\n" + str1)
	      case CPreprocessorInstruction(precompInstr) =>
	        val result = generateControlLine(precompInstr, varEnv, funEnv)
	        val (varEnv1, funEnv1, str1) = generateExternalDeclarations(varEnv, funEnv)(tail)
	        (varEnv1, funEnv1, result + "\n" + str1)
	    }
	    
	}
  
  /**
   * Generate a function declaration
   */
  def generateFunctionDec(varEnv: VarEnv, funEnv: FunEnv, functionDec: CFunctionDec): (FunEnv, String) = {
    val (ident, str) = generateDeclarator(varEnv, funEnv)(functionDec.declarator)
    /*val returnType = functionDec.declarationSpecifiers match {
      case Some(ds) => ds.typeSpec
      case None => TypeInteger
    }*/
    val parameters = functionDec.declarationList match { //FIXME
      case Some(p) => p
      case None => List()
    }
    
    val funEnv1 = funEnv + (ident -> parameters)//FIXME
      
    val declarationSpecifiers = for {decSpecs <- functionDec.declarationSpecifiers} yield generateDeclarationSpecifiers(decSpecs)

    //val parametersStr = (parameters.map(generateDeclaration(varEnv, funEnv))).mkString("(", ", ", ")")
    
    val body = generateStmt(varEnv, funEnv1)(functionDec.compoundStmt)//FIXME

    (funEnv1, declarationSpecifiers.getOrElse("") + " " + str + "\n" + body)
  }  

  
  def generateDeclarationSpecifiers(dec: CDeclarationSpecifiers): String = {
    val decs = dec.decSpecs.map(spec => spec match {
      case st: CStorageClassSpecifier => generateStorageClassSpecifier(st)
      case q: CTypeQualifier => generateTypeQualifier(q)
      case t: CTypeSpecifier => generateTypeSpecifier(t)
    })
    
    /*val storage = for { st <- dec.storage } yield generateStorageClassSpecifier(st) + " "
    val qualifier = for { q <- dec.qualifier } yield generateTypeQualifier(q) + " "
    
    storage.getOrElse("") + qualifier.getOrElse("") + generateTypeSpecifier(dec.typeSpec)*/
    decs.mkString(" ")
  }
  
  def generateDeclaration(varEnv: VarEnv, funEnv: FunEnv)(dec: CDeclaration): (VarEnv, String) = {
    val decSpecs = generateDeclarationSpecifiers(dec.decSpecs)
    
    def buildDeclarators(varEnv: VarEnv, funEnv: FunEnv)(decs: List[CInitDeclarator]): (VarEnv, String) =
      decs match {
        case Nil => (varEnv, "")
        case head :: tail => {
          val (varEnv1, str1) = generateInitDeclarator(varEnv, funEnv)(head)
          val (varEnv2, str2) = buildDeclarators(varEnv, funEnv)(tail)
          val comma = str2 match {
            case "" => "" 
            case _ => ", "
          }
          (varEnv2, str1 + comma + str2)
        }
      }
    
    val (varEnv1, str) = buildDeclarators(varEnv, funEnv)(dec.declarators)
    
    (varEnv1, decSpecs + " " + str + ";")
  }
  
  def generateInitDeclarator(varEnv: VarEnv, funEnv: FunEnv)(dec: CInitDeclarator): (String, String) = {
    dec match {
      case CDeclaratorWrap(d) => generateDeclarator(varEnv, funEnv)(d)
      case CDeclaratorWithAssign(d, a) => {
        val (ident, str) = generateDeclarator(varEnv, funEnv)(d)
        (ident, str + " = " + generateInitializer(varEnv, funEnv)(a))
      }
    }
  }
  
  def generateInitializer(varEnv: VarEnv, funEnv: FunEnv)(init: CInitializer): String = {
    init match {
      case CExpressionInitializer(expr) => generateExpression(varEnv, funEnv)(expr)
      case CScalar(initializers) => "{" + initializers.map(i => generateInitializer(varEnv, funEnv)(i)).mkString(", ") + "}"
    }
  }
  
  /*def generateDeclarator(varEnv: VarEnv, funEnv: FunEnv)(dec: CDeclarator): (String, String) = {    
    val ps = dec.pointer match {
      case Some(p) => {
        generatePointer(p)
      }
      case None => ""
    }
    
    val (ident, str) = generateDirectDeclarator(varEnv, funEnv)(dec.directDeclarator) 
    
    (ident, ps + str) 
  }*/
  
  def generatePointer(point: CPointer): String = {
	  val p = point.pointer match {
	    case Some(pp) => generatePointer(pp)
	    case None => ""
	  }
	  
	  val q = point.typeQualifier match {
	    case Some(qs) => qs.mkString("", " ", " ")
	    case None => ""
	  }
	  
	  "*" + q + p
  }
  
  def generateDeclarator(varEnv: VarEnv, funEnv: FunEnv)(dec: CDeclarator): (String, String) = {
    dec match {
      case CPointerDeclarator(pointer, declarator) => 
        val (ident, str) = generateDeclarator(varEnv, funEnv)(declarator)
        (ident, generatePointer(pointer) + str)
      case CDeclareIdentifier(name) => (name, name)
      case CParenthesiseDeclarator(declarator) => 
        val (ident, str) = generateDeclarator(varEnv, funEnv)(declarator)
        (ident, "(" + str + ")")
      case CDeclareArray(dirDecl, expr) => {
    	val exprVal = expr match {
    	  case Some(e) => generateExpression(varEnv, funEnv)(e)
    	  case None => ""
    	}
    	val (ident, str) = generateDeclarator(varEnv, funEnv)(dirDecl)
    	(ident, str + "[" + exprVal + "]")
      }
      case CParameterList(d, p) => {
        val (ident, str) = generateDeclarator(varEnv, funEnv)(d)
        val ps = p.map(pp => generateParameterDeclaration(varEnv, funEnv)(pp)).mkString(", ")
        (ident, str + "(" + ps + ")")
      } 
      case CParameterListWithEllipsis(d, p) =>
        val (ident, str) = generateDeclarator(varEnv, funEnv)(d)
        val ps = p.map(pp => generateParameterDeclaration(varEnv, funEnv)(pp)).mkString(", ")
        (ident, str + "(" + ps + ", ...)")
      case CIdentifierList(d, i) => {
        val (ident, str) = generateDeclarator(varEnv, funEnv)(d)
        val is = i match {
          case Some(ii) => ii.mkString(", ")
          case None => ""
        }
        (ident, is + str)
      }
    }
  }
  
  def generateParameterDeclaration(varEnv: VarEnv, funEnv: FunEnv)(p: CParameterDeclaration): String = {
    p match {
      case CNormalDeclaration(decSpecs, dec) => {
        val str = generateDeclarator(varEnv, funEnv)(dec)._2
        generateDeclarationSpecifiers(decSpecs) + " " + str
      }
      case CAbstractDeclaration(decSpecs, dec) => {
        val str = dec match {
          case Some(d) => generateAbstractDeclarator(varEnv, funEnv)(d)
          case None => ""
        }
        generateDeclarationSpecifiers(decSpecs) + " " + str
      }
    }
  }
  
  def generateAbstractDeclarator(varEnv: VarEnv, funEnv: FunEnv)(a: CAbstractDeclarator): String = {
    a match {
      case CAbstractPointer(p) => generatePointer(p)
      case CNormalDirectAbstractDeclarator(p, dec) => {
        val ps = p match {
          case Some(p) => generatePointer(p)
          case None => ""
        }
        
        ps + generateDirectAbstractDeclarator(varEnv, funEnv)(dec)
      }
    }
  }
  
  def generateDirectAbstractDeclarator(varEnv: VarEnv, funEnv: FunEnv)(dac: CDirectAbstractDeclarator): String = {
    dac match {
      case CParenthesiseAbDec(a) => {
        "(" + generateAbstractDeclarator(varEnv, funEnv)(a) + ")"
      }
      case CArrayAbDec(d, e) => {
        val str = d match {
          case Some(d) => generateDirectAbstractDeclarator(varEnv, funEnv)(d)
          case None => (None, "")
        }
        
        str + "[" + generateExpression(varEnv, funEnv)(e) + "]"
      }
      case CFunctionAbDec(d, p, e) => {
        val str = d match {
          case Some(d) => generateDirectAbstractDeclarator(varEnv, funEnv)(d)
          case None => (None, "") 
        }
        
        val es = e match {
          case true => ", ..."
          case false => ""
        }
        
        str + "(" + p.map(p => generateParameterDeclaration(varEnv, funEnv)(p)).mkString(", ") + es + ")"
      }
    }
  }
  
  def generateStorageClassSpecifier(storageSpecs: CStorageClassSpecifier) = {
    storageSpecs match {
      case CAuto => "auto"
      case CRegister => "register"
      case CStatic => "static"
      case CExtern => "extern"
      case CTypedef => "typedef"
    }
  }
  
  def generateTypeSpecifier(typeSpec: CTypeSpecifier) = {
    typeSpec match {
      case CTypeVoid => "void"
      case CTypeChar => "char"
      case CTypeShort => "short"
      case CTypeInteger => "int"
      case CTypeLong => "long"
      case CTypeFloat => "float"
      case CTypeDouble => "double"
      case CTypeSigned => "signed"
      case CTypeUnsigned => "unsigned"
      case CTypeStruct(name) => name.getOrElse("")
      case CTypeEnum(name) => name.getOrElse("")
      case CTypeUnion(name) => name.getOrElse("")
    }
  }
  
  def generateTypeQualifier(typeQual: CTypeQualifier) = {
    typeQual match {
      case CConst => "const"
      case CVolatile => "volatile"
    }
  }
  
  
  
  def generateStmt(varEnv: VarEnv, funEnv: FunEnv)(stmt: CStatement): String = {
    stmt match {
      case CExpressionStmt(expr) => expr match {
          case Some(es) => generateExpression(varEnv, funEnv)(es) + ";"
          case None => ";"
        }
      case CCompoundStmt(cs) => generateCompoundStmt(varEnv, funEnv)(cs)
      
      //Labeled Statements
      case CLabelStmt(i, s) => i + ": " + generateStmt(varEnv, funEnv)(s) + "\n"
      case CCaseStmt(e, s) => "case " + generateExpression(varEnv, funEnv)(e) + ": " + generateStmt(varEnv, funEnv)(s)
      case CDefaultCaseStmt(s) => "default: \n" + generateStmt(varEnv, funEnv)(s)
      
      //Iteration Statements
      case CWhile(e, s) => "while(" + generateExpression(varEnv, funEnv)(e) + ") \n" + generateStmt(varEnv, funEnv)(s)
      case CFor(i, e, c, s) => {
        val ss = List(i, e, c).map({
          case expr => expr match {
            case Some(e) => generateExpression(varEnv, funEnv)(e)
            case None => ""
          }
        })
        "for(" + ss.mkString("; ") + ") \n" + generateStmt(varEnv, funEnv)(s)
        
      }
      case CDoWhile(s, e) => "do {\n" + generateStmt(varEnv, funEnv)(s) + "} while (" + generateExpression(varEnv, funEnv)(e) + ");\n"

      //Selection Statements
      case CIf(e, s) => "if(" + generateExpression(varEnv, funEnv)(e) + ") " + generateStmt(varEnv, funEnv)(s) + "\n"
      case CIfElse(e, s1, s2) => "if(" + generateExpression(varEnv, funEnv)(e) + ") " + generateStmt(varEnv, funEnv)(s1) + "\nelse " + generateStmt(varEnv, funEnv)(s2) + "\n"
      case CSwitch(e, s) => "switch(" + generateExpression(varEnv, funEnv)(e) + ") " + generateStmt(varEnv, funEnv)(s)

      //Jump Statements
      case CGoto(i) => "goto " + i + ";"
      case CContinue => "continue;"
      case CBreak => "break;"
      case CReturn(e) => {
        val es = e match {
          case Some(expr) => generateExpression(varEnv, funEnv)(expr)
          case None => ""
        }
        "return " + es + ";"
      }
    }
  }
  
  def generateCompoundStmt(varEnv: VarEnv, funEnv: FunEnv)(stmts: List[CStmtOrDec]): String = {
    def buildStmts(varEnv: VarEnv, funEnv:FunEnv)(stmts:List[CStmtOrDec]): String = {
      stmts match {
        case Nil => ""
        case head :: tail => {
          val (varEnv1, str1) = generateStmtOrDec(varEnv, funEnv)(head)
          val str2 = buildStmts(varEnv1, funEnv)(tail)
          str1 + "\n" + str2
        }
      }
    }
     
    "{\n" + buildStmts(varEnv, funEnv)(stmts) + "}\n"
  }
  
  def generateStmtOrDec(varEnv: VarEnv, funEnv: FunEnv)(sord: CStmtOrDec): (VarEnv, String) =
    sord match {
      case stmt: CStatement => (varEnv, generateStmt(varEnv, funEnv)(stmt))
      case CLocalDeclaration(decSpecs, declarators) => generateDeclaration(varEnv, funEnv)(CDeclaration(decSpecs, declarators))
    }

  
  def generateUnaryOp(ope: CUnaryOp): String =
    ope match {
      case CAddress => "&"
      case CDeref => "*"
      case CPositive => "+"
      case CNegative => "-"
      case COnesCompliment => "~"
      case CNegation => "!"
    }
  
  def generateBinaryOp(ope: CBinaryOp): String =
    ope match {
      case CBinaryPlus => "+"
      case CBinaryMinus => "-"
      case CBinaryTimes => "*"
      case CBinaryDivide => "/"
      case CBinaryModulo => "%"
      case CBinaryEquality => "=="
      case CBinaryLessThan => "<"
      case CBinaryLessThanOrEquals => "<="
      case CBinaryGreaterThan => ">"
      case CBinaryGreaterThanOrEquals => ">="
      case CBinaryBitwiseOr => "|"
      case CBinaryBitwiseAnd => "&"
      case CBinaryBitwiseXOR => "^"
      case CBinaryLogicalAnd => "&&"
      case CBinaryLogicalOr => "||"
      case CBinaryShiftRight => ">>"
      case CBinaryShiftLeft => "<<"
    }
    
  def generateAssignmentOp(ope: CAssignmentOperator): String =
    ope match {
      case CEquals => "="
      case CTimesEquals => "*="
      case CDivisionEquals => "/="
      case CModuloEquals => "%="
      case CPlusEquals => "+="
      case CMinusEquals => "-="
      case CShiftLeftEquals => "<<="
      case CShiftRightEquals => ">>="
      case CBitwiseAndEquals => "&="
      case CBitwiseOrEquals => "|="
      case CBitwiseXOREquals => "^="
    }
    
  def generateTypeName(varEnv: VarEnv, funEnv: FunEnv)(tn: CTypeName): String = {
    val adStr = (for {ad <- tn.abstractDeclarator} yield generateAbstractDeclarator(varEnv, funEnv)(ad))
    val a = adStr match {
      case Some(a) => " " + a
      case None => ""
    }
    generateTypeSpecifierQualifier(varEnv, funEnv)(tn.qualifierSpecifierList) + a
    
  }
  
  def generateTypeSpecifierQualifier(varEnv: VarEnv, funEnv: FunEnv)(tsq: CTypeSpecifierQualifier): String = {
    val tq = tsq match {
      case CTypeSpecifierQualifier(ts, Some(tq)) => generateTypeQualifier(tq) + " "
      case CTypeSpecifierQualifier(ts, None) => ""
    }
    
    tq + generateTypeSpecifier(tsq.typeSpecifier)
  }
  
  def generateExpression(varEnv: VarEnv, funEnv: FunEnv)(e: CExpression): String =
    e match {
      //Assignment Expression
      case CAssign(assignTo, operator, expr) =>  //TODO make sure this works with the varEnv
        val assignToStr = generateExpression(varEnv, funEnv)(assignTo)
        val opeStr = generateAssignmentOp(operator)
        val exprStr = generateExpression(varEnv, funEnv)(expr)
        assignToStr + " " + opeStr + " " + exprStr
    
      //Conditional Expression
      case CConditionalExpression(expr1, expr2, expr3) => 
        generateExpression(varEnv, funEnv)(expr1) + " ? " + generateExpression(varEnv, funEnv)(expr2) + " : " + generateExpression(varEnv, funEnv)(expr3)
    
      //General Expression
      case CBinaryPrim(ope, expr1, expr2) =>
        generateExpression(varEnv, funEnv)(expr1) + " " + generateBinaryOp(ope) + " " + generateExpression(varEnv, funEnv)(expr2)
    
      //Cast Expression
	  case CCast(newType, expr) => "(" + generateTypeName(varEnv, funEnv)(newType) + ") " + generateExpression(varEnv, funEnv)(expr)  
    
      //Unary Expression
      case CUnaryPrim (operator, expression) => generateUnaryOp(operator) + generateExpression(varEnv, funEnv)(expression) //Unary primitive operator
      case CPrefixIncrement (expression) => "++" + generateExpression(varEnv, funEnv)(expression)
      case CPrefixDecrement (expression) => "--" + generateExpression(varEnv, funEnv)(expression)
      case CSizeofUnary (expression) => "sizeof " + generateExpression(varEnv, funEnv)(expression) 
      case CSizeofTypeName (typeName) => "sizeof(" + generateTypeName(varEnv, funEnv)(typeName) + ")"
    
      //Postfix Expressions
      case CPostfixIncrement (expression) => generateExpression(varEnv, funEnv)(expression) + "++"
      case CPostfixDecrement (expression) => generateExpression(varEnv, funEnv)(expression) + "--"
      case CAccessIndex (postfixExpr, expr) => 
        generateExpression(varEnv,funEnv)(postfixExpr) + "[" + generateExpression(varEnv, funEnv)(expr) + "]"
      case CCall (postfixExpression, arguments) => 
        //if(!lookupFunc(funEnv, identifier))
        //  printf("Warning: Function " + identifier + " is unknown.\n\n")
        generateExpression(varEnv, funEnv)(postfixExpression) + arguments.map(generateExpression(varEnv, funEnv)).mkString("(", ", ", ")")
      case CAccessMember (postfixExpr, memberToAccess) => 
        generateExpression(varEnv, funEnv)(postfixExpr) + "." + generateDeclarator(varEnv, funEnv)(memberToAccess)._2
      case CAccessArrowMember (postfixExpr, memberToAccess) =>
        generateExpression(varEnv, funEnv)(postfixExpr) + "->" + generateDeclarator(varEnv, funEnv)(memberToAccess)._2
    
	  //Primary Expressions
      case CAccessIdentifier(name) => name
      case CConstantInteger(contents) => contents.toString()
      case CConstantChar (contents) => "'" + contents.toString() + "'"
      case CConstantFloat (contents) => {
        // Set det locale to English, so commas in floats are written as "."
 	    java.util.Locale.setDefault(java.util.Locale.ENGLISH)
 	    "%.6f".format(contents) + "f"
      }
      case CConstantEnumeration => "" //TODO find out what this is
      case CCharArray (content) => "\"" + content + "\""
      case CParenthesiseExpr(content) => "(" + generateExpression(varEnv, funEnv)(content) + ")"
      case expr: CPostfixExpression => generateExpression(varEnv, funEnv)(expr)
    }
  
    
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





