/*
----------------------------------------------------------------------
Copyright (C) 2013 by Nicolai Dahl, Christian Harrington

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
----------------------------------------------------------------------
*/
package dk.itu.cinscala
import scala.collection.mutable.HashMap
import CAbstractSyntax._

trait CGenerator {
  def getEmptyVarEnv: Set[String] = Set()
  def getEmptyFunEnv: Set[String] = Set()
  
  class CASTException(val smth:String) extends Exception(smth)
  case class UnknownFunctionException(smth1:String)  extends CASTException(smth1)
  case class UnknownVariableException(smth1:String)  extends CASTException(smth1)
  case class VariableRedefinitionException(smth1:String) extends CASTException(smth1)
  case class FunctionRedefinitionException(smth1: String) extends CASTException(smth1)
  
  /*
   * Returns true if a variable already exists in the environment
   */
  def lookupVar(varEnv: VarEnv, identifier: String): Boolean =
    varEnv.exists(_.equals(identifier))
  
  /*
   * Returns true if a function already exists in the environment
   */
  def lookupFunc(funEnv: FunEnv, identifier: String): Boolean =
    funEnv.exists(_.equals(identifier))
    
  /*
   * Given an AST for a program, returns a string containing C code for that AST.
   */
  def generate (prog: Program, varEnv: VarEnv, funEnv: FunEnv): String = {
    generateExternalDeclarations(varEnv, funEnv)(prog.contents)._3
  }
  
  /*
   * Calls functions to generate global declarations, functions or control lines
   */
  def generateExternalDeclarations (varEnv: VarEnv, funEnv: FunEnv)(topDecs: List[CExternalDeclaration]): (VarEnv, FunEnv, String) =
	topDecs match {
	  case Nil => (varEnv, funEnv, "")
	  case head :: tail =>
	    head match {
	      case CPreprocessorInstruction(precompInstr) =>
	        val result = generateControlLine(precompInstr, varEnv, funEnv)
	        val (varEnv1, funEnv1, str1) = generateExternalDeclarations(varEnv, funEnv)(tail)
	        (varEnv1, funEnv1, result + "\n" + str1)
	      case function: CFunctionDec => 
	        val (funEnv1, str) = generateFunctionDec(varEnv, funEnv, function)
	        val (varEnv1, funEnv2, str1) = generateExternalDeclarations(varEnv, funEnv1)(tail)
	        (varEnv1, funEnv2, str + "\n" + str1)
	      case CGlobalDeclaration(decSpecs, declarators) =>
	        val (varEnv1, str) = generateDeclaration(varEnv, funEnv)(CDeclaration(decSpecs, declarators))
	        val (varEnv2, funEnv1, str1) = generateExternalDeclarations(varEnv1, funEnv)(tail)
	        (varEnv2, funEnv1, str + "\n\n" + str1)
	    }	    
	}
  
  /*
   * Generates preprocessor instructions
   */
  def generateControlLine(instr: CControlLine, varEnv: VarEnv, funEnv: FunEnv) = {
    instr match {
      case CDefine(i, d) => "#define " + i + " " + d
      case CUndefine(i) => "#undef " + i
      case CIncludeLocal(s) => "#include \"" + s + "\""
      case CIncludeGlobal(s) => "#include <" + s + "> "
      case CLine(l, f) => "#line " + l.contents + " " + f.getOrElse("")
      case CError(t) => "#error " + t.getOrElse("")
      case CPragma(t) => "#pragma " + t.getOrElse("")
      case instr: CControlLineConditional => generateControlLineConditional(instr)
    }
  }
  
  /*
   * Generates preprocessor conditionals
   */
  def generateControlLineConditional(c: CControlLineConditional) = {
    c.ifLine match {
      case CControlLineIfCond => "#if " + c.iff.cond + " " + c.iff.thenBranch
      case CControlLineIfDef => "#ifdef"
      case CControlLineIfNDef => "#ifndef"
    }
  }
  
  /*
   * Generates a function declaration
   */
  def generateFunctionDec(varEnv: VarEnv, funEnv: FunEnv, functionDec: CFunctionDec): (FunEnv, String) = {
    val (varEnv1, ident, str) = generateDeclarator(varEnv, funEnv)(functionDec.declarator)
    
    if (lookupFunc(funEnv, ident)) throw new FunctionRedefinitionException(ident + " already defined")
    val funEnv1 = funEnv + ident
      
    val declarationSpecifiers = for {decSpecs <- functionDec.declarationSpecifiers} yield generateDeclarationSpecifiers(decSpecs)
    
    val body = generateStmt(varEnv1, funEnv1)(functionDec.compoundStmt)

    (funEnv1, declarationSpecifiers.getOrElse("") + " " + str + "\n" + body)
  }
  
  /*
   * Generates a declaration
   */
  def generateDeclaration(varEnv: VarEnv, funEnv: FunEnv)(dec: CDeclaration): (VarEnv, String) = {
    val decSpecs = generateDeclarationSpecifiers(dec.decSpecs)
    
    def buildDeclarators(varEnv: VarEnv, funEnv: FunEnv)(decs: List[CInitDeclarator], decSpecs: CDeclarationSpecifiers): (VarEnv, String) =
      decs match {
        case Nil => (varEnv, "")
        case head :: tail => {
          val (varEnv1, ident1, str1) = generateInitDeclarator(varEnv, funEnv)(head)
          val (varEnv2, str2) = buildDeclarators(varEnv1, funEnv)(tail, decSpecs)
          val comma = str2 match {
            case "" => "" 
            case _ => ", "
          }
          
          (varEnv2, str1 + comma + str2)
        }
      }
    
    val (varEnv1, str) = buildDeclarators(varEnv, funEnv)(dec.declarators, dec.decSpecs)
    
    (varEnv1, decSpecs + " " + str + ";")
  }

  /*
   * Generates declaration specifiers
   */
  def generateDeclarationSpecifiers(dec: CDeclarationSpecifiers): String = {
    val decs = dec.decSpecs.map(spec => spec match {
      case st: CStorageClassSpecifier => generateStorageClassSpecifier(st)
      case q: CTypeQualifier => generateTypeQualifier(q)
      case t: CTypeSpecifier => generateTypeSpecifier(t)
    })

    decs.mkString(" ")
  }
  
  /*
   * Generates storage class specifiers
   */
  def generateStorageClassSpecifier(storageSpecs: CStorageClassSpecifier) = {
    storageSpecs match {
      case CAuto => "auto"
      case CRegister => "register"
      case CStatic => "static"
      case CExtern => "extern"
      case CTypedef => "typedef"
    }
  }
  
  /*
   * Generates type qualifiers
   */
  def generateTypeQualifier(typeQual: CTypeQualifier) = {
    typeQual match {
      case CConst => "const"
      case CVolatile => "volatile"
    }
  }
  
  /*
   * Generates type specifiers
   */
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
  
  /*
   * Generates an init declarator
   */
  def generateInitDeclarator(varEnv: VarEnv, funEnv: FunEnv)(dec: CInitDeclarator): (VarEnv, String, String) = {
    dec match {
      case CDeclaratorWrap(d) => {
        val (varEnv1, ident, str) = generateDeclarator(varEnv, funEnv)(d)      
        val varEnv2 = varEnv1 + ident
        
        (varEnv2, ident, str)
      }
      case CDeclaratorWithAssign(d, a) => {
        val (varEnv1, ident, str) = generateDeclarator(varEnv, funEnv)(d)
        val varEnv2 = varEnv + ident
        
        (varEnv2, ident, str + " = " + generateInitializer(varEnv, funEnv)(a))
      }
    }
  }
  
  /*
   * Generates a declarator
   */
  def generateDeclarator(varEnv: VarEnv, funEnv: FunEnv)(dec: CDeclarator): (VarEnv, String, String) = {
    dec match {
      // Pointer
      case CPointerDeclarator(pointer, declarator) => 
        val (varEnv1, ident, str) = generateDeclarator(varEnv, funEnv)(declarator)
        val pointerStr = generatePointer(pointer)
        (varEnv1, ident, pointerStr + str)
      // Identifier
      case CDeclareIdentifier(name) => (varEnv, name, name)
      // Parenthesise declarator
      case CParenthesiseDeclarator(declarator) => 
        val (varEnv1, ident, str) = generateDeclarator(varEnv, funEnv)(declarator)
        (varEnv1, ident, "(" + str + ")")
      // Array
      case CDeclareArray(dirDecl, expr) => {
    	val exprVal = expr match {
    	  case Some(e) => generateExpression(varEnv, funEnv)(e)
    	  case None => ""
    	}
    	val (varEnv1, ident, str) = generateDeclarator(varEnv, funEnv)(dirDecl)
    	(varEnv1, ident, str + "[" + exprVal + "]")
      }
      // Parameters
      case CParameterList(d, p) => {
        val (varEnv1, ident, str) = generateDeclarator(varEnv, funEnv)(d)
        val ps = p.map(generateParameterDeclaration(varEnv1, funEnv))
        val varEnv2: VarEnv = varEnv1 ++ ps.map(_._1).toSet
        (varEnv2, ident, str + "(" + ps.map(_._2).mkString(", ") + ")")
      } 
      // Parameters with ellipsis
      case CParameterListWithEllipsis(d, p) =>
        val (varEnv1, ident, str) = generateDeclarator(varEnv, funEnv)(d)
        val ps = p.map(generateParameterDeclaration(varEnv1, funEnv))
        val varEnv2: VarEnv = varEnv1 ++ ps.map(_._1).toSet
        (varEnv1, ident, str + "(" + ps.map(_._2).mkString(", ") + ", ...)")
      // Identifier list
      case CIdentifierList(d, i) => {
        val (varEnv1, ident, str) = generateDeclarator(varEnv, funEnv)(d)
        val is = i match {
          case Some(ii) => ii.mkString(", ")
          case None => ""
        }
        (varEnv1, ident, is + str)
      }
    }
  }
  
  /*
   * Generates an initializer, either with an expression or as a scalar
   */
  def generateInitializer(varEnv: VarEnv, funEnv: FunEnv)(init: CInitializer): String = {
    init match {
      case CExpressionInitializer(expr) => generateExpression(varEnv, funEnv)(expr)
      case CScalar(initializers) => "{" + initializers.map(i => generateInitializer(varEnv, funEnv)(i)).mkString(", ") + "}"
    }
  }
  
  /*
   * Generates a parameter declaration, either a normal or abstract
   */
  def generateParameterDeclaration(varEnv: VarEnv, funEnv: FunEnv)(p: CParameterDeclaration): (String, String) = {
    p match {
      case CNormalDeclaration(decSpecs, dec) => {
        val str = generateDeclarator(varEnv, funEnv)(dec)
        (str._2, generateDeclarationSpecifiers(decSpecs) + " " + str._3)
      }
      case CAbstractDeclaration(decSpecs, dec) => {
        val str = dec match {
          case Some(d) => generateAbstractDeclarator(varEnv, funEnv)(d)
          case None => ""
        }
        (str, generateDeclarationSpecifiers(decSpecs) + " " + str)
      }
    }
  }
  
  /*
   * Generates an abstract declarator
   */
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
  
  /*
   * Generates a direct abstract declarator
   */
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
  
  /*
   * Generates a pointer
   */
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
  
  /*
   * Generates a compound statement
   */
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
  
  /*
   * Generates a statement or a declaration
   */
  def generateStmtOrDec(varEnv: VarEnv, funEnv: FunEnv)(sord: CStmtOrDec): (VarEnv, String) =
    sord match {
      case stmt: CStatement => (varEnv, generateStmt(varEnv, funEnv)(stmt))
      case CLocalDeclaration(decSpecs, declarators) => generateDeclaration(varEnv, funEnv)(CDeclaration(decSpecs, declarators))
    }
  
  /*
   * Generates a statement
   */
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
 
  /*
   * Generates a type name
   */
  def generateTypeName(varEnv: VarEnv, funEnv: FunEnv)(tn: CTypeName): String = {
    val adStr = (for {ad <- tn.abstractDeclarator} yield generateAbstractDeclarator(varEnv, funEnv)(ad))
    val a = adStr match {
      case Some(a) => " " + a
      case None => ""
    }
    generateTypeSpecifierQualifier(varEnv, funEnv)(tn.qualifierSpecifierList) + a
    
  }
 
  /*
   * Generates a type specifier and possibly a type qualifier
   */
  def generateTypeSpecifierQualifier(varEnv: VarEnv, funEnv: FunEnv)(tsq: CTypeSpecifierQualifier): String = {
    val tq = tsq match {
      case CTypeSpecifierQualifier(ts, Some(tq)) => generateTypeQualifier(tq) + " "
      case CTypeSpecifierQualifier(ts, None) => ""
    }
    
    tq + generateTypeSpecifier(tsq.typeSpecifier)
  }
  
  /*
   * Generates an expression
   */
  def generateExpression(varEnv: VarEnv, funEnv: FunEnv, checkEnv: Boolean = true)(e: CExpression): String =
    e match {
      //Assignment Expression
      case CAssign(assignTo, operator, expr) =>
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
        postfixExpression match {
          case identifier: CAccessIdentifier => {
             
            /*if(!lookupFunc(funEnv, identifier.name)) {
              throw new UnknownFunctionException("Function '" + identifier.name + "' is not defined")
            }*/
          }
        }
        
        generateExpression(varEnv, funEnv, false)(postfixExpression) + arguments.map(generateExpression(varEnv, funEnv)).mkString("(", ", ", ")")
      case CAccessMember (postfixExpr, memberToAccess) => 
        generateExpression(varEnv, funEnv)(postfixExpr) + "." + generateDeclarator(varEnv, funEnv)(memberToAccess)._2
      case CAccessArrowMember (postfixExpr, memberToAccess) =>
        generateExpression(varEnv, funEnv)(postfixExpr) + "->" + generateDeclarator(varEnv, funEnv)(memberToAccess)._2
    
	  //Primary Expressions
      case CAccessIdentifier(name) => {
        if (checkEnv && !lookupVar(varEnv, name)) 
          throw new UnknownVariableException("Variable '" + name + "' not defined")
        name
      }
      case CConstantInteger(contents) => contents.toString()
      case CConstantChar (contents) => "'" + contents.toString() + "'"
      case CConstantFloat (contents) => {
        // Set the locale to English, so commas in floats are written as "."
 	    java.util.Locale.setDefault(java.util.Locale.ENGLISH)
 	    "%.6f".format(contents) + "f"
      }
      case CConstantEnumeration(s) => s 
      case CCharArray (content) => "\"" + content + "\""
      case CParenthesiseExpr(content) => "(" + generateExpression(varEnv, funEnv)(content) + ")"
      case expr: CPostfixExpression => generateExpression(varEnv, funEnv)(expr)
    }
  
  /*
   * Unary operators
   */
  def generateUnaryOp(ope: CUnaryOp): String =
    ope match {
      case CAddress => "&"
      case CDeref => "*"
      case CPositive => "+"
      case CNegative => "-"
      case COnesCompliment => "~"
      case CNegation => "!"
    }
  
  /*
   * Binary operators
   */
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
  
  /*
   * Assignment operators
   */
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
}





