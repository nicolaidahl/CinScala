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
    generateExternalDeclarations(varEnv, funEnv)(prog.contents)._3
  }
  
  def generateControlLine(instr: ControlLine, varEnv: VarEnv, funEnv: FunEnv) = {
    instr match {
      case IncludeLocal(s) => "#include \"" + s + "\" \n"
      case IncludeGlobal(s) => "#include <" + s + "> \n"
    }
  }
  
  def generateExternalDeclarations (varEnv: VarEnv, funEnv: FunEnv)(topDecs: List[ExternalDeclaration]): (VarEnv, FunEnv, String) =
	topDecs match {
	  case Nil => (varEnv, funEnv, "")
	  case head :: tail =>
	    head match {
	      case variable: Declaration =>
	        val (varEnv1, str) = generateDeclaration(varEnv, funEnv)(variable)
	        val (varEnv2, funEnv1, str1) = generateExternalDeclarations(varEnv1, funEnv)(tail)
	        (varEnv2, funEnv1, str + str1)
	      case function: FunctionDec => 
	        val (funEnv1, str) = generateFunctionDec(varEnv, funEnv, function)
	        val (varEnv1, funEnv2, str1) = generateExternalDeclarations(varEnv, funEnv1)(tail)
	        (varEnv1, funEnv2, str + str1)
	      case PreprocessorInstruction(precompInstr) =>
	        val result = generateControlLine(precompInstr, varEnv, funEnv)
	        val (varEnv1, funEnv1, str1) = generateExternalDeclarations(varEnv, funEnv)(tail)
	        (varEnv1, funEnv1, result + str1)
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

  
  
  def generateDeclaration(varEnv: VarEnv, funEnv: FunEnv)(dec: Declaration): (VarEnv, String) = {
    val decSpecs = generateDeclarationSpecifiers(dec.decSpecs)
    
    def buildDeclarators(varEnv: VarEnv, funEnv: FunEnv)(decs: List[InitDeclarator]): (VarEnv, String) =
      decs match {
        case Nil => (varEnv, "")
        case head :: tail => {
          val (varEnv1, str1) = generateInitDeclarator(varEnv, funEnv)(head)
          val (varEnv2, str2) = buildDeclarators(varEnv, funEnv)(tail)
          (varEnv2, str1 + ", " + str2)
        }
      }
    
    val (varEnv1, str) = buildDeclarators(varEnv, funEnv)(dec.declarators)
    
    (varEnv1, decSpecs + " " + str)
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
  
  def generatePointer(varEnv: VarEnv, funEnv: FunEnv)(point: Pointer): String = {
	  val p = point.pointer match {
	    case Some(pp) => generatePointer(varEnv, funEnv)(pp)
	    case None => ""
	  }
	  
	  val q = point.typeQualifier match {
	    case Some(qs) => qs.mkString(" ")
	    case None => ""
	  }
	  
	  "*" + q.mkString("", " ", " ") + p
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
      case ParameterList(d, p, e) => {
        val (ident, str) = generateDirectDeclarator(varEnv, funEnv)(d)
        val ps = p.map(pp => generateParameterDeclaration(varEnv, funEnv)(pp)).mkString(", ")
        (ident, str + "(" + ps + ")")
      } 
      case IdentifierList(d, i) => {
        val (ident, str) = generateDirectDeclarator(varEnv, funEnv)(d)
        val is = i match {
          case Some(ii) => ii.mkString(", ")
          case None => ""
        }
        (ident, is + str)
      }
    }
  }
  
  def generateParameterDeclaration(varEnv: VarEnv, funEnv: FunEnv)(p: ParameterDeclaration): String = {
    p match {
      case NormalDeclaration(decSpecs, dec) => {
        val (ident, str) = generateDeclarator(varEnv, funEnv)(dec)
        generateDeclarationSpecifiers(decSpecs) + " " + str
      }
      case AbstractDeclaration(decSpecs, dec) => {
        val (ident, str) = dec match {
          case Some(d) => generateAbstractDeclarator(varEnv, funEnv)(d)
          case None => ("", "")
        }
        generateDeclarationSpecifiers(decSpecs) + " " + str
      }
    }
  }
  
  def generateAbstractDeclarator(varEnv: VarEnv, funEnv: FunEnv)(a: AbstractDeclarator): (String, String) = {
    a match {
      case AbstractPointer(p) => ("", generatePointer(varEnv, funEnv)(p)) //FIXME
      case NormalDirectAbstractDeclarator(p, dec) => {
        val ps = p match {
          case Some(p) => generatePointer(varEnv, funEnv)(p)
          case None => ""
        }
        val (ident, str) = generateDirectAbstractDeclarator(varEnv, funEnv)(dec)
        
        (ident, ps + str)
      }
    }
  }
  
  def generateDirectAbstractDeclarator(varEnv: VarEnv, funEnv: FunEnv)(dac: DirectAbstractDeclarator): (String, String) = {
    dac match {
      case ParenthesiseAbDec(a) => {
        val (ident, str) = generateAbstractDeclarator(varEnv, funEnv)(a)
        (ident, "(" + str + ")")
      }
      case ArrayAbDec(d, e) => {
        val (ident, str) = d match {
          case Some(d) => generateDirectAbstractDeclarator(varEnv, funEnv)(d)
          case None => ("", "") //FIXME
        }
        
        (ident, str + "[" + generateConstantExpression(varEnv, funEnv)(e) + "]")
      }
      case FunctionAbDec(d, p, e) => {
        val (ident, str) = d match {
          case Some(d) => generateDirectAbstractDeclarator(varEnv, funEnv)(d)
          case None => ("", "") //FIXME
        }
        
        val es = e match {
          case true => "..."
          case false => ""
        }
        
        (ident, str + "(" + p.map(p => generateParameterDeclaration(varEnv, funEnv)(p)).mkString(", ") + es + ")")
      }
    }
  }
  
  def generateDeclarationSpecifiers(decSpecs: DeclarationSpecifiers): String = {
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
  
  def generateUnaryOp(ope: UnaryOp): String =
    ope match {
      case Address => "&"
      case Deref => "*"
      case Positive => "+"
      case Negative => "-"
      case OnesCompliment => "~"
      case Negation => "!"
    }
  
  def generateBinaryOp(ope: BinaryOp): String =
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
    
  def generateAssignmentOp(ope: AssignmentOperator): String =
    ope match {
      case Equals => "="
      case TimesEquals => "*="
      case DivisionEquals => "/="
      case ModuloEquals => "%="
      case PlusEquals => "+="
      case MinusEquals => "-="
      case ShiftLeftEquals => "<<="
      case ShiftRightEquals => ">>="
      case BitwiseAndEquals => "&="
      case BitwiseOrEquals => "|="
      case BitwiseXOREquals => "^="
    }
    
  def generateTypeName(varEnv: VarEnv, funEnv: FunEnv)(tn: TypeName): String =
    generateTypeSpecifierQualifier(varEnv, funEnv)(tn.qualifierSpecifierList) + " " + (for {ad <- tn.abstractDeclarator} yield generateAbstractD)
    
  def generateTypeSpecifierQualifier(varEnv: VarEnv, funEnv: FunEnv)(tsq: TypeSpecifierQualifier): String =
    ""
  
  def generateExpression(varEnv: VarEnv, funEnv: FunEnv)(e: Expression): String =
    e match {
    case Assign(assignTo, operator, expr) =>  //TODO make sure this works with the varEnv
      val assignToStr = generateUnaryExpression(varEnv, funEnv)(assignTo)
      val opeStr = generateAssignmentOp(operator)
      val exprStr = generateExpression(varEnv, funEnv)(expr)
      assignToStr + " " + opeStr + " " + exprStr
    case ConstantExpr(cstexpr) => generateConstantExpression(varEnv, funEnv)(cstexpr)
  }
  
  /*def generateExpression(varEnv: VarEnv, funEnv: FunEnv)(e: Expression): String =
    e match {
      case Assign(access, expr) => 
        generateAccess(access, varEnv, funEnv) + " = " + generateExpr(varEnv, funEnv)(expr)
      
    }*/
  
  def generateConstantExpression(varEnv: VarEnv, funEnv: FunEnv)(e: ConstantExpression): String =
    e match {
      case GeneralExpr(expr) => generateGeneralExpression(varEnv, funEnv)(expr)
      case ConditionalExpression(expr1, expr2, expr3) => 
        generateGeneralExpression(varEnv, funEnv)(expr1) + " ? " + generateExpression(varEnv, funEnv)(expr2) + " : " + generateConstantExpression(varEnv, funEnv)(expr3)
  	}
  
  def generateGeneralExpression(varEnv: VarEnv, funEnv: FunEnv)(e: GeneralExpression): String = 
    e match {
      case CastExpr(castExpr) => generateCastExpression(varEnv, funEnv)(castExpr)
      case BinaryPrim(ope, expr1, expr2) =>
        generateExpression(varEnv, funEnv)(expr1) + " " + generateBinaryOp(ope) + " " + generateExpression(varEnv, funEnv)(expr2)
    }
    
  def generateCastExpression(varEnv: VarEnv, funEnv: FunEnv)(e: CastExpression): String =
    e match {
      case UnaryExpr(unaryExpr) => generateUnaryExpression(varEnv, funEnv)(unaryExpr)
      case Cast(newType, expr) => "(" + generateTypeName(varEnv, funEnv)(newType) + ") " + generateCastExpression(varEnv, funEnv)(expr)
    }
    
  def generateUnaryExpression(varEnv: VarEnv, funEnv: FunEnv)(e: UnaryExpression): String =
    e match {
      case PostfixExpr (postfixExpr) => generatePostfixExpression(varEnv, funEnv)(postfixExpr)
      case UnaryPrim (operator, expression) => generateUnaryOp(operator) + generateCastExpression(varEnv, funEnv)(expression) //Unary primitive operator
      case PrefixIncrement (expression) => "++" + generateUnaryExpression(varEnv, funEnv)(expression)
      case PrefixDecrement (expression) => "--" + generateUnaryExpression(varEnv, funEnv)(expression)
      case SizeofUnary (expression) => "sizeof " + generateUnaryExpression(varEnv, funEnv)(expression) 
      case SizeofTypeName (typeName) => "sizeof(" + generateTypeName(varEnv, funEnv)(typeName) + ")"
      
  }
  
  def generatePostfixExpression(varEnv: VarEnv, funEnv: FunEnv)(e: PostfixExpression): String =
    e match {
      case PrimaryExpr (primaryExpression) => generatePrimaryExpression(varEnv, funEnv)(primaryExpression)
      case PostfixIncrement (expression) => generatePostfixExpression(varEnv, funEnv)(expression) + "++"
      case PostfixDecrement (expression) => generatePostfixExpression(varEnv, funEnv)(expression) + "++"
      case AccessIndex (postfixExpr, expr) => 
        generatePostfixExpression(varEnv,funEnv)(postfixExpr) + "[" + generateExpression(varEnv, funEnv)(expr) + "]"
      case Call (postfixExpression, arguments) => 
        //if(!lookupFunc(funEnv, identifier))
        //  printf("Warning: Function " + identifier + " is unknown.\n\n")
        generatePostfixExpression(varEnv, funEnv)(postfixExpression) + arguments.map(generateExpression(varEnv, funEnv)).mkString("(", ", ", ")")
      case AccessMember (postfixExpr, memberToAccess) => 
        generatePostfixExpression(varEnv, funEnv)(postfixExpr) + "." + generateDirectDeclarator(varEnv, funEnv)(memberToAccess) 
      case AccessArrowMember (postfixExpr, memberToAccess) =>
        generatePostfixExpression(varEnv, funEnv)(postfixExpr) + "->" + generateDirectDeclarator(varEnv, funEnv)(memberToAccess)
  }
    
  def generatePrimaryExpression(varEnv: VarEnv, funEnv: FunEnv)(e: PrimaryExpression): String =
    e match {
      case ConstantInteger(contents) => contents.toString()
      case ConstantChar (contents) => contents.toString()
      case ConstantFloat (contents) => contents.toString()
      case ConstantEnumeration => "" //TODO find out what this is
      case CharArray (content) => content
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





