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

object CAbstractSyntax {
  //Variable environment: Set of variable identifiers
  type VarEnv = Set[String] 
  //Function environment: Set of function identifiers
  type FunEnv = Set[String]
  
  trait Program{
    val contents: List[CExternalDeclaration]
  }
  case class CProgram (contents: List[CExternalDeclaration]) extends Program
  
  //Top level declaration
  abstract class CExternalDeclaration
  case class CFunctionDec(declarationSpecifiers: Option[CDeclarationSpecifiers], declarator: CDeclarator, declarationList: Option[List[CDeclaration]], 
      compoundStmt: CCompoundStmt) extends CExternalDeclaration
  case class CGlobalDeclaration(decSpecs: CDeclarationSpecifiers, declarators: List[CInitDeclarator]) extends CExternalDeclaration
  case class CPreprocessorInstruction (controlLine: CControlLine) extends CExternalDeclaration
  
  //Preprocessor instructions (control line)
  sealed abstract class CControlLine
  case class CDefine (ident: String, definition: String) extends CControlLine
  case class CUndefine (ident: String) extends CControlLine
  case class CIncludeLocal (fileName: String) extends CControlLine
  case class CIncludeGlobal (fileName: String) extends CControlLine
  case class CLine (c: CConstantInteger, filename: Option[String]) extends CControlLine
  case class CError (tokens: Option[String]) extends CControlLine
  case class CPragma (tokens: Option[String]) extends CControlLine
  
  //Preprocessor conditionals
  case class CControlLineConditional (ifLine: CControlLineIfLine, iff: CControlLineIf, 
      elses: List[CControlLineIfElse], cElse: Option[CControlLineElse] = None) extends CControlLine
  
  case class CControlLineIf (cond: String, thenBranch: String)
  case class CControlLineIfElse (cond: String, thenBranch: String)
  case class CControlLineElse (thenBranch: String)
  
  sealed abstract class CControlLineIfLine
  case object CControlLineIfCond extends CControlLineIfLine
  case object CControlLineIfDef extends CControlLineIfLine
  case object CControlLineIfNDef extends CControlLineIfLine
  
  //Declaration specifier
  case class CDeclarationSpecifiers(decSpecs: List[CDeclarationSpecifierUnit])
  object CDeclarationSpecifiers {
    def apply(decSpecs: CDeclarationSpecifierUnit*): CDeclarationSpecifiers = this.apply(decSpecs.toList)
  }
  
  //Statements or declarations
  sealed abstract class CStmtOrDec
  sealed abstract class CStatement extends CStmtOrDec
  case class CLocalDeclaration (decSpecs: CDeclarationSpecifiers, declarators: List[CInitDeclarator]) extends CStmtOrDec

  case class CDeclaration(decSpecs: CDeclarationSpecifiers, declarators: List[CInitDeclarator])

  //Init Declarator (wrapper for Declarator allowing assignment)
  sealed abstract class CInitDeclarator
  case class CDeclaratorWrap(dec: CDeclarator) extends CInitDeclarator
  case class CDeclaratorWithAssign(dec: CDeclarator, assignment: CInitializer) extends CInitDeclarator
  
  sealed abstract class CInitializer
  case class CExpressionInitializer (expr: CExpression) extends CInitializer
  case class CScalar (initializers: List[CInitializer]) extends CInitializer //{ initializer-list }
  
  sealed abstract class CDeclarator
  case class CPointerDeclarator(pointer: CPointer, declarator: CDeclarator) extends CDeclarator
  case class CDeclareIdentifier(name: String) extends CDeclarator
  case class CParenthesiseDeclarator(declarator: CDeclarator) extends CDeclarator //(declarator)
  case class CDeclareArray(directDeclarator: CDeclarator, expr: Option[CExpression] = None) extends CDeclarator //direct-declarator [ constant-expressionopt ]
  object CDeclareArray {
    def apply(directDeclarator: CDeclarator, expr: CExpression): CDeclareArray = CDeclareArray(directDeclarator, Some(expr))
  }
  case class CParameterList(directDeclarator: CDeclarator, paramList: List[CParameterDeclaration]) extends CDeclarator //direct-declarator ( param1, param2 ) 
  case class CParameterListWithEllipsis(directDeclarator: CDeclarator, paramList: List[CParameterDeclaration]) extends CDeclarator //direct-declarator ( param1, param2, ... )
  case class CIdentifierList(directDeclarator: CDeclarator, identifierList: Option[List[String]]) extends CDeclarator //direct-declarator ( identifier-list_opt )
  
  case class CPointer(pointer: Option[CPointer] = None, typeQualifier: Option[List[CTypeQualifier]] = None) //*type-qualifier-list_opt pointer_opt
  
  sealed abstract class CParameterDeclaration
  case class CNormalDeclaration(decSpec: CDeclarationSpecifiers, declarator: CDeclarator) extends CParameterDeclaration
  case class CAbstractDeclaration(decSpec: CDeclarationSpecifiers, abstractDeclarator: Option[CAbstractDeclarator]) extends CParameterDeclaration
  
  sealed abstract class CAbstractDeclarator
  case class CAbstractPointer(pointer: CPointer) extends CAbstractDeclarator
  case class CNormalDirectAbstractDeclarator(pointer: Option[CPointer], directAbDec: CDirectAbstractDeclarator) extends CAbstractDeclarator //pointer_opt direct-abstract-declarator
  
  sealed abstract class CDirectAbstractDeclarator
  case class CParenthesiseAbDec(abstractDeclarator: CAbstractDeclarator) extends CDirectAbstractDeclarator
  case class CArrayAbDec(directAbstractDeclarator: Option[CDirectAbstractDeclarator], expr: CConstantExpression) extends CDirectAbstractDeclarator //direct-abstract-declaratoropt [constant-expression_opt]
  case class CFunctionAbDec(directAbstractDeclarator: Option[CDirectAbstractDeclarator], paramList: List[CParameterDeclaration], ellipsis: Boolean) extends CDirectAbstractDeclarator //direct-abstract-declarator_opt (parameter-type-list_opt)
  
  //Storage, type qualifier & type speficier
  sealed abstract class CDeclarationSpecifierUnit
  
  //Storage class
  sealed abstract class CStorageClassSpecifier extends CDeclarationSpecifierUnit
  case object CAuto extends CStorageClassSpecifier
  case object CRegister extends CStorageClassSpecifier
  case object CStatic extends CStorageClassSpecifier
  case object CExtern extends CStorageClassSpecifier
  case object CTypedef extends CStorageClassSpecifier

  //Type qualifier
  sealed abstract class CTypeQualifier extends CDeclarationSpecifierUnit
  case object CConst extends CTypeQualifier
  case object CVolatile extends CTypeQualifier
  
  //C statements
  case class CExpressionStmt(expr: Option[CExpression]) extends CStatement
  object CExpressionStmt {
    def apply(): CExpressionStmt = CExpressionStmt(None)
    def apply(expr: CExpression): CExpressionStmt = CExpressionStmt(Some(expr))
  }
  case class CCompoundStmt(stmtOrDecList: List[CStmtOrDec]) extends CStatement //{ declaration-list_opt statement-list_opt }
  /*object CCompoundStmt {
    def apply(stmts: CStmtOrDec*): CCompoundStmt = CCompoundStmt(stmts.toList)
  }*/
  
  sealed abstract class CLabeledStatement extends CStatement
  case class CLabelStmt(ident: String, stmt: CStatement) extends CLabeledStatement //ident : stmt
  case class CCaseStmt(expr: CConstantExpression, stmt: CStatement) extends CLabeledStatement // case expr : stmt
  case class CDefaultCaseStmt(stmt: CStatement) extends CLabeledStatement // default : stmt
  
  sealed abstract class CSelectionStatement extends CStatement
  case class CIf (condition: CExpression, stmt: CStatement) extends CSelectionStatement
  case class CIfElse(condition: CExpression, trueBranch: CStatement, elseBranch: CStatement) extends CSelectionStatement
  case class CSwitch (switchExpr: CExpression, stmt: CStatement) extends CSelectionStatement //switch(expression) statement
  
  sealed abstract class CIterationStatement extends CStatement
  case class CWhile (condition: CExpression, contents: CStatement) extends CIterationStatement
  case class CFor (initialization: Option[CExpression], condition: Option[CExpression], counter: Option[CExpression], contents: CStatement) extends CIterationStatement
  case class CDoWhile (contents: CStatement, condition: CExpression) extends CIterationStatement
  
  sealed abstract class CJumpStatement extends CStatement
  case class CGoto(identifier: String) extends CJumpStatement
  case object CContinue extends CJumpStatement
  case object CBreak extends CJumpStatement
  case class CReturn (returnExpression: Option[CExpression] = None) extends CJumpStatement
  object CReturn {
    def apply(expr: CExpression): CReturn = CReturn(Some(expr))
  }
  
  //C types
  sealed abstract class CTypeSpecifier extends CDeclarationSpecifierUnit
  case object CTypeVoid extends CTypeSpecifier
  case object CTypeChar extends CTypeSpecifier
  case object CTypeShort extends CTypeSpecifier
  case object CTypeInteger extends CTypeSpecifier
  case object CTypeLong extends CTypeSpecifier
  case object CTypeFloat extends CTypeSpecifier
  case object CTypeDouble extends CTypeSpecifier
  case object CTypeSigned extends CTypeSpecifier
  case object CTypeUnsigned extends CTypeSpecifier
  case class  CTypeStruct(ident: Option[String] = None) extends CTypeSpecifier
  object CTypeStruct {
    def apply(s: String): CTypeStruct = CTypeStruct(Some(s))
  }
  case class CTypeUnion(ident: Option[String] = None) extends CTypeSpecifier
  object CTypeUnion {
    def apply(s: String): CTypeUnion = CTypeUnion(Some(s))
  }
  case class CTypeEnum(ident: Option[String] = None) extends CTypeSpecifier
  object CTypeEnum {
    def apply(s: String): CTypeEnum = CTypeEnum(Some(s))
  }
  
  // Union & enums
  case class CStructUnionDeclaration(typeQualifier: CTypeQualifier, typeSpecifier: CTypeSpecifier, declarator: List[CDeclarator]) //const int foo = 2, bar;
  
  abstract class CEnumeration
  case class CEnumerationWithAssign(ident: Option[String], assignment: List[CEnumerator]) extends CEnumeration //enum ident { foo = 2, bar = 4 };
 
  abstract class CEnumerator
  case class CEnumeratorWithAssign(assigns: List[CAssign]) //enum ident { foo = 2, bar = 4 }
  case class CEnumeratorWithoutAssign(identifiers: List[String]) //enum ident { foo, bar }
  
  //C Unary operators
  abstract class CUnaryOp 
  case object CAddress extends CUnaryOp //&
  case object CDeref extends CUnaryOp //*
  case object CPositive extends CUnaryOp //+
  case object CNegative extends CUnaryOp //-
  case object COnesCompliment extends CUnaryOp //~ 
  case object CNegation extends CUnaryOp //!
  
  //C Binary Operators
  abstract class CBinaryOp
  case object CBinaryPlus extends CBinaryOp
  case object CBinaryMinus extends CBinaryOp
  case object CBinaryTimes extends CBinaryOp
  case object CBinaryDivide extends CBinaryOp
  case object CBinaryModulo extends CBinaryOp
  case object CBinaryEquality extends CBinaryOp
  case object CBinaryLessThan extends CBinaryOp
  case object CBinaryLessThanOrEquals extends CBinaryOp
  case object CBinaryGreaterThan extends CBinaryOp
  case object CBinaryGreaterThanOrEquals extends CBinaryOp
  case object CBinaryBitwiseOr extends CBinaryOp
  case object CBinaryBitwiseAnd extends CBinaryOp
  case object CBinaryBitwiseXOR extends CBinaryOp
  case object CBinaryLogicalAnd extends CBinaryOp
  case object CBinaryLogicalOr extends CBinaryOp
  case object CBinaryShiftRight extends CBinaryOp
  case object CBinaryShiftLeft extends CBinaryOp
  
  //C AssignmentOperators
  abstract class CAssignmentOperator
  case object CEquals extends CAssignmentOperator // =
  case object CTimesEquals extends CAssignmentOperator // *=
  case object CDivisionEquals extends CAssignmentOperator // /=
  case object CModuloEquals extends CAssignmentOperator // %=
  case object CPlusEquals extends CAssignmentOperator // +=
  case object CMinusEquals extends CAssignmentOperator // -=
  case object CShiftLeftEquals extends CAssignmentOperator // <<=
  case object CShiftRightEquals extends CAssignmentOperator // >>=
  case object CBitwiseAndEquals extends CAssignmentOperator // &=
  case object CBitwiseOrEquals extends CAssignmentOperator // |=
  case object CBitwiseXOREquals extends CAssignmentOperator // ^=
    
  //TypeName
  case class CTypeName(qualifierSpecifierList: CTypeSpecifierQualifier, abstractDeclarator: Option[CAbstractDeclarator])
  case class CTypeSpecifierQualifier(typeSpecifier: CTypeSpecifier, typeQualifier: Option[CTypeQualifier])
  
  //C Expressions
  abstract class CExpression
  case class CAssign (assignTo: CUnaryExpression, operator: CAssignmentOperator, expr: CExpression) extends CExpression  //x=e  or  *p=e  or  a[e]=e 
  
  abstract class CConstantExpression extends CExpression
  case class CConditionalExpression (expr1: CGeneralExpression, expr2: CExpression, expr3: CConstantExpression) extends CConstantExpression //e1 ? e2 : e3
  
  abstract class CGeneralExpression extends CConstantExpression
  case class CBinaryPrim (operator: CBinaryOp, expression1: CExpression, expression2: CExpression) extends CGeneralExpression //Binary primitive operator
  
  abstract class CCastExpression extends CGeneralExpression
  case class CCast(newType: CTypeName, expression: CCastExpression) extends CCastExpression //(newType) expression;
  
  abstract class CUnaryExpression extends CCastExpression
  case class CUnaryPrim (operator: CUnaryOp, expression: CCastExpression) extends CUnaryExpression //Unary primitive operator
  case class CPrefixIncrement (expression: CUnaryExpression) extends CUnaryExpression
  case class CPrefixDecrement (expression: CUnaryExpression) extends CUnaryExpression
  case class CSizeofUnary (expression: CUnaryExpression) extends CUnaryExpression
  case class CSizeofTypeName (typeName: CTypeName) extends CUnaryExpression
  
  abstract class CPostfixExpression extends CUnaryExpression
  case class CPostfixIncrement (expression: CPostfixExpression) extends CPostfixExpression
  case class CPostfixDecrement (expression: CPostfixExpression) extends CPostfixExpression
  case class CAccessIndex (postfixExpr: CPostfixExpression, expr: CExpression) extends CPostfixExpression //postfix-expression[expression]
  case class CCall (postfixExpression: CPostfixExpression, arguments: List[CExpression]) extends CPostfixExpression //postfix-expression(argument-expression-list_opt)
  case class CAccessMember (postfixExpr: CPostfixExpression, memberToAccess: CDeclareIdentifier) extends CPostfixExpression //postfix-expression.identifier
  case class CAccessArrowMember (postfixExpr: CPostfixExpression, memberToAccess: CDeclareIdentifier) extends CPostfixExpression // postfix-expression->identifier
  
  abstract class CPrimaryExpression extends CPostfixExpression
  case class CAccessIdentifier(name: String) extends CPrimaryExpression
  case class CConstantInteger (contents: Int) extends CPrimaryExpression
  case class CConstantChar (contents: Char) extends CPrimaryExpression
  case class CConstantFloat (contents: Float) extends CPrimaryExpression
  case class CConstantEnumeration (name: String) extends CPrimaryExpression
  case class CCharArray (content: String) extends CPrimaryExpression
  case class CParenthesiseExpr (expression: CExpression) extends CPrimaryExpression
}
