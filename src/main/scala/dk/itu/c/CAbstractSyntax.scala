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

trait CAbstractSyntax {
  
  //Variable environment: Map from identifier to variable type
  type VarEnv = Map[String, CTypeSpecifier] 
  //Function environment: Map from identifier to tuple of return type and argument list
  type FunEnv = Map[String, List[CDeclaration]]
  
  case class Program (contents: List[CExternalDeclaration])
  
  //Top level declaration
  abstract class CExternalDeclaration
  case class CFunctionDec(declarationSpecifiers: Option[CDeclarationSpecifiers], declarator: CDeclarator, declarationList: Option[List[CDeclaration]], 
  compoundStmt: CompoundStmt) extends CExternalDeclaration
  case class GlobalDeclaration(decSpecs: CDeclarationSpecifiers, declarators: List[CInitDeclarator]) extends CExternalDeclaration
  case class PreprocessorInstruction (controlLine: CControlLine) extends CExternalDeclaration
  
  sealed abstract class CControlLine //TODO implement the rest
  case class IncludeLocal (fileName: String) extends CControlLine
  case class IncludeGlobal (fileName: String) extends CControlLine
  
  //Declaration specifier
  case class CDeclarationSpecifiers(storage: Option[CStorageClassSpecifier], typeSpec: CTypeSpecifier, qualifier: Option[CTypeQualifier])

  //Any declaration
  case class CDeclaration(decSpecs: CDeclarationSpecifiers, declarators: List[CInitDeclarator])

  //Init Declarator (wrapper for Declarator allowing assignment)
  sealed abstract class CInitDeclarator
  case class DeclaratorWrap(dec: CDeclarator) extends CInitDeclarator
  case class DeclaratorWithAssign(dec: CDeclarator, assignment: CInitializer) extends CInitDeclarator
  
  sealed abstract class CInitializer
  case class ExpressionInitializer (expr: CExpression) extends CInitializer
  case class Scalar (initializers: List[CInitializer]) extends CInitializer //{ initializer-list }
  
  case class CDeclarator(pointer: Option[CPointer], directDeclarator: CDirectDeclarator)
  
  sealed abstract class CDirectDeclarator
  case class DeclareIdentifier(name: String) extends CDirectDeclarator
  case class ParenthesiseDeclarator(declarator: CDeclarator) extends CDirectDeclarator //(declarator)
  case class DeclareArray(directDeclarator: CDirectDeclarator, expr: Option[CExpression]) extends CDirectDeclarator //direct-declarator [ constant-expressionopt ]
  case class ParameterList(directDeclarator: CDirectDeclarator, paramList: List[CParameterDeclaration], ellipsis: Boolean) extends CDirectDeclarator //direct-declarator ( param1, param2, ... ) 
  case class IdentifierList(directDeclarator: CDirectDeclarator, identifierList: Option[List[String]]) extends CDirectDeclarator //direct-declarator ( identifier-list_opt )
  
  case class CPointer(pointer: Option[CPointer], typeQualifier: Option[List[CTypeQualifier]]) //*type-qualifier-list_opt pointer_opt
  
  sealed abstract class CParameterDeclaration
  case class NormalDeclaration(decSpec: CDeclarationSpecifiers, declarator: CDeclarator) extends CParameterDeclaration
  case class AbstractDeclaration(decSpec: CDeclarationSpecifiers, abstractDeclarator: Option[CAbstractDeclarator]) extends CParameterDeclaration
  
  sealed abstract class CAbstractDeclarator
  case class AbstractPointer(pointer: CPointer) extends CAbstractDeclarator
  case class NormalDirectAbstractDeclarator(pointer: Option[CPointer], directAbDec: CDirectAbstractDeclarator) extends CAbstractDeclarator //pointer_opt direct-abstract-declarator
  
  sealed abstract class CDirectAbstractDeclarator
  case class ParenthesiseAbDec(abstractDeclarator: CAbstractDeclarator) extends CDirectAbstractDeclarator
  case class ArrayAbDec(directAbstractDeclarator: Option[CDirectAbstractDeclarator], expr: CConstantExpression) extends CDirectAbstractDeclarator //direct-abstract-declaratoropt [constant-expression_opt]
  case class FunctionAbDec(directAbstractDeclarator: Option[CDirectAbstractDeclarator], paramList: List[CParameterDeclaration], ellipsis: Boolean) extends CDirectAbstractDeclarator //direct-abstract-declarator_opt (parameter-type-list_opt)
  
  //Storage class
  sealed abstract class CStorageClassSpecifier
  case object Auto extends CStorageClassSpecifier
  case object Register extends CStorageClassSpecifier
  case object Static extends CStorageClassSpecifier
  case object Extern extends CStorageClassSpecifier
  case object Typedef extends CStorageClassSpecifier

  //Type qualifier
  sealed abstract class CTypeQualifier
  case object Const extends CTypeQualifier
  case object Volatile extends CTypeQualifier
  
  //C statements
  sealed abstract class CStatement
  case class ExpressionStmt(expr: Option[CExpression]) extends CStatement
  case class CompoundStmt(stmtOrDecList: List[CStmtOrDec]) extends CStatement //{ declaration-list_opt statement-list_opt }
  
  sealed abstract class CLabeledStatement extends CStatement
  case class LabelStmt(ident: String, stmt: CStatement) extends CLabeledStatement //ident : stmt
  case class CaseStmt(expr: CConstantExpression, stmt: CStatement) extends CLabeledStatement // case expr : stmt
  case class DefaultCaseStmt(stmt: CStatement) extends CLabeledStatement // default : stmt
  
  sealed abstract class CSelectionStatement extends CStatement
  case class If (condition: CExpression, stmt: CStatement) extends CSelectionStatement
  case class IfElse(condition: CExpression, trueBranch: CStatement, elseBranch: CStatement) extends CSelectionStatement
  case class Switch (switchExpr: CExpression, stmt: CStatement) extends CSelectionStatement //switch(expression) statement
  
  sealed abstract class CIterationStatement extends CStatement
  case class While (condition: CExpression, contents: CStatement) extends CIterationStatement
  case class For (initialization: Option[CExpression], condition: Option[CExpression], counter: Option[CExpression], contents: CStatement) extends CIterationStatement
  case class DoWhile (contents: CStatement, condition: CExpression) extends CIterationStatement
  
  sealed abstract class CJumpStatement extends CStatement
  case class Goto(identifier: String) extends CJumpStatement
  case object Continue extends CJumpStatement
  case object Break extends CJumpStatement
  case class Return (returnExpression: Option[CExpression]) extends CJumpStatement
  
  //Statements or declarations
  sealed abstract class CStmtOrDec
  case class Stmt (statement: CStatement) extends CStmtOrDec
  case class LocalDeclaration (decSpecs: CDeclarationSpecifiers, declarators: List[CInitDeclarator]) extends CStmtOrDec
  
  //C types
  sealed abstract class CTypeSpecifier
  case object TypeVoid extends CTypeSpecifier
  case object TypeChar extends CTypeSpecifier
  case object TypeShort extends CTypeSpecifier
  case object TypeInteger extends CTypeSpecifier
  case object TypeLong extends CTypeSpecifier
  case object TypeFloat extends CTypeSpecifier
  case object TypeDouble extends CTypeSpecifier
  case object TypeSigned extends CTypeSpecifier
  case object TypeUnsigned extends CTypeSpecifier
  case class  TypeStruct(ident: Option[String], structDeclarations: List[CStructUnionDeclaration]) extends CTypeSpecifier
  case class  TypeStructShort(ident: String, structDeclarations: Option[List[CStructUnionDeclaration]]) extends CTypeSpecifier
  case class  TypeUnion(ident: Option[String], structDeclarations: List[CStructUnionDeclaration]) extends CTypeSpecifier
  case class  TypeUnionShort(ident: String, structDeclarations: Option[List[CStructUnionDeclaration]]) extends CTypeSpecifier
  case class  TypeEnum(ident: Option[String], enumerations: List[CEnumerationDec]) extends CTypeSpecifier
  case class  TypeEnumShort(ident: String, enumerations: Option[List[CEnumerationDec]]) extends CTypeSpecifier
  
  case class CStructUnionDeclaration(typeQualifier: CTypeQualifier, typeSpecifier: CTypeSpecifier, declarator: List[CDeclarator]) //const int foo = 2, bar;
  
  case class CEnumerationDec(ident: String, assignment: Option[CExpression]) //enum ident { foo = 2, bar = 4 };
  
  //C Unary operators
  abstract class CUnaryOp 
  case object Address extends CUnaryOp //&
  case object Deref extends CUnaryOp //*
  case object Positive extends CUnaryOp //+
  case object Negative extends CUnaryOp //-
  case object OnesCompliment extends CUnaryOp //~ 
  case object Negation extends CUnaryOp //!
  
  //C Binary Operators
  abstract class CBinaryOp
  case object BinaryPlus extends CBinaryOp
  case object BinaryMinus extends CBinaryOp
  case object BinaryTimes extends CBinaryOp
  case object BinaryDivide extends CBinaryOp
  case object BinaryModulo extends CBinaryOp
  case object BinaryEquality extends CBinaryOp
  case object BinaryLessThan extends CBinaryOp
  case object BinaryLessThanOrEquals extends CBinaryOp
  case object BinaryGreaterThan extends CBinaryOp
  case object BinaryGreaterThanOrEquals extends CBinaryOp
  case object BinaryBitwiseOr extends CBinaryOp
  case object BinaryBitwiseAnd extends CBinaryOp
  case object BinaryBitwiseXOR extends CBinaryOp
  case object BinaryLogicalAnd extends CBinaryOp
  case object BinaryLogicalOr extends CBinaryOp
  case object BinaryShiftRight extends CBinaryOp
  case object BinaryShiftLeft extends CBinaryOp
  
  //C AssignmentOperators
  abstract class CAssignmentOperator
  case object Equals extends CAssignmentOperator // =
  case object TimesEquals extends CAssignmentOperator // *=
  case object DivisionEquals extends CAssignmentOperator // /=
  case object ModuloEquals extends CAssignmentOperator // %=
  case object PlusEquals extends CAssignmentOperator // +=
  case object MinusEquals extends CAssignmentOperator // -=
  case object ShiftLeftEquals extends CAssignmentOperator // <<=
  case object ShiftRightEquals extends CAssignmentOperator // >>=
  case object BitwiseAndEquals extends CAssignmentOperator // &=
  case object BitwiseOrEquals extends CAssignmentOperator // |=
  case object BitwiseXOREquals extends CAssignmentOperator // ^=
    
  //TypeName
  case class CTypeName(qualifierSpecifierList: CTypeSpecifierQualifier, abstractDeclarator: Option[CAbstractDeclarator])
  case class CTypeSpecifierQualifier(typeSpecifier: CTypeSpecifier, typeQualifier: Option[CTypeQualifier])
  
  //C Expressions
  abstract class CExpression
  case class Assign (assignTo: CUnaryExpression, operator: CAssignmentOperator, expr: CExpression) extends CExpression  //x=e  or  *p=e  or  a[e]=e 
  
  abstract class CConstantExpression extends CExpression
  case class ConditionalExpression (expr1: CGeneralExpression, expr2: CExpression, expr3: CConstantExpression) extends CConstantExpression //e1 ? e2 : e3
  
  abstract class CGeneralExpression extends CConstantExpression
  case class BinaryPrim (operator: CBinaryOp, expression1: CExpression, expression2: CExpression) extends CGeneralExpression //Binary primitive operator
  
  abstract class CCastExpression extends CConstantExpression
  case class Cast(newType: CTypeName, expression: CCastExpression) extends CCastExpression //(newType) expression;
  
  abstract class CUnaryExpression extends CCastExpression
  case class UnaryPrim (operator: CUnaryOp, expression: CCastExpression) extends CUnaryExpression //Unary primitive operator
  case class PrefixIncrement (expression: CUnaryExpression) extends CUnaryExpression
  case class PrefixDecrement (expression: CUnaryExpression) extends CUnaryExpression
  case class SizeofUnary (expression: CUnaryExpression) extends CUnaryExpression
  case class SizeofTypeName (typeName: CTypeName) extends CUnaryExpression
  
  abstract class CPostfixExpression extends CUnaryExpression
  case class PostfixIncrement (expression: CPostfixExpression) extends CPostfixExpression
  case class PostfixDecrement (expression: CPostfixExpression) extends CPostfixExpression
  case class AccessIndex (postfixExpr: CPostfixExpression, expr: CExpression) extends CPostfixExpression //postfix-expression[expression]
  case class Call (postfixExpression: CPostfixExpression, arguments: List[CExpression]) extends CPostfixExpression //postfix-expression(argument-expression-list_opt)
  case class AccessMember (postfixExpr: CPostfixExpression, memberToAccess: DeclareIdentifier) extends CPostfixExpression //postfix-expression.identifier
  case class AccessArrowMember (postfixExpr: CPostfixExpression, memberToAccess: DeclareIdentifier) extends CPostfixExpression // postfix-expression->identifier
  
  abstract class CPrimaryExpression extends CPostfixExpression
  case class AccessIdentifier(name: String) extends CPrimaryExpression
  case class ConstantInteger (contents: Integer) extends CPrimaryExpression
  case class ConstantChar (contents: Character) extends CPrimaryExpression
  case class ConstantFloat (contents: Float) extends CPrimaryExpression
  case object ConstantEnumeration extends CPrimaryExpression //TODO find out what this is
  case class CharArray (content: String) extends CPrimaryExpression
  case class ParenthesiseExpr (expression: CExpression) extends CPrimaryExpression
  
}







