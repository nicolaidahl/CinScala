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
import com.sun.source.tree.LabeledStatementTree

trait CAbstractSyntax {
  
  //Variable environment: Map from identifier to variable type
  type VarEnv = Map[String, CTypeSpecifier] 
  //Function environment: Map from identifier to tuple of return type and argument list
  type FunEnv = Map[String, List[Declaration]]
  
  case class Program (contents: List[ExternalDeclaration])
  
  //Top level declaration
  abstract class ExternalDeclaration
  case class CFunctionDec(declarationSpecifiers: Option[DeclarationSpecifiers], declarator: Declarator, declarationList: Option[List[Declaration]], 
  compoundStmt: CompoundStmt) extends ExternalDeclaration
  case class GlobalDeclaration(decSpecs: DeclarationSpecifiers, declarators: List[InitDeclarator]) extends ExternalDeclaration
  case class PreprocessorInstruction (controlLine: ControlLine) extends ExternalDeclaration
  
  sealed abstract class ControlLine //TODO implement the rest
  case class IncludeLocal (fileName: String) extends ControlLine
  case class IncludeGlobal (fileName: String) extends ControlLine
  
  //Declaration specifier
  case class DeclarationSpecifiers(storage: Option[StorageClassSpecifier], typeSpec: CTypeSpecifier, qualifier: Option[TypeQualifier])

  //Any declaration
  case class Declaration(decSpecs: DeclarationSpecifiers, declarators: List[InitDeclarator])

  //Init Declarator (wrapper for Declarator allowing assignment)
  sealed abstract class InitDeclarator
  case class DeclaratorWrap(dec: Declarator) extends InitDeclarator
  case class DeclaratorWithAssign(dec: Declarator, assignment: Initializer) extends InitDeclarator
  
  sealed abstract class Initializer
  case class ExpressionInitializer (expr: CExpression) extends Initializer
  case class Scalar (initializers: List[Initializer]) extends Initializer //{ initializer-list }
  
  case class Declarator(pointer: Option[Pointer], directDeclarator: DirectDeclarator)
  
  sealed abstract class DirectDeclarator
  case class DeclareIdentifier(name: String) extends DirectDeclarator
  case class ParenthesiseDeclarator(declarator: Declarator) extends DirectDeclarator //(declarator)
  case class DeclareArray(directDeclarator: DirectDeclarator, expr: Option[CExpression]) extends DirectDeclarator //direct-declarator [ constant-expressionopt ]
  case class ParameterList(directDeclarator: DirectDeclarator, paramList: List[ParameterDeclaration], ellipsis: Boolean) extends DirectDeclarator //direct-declarator ( param1, param2, ... ) 
  case class IdentifierList(directDeclarator: DirectDeclarator, identifierList: Option[List[String]]) extends DirectDeclarator //direct-declarator ( identifier-list_opt )
  
  case class Pointer(pointer: Option[Pointer], typeQualifier: Option[List[TypeQualifier]]) //*type-qualifier-list_opt pointer_opt
  
  sealed abstract class ParameterDeclaration
  case class NormalDeclaration(decSpec: DeclarationSpecifiers, declarator: Declarator) extends ParameterDeclaration
  case class AbstractDeclaration(decSpec: DeclarationSpecifiers, abstractDeclarator: Option[AbstractDeclarator]) extends ParameterDeclaration
  
  sealed abstract class AbstractDeclarator
  case class AbstractPointer(pointer: Pointer) extends AbstractDeclarator
  case class NormalDirectAbstractDeclarator(pointer: Option[Pointer], directAbDec: DirectAbstractDeclarator) extends AbstractDeclarator //pointer_opt direct-abstract-declarator
  
  sealed abstract class DirectAbstractDeclarator
  case class ParenthesiseAbDec(abstractDeclarator: AbstractDeclarator) extends DirectAbstractDeclarator
  case class ArrayAbDec(directAbstractDeclarator: Option[DirectAbstractDeclarator], expr: CConstantExpression) extends DirectAbstractDeclarator //direct-abstract-declaratoropt [constant-expression_opt]
  case class FunctionAbDec(directAbstractDeclarator: Option[DirectAbstractDeclarator], paramList: List[ParameterDeclaration], ellipsis: Boolean) extends DirectAbstractDeclarator //direct-abstract-declarator_opt (parameter-type-list_opt)
  
  //Storage class
  sealed abstract class StorageClassSpecifier
  case object Auto extends StorageClassSpecifier
  case object Register extends StorageClassSpecifier
  case object Static extends StorageClassSpecifier
  case object Extern extends StorageClassSpecifier
  case object Typedef extends StorageClassSpecifier

  //Type qualifier
  sealed abstract class TypeQualifier
  case object Const extends TypeQualifier
  case object Volatile extends TypeQualifier
  
  //C statements
  sealed abstract class Statement
  case class LabeledStmt(labeledStmt: LabeledStatement) extends Statement
  case class ExpressionStmt(expr: Option[CExpression]) extends Statement
  case class CompoundStmt(stmtOrDecList: List[StmtOrDec]) extends Statement //{ declaration-list_opt statement-list_opt }
  case class SelectionStmt(selectionStmt: SelectionStatement) extends Statement
  case class IterationStmt(iterationStatement: IterationStatement) extends Statement
  case class JumpStmt(jumpStatement: JumpStatement) extends Statement

  sealed abstract class LabeledStatement
  case class LabelStmt(ident: String, stmt: Statement) extends LabeledStatement //ident : stmt
  case class CaseStmt(expr: CConstantExpression, stmt: Statement) extends LabeledStatement // case expr : stmt
  case class DefaultCaseStmt(stmt: Statement) extends LabeledStatement // default : stmt
  
  sealed abstract class SelectionStatement
  case class If (condition: CExpression, stmt: Statement) extends SelectionStatement
  case class IfElse(condition: CExpression, trueBranch: Statement, elseBranch: Statement) extends SelectionStatement
  case class Switch (switchExpr: CExpression, stmt: Statement) extends SelectionStatement //switch(expression) statement
  
  sealed abstract class IterationStatement
  case class While (condition: CExpression, contents: Statement) extends IterationStatement
  case class For (initialization: Option[CExpression], condition: Option[CExpression], counter: Option[CExpression], contents: Statement) extends IterationStatement
  case class DoWhile (contents: Statement, condition: CExpression) extends IterationStatement
  
  sealed abstract class JumpStatement
  case class Goto(identifier: String) extends JumpStatement
  case object Continue extends JumpStatement
  case object Break extends JumpStatement
  case class Return (returnExpression: Option[CExpression]) extends JumpStatement
  
  //Statements or declarations
  sealed abstract class StmtOrDec
  case class Stmt (statement: Statement) extends StmtOrDec
  case class Dec (declaration: Declaration) extends StmtOrDec
  
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
  case class  TypeStruct(ident: Option[String], structDeclarations: List[StructUnionDeclaration]) extends CTypeSpecifier
  case class  TypeStructShort(ident: String, structDeclarations: Option[List[StructUnionDeclaration]]) extends CTypeSpecifier
  case class  TypeUnion(ident: Option[String], structDeclarations: List[StructUnionDeclaration]) extends CTypeSpecifier
  case class  TypeUnionShort(ident: String, structDeclarations: Option[List[StructUnionDeclaration]]) extends CTypeSpecifier
  case class  TypeEnum(ident: Option[String], enumerations: List[EnumerationDec]) extends CTypeSpecifier
  case class  TypeEnumShort(ident: String, enumerations: Option[List[EnumerationDec]]) extends CTypeSpecifier
  
  
  case class StructUnionDeclaration(typeQualifier: TypeQualifier, typeSpecifier: CTypeSpecifier, declarator: List[Declarator]) //const int foo = 2, bar;
  
  case class EnumerationDec(ident: String, assignment: Option[CExpression]) //enum ident { foo = 2, bar = 4 };
  
  
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
  case class CTypeName(qualifierSpecifierList: CTypeSpecifierQualifier, abstractDeclarator: Option[AbstractDeclarator])
  case class CTypeSpecifierQualifier(typeSpecifier: CTypeSpecifier, typeQualifier: TypeQualifier)
  
  //C Expressions
  abstract class CExpression
  case class ConstantExpr (constantExpr: CConstantExpression) extends CExpression
  case class Assign (assignTo: CUnaryExpression, operator: CAssignmentOperator, expr: CExpression) extends CExpression  //x=e  or  *p=e  or  a[e]=e 
  
  abstract class CConstantExpression
  case class GeneralExpr(otherExpression: CGeneralExpression) extends CConstantExpression
  case class ConditionalExpression (expr1: CGeneralExpression, expr2: CExpression, expr3: CConstantExpression) extends CConstantExpression //e1 ? e2 : e3
  
  abstract class CGeneralExpression
  case class CastExpr (castExpression: CCastExpression) extends CGeneralExpression
  case class BinaryPrim (operator: CBinaryOp, expression1: CExpression, expression2: CExpression) extends CGeneralExpression //Binary primitive operator
  
  abstract class CCastExpression
  case class UnaryExpr(unaryExpr: CUnaryExpression) extends CCastExpression
  case class Cast(newType: CTypeName, expression: CCastExpression) extends CCastExpression //(newType) expression;
  
  abstract class CUnaryExpression
  case class UnaryPrim (operator: CUnaryOp, expression: CCastExpression) extends CUnaryExpression //Unary primitive operator
  case class PrefixIncrement (expression: CUnaryExpression) extends CUnaryExpression
  case class PrefixDecrement (expression: CUnaryExpression) extends CUnaryExpression
  case class SizeofUnary (expression: CUnaryExpression) extends CUnaryExpression
  case class SizeofTypeName (typeName: CTypeName) extends CUnaryExpression
  case class PostfixExpr (postfixExpr: CPostfixExpression) extends CUnaryExpression
  
  abstract class CPostfixExpression
  case class PrimaryExpr (primaryExpression: CPrimaryExpression) extends CPostfixExpression
  case class PostfixIncrement (expression: CPostfixExpression) extends CPostfixExpression
  case class PostfixDecrement (expression: CPostfixExpression) extends CPostfixExpression
  case class AccessIndex (postfixExpr: CPostfixExpression, expr: CExpression) extends CPostfixExpression //postfix-expression[expression]
  case class Call (postfixExpression: CPostfixExpression, arguments: List[CExpression]) extends CPostfixExpression //postfix-expression(argument-expression-list_opt)
  case class AccessMember (postfixExpr: CPostfixExpression, memberToAccess: DeclareIdentifier) extends CPostfixExpression //postfix-expression.identifier
  case class AccessArrowMember (postfixExpr: CPostfixExpression, memberToAccess: DeclareIdentifier) extends CPostfixExpression // postfix-expression->identifier
  
  abstract class CPrimaryExpression
  case class AccessIdentifier(name: String) extends CPrimaryExpression
  case class ConstantInteger (contents: Integer) extends CPrimaryExpression
  case class ConstantChar (contents: Character) extends CPrimaryExpression
  case class ConstantFloat (contents: Float) extends CPrimaryExpression
  case object ConstantEnumeration extends CPrimaryExpression //TODO find out what this is
  case class CharArray (content: String) extends CPrimaryExpression
  case class ParenthesiseExpr (expression: CExpression) extends CPrimaryExpression
  
}







