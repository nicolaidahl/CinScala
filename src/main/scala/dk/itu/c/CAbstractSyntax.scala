package dk.itu.c
import com.sun.source.tree.LabeledStatementTree

trait CAbstractSyntax {
  
  //Variable environment: Map from identifier to variable type
  type VarEnv = Map[String, TypeSpecifier] 
  //Function environment: Map from identifier to tuple of return type and argument list
  type FunEnv = Map[String, List[Declaration]]
  
  case class Program (contents: List[ExternalDeclaration])
  
  //Top level declaration
  abstract class ExternalDeclaration
  trait FunctionDec extends ExternalDeclaration {
    val declarationSpecifiers: Option[DeclarationSpecifiers]
    val declarator: Declarator
    val declarationList: Option[List[Declaration]]
    val compoundStmt: CompoundStmt
  }
  case class CFunctionDec(declarationSpecifiers: Option[DeclarationSpecifiers], declarator: Declarator, declarationList: Option[List[Declaration]], 
  compoundStmt: CompoundStmt) extends FunctionDec
  case class GlobalDeclaration(decSpecs: DeclarationSpecifiers, declarators: List[InitDeclarator]) extends ExternalDeclaration
  case class PreprocessorInst (controlLine: ControlLine) extends ExternalDeclaration
  
  sealed abstract class ControlLine //TODO implement the rest
  case class IncludeLocal (fileName: String) extends ControlLine
  case class IncludeGlobal (fileName: String) extends ControlLine
  
  //Declaration specifier
  case class DeclarationSpecifiers(storage: Option[StorageClassSpecifier], typeSpec: TypeSpecifier, qualifier: Option[TypeQualifier])

  //Any declaration
  case class Declaration(decSpecs: DeclarationSpecifiers, declarators: List[InitDeclarator])

  //Init Declarator (wrapper for Declarator allowing assignment)
  sealed abstract class InitDeclarator
  case class DeclaratorWrap(dec: Declarator) extends InitDeclarator
  case class DeclaratorWithAssign(dec: Declarator, assignment: Initializer) extends InitDeclarator
  
  sealed abstract class Initializer
  case class ExpressionInitializer (expr: Expression) extends Initializer
  case class Scalar (initializers: List[Initializer]) extends Initializer //{ initializer-list }
  
  case class Declarator(pointer: Option[Pointer], directDeclarator: DirectDeclarator)
  
  sealed abstract class DirectDeclarator
  case class DeclareIdentifier(name: String) extends DirectDeclarator
  case class ParenthesiseDeclarator(declarator: Declarator) extends DirectDeclarator //(declarator)
  case class DeclareArray(directDeclarator: DirectDeclarator, expr: Option[Expression]) extends DirectDeclarator //direct-declarator [ constant-expressionopt ]
  case class ParameterList(directDeclarator: DirectDeclarator, paramList: List[ParameterDeclaration], ellipsis: Boolean) //direct-declarator ( param1, param2, ... ) 
  case class IdentifierList(directDeclarator: DirectDeclarator, identifierList: Option[List[String]])//direct-declarator ( identifier-list_opt )
  
  case class Pointer(pointer: Option[Pointer], typeQualifier: Option[TypeQualifier]) //*type-qualifier-list_opt pointer_opt
  
  sealed abstract class ParameterDeclaration
  case class NormalDeclaration(decSpec: DeclarationSpecifiers, declarator: Declarator)
  case class AbstractDeclaration(decSpec: DeclarationSpecifiers, abstractDeclarator: Option[AbstractDeclarator])
  
  sealed abstract class AbstractDeclarator
  case class AbstractPointer(pointer: Pointer) extends AbstractDeclarator
  case class NormalDirectAbstractDeclarator(pointer: Option[Pointer], directAbDec: DirectAbstractDeclarator) extends AbstractDeclarator //pointer_opt direct-abstract-declarator
  
  sealed abstract class DirectAbstractDeclarator
  case class ParenthesiseAbDec(abstractDeclarator: AbstractDeclarator) extends DirectAbstractDeclarator
  case class ArrayAbDec(directAbstractDeclarator: Option[DirectAbstractDeclarator], expr: ConstantExpression) extends DirectAbstractDeclarator //direct-abstract-declaratoropt [constant-expression_opt]
  case class FunctionAbDec(directAbstractDeclarator: Option[DirectAbstractDeclarator], paramList: List[ParameterDeclaration], ellipsis: Boolean) //direct-abstract-declarator_opt (parameter-type-list_opt)
  
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
  case class ExpressionStmt(expr: Option[Expression]) extends Statement
  case class CompoundStmt(stmtOrDecList: List[StmtOrDec]) extends Statement //{ declaration-list_opt statement-list_opt }
  case class SelectionStmt(selectionStmt: SelectionStatement) extends Statement
  case class IterationStmt(iterationStatement: IterationStatement) extends Statement
  case class JumpStmt(jumpStatement: JumpStatement) extends Statement

  sealed abstract class LabeledStatement
  case class LabelStmt(ident: String, stmt: Statement) extends LabeledStatement //ident : stmt
  case class CaseStmt(expr: ConstantExpression, stmt: Statement) extends LabeledStatement // case expr : stmt
  case class DefaultCaseStmt(stmt: Statement) extends LabeledStatement // default : stmt
  
  sealed abstract class SelectionStatement
  case class If (condition: Expression, stmt: Statement) extends SelectionStatement
  case class IfElse(condition: Expression, trueBranch: Statement, elseBranch: Statement) extends SelectionStatement
  case class Switch (switchExpr: Expression, stmt: Statement) extends SelectionStatement //switch(expression) statement
  
  sealed abstract class IterationStatement
  case class While (condition: Expression, contents: Statement) extends IterationStatement
  case class For (initialization: Option[Expression], condition: Option[Expression], counter: Option[Expression], contents: Statement) extends IterationStatement
  case class DoWhile (contents: Statement, condition: Expression) extends IterationStatement
  
  sealed abstract class JumpStatement
  case class Goto(identifier: String) extends JumpStatement
  case object Continue extends JumpStatement
  case object Break extends JumpStatement
  case class Return (returnExpression: Option[Expression]) extends JumpStatement
  
  //Statements or declarations
  sealed abstract class StmtOrDec
  case class Stmt (statement: Statement) extends StmtOrDec
  case class Dec (declaration: Declaration) extends StmtOrDec
  
  //C types
  sealed abstract class TypeSpecifier
  case object TypeVoid extends TypeSpecifier
  case object TypeChar extends TypeSpecifier
  case object TypeShort extends TypeSpecifier
  case object TypeInteger extends TypeSpecifier
  case object TypeLong extends TypeSpecifier
  case object TypeFloat extends TypeSpecifier
  case object TypeDouble extends TypeSpecifier
  case object TypeSigned extends TypeSpecifier
  case object TypeUnsigned extends TypeSpecifier
  case class  TypeStruct(ident: Option[String], structDeclarations: List[StructUnionDeclaration]) extends TypeSpecifier
  case class  TypeStructShort(ident: String, structDeclarations: Option[List[StructUnionDeclaration]]) extends TypeSpecifier
  case class  TypeUnion(ident: Option[String], structDeclarations: List[StructUnionDeclaration]) extends TypeSpecifier
  case class  TypeUnionShort(ident: String, structDeclarations: Option[List[StructUnionDeclaration]]) extends TypeSpecifier
  case class  TypeEnum(ident: Option[String], enumerations: List[EnumerationDec]) extends TypeSpecifier
  case class  TypeEnumShort(ident: String, enumerations: Option[List[EnumerationDec]]) extends TypeSpecifier
  
  
  case class StructUnionDeclaration(typeQualifier: TypeQualifier, typeSpecifier: TypeSpecifier, declarator: List[Declarator]) //const int foo = 2, bar;
  
  case class EnumerationDec(ident: String, assignment: Option[Expression]) //enum ident { foo = 2, bar = 4 };
  
  
  //C Unary operators
  sealed abstract class UnaryOp 
  case object Address extends UnaryOp //&
  case object Deref extends UnaryOp //*
  case object Positive extends UnaryOp //+
  case object Negative extends UnaryOp //-
  case object OnesCompliment extends UnaryOp //
  case object Negation extends UnaryOp //!
  
  //C Binary Operators
  sealed abstract class BinaryOp
  case object BinaryPlus extends BinaryOp
  case object BinaryMinus extends BinaryOp
  case object BinaryTimes extends BinaryOp
  case object BinaryDivide extends BinaryOp
  case object BinaryModulo extends BinaryOp
  case object BinaryEquality extends BinaryOp
  case object BinaryLessThan extends BinaryOp
  case object BinaryLessThanOrEquals extends BinaryOp
  case object BinaryGreaterThan extends BinaryOp
  case object BinaryGreaterThanOrEquals extends BinaryOp
  case object BinaryBitwiseOr extends BinaryOp
  case object BinaryBitwiseAnd extends BinaryOp
  case object BinaryBitwiseXOR extends BinaryOp
  case object BinaryLogicalAnd extends BinaryOp
  case object BinaryLogicalOr extends BinaryOp
  case object BinaryShiftRight extends BinaryOp
  case object BinaryShiftLeft extends BinaryOp
  
  //C AssignmentOperators
  sealed abstract class AssignmentOperator
  case object Equals // =
  case object TimesEquals // *=
  case object DivisionEquals // /=
  case object ModuloEquals // %=
  case object PlusEquals // +=
  case object MinusEquals // -=
  case object ShiftLeftEquals // <<=
  case object ShiftRightEquals // >>=
  case object BitwiseAndEquals // &=
  case object BitwiseOrEquals // |=
  case object BitwiseXOREquals // ^=
    
  //TypeName
  case class TypeName(qualifierSpecifierList: TypeSpecifierQualifier, abstractDeclarator: Option[AbstractDeclarator])
  case class TypeSpecifierQualifier(typeSpecifier: TypeSpecifier, typeQualifier: TypeQualifier)
  
  //C Expressions
  sealed abstract class Expression
  case class ConstantExpr (constantExpr: ConstantExpression) extends Expression
  case class Assign (assignTo: UnaryExpression, operator: AssignmentOperator, expr: Expression) extends Expression  //x=e  or  *p=e  or  a[e]=e 
  
  sealed abstract class ConstantExpression
  case class OtherExpr(otherExpression: OtherExpression) extends OtherExpression
  case class ConditionalExpression (expr1: Expression, expr2: Expression, expr3: Expression) extends ConstantExpression //e1 ? e2 : e3
  
  sealed abstract class OtherExpression
  case class CastExpr (castExpression: CastExpression) extends OtherExpression
  case class BinaryPrim (operator: BinaryOp, expression1: Expression, expression2: Expression) extends OtherExpression //Binary primitive operator
  
  sealed abstract class CastExpression
  case class UnaryExpr(unaryExpr: UnaryExpression) extends CastExpression
  case class Cast(newType: TypeName, expression: CastExpression) extends CastExpression //(newType) expression;
  
  sealed abstract class UnaryExpression
  case class UnaryPrim (operator: UnaryOp, expression: CastExpression) extends UnaryExpression //Unary primitive operator
  case class PrefixIncrement (expression: UnaryExpression) extends UnaryExpression
  case class PrefixDecrement (expression: UnaryExpression) extends UnaryExpression
  case class SizeofUnary (expression: UnaryExpression) extends UnaryExpression
  case class SizeofTypeName (typeName: TypeName) extends UnaryExpression
  case class PostfixExpr (postfixExpr: PostfixExpression) extends UnaryExpression
  
  sealed abstract class PostfixExpression
  case class PrimaryExpr (primaryExpression: PrimaryExpression) extends PostfixExpression
  case class PostfixIncrement (expression: PostfixExpression) extends PostfixExpression
  case class PostfixDecrement (expression: PostfixExpression) extends PostfixExpression
  case class AccessIndex (postfixExpr: PostfixExpression, expr: Expression) extends PostfixExpression //postfix-expression[expression]
  case class Call (postfixExpression: PostfixExpression, arguments: List[Expression]) extends PostfixExpression //postfix-expression(argument-expression-list_opt)
  case class AccessMember (postfixExpr: PostfixExpression, memberToAccess: DeclareIdentifier) extends PostfixExpression //postfix-expression.identifier
  case class AccessArrowMember (postfixExpr: PostfixExpression, memberToAccess: DeclareIdentifier) extends PostfixExpression // postfix-expression->identifier
  
  sealed abstract class PrimaryExpression
  case class AccessIdentifier(name: String) extends PrimaryExpression
  case class ConstantInteger (contents: Integer) extends PrimaryExpression
  case class ConstantChar (contents: Character) extends PrimaryExpression
  case class ConstantFloat (contents: Float) extends PrimaryExpression
  case class ConstantEnumeration extends PrimaryExpression //TODO find out what this is
  case class String (content: String) extends PrimaryExpression
  case class ParenthesiseExpr (expression: Expression) extends PrimaryExpression
  
}







