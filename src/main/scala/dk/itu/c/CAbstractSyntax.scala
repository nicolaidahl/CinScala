package dk.itu.c
import com.sun.source.tree.LabeledStatementTree

trait CAbstractSyntax {
  
  //Variable environment: Map from identifier to variable type
  type VarEnv = Map[String, TypeSpecifier] 
  //Function environment: Map from identifier to tuple of return type and argument list
  type FunEnv = Map[String, (Option[TypeSpecifier], ArgList)]
  //Argument list
  type ArgList = List[(TypeSpecifier, String)] 
  
  case class Program (contents: List[ExternalDeclaration])
  
  //Top level declaration
  abstract class ExternalDeclaration
  trait FunctionDec extends ExternalDeclaration {
    val declarationSpecifiers: Option[DeclarationSpecifiers]
    val declarator: Declarator
    val parameters: Option[List[Declaration]]
    val compoundStmt: CompoundStatement
   }
  case class CFunctionDec(declarationSpecifiers: Option[DeclarationSpecifiers], declarator: Declarator,
    parameters: Option[List[Declaration]], compoundStmt: CompoundStatement) extends FunctionDec
  case class GlobalDeclaration(decSpecs: DeclarationSpecifiers, declarators: List[InitDeclarator]) extends ExternalDeclaration
  
  
  //Declaration specifier
  case class DeclarationSpecifiers(storage: Option[StorageClassSpecifier], typeSpec: Option[TypeSpecifier], qualifier: Option[TypeQualifier])
  
  //Any declaration
  case class Declaration(decSpecs: DeclarationSpecifiers, declarators: List[InitDeclarator])
  
  sealed abstract class InitDeclarator
  case class DeclaratorWrap(dec: Declarator) extends InitDeclarator
  case class DeclaratorWithAssign(dec: Declarator, assignment: Expression) extends InitDeclarator
  
  case class Declarator(pointer: Option[Pointer], directDeclarator: DirectDeclarator)
  
  sealed abstract class DirectDeclarator
  case class Identifier(name: String) extends DirectDeclarator
  case class Parenthesise(declarator: Declarator) extends DirectDeclarator //(declarator)
  case class Array(directDeclarator: DirectDeclarator, expr: Option[Expression]) extends DirectDeclarator //direct-declarator [ constant-expressionopt ]
  case class ParameterList(directDeclarator: DirectDeclarator, ptlt: ParameterTypeListTest) //direct-declarator ( parameter-type-list ) 
  case class IdentifierList(directDeclarator: DirectDeclarator, ilo: Option[List[String]])//direct-declarator ( identifier-list_opt )
  
  case class Pointer(typeQualifier: Option[TypeQualifier], pointer: Option[Pointer]) //*type-qualifier-list_opt pointer_opt
  
  
  sealed abstract class ParameterTypeListTest
  
  
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
  case class IdentColonStmt(ident: String, stmt: Statement) extends LabeledStatement //ident : stmt
  case class CaseStmt(expr: ConstantExpressionTest, stmt: Statement) extends LabeledStatement // case expr : stmt
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
  sealed abstract class UnaryOp //Missing: &*+-~!
  case object UnaryDecrement extends UnaryOp
  case object UnaryIncrement extends UnaryOp
  
  //C Binary Operators
  sealed abstract class BinaryOp
  case object BinaryPlus extends BinaryOp
  case object BinaryMinus extends BinaryOp
  case object BinaryTimes extends BinaryOp
  case object BinaryDivide extends BinaryOp
  case object BinaryModulo extends BinaryOp
  case object BinaryEquals extends BinaryOp
  case object BinaryLessThan extends BinaryOp
  case object BinaryLessThanOrEquals extends BinaryOp
  case object BinaryGreaterThan extends BinaryOp
  case object BinaryGreaterThanOrEquals extends BinaryOp
  case object BinaryBitwiseOr extends BinaryOp
  case object BinaryBitwiseAnd extends BinaryOp
  case object BinaryBitwiseXOR extends BinaryOp
  
  sealed abstract class ConstantExpressionTest
  
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
  case class ConditionalExpression (expr1: Expression, expr2: Expression, expr3: Expression) extends Expression //e1 ? e2 : e3
  case class Cast(expression: Expression, newType: TypeSpecifier) extends Expression //(int) a;

  
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
    
  
  //C variable access
  sealed abstract class Access
  case class AccessVariable (ident: String) extends Access //Variable access  ident
  case class AccessDeref (expr: Expression) extends Access //Pointer dereferencing  *p
  case class AccessIndex (access: Access, expr: Expression) extends Access //Access Array indexing  a[e]  

}