package dk.itu.c

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
    val returnType: Option[TypeSpecifier]
    val identifier: String
    val parameters: ArgList
    val stmtOrDecs: List[StmtOrDec]
   }
  case class CFunctionDec(returnType: Option[TypeSpecifier], identifier: String,
    parameters: ArgList, stmtOrDecs: List[StmtOrDec]) extends FunctionDec
  case class Declaration (decSpecs: DeclarationSpecifiers, declarators: List[InitDeclarator]) extends ExternalDeclaration
  
  
  //Statements or declarations
  sealed abstract class StmtOrDec
  case class Stmt (statement: Statement) extends StmtOrDec
  case class Dec (declaration: Declaration) extends StmtOrDec
  
  //Declarations
  /*sealed abstract class Declaration
  case class LocalVariable (variableType: TypeSpecifier, identifier: String) extends Declaration //int x;
  case class LocalVariableWithAssign (variableType: TypeSpecifier, identifier: String, expr: Expression) extends Declaration //int x = e;*/
  
  //Declaration specifier
  case class DeclarationSpecifiers(storage: Option[StorageClassSpecifier], typeSpec: Option[TypeSpecifier], qualifier: Option[TypeQualifier])
  
  sealed abstract class InitDeclarator
  case class DeclaratorWrap(dec: Declarator) extends InitDeclarator
  case class DeclaratorWithAssign(dec: Declarator, assignment: Expression) extends InitDeclarator
  
  case class Declarator(pointer: Option[PointerTest], directDeclarator: DirectDeclarator)
  
  sealed abstract class DirectDeclarator
  case class Identifier(name: String) extends DirectDeclarator
  case class Parenthesise(declarator: Declarator) extends DirectDeclarator //(declarator)
  case class Array(directDeclarator: DirectDeclarator, expr: Option[Expression]) extends DirectDeclarator //direct-declarator [ constant-expressionopt ]
  case class ParameterList(directDeclarator: DirectDeclarator, ptlt: ParameterTypeListTest) //direct-declarator ( parameter-type-list ) 
  case class IdentifierList(directDeclarator: DirectDeclarator, ilo: Option[List[String]])//direct-declarator ( identifier-list opt )
  
  sealed abstract class PointerTest
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
  case class Block (contents: List[StmtOrDec]) extends Statement
  case class ExpressionStatement (expr: Expression) extends Statement
  case class If (condition: Expression, ifBranch: List[StmtOrDec], elseIfBranches: Option[List[(Expression, List[StmtOrDec])]], elseBranch: Option[List[StmtOrDec]]) extends Statement
  case class Switch (switchExpr: Expression, cases: List[(Expression, List[StmtOrDec])], default: Option[List[StmtOrDec]]) extends Statement
  case class While (condition: Expression, contents: Statement) extends Statement
  case class For (initialization: Expression, condition: Expression, counter: Expression, contents: Statement) extends Statement
  case class DoWhile (contents: Statement, condition: Expression) extends Statement
  case class Return (returnExpression: Option[Expression]) extends Statement
  
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
  
  /*case class TypeArray (arrayType: Type, length: Option[Integer]) extends TypeSpecifier
  case class TypePointer (pointerType: Type) extends TypeSpecifier*/
  
  //C Unary operators
  sealed abstract class UnaryOp
  case object UnaryDecrement extends UnaryOp
  case object UnaryIncrement extends UnaryOp

  //C Binary Operators
  sealed abstract class BinaryOp
  case object BinaryPlus extends BinaryOp
  case object BinaryMinus extends BinaryOp
  case object BinaryTimes extends BinaryOp
  case object BinaryDivide extends BinaryOp
  case object BinaryEquals extends BinaryOp
  case object BinaryLessThan extends BinaryOp
  case object BinaryLessThanOrEquals extends BinaryOp
  case object BinaryGreaterThan extends BinaryOp
  case object BinaryGreaterThanOrEquals extends BinaryOp
  
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
  case class ConditionExpression (expr1: Expression, expr2: Expression, expr3: Expression) extends Expression //e1 ? e2 : e3
  case class Cast(expression: Expression, newType: TypeSpecifier) extends Expression //(int) a;

  //C variable access
  sealed abstract class Access
  case class AccessVariable (ident: String) extends Access //Variable access  ident
  case class AccessDeref (expr: Expression) extends Access //Pointer dereferencing  *p
  case class AccessIndex (access: Access, expr: Expression) extends Access //Access Array indexing  a[e]  

}