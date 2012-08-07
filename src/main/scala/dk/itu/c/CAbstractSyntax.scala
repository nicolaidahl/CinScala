package dk.itu.c

trait CAbstractSyntax {
  
  //Variable environment: Map from identifier to variable type
  type VarEnv = Map[String, Type] 
  //Function environment: Map from identifier to tuple of return type and argument list
  type FunEnv = Map[String, (Option[Type], ArgList)]
  //Argument list
  type ArgList = List[(Type, String)] 
  
  case class Program (contents: List[TopDec])
  
  //Top level declaration
  abstract class TopDec
  trait FunctionDec extends TopDec {
    val returnType: Option[Type]
    val identifier: String
    val parameters: ArgList
    val stmtOrDecs: List[StmtOrDec]
   }
  case class CFunctionDec(returnType: Option[Type], identifier: String,
    parameters: ArgList, stmtOrDecs: List[StmtOrDec]) extends FunctionDec
  case class VariableDec (variableType: Type, identifier: String) extends TopDec
  case class PrecompileInstr (instruction: PrecompileInstruction) extends TopDec
  
  sealed abstract class PrecompileInstruction
  case class IncludeLoc (fileName: String) extends PrecompileInstruction
  case class IncludeStd (fileName: String) extends PrecompileInstruction
  
  //Declarations inside a block
  sealed abstract class StmtOrDec
  case class Stmt (statement: Statement) extends StmtOrDec
  case class LocalVariable (variableType: Type, identifier: String) extends StmtOrDec //int x;
  case class LocalVariableWithAssign (variableType: Type, identifier: String, expr: Expression) extends StmtOrDec //int x = e;
  
  //C statements
  sealed abstract class Statement
  case class Block (contents: List[StmtOrDec]) extends Statement
  case class ExpressionStatement (expr: Expression) extends Statement
  case class If (condition: Expression, ifBranch: List[StmtOrDec], elseIfBranches: Option[List[(Expression, List[StmtOrDec])]], elseBranch: Option[List[StmtOrDec]]) extends Statement
  case class Switch (switchExpr: Expression, cases: List[(Expression, List[StmtOrDec])], default: Option[List[StmtOrDec]]) extends Statement
  case class While (condition: Expression, contents: Statement) extends Statement
  case class Return (returnExpression: Option[Expression]) extends Statement
  
  //C types
  sealed abstract class Type
  case object TypeInteger extends Type
  case object TypeChar extends Type
  case class TypeArray (arrayType: Type, length: Option[Integer]) extends Type
  case class TypePointer (pointerType: Type) extends Type
  
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
  case class Cast(expression: Expression, newType: Type) extends Expression //(int) a;

  //C variable access
  sealed abstract class Access
  case class AccessVariable (ident: String) extends Access //Variable access  ident
  case class AccessDeref (expr: Expression) extends Access //Pointer dereferencing  *p
  case class AccessIndex (access: Access, expr: Expression) extends Access //Access Array indexing  a[e]  

}