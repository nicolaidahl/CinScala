package dk.itu.c

object CSyntax {
  import CAbstractSyntax._
  
  implicit def string2CDeclarator(name: String): CDeclarator = CDeclareIdentifier(name)
  implicit def cExpression2CExpressionInitializer(e: CExpression): CExpressionInitializer = CExpressionInitializer(e)
  implicit def cExpression2CExpressionStmt(e: CExpression): CExpressionStmt = CExpressionStmt(Some(e))
  implicit def cTypeSpecifier2CDeclarationSpecifiers(t: CTypeSpecifier): CDeclarationSpecifiers = CDeclarationSpecifiers(t)
  
  case class PointerSyntax(dec: CDeclarator) {
    def pointer(): CDeclarator = CPointerDeclarator(CPointer(), dec)
  }
  implicit def declarator2PointerSyntax(dec: CDeclarator): PointerSyntax = PointerSyntax(dec)
  
  case class CCallSyntax (f: CPostfixExpression) {
    def apply(args: CExpression*): CCall = CCall(f, args.toList)
  }
  implicit def cPostfixExpression2CCallSyntax(cpf: CPostfixExpression): CCallSyntax = CCallSyntax(cpf)
  
  // Types
  def int(n: Int): CConstantInteger = CConstantInteger(n)
  def float(n: Float): CConstantFloat = CConstantFloat(n)
  def char(c: Char): CConstantChar = CConstantChar(c)
  def string(s: String): CCharArray = CCharArray(s)
  def wrap(e: CExpression): CParenthesiseExpr = CParenthesiseExpr(e)
  
  def acc(n: String): CAccessIdentifier = CAccessIdentifier(n)
  
  // Binary expression operators
  case class ExprSyntax(e: CExpression) {
    def +(other: CExpression): CExpression = CBinaryPrim(CBinaryPlus, e, other)
    def -(other: CExpression): CExpression = CBinaryPrim(CBinaryMinus, e, other)
    def *(other: CExpression): CExpression = CBinaryPrim(CBinaryTimes, e, other)
    def /(other: CExpression): CExpression = CBinaryPrim(CBinaryDivide, e, other)
    def %(other: CExpression): CExpression = CBinaryPrim(CBinaryModulo, e, other)
    def ==(other: CExpression): CExpression = CBinaryPrim(CBinaryEquality, e, other)
    def <(other: CExpression): CExpression = CBinaryPrim(CBinaryLessThan, e, other)
    def <=(other: CExpression): CExpression = CBinaryPrim(CBinaryLessThanOrEquals, e, other)
    def >(other: CExpression): CExpression = CBinaryPrim(CBinaryGreaterThan, e, other)
    def >=(other: CExpression): CExpression = CBinaryPrim(CBinaryGreaterThanOrEquals, e, other)
    def |(other: CExpression): CExpression = CBinaryPrim(CBinaryBitwiseOr, e, other)
    def &(other: CExpression): CExpression = CBinaryPrim(CBinaryBitwiseAnd, e, other)
    def ^(other: CExpression): CExpression = CBinaryPrim(CBinaryBitwiseXOR, e, other)
    def &&(other: CExpression): CExpression = CBinaryPrim(CBinaryLogicalAnd, e, other)
    def ||(other: CExpression): CExpression = CBinaryPrim(CBinaryLogicalOr, e, other)
    def >>(other: CExpression): CExpression = CBinaryPrim(CBinaryShiftRight, e, other)
    def <<(other: CExpression): CExpression = CBinaryPrim(CBinaryShiftLeft, e, other)
  }
  implicit def cExpr2ExprSyntax(e: CExpression): ExprSyntax = ExprSyntax(e)
  
  // Postfix operators
  case class PostfixSyntax(p: CPostfixExpression) {
    def ++(): CPostfixIncrement = CPostfixIncrement(p)
    def --(): CPostfixDecrement = CPostfixDecrement(p)
    def get(e: CExpression): CAccessIndex = CAccessIndex(p, e)
    def dot(m: CDeclareIdentifier): CAccessMember = CAccessMember(p, m)
    def ->(m: CDeclareIdentifier): CAccessArrowMember = CAccessArrowMember(p, m)
  }
  implicit def cExpr2PostfixSyntax(e: CPostfixExpression): PostfixSyntax = PostfixSyntax(e)
  
  // Prefix operators
  def ++(e: CUnaryExpression) = CPrefixIncrement(e)
  def --(e: CUnaryExpression) = CPrefixDecrement(e)
  def sizeof(e: CUnaryExpression) = CSizeofUnary(e)
  def sizeof(t: CTypeName) = CSizeofTypeName(t)
  def &(e: CCastExpression) = CUnaryPrim(CAddress, e)
  def *(e: CCastExpression) = CUnaryPrim(CDeref, e)
  def +(e: CCastExpression) = CUnaryPrim(CPositive, e)
  def -(e: CCastExpression) = CUnaryPrim(CNegative, e)
  def ~(e: CCastExpression) = CUnaryPrim(COnesCompliment, e)
  def !(e: CCastExpression) = CUnaryPrim(CNegation, e)
  
  // Function dec
  def Function(decSpecs: CDeclarationSpecifiers, declarator: CDeclarator, decList: List[CDeclaration])(contents: CStmtOrDec*): CFunctionDec = 
    CFunctionDec(Some(decSpecs), declarator, Some(decList), CCompoundStmt(contents.toList))
    
  // Selection
  def If (condition: CExpression)(stmt: CStatement): CIf = 
    CIf(condition, stmt)
  
  def IfElse(condition: CExpression)(trueBranch: CStatement)(elseBranch: CStatement): CIfElse =
    CIfElse(condition, trueBranch, elseBranch)
  
  def Switch(expression: CExpression)(stmt: CStatement): CSwitch =
    CSwitch(expression, stmt)
    
  // Case
  def Case(expression: CConstantExpression)(stmt: CStatement): CCaseStmt =
    CCaseStmt(expression, stmt)
  
  // Loops
  def While(condition: CExpression)(contents: CStatement): CWhile = 
    CWhile(condition, contents)
    
  def For(initialization: CExpression, condition: CExpression, counter: CExpression)(contents: CStatement): CFor =
    CFor(Some(initialization), Some(condition), Some(counter), contents)
    
  def DoWhile(contents: CStatement)(condition: CExpression): CDoWhile =
    CDoWhile(contents, condition)
}