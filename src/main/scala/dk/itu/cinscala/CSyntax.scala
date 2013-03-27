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
import scala.language._

object CSyntax {
  import CAbstractSyntax._
  
  // Constants
  def int   (n: Int):         CConstantInteger  = CConstantInteger(n)
  def float (n: Float):       CConstantFloat    = CConstantFloat(n)
  def char  (c: Char):        CConstantChar     = CConstantChar(c)
  def string(s: String):      CCharArray        = CCharArray(s)
  def wrap  (e: CExpression): CParenthesiseExpr = CParenthesiseExpr(e)
  
  // Types
  val void     = CTypeVoid
  val char     = CTypeChar
  val short    = CTypeShort
  val int      = CTypeInteger
  val long     = CTypeLong
  val float    = CTypeFloat
  val double   = CTypeDouble
  val signed   = CTypeSigned
  val unsigned = CTypeUnsigned
  
  def struct(structName: String) = CTypeStruct(Some(structName))
  def union (unionName:  String) = CTypeUnion (Some(unionName))
  def enum  (enumName:   String) = CTypeEnum  (Some(enumName))
  
  def pointer(name: String) = CPointerDeclarator(CPointer(), CDeclareIdentifier(name))
  
  implicit def string2CDeclareIdentifier(name: String): CDeclareIdentifier = CDeclareIdentifier(name)
  implicit def cExpression2CExpressionInitializer(e: CExpression): CExpressionInitializer = CExpressionInitializer(e)
  implicit def cExpression2CExpressionStmt(e: CExpression): CExpressionStmt = CExpressionStmt(Some(e))
  implicit def cTypeSpecifier2CDeclarationSpecifiers(t: CTypeSpecifier): CDeclarationSpecifiers = CDeclarationSpecifiers(t)
  implicit def cDeclarationSpecifierUnit2ListOfCDeclationSpecifierUnit(csu: CDeclarationSpecifierUnit): List[CDeclarationSpecifierUnit] = List(csu)
  implicit def argTuple2ListOfArgTuple(arg: (CTypeSpecifier, String)): List[(CTypeSpecifier, String)] = List((arg._1, arg._2))
  
  // Pointer
  case class PointerSyntax(dec: CDeclarator) {
    def pointer(): CDeclarator = CPointerDeclarator(CPointer(), dec)
  }
  implicit def declarator2PointerSyntax(dec: CDeclarator): PointerSyntax = PointerSyntax(dec)
  
  // Function calls
  case class CCallSyntax (cpf: CPostfixExpression) {
    def apply(args: CExpression*): CCall = CCall(cpf, args.toList)
  }
  implicit def cPostfixExpression2CCallSyntax(cpf: CPostfixExpression): CCallSyntax = CCallSyntax(cpf)
  
  // Assignment
  case class CAssignSyntax (cue: CUnaryExpression) {
    def =-(e: CExpression) = CAssign(cue, CEquals, e)
    def *=(e: CExpression) = CAssign(cue, CTimesEquals, e)
    def /=(e: CExpression) = CAssign(cue, CDivisionEquals, e)
    def %=(e: CExpression) = CAssign(cue, CModuloEquals, e)
    def +=(e: CExpression) = CAssign(cue, CPlusEquals, e)
    def -=(e: CExpression) = CAssign(cue, CMinusEquals, e)
    def <<=(e: CExpression) = CAssign(cue, CShiftLeftEquals, e)
    def >>=(e: CExpression) = CAssign(cue, CShiftRightEquals, e)
    def &=(e: CExpression) = CAssign(cue, CBitwiseAndEquals, e)
    def |=(e: CExpression) = CAssign(cue, CBitwiseOrEquals, e)
    def ^=(e: CExpression) = CAssign(cue, CBitwiseXOREquals, e)
  }
  implicit def cUnaryExpression2CAssignSyntax(cue: CUnaryExpression): CAssignSyntax = CAssignSyntax(cue)
  
  // Binary expression operators
  case class ExprSyntax(e: CExpression) {
    def +(other: CExpression): CExpression = CBinaryPrim(CBinaryPlus, e, other)
    def -(other: CExpression): CExpression = CBinaryPrim(CBinaryMinus, e, other)
    def *(other: CExpression): CExpression = CBinaryPrim(CBinaryTimes, e, other)
    def /(other: CExpression): CExpression = CBinaryPrim(CBinaryDivide, e, other)
    def %(other: CExpression): CExpression = CBinaryPrim(CBinaryModulo, e, other)
    def ==-(other: CExpression): CExpression = CBinaryPrim(CBinaryEquality, e, other)
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
  
  // Shorthand for access
  def acc(n: String): CAccessIdentifier = CAccessIdentifier(n)
  
  // Functions
  def Func(ctype: List[CDeclarationSpecifierUnit], identifier: String, args: List[(CTypeSpecifier, CDeclarator)])(contents: CStmtOrDec*): CFunctionDec = {
    val params = args.map(a => CNormalDeclaration(a._1, a._2))
      
    CFunctionDec(Some(CDeclarationSpecifiers(ctype)), CParameterList(identifier, params), None, CCompoundStmt(contents.toList))
  }
  
  // Local declarations
  case object Dec {
    def apply(ctype: List[CDeclarationSpecifierUnit], identifier: String): CLocalDeclaration =
      apply(ctype, List(identifier), List(), None)
    
    def apply(ctype: List[CDeclarationSpecifierUnit], identifier: String, expression: CExpression): CLocalDeclaration =
      apply(ctype, List(identifier), List(expression), None)
    
    def apply(ctype: List[CDeclarationSpecifierUnit], identifier: String, expressions: List[CExpression]): CLocalDeclaration =
      apply(ctype, List(identifier), expressions, None)
      
    def apply(ctype: List[CDeclarationSpecifierUnit], identifier: String, arraySize: Int): CLocalDeclaration =
      apply(ctype, List(identifier), List(), Some(arraySize))
    
    def apply(ctype: List[CDeclarationSpecifierUnit], identifier: String, expression: CExpression, arraySize: Int): CLocalDeclaration =
      apply(ctype, List(identifier), List(expression), Some(arraySize))
    
    def apply(ctype: List[CDeclarationSpecifierUnit], identifier: String, expressions: List[CExpression], arraySize: Int): CLocalDeclaration =
      apply(ctype, List(identifier), expressions, Some(arraySize))
    
    /**
     * Produces a CLocalDeclaration based on the parameters.
     * 
     * @param ctype The type of the declaration
     * @param identifier The name of the declaration
     * @param expressions A list of expressions used for initialization. A list with 1 expression will produce a regular assign, while a longer list will produce a scalar.
     * @param array None means the local declaration will not be an array. Some(i) will produce an array of size i.
     * @return A CLocalDeclaration
     */
    def apply(ctype: List[CDeclarationSpecifierUnit], identifiers: List[String], expressions: List[CExpression] = List(), array: Option[Int] = None): CLocalDeclaration = {
      // Is this an array?
      val dec = array match {
        case None    => identifiers.map(id => CDeclareIdentifier(id))
        case Some(i) => identifiers.map(id => CDeclareArray(id, Some(int(i))))
      }
      
      // Without assign, with 1 assign, or scalar?
      val exps = expressions match {
        case Nil      => None
        case h :: Nil => array match {
          case None    => Some(CExpressionInitializer(h))
          case Some(i) => Some(CScalar(List(CExpressionInitializer(h)))) // Only one expression, but it's for an array
        }
        case h :: t   => Some(CScalar(expressions.map(e => CExpressionInitializer(e))))
      }
      
      // If there are no init expressions, don't do an assign
      exps match {
        case None       => CLocalDeclaration(CDeclarationSpecifiers(ctype), dec.map(d => CDeclaratorWrap(d)))
        case Some(exps) => CLocalDeclaration(CDeclarationSpecifiers(ctype), dec.map(d => CDeclaratorWithAssign(d, exps)))
      }
    }
  }
    
  // Selection
  def If (condition: CExpression)(contents: CStmtOrDec*): CIf = 
    CIf(condition, CCompoundStmt(contents.toList))
  
  def IfElse(condition: CExpression)(contentsTrue: CStmtOrDec*)(contentsFalse: CStmtOrDec*): CIfElse =
    CIfElse(condition, CCompoundStmt(contentsTrue.toList), CCompoundStmt(contentsFalse.toList))
  
  def Switch(expression: CExpression)(contents: CStmtOrDec*): CSwitch =
    CSwitch(expression, CCompoundStmt(contents.toList))
    
  // Case
  def Case(expression: CConstantExpression)(contents: CStmtOrDec*): CCaseStmt =
    CCaseStmt(expression, CCompoundStmt(contents.toList))
  
  // Loops
  def While(condition: CExpression)(contents: CStmtOrDec*): CWhile = 
    CWhile(condition, CCompoundStmt(contents.toList))
    
  def For(initialization: CExpression, condition: CExpression, counter: CExpression)(contents: CStmtOrDec*): CFor =
    CFor(Some(initialization), Some(condition), Some(counter), CCompoundStmt(contents.toList))
    
  def DoWhile(contents: CStmtOrDec*)(condition: CExpression): CDoWhile =
    CDoWhile(CCompoundStmt(contents.toList), condition)
}