package dk.itu.c


trait CUDAAbstractSyntax extends CAbstractSyntax {
  
  
  case class CUDAFunctionDec(functionType: CUDAFunctionQualifier, declarationSpecifiers: TypeSpecifier, declarator: Declarator, declarationList: Option[List[Declaration]], 
  compoundStmt: CompoundStmt) extends ExternalDeclaration
    
  sealed abstract class CUDAFunctionQualifier
  case class CUDAGlobalType extends CUDAFunctionQualifier
  case class CUDADeviceType extends CUDAFunctionQualifier
  case class CUDAHostType extends CUDAFunctionQualifier
  case class CUDANoInlineType extends CUDAFunctionQualifier
  
  
  case class CUDAKernelCall(dimGrid: Integer, dimBlock: Integer, postfixExpression: PostfixExpression, arguments: List[Expression]) extends PostfixExpression
  
  
}