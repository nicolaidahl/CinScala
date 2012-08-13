package dk.itu.c


trait CUDAAbstractSyntax extends CAbstractSyntax {
  
  
  case class CUDAFunctionDec(functionType: CUDAFunctionQualifier, declarationSpecifiers: CTypeSpecifier, declarator: CDeclarator, declarationList: Option[List[CDeclaration]], 
  compoundStmt: CompoundStmt) extends CExternalDeclaration
    
  sealed abstract class CUDAFunctionQualifier
  case class CUDAGlobalType extends CUDAFunctionQualifier
  case class CUDADeviceType extends CUDAFunctionQualifier
  case class CUDAHostType extends CUDAFunctionQualifier
  case class CUDANoInlineType extends CUDAFunctionQualifier
  
  trait CUDAPostfixExpression extends CPostfixExpression
  case class CUDAKernelCall(dimGrid: Integer, dimBlock: Integer, postfixExpression: CPostfixExpression, arguments: List[CExpression]) extends CUDAPostfixExpression
  case class CUDAKernelCallExtensive(dimGrid: Integer, dimBlock: Integer, sharedMemory: Option[Integer], cudaStream: Option[Integer], 
      postfixExpression: CPostfixExpression, arguments: List[CExpression]) extends CUDAPostfixExpression
  
  
}