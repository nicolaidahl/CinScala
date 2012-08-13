package dk.itu.c


trait CUDAAbstractSyntax extends CAbstractSyntax {
  
  
  case class CUDAFunctionDec(functionType: CUDAFunctionQualifier, declarationSpecifiers: CTypeSpecifier, declarator: CDeclarator, declarationList: Option[List[CDeclaration]], 
  compoundStmt: CompoundStmt) extends CExternalDeclaration
    
  sealed abstract class CUDAFunctionQualifier
  case class CUDAGlobalQualifier extends CUDAFunctionQualifier
  case class CUDADeviceFuncQualifier extends CUDAFunctionQualifier
  case class CUDAHostQualifier extends CUDAFunctionQualifier
  case class CUDANoInlineQualifier extends CUDAFunctionQualifier
  case class CUDAForceInlineQualifier extends CUDAFunctionQualifier
  
  sealed abstract class CUDAVariableQualifier
  case class CUDADeviceVarQualifier extends CUDAVariableQualifier
  case class CUDAConstantQualifier extends CUDAVariableQualifier
  case class CUDASharedQualifier extends CUDAVariableQualifier
  
  
  sealed abstract class CUDAPostfixExpression extends CPostfixExpression
  case class CUDAKernelCall(dimGrid: Integer, dimBlock: Integer, postfixExpression: CPostfixExpression, arguments: List[CExpression]) extends CUDAPostfixExpression
  case class CUDAKernelCallExtensive(dimGrid: Integer, dimBlock: Integer, sharedMemory: Option[Integer], cudaStream: Option[Integer], 
      postfixExpression: CPostfixExpression, arguments: List[CExpression]) extends CUDAPostfixExpression
  
  
}