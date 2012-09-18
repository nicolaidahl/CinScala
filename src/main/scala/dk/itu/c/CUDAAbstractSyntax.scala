package dk.itu.c
import dk.itu.c.CAbstractSyntax._

object CUDAAbstractSyntax {
  
  case class CUDAProgram(contents: List[CExternalDeclaration]) extends Program
  
  case class CUDAFunctionDec(functionType: CUDAFunctionQualifier, declarationSpecifiers: Option[CDeclarationSpecifiers], declarator: CDeclarator, declarationList: Option[List[CDeclaration]], 
  compoundStmt: CCompoundStmt) extends CExternalDeclaration
    
  sealed abstract class CUDAFunctionQualifier
  case object CUDAGlobalQualifier extends CUDAFunctionQualifier
  case object CUDADeviceFuncQualifier extends CUDAFunctionQualifier
  case object CUDAHostQualifier extends CUDAFunctionQualifier
  case object CUDANoInlineQualifier extends CUDAFunctionQualifier
  case object CUDAForceInlineQualifier extends CUDAFunctionQualifier
  
  sealed abstract class CUDAVariableQualifier
  case object CUDADeviceVarQualifier extends CUDAVariableQualifier
  case object CUDAConstantQualifier extends CUDAVariableQualifier
  case object CUDASharedQualifier extends CUDAVariableQualifier
  
  
  sealed abstract class CUDAPostfixExpression extends CPostfixExpression
  case class CUDAKernelCall(dimGrid: Int, dimBlock: Int, postfixExpression: CPostfixExpression, arguments: List[CExpression]) extends CUDAPostfixExpression
  case class CUDAKernelCallExtensive(dimGrid: Int, dimBlock: Int, sharedMemory: Option[Int], cudaStream: Option[Int], 
      postfixExpression: CPostfixExpression, arguments: List[CExpression]) extends CUDAPostfixExpression
  
  
}