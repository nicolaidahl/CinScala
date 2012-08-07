package dk.itu.c


trait CUDAAbstractSyntax extends CAbstractSyntax {
  
  
  case class CUDAFunctionDec(functionType: CUDAFunctionType,
    returnType: Option[Type], identifier: String, parameters: ArgList,
    stmtOrDecs: List[StmtOrDec]) extends FunctionDec
  
  sealed abstract class CUDAFunctionType
  case class GlobalType extends CUDAFunctionType
  case class DeviceType extends CUDAFunctionType
  
  
  
  
}