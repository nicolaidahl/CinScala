package dk.itu.c


trait CUDAAbstractSyntax extends CAbstractSyntax {
  
  
  case class CUDAFunctionDec(functionType: CUDAFunctionType, funDec: FunctionDec) extends TopDec
  
  sealed abstract class CUDAFunctionType
  case class GlobalType extends CUDAFunctionType
  case class DeviceType extends CUDAFunctionType
  
}