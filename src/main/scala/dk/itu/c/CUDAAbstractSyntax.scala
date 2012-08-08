package dk.itu.c


trait CUDAAbstractSyntax extends CAbstractSyntax {
  
  
  case class CUDAFunctionDec(functionType: CUDAFunctionType, declarationSpecifiers: Option[DeclarationSpecifiers], declarator: Declarator, declarationList: Option[List[Declaration]], 
  compoundStmt: CompoundStmt) extends FunctionDec
    
  sealed abstract class CUDAFunctionType
  case class GlobalType extends CUDAFunctionType
  case class DeviceType extends CUDAFunctionType
  
  
  
  
}