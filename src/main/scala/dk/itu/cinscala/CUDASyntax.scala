package dk.itu.cinscala

object CUDASyntax {
  import CAbstractSyntax._
  import CUDAAbstractSyntax._
  import CSyntax._

  def CUDAFunction(functionType: CUDAFunctionQualifier, declarationSpecifiers: CDeclarationSpecifiers, declarator: CDeclarator, declarationList: Option[List[CDeclaration]] = None) 
  (contents: CStmtOrDec*): CUDAFunctionDec = CUDAFunctionDec(functionType, Some(declarationSpecifiers), declarator, declarationList, CCompoundStmt(contents.toList))
}