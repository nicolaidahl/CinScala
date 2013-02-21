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
import dk.itu.cinscala.CAbstractSyntax._

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