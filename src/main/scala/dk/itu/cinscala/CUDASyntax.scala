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

object CUDASyntax {
  import CAbstractSyntax._
  import CUDAAbstractSyntax._
  import CSyntax._

  // CUDA qualifiers
  val global   = CUDAGlobalQualifier
  val device   = CUDADeviceFuncQualifier
  val shared   = CUDASharedQualifier
  val constant = CUDAConstantQualifier
  
  /**
   * Produces a CUDAFunctionDec based on the supplied parameters.
   * 
   * @param functionType Can be device or global
   * @param ctype The return type of the function
   * @param identifier The name of the declaration
   * @param args A list of arguments, consisting of (type, declarator)
   * @param contents The contents of the function
   * @return A CUDAFunctionDec
   */
  def CUDAFunc(functionType: CUDAFunctionQualifier, ctype: List[CDeclarationSpecifierUnit], identifier: String, args: List[(CTypeSpecifier, CDeclarator)])(contents: CStmtOrDec*): CUDAFunctionDec = {
    val params = args.map(a => CNormalDeclaration(a._1, a._2))
      
    CUDAFunctionDec(functionType, Some(CDeclarationSpecifiers(ctype)), CParameterList(identifier, params), None, CCompoundStmt(contents.toList))
  }
}
