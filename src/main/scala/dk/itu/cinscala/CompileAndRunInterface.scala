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



trait CompileAndRunInterface {
  type Program
  type Prog
  type Result
  
  val commandRunner = CommandRunner
  
  def compile(abstractSyntaxTree: Program, varEnv: Set[String], funEnv: Set[String]): Prog
  def run(program: Prog): Result
  def compileAndRun(abstractSyntaxTree: Program, varEnv: Set[String], funEnv: Set[String]): Result
}

trait CCompileAndRun extends CompileAndRunInterface with CGenerator {
  import CAbstractSyntax._
  
  type Program = CProgram
  type Prog = String
  type Result = String
  
  def compile(abstractSyntaxTree: Program, varEnv: VarEnv = getEmptyVarEnv, funEnv: FunEnv = getEmptyFunEnv): Prog = {
	generate(abstractSyntaxTree, varEnv, funEnv)
  }
  
  def run(program: Prog): Result = {
    val fileName = "cProgram"
    commandRunner.writeToFile(fileName + ".c", program)
    
    //Compile and run
    val (_, stderr, exitCode) = commandRunner.run("gcc " + fileName + ".c -o " + fileName + ".o")
    stderr.size match {
      case 0 => {
        val (stdout, stderr1, exitCode1) = commandRunner.run("./" + fileName + ".o")
        stdout.mkString("\n")
      }
      case _ => throw new IllegalArgumentException(program +  "\n Does not compile: \n" + stderr.mkString("\n"))
    } 
  }
  
  def compileAndRun(abstractSyntaxTree: Program, varEnv: Set[String] = getEmptyVarEnv, funEnv: Set[String] = getEmptyFunEnv): Result =
    run(compile(abstractSyntaxTree, varEnv, funEnv)) 
}

object CUDACompileAndRun extends CompileAndRunInterface with CUDAGenerator {
  import CAbstractSyntax._
  import CUDAAbstractSyntax._
  
  type Program = CUDAProgram
  type Prog = String
  type Result = String
  
  def compile(abstractSyntaxTree: Program, varEnv: VarEnv = getEmptyVarEnv, funEnv: FunEnv = getEmptyFunEnv): Prog = {
	generate(abstractSyntaxTree, varEnv, funEnv)
  }
  
  def run(program: Prog): Result = {
    val fileName = "cudaProgram"
    commandRunner.writeToFile(fileName, program)
    
    //Compile and run
    val (_, stderr, exitCode) = commandRunner.run("nvcc -O3 " + fileName + ".cu -o " + fileName)
    stderr.size match {
      case 0 => {
        val (stdout, stderr1, exitCode1) = commandRunner.run("./" + fileName)
        stdout.mkString("\n")
      }
      case _ => stderr.mkString("\n")
    } 
  }
  
  def compileAndRun(abstractSyntaxTree: Program, varEnv: Set[String] = getEmptyVarEnv, funEnv: Set[String] = getEmptyFunEnv): Result =
    run(compile(abstractSyntaxTree, varEnv, funEnv)) 
}