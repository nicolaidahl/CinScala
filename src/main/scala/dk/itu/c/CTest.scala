package dk.itu.c
import CAbstractSyntax._

trait TestFunctions {  
  val mainParams = List(NormalDeclaration(CDeclarationSpecifiers(None, TypeInteger, None), DeclareIdentifier("argc")), NormalDeclaration(CDeclarationSpecifiers(None, TypeInteger, Some(Const)), PointerDeclarator(CPointer(None, None), DeclareArray(DeclareIdentifier("argv"), None))))
  
  def generateLocalDeclaration(t: CDeclarationSpecifiers, i: String, v: CPrimaryExpression) = LocalDeclaration(t, List(DeclaratorWithAssign(DeclareIdentifier(i), ExpressionInitializer(v))))
  def generateFunction(returnType: CDeclarationSpecifiers, name: String, params: List[CParameterDeclaration], body: CompoundStmt) = CFunctionDec(Some(returnType), ParameterList(DeclareIdentifier(name), params), None, body)
  def generateMain(body: CompoundStmt) = generateFunction(CDeclarationSpecifiers(None, TypeInteger, None), "main", mainParams, body)
}

trait Test extends App with CCompileAndRun with TestFunctions {
  def test: Prog
}

object MainFunctionTest extends Test {
  val globalDecs = GlobalDeclaration(CDeclarationSpecifiers(None, TypeInteger, None), List(DeclaratorWithAssign(DeclareIdentifier("a"), ExpressionInitializer(ConstantInteger(0))), DeclaratorWithAssign(DeclareIdentifier("b"), ExpressionInitializer(ConstantInteger(1))))) 
  
  val abBody = CompoundStmt(List(Stmt(Return(Some(BinaryPrim(BinaryPlus, AccessIdentifier("a"), AccessIdentifier("b")))))))
  val ab = generateFunction(CDeclarationSpecifiers(None, TypeInteger, None), "ab", List(), abBody)
  
  val mainBody = CompoundStmt(List(Stmt(ExpressionStmt(Some(Call(AccessIdentifier("printf"), List(CharArray("\"%d\""), Call(AccessIdentifier("ab"), List())))))), Stmt(Return(Some(ConstantInteger(0))))))
  val main = generateMain(mainBody)
  
  val ast = CProgram(List(PreprocessorInstruction(IncludeGlobal("stdio.h")), globalDecs, ab, main))
  
  def test = compile(ast)
    
  println(test)
}

object IfTest extends Test {
  val x = generateLocalDeclaration(CDeclarationSpecifiers(None, TypeInteger, None), "x", ConstantInteger(0))
  val ifStmt = Stmt(If(BinaryPrim(BinaryEquality, AccessIdentifier("x"), ConstantInteger(2)), ExpressionStmt(Some(Call(AccessIdentifier("printf"), List(CharArray("Yay")))))))
  val mainBody = CompoundStmt(List(x, ifStmt, Stmt(Return(Some(ConstantInteger(0))))))
  val ast = CProgram(List(PreprocessorInstruction(IncludeGlobal("stdio.h")), generateMain(mainBody)))
  
  def test = compileAndRun(ast)
  
  println(test)
}

object IfElseTest extends Test {
  val x = generateLocalDeclaration(CDeclarationSpecifiers(None, TypeInteger, None), "x", ConstantInteger(0))
  val trueBranch = ExpressionStmt(Some(Call(AccessIdentifier("printf"), List(CharArray("Yay")))))
  val falseBranch = ExpressionStmt(Some(Call(AccessIdentifier("printf"), List(CharArray("Boo")))))
  val ifElseStmt = Stmt(IfElse(BinaryPrim(BinaryEquality, AccessIdentifier("x"), ConstantInteger(2)), trueBranch, falseBranch))
  val mainBody = CompoundStmt(List(x, ifElseStmt, Stmt(Return(Some(ConstantInteger(0))))))
  val ast = CProgram(List(PreprocessorInstruction(IncludeGlobal("stdio.h")), generateMain(mainBody)))
  
  def test = compileAndRun(ast)
  
  println(test)
}

object SwitchTest extends Test {
  val x = generateLocalDeclaration(CDeclarationSpecifiers(None, TypeInteger, None), "x", ConstantInteger(0))
  val case0 = Stmt(CaseStmt(ConstantInteger(0), CompoundStmt(List(Stmt(Return(Some(ConstantInteger(0))))))))
  val case1 = Stmt(CaseStmt(ConstantInteger(1), CompoundStmt(List(Stmt(Return(Some(ConstantInteger(1))))))))
  val switchBody = CompoundStmt(List(case0, case1))
  val switchStmt = Stmt(Switch(AccessIdentifier("x"), switchBody))
  val mainBody = CompoundStmt(List(x, switchStmt, Stmt(Return(Some(ConstantInteger(0))))))
  val ast = CProgram(List(PreprocessorInstruction(IncludeGlobal("stdio.h")), generateMain(mainBody)))
  
  def test = compileAndRun(ast)
  
  println(test)
}

object WhileTest extends Test {
  val i = generateLocalDeclaration(CDeclarationSpecifiers(None, TypeInteger, None), "i", ConstantInteger(0))
  val whileBody = CompoundStmt(List(Stmt(ExpressionStmt(Some(Call(AccessIdentifier("printf"), List(CharArray("%i\\n"), AccessIdentifier("i")))))), Stmt(ExpressionStmt(Some(PostfixIncrement(AccessIdentifier("i")))))))
  val whileStmt = Stmt(While(BinaryPrim(BinaryLessThan, AccessIdentifier("i"), ConstantInteger(2)), whileBody))
  val mainBody = CompoundStmt(List(i, whileStmt, Stmt(Return(Some(ConstantInteger(0))))))
  val ast = CProgram(List(PreprocessorInstruction(IncludeGlobal("stdio.h")), generateMain(mainBody)))
  
  def test = compileAndRun(ast)
  
  println(test)
}

object DoWhileTest extends Test {
  val i = generateLocalDeclaration(CDeclarationSpecifiers(None, TypeInteger, None), "i", ConstantInteger(0))
  val doWhileBody = CompoundStmt(List(Stmt(ExpressionStmt(Some(Call(AccessIdentifier("printf"), List(CharArray("%i\\n"), AccessIdentifier("i")))))), Stmt(ExpressionStmt(Some(PostfixIncrement(AccessIdentifier("i")))))))
  val doWhileStmt = Stmt(DoWhile(doWhileBody, BinaryPrim(BinaryLessThanOrEquals, AccessIdentifier("i"), ConstantInteger(2))))
  val mainBody = CompoundStmt(List(i, doWhileStmt, Stmt(Return(Some(ConstantInteger(0))))))
  val ast = CProgram(List(PreprocessorInstruction(IncludeGlobal("stdio.h")), generateMain(mainBody)))
  
  def test = compileAndRun(ast)
  
  println(test)
}

object ForTest extends Test {
  val i = generateLocalDeclaration(CDeclarationSpecifiers(None, TypeInteger, None), "i", ConstantInteger(0))
  val forInit = Assign(AccessIdentifier("i"), Equals, ConstantInteger(10))
  val forCond = BinaryPrim(BinaryGreaterThanOrEquals, AccessIdentifier("i"), ConstantInteger(0))
  val forInc = PrefixDecrement(AccessIdentifier("i"))
  val forBody = CompoundStmt(List(Stmt(ExpressionStmt(Some(Call(AccessIdentifier("printf"), List(CharArray("%i\\n"), AccessIdentifier("i"))))))))
  val forStmt = Stmt(For(Some(forInit), Some(forCond), Some(forInc), forBody))
  val mainBody = CompoundStmt(List(i, forStmt, Stmt(Return(Some(ConstantInteger(0))))))
  val ast = CProgram(List(PreprocessorInstruction(IncludeGlobal("stdio.h")), generateMain(mainBody)))
  
  def test = compileAndRun(ast)
  
  println(test)
}

/*
object Test extends CCompileAndRun with App {
  
  
  def generateMainFunction(stmtOrDecs: List[StmtOrDec]): FunctionDec =
    CFunctionDec(Some(TypeInteger), "main", List((TypeInteger, "argc"), (TypePointer(TypePointer(TypeChar)), "args")), stmtOrDecs)
  
  //Tests the main function generation with if and return statements
  def mainFunctionTest: Program = {
	val locVarAss = new LocalVariableWithAssign(TypeInteger, "testVar", ConstantInteger(2))
	val ifstmt = If(AccessExpr(AccessVariable("testVar")), List(Stmt(Return(Some(ConstantInteger(2))))), None, Some(List(Stmt(Return(Some(ConstantInteger(5)))))))
    
    val statements = List(Dec(locVarAss), Stmt(ifstmt))
    val mainFunc = generateMainFunction(statements)
    
    Program(List(mainFunc))
  }
  //println(generate(mainFunctionTest, getEmptyVarEnv, getEmptyFunEnv))
  
  //Testing that variable declaration and assignment works
  def variableDec: Program = {
    val locVar = LocalVariable(TypeInteger, "testVar")
    val assign = Assign(AccessVariable("testVar"), ConstantInteger(2))
    val locVar2 = LocalVariableWithAssign(TypeInteger, "testVar2", ConstantInteger(3))
    
    val statements = List(Dec(locVar), Stmt(ExpressionStatement(assign)), Dec(locVar2))
    val mainFunc = generateMainFunction(statements)
    
    Program(List(mainFunc))
  }
  //println(generate(variableDec, getEmptyVarEnv, getEmptyFunEnv))
  
  
  //Testing variable scope for access to variable in unreachable scope
  def variableScope: Program = {
    val locVar = LocalVariable(TypeInteger, "testVar")
    val block = Block(List(Dec(locVar)))
   
    val assign = Assign(AccessVariable("testVar"), ConstantInteger(2))
    
    val statements = List(Stmt(block), Stmt(Return(Some(assign))))
    val mainFunc = generateMainFunction(statements)
    
    Program(List(mainFunc))

  }
  //println(generate(variableScope, getEmptyVarEnv, getEmptyFunEnv))
  
  //Testing redefinition of variable
  def variableRedefinition: Program = {
    val locVar = LocalVariable(TypeInteger, "testVar")
    val assign = Assign(AccessVariable("testVar"), ConstantInteger(2))
    val locVar2 = LocalVariableWithAssign(TypeInteger, "testVar", ConstantInteger(3))
    
    val statements = List(Dec(locVar), Stmt(ExpressionStatement(assign)), Dec(locVar2))
    val mainFunc = generateMainFunction(statements)
    
    Program(List(mainFunc))

  }
  //println(generate(variableRedefinition, getEmptyVarEnv, getEmptyFunEnv))
  
  //Testing call of function in scope
  def multipleFunctions: Program = {
    val locVar = LocalVariable(TypeInteger, "testVar")
    val assign = Assign(AccessVariable("testVar"), ConstantInteger(2))
    val locVar2 = LocalVariableWithAssign(TypeInteger, "testVar1", ConstantInteger(3))
    
    val statements = List(Dec(locVar), Stmt(ExpressionStatement(assign)), Dec(locVar2))
    val testFun = CFunctionDec(Some(TypeInteger), "testFunction", List((TypePointer(TypeInteger), "input")), statements)
    
    val funcCall = Call("testFunction", List(ConstantInteger(2)))
    val mainStatements = List(Stmt(ExpressionStatement(funcCall)))
    val mainFunc = generateMainFunction(mainStatements)
    
    Program(List(testFun, mainFunc))

  }
  //println(generate(multipleFunctions, getEmptyVarEnv, getEmptyFunEnv))
  
  //Testing redefinition of function
  def redefinitionOfFunction: Program = {
    
    val testFun = CFunctionDec(Some(TypeInteger), "testFunction", List((TypePointer(TypeInteger), "input")), List())
    val testFun2 = CFunctionDec(Some(TypeInteger), "testFunction", List((TypePointer(TypeInteger), "input")), List())
    
    val funcCall = Call("testFunction", List(ConstantInteger(2)))
    val mainStatements = List(Stmt(ExpressionStatement(funcCall)))
    val mainFunc = generateMainFunction(mainStatements)
    
    Program(List(testFun, testFun2, mainFunc))

  }
  //println(generate(redefinitionOfFunction, getEmptyVarEnv, getEmptyFunEnv))
  
  //Testing switch case statement
  def switchTest: Program = {
    
	val ifstmt = If(ConstantInteger(1), List(Stmt(Return(Some(ConstantInteger(2))))), None, Some(List(Stmt(Return(Some(ConstantInteger(5)))))))
    val plusExpr = BinaryPrim(BinaryPlus, ConstantInteger(2), ConstantInteger(4))
	
	val switchCase = Switch(ConstantInteger(1), List((ConstantInteger(1), List(Stmt(ExpressionStatement(plusExpr))))), Some(List(Stmt(ifstmt))))
	
    val mainStatements = List(Stmt(switchCase))
    val mainFunc = generateMainFunction(mainStatements)
    
    Program(List(PrecompileInstr(IncludeStd("stdio.h")), mainFunc))

  }
  //println(generate(switchTest, getEmptyVarEnv, getEmptyFunEnv))
  
  println(compile(switchTest))
  
  //Testing while and return
  def whileReturnTest: Program = {
     val whilestmt = While(BinaryPrim(BinaryLessThanOrEquals, ConstantInteger(5), ConstantInteger(6)), Block(List(Stmt(Return(Some(ConstantInteger(2)))))))
    
     val mainStatements = List(Stmt(whilestmt))
     val mainFunc = generateMainFunction(mainStatements)
    
     Program(List(PrecompileInstr(IncludeStd("stdio.h")), mainFunc))
  }
  
  println(compile(whileReturnTest))
  
  // Testing for loops
  def forTest: Program = {
    val iassign = LocalVariable(TypeInteger, "i")
    val forstmt = For(Assign(AccessVariable("i"), ConstantInteger(0)), BinaryPrim(BinaryLessThan, AccessExpr(AccessVariable("i")), ConstantInteger(5)), UnaryPrim(UnaryIncrement, AccessExpr(AccessVariable("i"))), Block(List(Stmt(Return(Some(ConstantInteger(0)))))))
  
    val mainStatements = List(Dec(iassign), Stmt(forstmt))
    val mainFunc = generateMainFunction(mainStatements)
    
    Program(List(PrecompileInstr(IncludeStd("stdio.h")), mainFunc))
  }
  println(compile(forTest))
  
  // Testing do while
  def doWhileTest: Program = {
    val iassign = LocalVariable(TypeInteger, "i")
    val doWhilestmt = DoWhile(ExpressionStatement(UnaryPrim(UnaryDecrement, AccessExpr(AccessVariable("i")))), BinaryPrim(BinaryGreaterThan, AccessExpr(AccessVariable("i")), ConstantInteger(0)))
  
    val mainStatements = List(Dec(iassign), Stmt(doWhilestmt))
    val mainFunc = generateMainFunction(mainStatements)
    
    Program(List(PrecompileInstr(IncludeStd("stdio.h")), mainFunc))
  }
  println(compile(doWhileTest))
}
*/



