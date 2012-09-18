package dk.itu.c
import CAbstractSyntax._

trait TestFunctions {  
  val mainParams = List(CNormalDeclaration(CDeclarationSpecifiers(List(CTypeInteger)), CDeclareIdentifier("argc")), CNormalDeclaration(CDeclarationSpecifiers(List(CTypeInteger, CConst)), CPointerDeclarator(CPointer(None, None), CDeclareArray(CDeclareIdentifier("argv"), None))))
  
  def generateLocalDeclaration(t: CDeclarationSpecifiers, i: String, v: CPrimaryExpression) = CLocalDeclaration(t, List(CDeclaratorWithAssign(CDeclareIdentifier(i), CExpressionInitializer(v))))
  def generateFunction(returnType: CDeclarationSpecifiers, name: String, params: List[CParameterDeclaration], body: CCompoundStmt) = CFunctionDec(Some(returnType), CParameterList(CDeclareIdentifier(name), params), None, body)
  def generateMain(body: CCompoundStmt) = generateFunction(CDeclarationSpecifiers(List(CTypeInteger)), "main", mainParams, body)
}

trait Test extends App with CCompileAndRun with TestFunctions {
  def test: Prog
}

object MainFunctionTest extends Test {
  val globalDecs = CGlobalDeclaration(CDeclarationSpecifiers(List(CTypeFloat)), List(CDeclaratorWithAssign(CDeclareIdentifier("a"), CExpressionInitializer(CConstantFloat(0))), CDeclaratorWithAssign(CDeclareIdentifier("b"), CExpressionInitializer(CConstantInteger(1))))) 
  
  val abBody = CCompoundStmt(List(CReturn(Some(CBinaryPrim(CBinaryPlus, CAccessIdentifier("a"), CAccessIdentifier("b"))))))
  val ab = generateFunction(CDeclarationSpecifiers(List(CTypeInteger)), "ab", List(), abBody)
  
  val mainBody = CCompoundStmt(List(CExpressionStmt(Some(CCall(CAccessIdentifier("printf"), List(CCharArray("%d"), CCall(CAccessIdentifier("ab"), List()))))), CReturn(Some(CConstantInteger(0)))))
  val main = generateMain(mainBody)
  
  val ast = CProgram(List(CPreprocessorInstruction(CIncludeGlobal("stdio.h")), globalDecs, ab, main))
  
  def test = compile(ast)
    
  println(test)
}

object IfTest extends Test {
  val x = generateLocalDeclaration(CDeclarationSpecifiers(List(CTypeInteger)), "x", CConstantInteger(0))
  val ifStmt = CIf(CBinaryPrim(CBinaryEquality, CAccessIdentifier("x"), CConstantInteger(2)), CExpressionStmt(Some(CCall(CAccessIdentifier("printf"), List(CCharArray("Yay"))))))
  val mainBody = CCompoundStmt(List(x, ifStmt, CReturn(Some(CConstantInteger(0)))))
  val ast = CProgram(List(CPreprocessorInstruction(CIncludeGlobal("stdio.h")), generateMain(mainBody)))
  
  def test = compileAndRun(ast)
  
  println(test)
}

object IfElseTest extends Test {
  val x = generateLocalDeclaration(CDeclarationSpecifiers(List(CTypeInteger)), "x", CConstantInteger(0))
  val trueBranch = CExpressionStmt(Some(CCall(CAccessIdentifier("printf"), List(CCharArray("Yay")))))
  val falseBranch = CExpressionStmt(Some(CCall(CAccessIdentifier("printf"), List(CCharArray("Boo")))))
  val ifElseStmt = CIfElse(CBinaryPrim(CBinaryEquality, CAccessIdentifier("x"), CConstantInteger(2)), trueBranch, falseBranch)
  val mainBody = CCompoundStmt(List(x, ifElseStmt, CReturn(Some(CConstantInteger(0)))))
  val ast = CProgram(List(CPreprocessorInstruction(CIncludeGlobal("stdio.h")), generateMain(mainBody)))
  
  def test = compileAndRun(ast)
  
  println(test)
}

object SwitchTest extends Test {
  val x = generateLocalDeclaration(CDeclarationSpecifiers(List(CTypeInteger)), "x", CConstantInteger(0))
  val case0 = CCaseStmt(CConstantInteger(0), CCompoundStmt(List(CReturn(Some(CConstantInteger(0))))))
  val case1 = CCaseStmt(CConstantInteger(1), CCompoundStmt(List(CReturn(Some(CConstantInteger(1))))))
  val switchBody = CCompoundStmt(List(case0, case1))
  val switchStmt = CSwitch(CAccessIdentifier("x"), switchBody)
  val mainBody = CCompoundStmt(List(x, switchStmt, CReturn(Some(CConstantInteger(0)))))
  val ast = CProgram(List(CPreprocessorInstruction(CIncludeGlobal("stdio.h")), generateMain(mainBody)))
  
  def test = compileAndRun(ast)
  
  println(test)
}

object WhileTest extends Test {
  val i = generateLocalDeclaration(CDeclarationSpecifiers(List(CTypeInteger)), "i", CConstantInteger(0))
  val whileBody = CCompoundStmt(List(CExpressionStmt(Some(CCall(CAccessIdentifier("printf"), List(CCharArray("%i\\n"), CAccessIdentifier("i"))))), CExpressionStmt(Some(CPostfixIncrement(CAccessIdentifier("i"))))))
  val whileStmt = CWhile(CBinaryPrim(CBinaryLessThan, CAccessIdentifier("i"), CConstantInteger(2)), whileBody)
  val mainBody = CCompoundStmt(List(i, whileStmt, CReturn(Some(CConstantInteger(0)))))
  val ast = CProgram(List(CPreprocessorInstruction(CIncludeGlobal("stdio.h")), generateMain(mainBody)))
  
  def test = compileAndRun(ast)
  
  println(test)
}

object DoWhileTest extends Test {
  val i = generateLocalDeclaration(CDeclarationSpecifiers(List(CTypeInteger)), "i", CConstantInteger(0))
  val doWhileBody = CCompoundStmt(List(CExpressionStmt(Some(CCall(CAccessIdentifier("printf"), List(CCharArray("%i\\n"), CAccessIdentifier("i"))))), CExpressionStmt(Some(CPostfixIncrement(CAccessIdentifier("i"))))))
  val doWhileStmt = CDoWhile(doWhileBody, CBinaryPrim(CBinaryLessThanOrEquals, CAccessIdentifier("i"), CConstantInteger(2)))
  val mainBody = CCompoundStmt(List(i, doWhileStmt, CReturn(Some(CConstantInteger(0)))))
  val ast = CProgram(List(CPreprocessorInstruction(CIncludeGlobal("stdio.h")), generateMain(mainBody)))
  
  def test = compileAndRun(ast)
  
  println(test)
}

object ForTest extends Test {
  val i = generateLocalDeclaration(CDeclarationSpecifiers(List(CTypeInteger)), "i", CConstantInteger(0))
  val forInit = CAssign(CAccessIdentifier("i"), CEquals, CConstantInteger(10))
  val forCond = CBinaryPrim(CBinaryGreaterThanOrEquals, CAccessIdentifier("i"), CConstantInteger(0))
  val forInc = CPrefixDecrement(CAccessIdentifier("i"))
  val forBody = CCompoundStmt(List(CExpressionStmt(Some(CCall(CAccessIdentifier("printf"), List(CCharArray("%i\\n"), CAccessIdentifier("i")))))))
  val forStmt = CFor(Some(forInit), Some(forCond), Some(forInc), forBody)
  val mainBody = CCompoundStmt(List(i, forStmt, CReturn(Some(CConstantInteger(0)))))
  val ast = CProgram(List(CPreprocessorInstruction(CIncludeGlobal("stdio.h")), generateMain(mainBody)))
  
  def test = compileAndRun(ast)
  
  println(test)
}

object ArrayTest extends Test {
  val a = CLocalDeclaration(CDeclarationSpecifiers(List(CTypeChar)), List(CDeclaratorWrap(CDeclareArray(CDeclareIdentifier("a"), Some(CConstantInteger(10))))))
  val assign = CExpressionStmt(Some(CAssign(CAccessIndex(CAccessIdentifier("a"), CConstantInteger(0)), CEquals, CConstantChar('Y'))))
  val print = CExpressionStmt(Some(CCall(CAccessIdentifier("printf"), List(CCharArray("%c\\n"), CAccessIndex(CAccessIdentifier("a"), CConstantInteger(0))))))
  val mainBody = CCompoundStmt(List(a, assign, print))
  val ast = CProgram(List(CPreprocessorInstruction(CIncludeGlobal("stdio.h")), generateMain(mainBody)))
  
  def test = compileAndRun(ast)
  
  println(test)
}

object PointerTest extends Test {
  val p = CLocalDeclaration(CDeclarationSpecifiers(List(CTypeChar)), List(CDeclaratorWrap(CPointerDeclarator(CPointer(None, None), CDeclareIdentifier("p")))))
  val pp = CLocalDeclaration(CDeclarationSpecifiers(List(CTypeChar)), List(CDeclaratorWrap(CPointerDeclarator(CPointer(Some(CPointer(None, None)), None), CDeclareIdentifier("pp")))))
  val malloc = CCall(CAccessIdentifier("malloc"), List(CSizeofTypeName(CTypeName(CTypeSpecifierQualifier(CTypeChar, None), None))))
  val assignP = CExpressionStmt(Some(CAssign(CAccessIdentifier("p"), CEquals, CCast(CTypeName(CTypeSpecifierQualifier(CTypeChar, None), Some(CAbstractPointer(CPointer(None, None)))), malloc))))
  val assignPP = CExpressionStmt(Some(CAssign(CAccessIdentifier("pp"), CEquals, CUnaryPrim(CAddress, CAccessIdentifier("p")))))
  val assignPP2 = CExpressionStmt(Some(CAssign(CUnaryPrim(CDeref, CAccessIdentifier("pp")), CEquals, CAccessIdentifier("p"))))
  val mainBody = CCompoundStmt(List(p, pp, assignP, assignPP, assignPP2))
  val ast = CProgram(List(CPreprocessorInstruction(CIncludeGlobal("stdlib.h")), generateMain(mainBody)))

  def test = compileAndRun(ast)
  
  println(test)
}

object StorageTest extends Test {
  val auto = CLocalDeclaration(CDeclarationSpecifiers(List(CAuto, CTypeInteger)), List(CDeclaratorWrap(CDeclareIdentifier("a"))))
  val register = CLocalDeclaration(CDeclarationSpecifiers(List(CRegister, CTypeInteger)), List(CDeclaratorWrap(CDeclareIdentifier("r"))))
  val static = CLocalDeclaration(CDeclarationSpecifiers(List(CStatic, CTypeInteger)), List(CDeclaratorWrap(CDeclareIdentifier("s"))))
  val extern = CLocalDeclaration(CDeclarationSpecifiers(List(CExtern, CTypeInteger)), List(CDeclaratorWrap(CDeclareIdentifier("e"))))
  val typedef = CLocalDeclaration(CDeclarationSpecifiers(List(CTypedef, CTypeInteger)), List(CDeclaratorWrap(CDeclareIdentifier("t"))))
  
  val mainBody = CCompoundStmt(List(auto, register, static, extern, typedef))
  
  val ast = CProgram(List(CPreprocessorInstruction(CIncludeGlobal("stdlib.h")), generateMain(mainBody)))
  
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
  
  //Testing CCall of function in scope
  def multipleFunctions: Program = {
    val locVar = LocalVariable(TypeInteger, "testVar")
    val assign = Assign(AccessVariable("testVar"), ConstantInteger(2))
    val locVar2 = LocalVariableWithAssign(TypeInteger, "testVar1", ConstantInteger(3))
    
    val statements = List(Dec(locVar), Stmt(ExpressionStatement(assign)), Dec(locVar2))
    val testFun = CFunctionDec(Some(TypeInteger), "testFunction", List((TypePointer(TypeInteger), "input")), statements)
    
    val funcCCall = CCall("testFunction", List(ConstantInteger(2)))
    val mainStatements = List(Stmt(ExpressionStatement(funcCCall)))
    val mainFunc = generateMainFunction(mainStatements)
    
    Program(List(testFun, mainFunc))

  }
  //println(generate(multipleFunctions, getEmptyVarEnv, getEmptyFunEnv))
  
  //Testing redefinition of function
  def redefinitionOfFunction: Program = {
    
    val testFun = CFunctionDec(Some(TypeInteger), "testFunction", List((TypePointer(TypeInteger), "input")), List())
    val testFun2 = CFunctionDec(Some(TypeInteger), "testFunction", List((TypePointer(TypeInteger), "input")), List())
    
    val funcCCall = CCall("testFunction", List(ConstantInteger(2)))
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



