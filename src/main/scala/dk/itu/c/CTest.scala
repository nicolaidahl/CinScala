package dk.itu.c


object Test extends CCompileAndRun with App {
  def buildInt(i: Int) = {
    ConstantExpr(GeneralExpr(CastExpr(UnaryExpr(PostfixExpr(PrimaryExpr(ConstantInteger(i)))))))
  }
  
  val globalDecs = GlobalDeclaration(CDeclaration(CDeclarationSpecifiers(None, TypeInteger, None), List(DeclaratorWithAssign(CDeclarator(None, DeclareIdentifier("a")), ExpressionInitializer(buildInt(0))), DeclaratorWithAssign(CDeclarator(None, DeclareIdentifier("b")), ExpressionInitializer(buildInt(2)))))) 
  val f = CFunctionDec(Some(CDeclarationSpecifiers(None, TypeInteger, None)), CDeclarator(None, DeclareIdentifier("xy")), None, CompoundStmt(List(Stmt(ExpressionStmt(Some(buildInt(3)))))))
  
  val ast = CProgram(List(globalDecs, f))
    
  println(compileAndRun(ast))
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



