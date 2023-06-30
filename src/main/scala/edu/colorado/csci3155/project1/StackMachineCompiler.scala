package edu.colorado.csci3155.project1

object StackMachineCompiler {



    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stack machine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = 
        e match{
            //:: is for item concat ::: is for list concat
            case Const(f) => { List(PushI(f)) }
            case Ident(id) => { List(StoreI(id))}
            case Plus(e1, e2) => { 
                val l1 = compileToStackMachineCode(e1)
                val l2 = compileToStackMachineCode(e2)
                val instr = List(AddI) //Need list to make this work
                //ran into issues with ++ and List().flatten
                l1:::l2:::instr
            }
            case Minus(e1, e2) => { 
                val l1 = compileToStackMachineCode(e1)
                val l2 = compileToStackMachineCode(e2)
                val instr = List(SubI)
                l1:::l2:::instr
            }
            case Mult(e1, e2) => { 
                val l1 = compileToStackMachineCode(e1)
                val l2 = compileToStackMachineCode(e2)
                val instr = List(MultI)
                l1:::l2:::instr
            }
            case Div(e1, e2) => {                 
                val l1 = compileToStackMachineCode(e1)
                val l2 = compileToStackMachineCode(e2)
                val instr = List(DivI)
                l1:::l2:::instr
            }
            case Exp(e) => {
                val l1 = compileToStackMachineCode(e)
                val instr = List(ExpI)
                l1:::instr
            }
            case Log(e) => {
                val l1 = compileToStackMachineCode(e)
                val instr = List(LogI)
                l1:::instr
            }
            case Sine(e) => {
                val l1 = compileToStackMachineCode(e)
                val instr = List(SinI)
                l1:::instr
            }
            case Cosine(e) => {
                val l1 = compileToStackMachineCode(e)
                val instr = List(CosI)
                l1:::instr
            }
            case Let(ident, e1, e2) => {
                val l1 = compileToStackMachineCode(e1)
                val l2 = compileToStackMachineCode(e2)
                val instr = List(LoadI(ident))
                l1:::instr:::l2
            }
        }
}
