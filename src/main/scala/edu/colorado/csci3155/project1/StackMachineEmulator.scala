package edu.colorado.csci3155.project1


/* -- Here are all the instructions to be supported --*/
sealed trait StackMachineInstruction
case class LoadI(s: String) extends StackMachineInstruction
case class  StoreI(s: String) extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {



    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: List[Double],
                                 env: Map[String, Double],
                                 ins: StackMachineInstruction): (List[Double], Map[String, Double]) = {
        // TODO: Your code here.
        ins match{
            case LoadI(s) => {
                  stack match {
                        case (List()) => throw new IllegalArgumentException("load failed: stack empty")
                        case (a::tail) => {
                              val newEnv = env + (s->a)
                              val newStack = tail
                              (newStack, newEnv)
                        }
                  }
            }
            case StoreI(s) => {
                  val storeVal = env.get(s)
                  storeVal match{
                        case None => {throw new IllegalArgumentException("store failed: identifier not found")}
                        case _ => {
                              val store:Double = storeVal.getOrElse(0)
                              val newStack = store::stack
                              (newStack,env)
                        }
                  }
            }
            case PushI(f) => {
                  val newStack = f::stack
                  (newStack, env)
            }
            case AddI => {
                  stack match{
                        case (a::tail) => {
                              val add = a
                              tail match {
                                    case (List()) => {throw new IllegalArgumentException("addition failed: insufficient elements in stack")}
                                    case (b::tail) => {
                                          val newValue = b+add
                                          val newStack = newValue::tail
                                          (newStack, env)
                                    }
                                    case _ => {throw new java.lang.IllegalArgumentException("addition failed")}
                              }
                        }
                        case _ => {throw new java.lang.IllegalArgumentException("addition failed: stack empty")}
                  }
            }
            case SubI => {
                  stack match{
                        case (a::tail) => {
                              val sub = a
                              tail match {
                                    case (List()) => {throw new IllegalArgumentException("subtraction failed: insufficient elements in stack")}
                                    case (b::tail) => {
                                          val newValue = b-sub
                                          val newStack = newValue::tail
                                          (newStack, env)
                                    }
                                    case _ => {throw new java.lang.IllegalArgumentException("subtraction failed")}
                              }
                        }
                        case _ => {throw new java.lang.IllegalArgumentException("subtraction failed: stack empty")}
                  }
            }
            case MultI => {
                  stack match{
                        case (a::tail) => {
                              val mult = a
                              tail match {
                                    case (List()) => {throw new IllegalArgumentException("multiplication failed: insufficient elements in stack")}
                                    case (b::tail) => {
                                          val newValue = b*mult
                                          val newStack = newValue::tail
                                          (newStack, env)
                                    }
                                    case _ => {throw new java.lang.IllegalArgumentException("multiplication failed")}

                              }
                        }
                        case _ => {throw new java.lang.IllegalArgumentException("multiplication failed: stack empty")}                 }
            }
            case DivI => {
                  stack match{
                        case (0::tail) => {throw new IllegalArgumentException("division failed: cannot divide by 0")}
                        case (a::tail) => {
                              val denom = a
                              tail match {
                                    case (List()) => {throw new IllegalArgumentException("division failed: insufficient elements in stack")}
                                    case (b::tail) => {
                                          val newValue = b/denom
                                          val newStack = newValue::tail
                                          (newStack, env)
                                    }
                                    case _ => {throw new java.lang.IllegalArgumentException("division failed: stack empty")}

                              }
                        }
                        case _ => {throw new java.lang.IllegalArgumentException("division failed")}
                  }
            }
            case ExpI => {
                  stack match{
                        case (List()) => throw new IllegalArgumentException("exponential failed: stack empty")
                        case (a::tail) =>{
                              val newEntry = math.exp(a)
                              val newStack = newEntry::tail
                              (newStack, env)
                        }
                  }
            }
            case LogI => {
                  stack match{
                        case (List()) => throw new IllegalArgumentException("log failed: stack empty")
                        case (a::tail) =>{
                              if (a<=0) {throw new IllegalArgumentException("log failed: input must be greater than 0")}
                              else {
                                    val newEntry = math.log(a)
                                    val newStack = newEntry::tail
                                    (newStack, env)
                              }
                        }
                  }
            }
            case SinI => {
                  stack match{
                        case (List()) => throw new IllegalArgumentException("sine failed: stack empty")
                        case (a::tail) =>{
                              val newEntry = math.sin(a)
                              val newStack = newEntry::tail
                              (newStack, env)
                        }
                  }
            }
            case CosI => {
                  stack match{
                        case (List()) => throw new IllegalArgumentException("cosine failed: stack empty")
                        case (a::tail) =>{
                              val newEntry = math.cos(a)
                              val newStack = newEntry::tail
                              (newStack, env)
                        }
                  }
            }
            case PopI => {
                  stack match{
                        case(List()) => throw new IllegalArgumentException("pop failed: stack empty")
                        case(a::tail) => {
                              val newStack = tail
                              (newStack, env)
                        }
                  }
            }
      }
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be the final environment.

       Hint: accumulator for foldLeft must be a tuple (List[Double], Map[String,Double])
             initial value of this accumulator must be (Nil, Map.empty)
             You should use emulateSingleInstruction to update the accmulator.
             It will all fit nicely once you figure it out.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Map[String, Double] =
        {
            val finalEnv = instructionList.foldLeft((Nil, Map.empty))
                  ((acc:(List[Double], Map[String,Double]), inst:StackMachineInstruction) => {
                        val curStack = acc._1
                        val curEnv = acc._2
                        emulateSingleInstruction(curStack,curEnv, inst)})
            finalEnv
        }

        //    val least = denoms.foldLeft(1)((y, x) => lcm(y,x))
}