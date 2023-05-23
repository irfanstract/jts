package cbsq.meta.asm.jsgen















object RewrittenJumpingInstruction
{

}



/**
 * 
 * "the style of the rewritten free-goto flow"
 * 
 */
abstract class RewrittenJumpingBasedFlowStyle
{
   
}
object RewrittenJumpingBasedFlowStyle
{

   /**
    * 
    * this style
    * pre-allocates the necessary `let`s and
    * is based on
    * a loop as shown below
    * 
    * ```
    * code
    * 
    * let localVar1: any = null ;
    * let localVar2: any = null ;
    * let localVar3: any = null ;
    * ...
    * ...
    * 
    * loop1 :
    * for (let nextBranch: int = 1 ; ; ) {
    * 
    *    switch (nextBranch) {
    * 
    *       case 1 :
    *          ...
    *          ...
    *          // from JVM opcode 'goto ...'
    *          nextBranch = ... ; continue loop1 ;
    * 
    *       case 5 :
    *          ...
    *          ...
    *          // from JVM opcode 'ifeq ...'
    *          if (...) {
    *             nextBranch = ... ; continue loop1 ;
    *          }
    *          ...
    *          ...
    *          // from JVM opcode 'goto ...'
    *          nextBranch = ... ; continue loop1 ;
    * 
    *       case 7 :
    *          ...
    *          ...
    *          // from JVM opcode '*return'
    *          return ... ;
    * 
    *       case 9 : 
    *          // it had 'ExceptionHandler'
    *          try {
    *             ...
    *             ...
    *             // from JVM opcode 'goto ...'
    *             nextBranch = ... ; continue loop1 ;
    *  
    *          } 
    *          catch (z) {
    *             S_exception = z ;
    * 
    *             // go to the appropriate block
    *             nextBranch = ... ; continue loop1 ;
    * 
    *          }
    * 
    *       ...
    *       ...
    *       ...
    * 
    *       default :
    *          throw AssertionError("unexpected branch-id") ;
    * 
    *    }
    * 
    *    throw AssertionError("missing explicit 'continue'") ;
    * }
    * ```
    * 
    */
   case class OfSwitchLoop()
   extends
   RewrittenJumpingBasedFlowStyle

}














































