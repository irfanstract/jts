package cbsq.meta.util



















@deprecatedInheritance("subject to change")
trait PwEmitter
extends
`Pwe%!`
{
   
   override 
   def printTo(o: java.io.PrintWriter) : Unit

}

@deprecatedInheritance("subject to change")
abstract class `Pwe%!` protected ()
{

   def printTo(o: java.io.PrintWriter) : Unit

   /**
    * 
    * runs `printTo` and
    * return the resulting `String`
    * 
    */
   inline
   def buildToString(): String = {
      val o = new java.io.StringWriter
      printTo((
         new java.io.PrintWriter(o, true)
      ))
      o.toString()
   }

   /**
    * 
    * the string repr --
    * runs `buildToString`
    * 
    */
   override
   inline
   def toString(): String = {
      buildToString()
   }

}

object PwEmitter {

   // type `%!` = `Pwe%!`

   extension (this1: PwEmitter) {
      
      def ++(that: PwEmitter) : PwEmitter = {

         PwEmitter((o: java.io.PrintWriter) => {
            this1 printTo o
            that  printTo o
         })
         
      }

      def +(that: PwEmitter) : PwEmitter = {
         this1 ++ that
      }

   }

   type PwuF = (
      (o: java.io.PrintWriter)
      => Unit
   )

   def apply(f: PwuF) : PwEmitter = {
      through(f)
   }

   def through(f: PwuF) : PwEmitter = {
      new PwEmitter {

         def printTo(o: java.io.PrintWriter) : Unit = {
            f(o)
         }

      }
   }

   lazy val empty = {
      through(_ => {})
   }

   def whichRuns(f: PwuF) : PwEmitter = {
      through(f)
   }

   def whichPrints(v: => String): PwEmitter = {
      through(o => (
         println(v)
      ))
   }

   def whichPrintsExactly(v: String): PwEmitter = {
      whichPrints(v)
   }

}



























