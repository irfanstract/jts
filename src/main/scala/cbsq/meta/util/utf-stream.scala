package cbsq.meta.util



















trait PwEmitter
// extends
// `Pwe%!`
{
   
   def printTo(o: java.io.PrintWriter) : Unit

   // opaque type SpecialString <: String = String

   // /**
   //  * 
   //  * the string repr --
   //  * runs `printTo`
   //  * 
   //  */
   // override
   // def toString(): SpecialString = {
   //    val o = new java.io.StringWriter
   //    printTo((
   //       new java.io.PrintWriter(o, true)
   //    ))
   //    o.toString()
   // }

}
// trait Pwes 
// { this : PwEmitter =>
// 
//    type SpecialString <: String
//    
//    override
//    def toString(): SpecialString
// 
// }
// abstract class `Pwe%!`
// { this : PwEmitter =>

// }
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
      new AnyRef with PwEmitter {

         def printTo(o: java.io.PrintWriter) : Unit = {
            f(o)
         }

         type SpecialString >: String <: String

         /**
          * 
          * the string repr --
          * runs `printTo`
          * 
          */
         override
         def toString(): SpecialString = {
            val o = new java.io.StringWriter
            printTo((
               new java.io.PrintWriter(o, true)
            ))
            o.toString()
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



























