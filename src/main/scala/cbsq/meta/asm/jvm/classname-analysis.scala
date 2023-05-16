package cbsq.meta.asm.jvm














trait ClassNamesAnalyser extends
   AnyRef
{
   
   extension (name0: ow.Type) {
      
         /**
          * 
          * the simple-name
          * .
          * analogue to
          * `getSimpleName` in `j.l.Class`
          * 
          */
         def getSimpleName(): String

   }

   extension (name0: ow.Type) {
      
         /**
          * 
          * "substitutes every path-separator with period-sign"
          * .
          * analogue to
          * `getCanonicalName` in `java.lang.Class`
          * 
          */
         def getCanonicalName(): String

   }

}












