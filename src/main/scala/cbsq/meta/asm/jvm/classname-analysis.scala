package cbsq.meta.asm.jvm














/**
 * 
 * provides method to analyse class-names,
 * preferably as-complete-as `java.lang.Class`
 * 
 */
// sealed
type ClassNamesAnalyser 
      <: FullyQualifiedClassNameAnalyser



/**
 * 
 * for *class-names returned by `java.lang.Class.prototype.getName`*,
 * hence these two methods
 * 
 * - `getCanonicalName`
 * - `getSimpleName`
 * 
 */
trait FullyQualifiedClassNameAnalyser extends
   AnyRef
{
   
   extension (name0: org.objectweb.asm.Type) {
      
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

   extension (name0: org.objectweb.asm.Type) {
      
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












