package cbsq.meta.asm.jvmc












/**
 * 
 * provides
 * facilities to translate JVM type-names into the associated target-platform repr(s) 
 * 
 * ```
 * // JS
 * return (
 *   java.lang.System.getProperty("os.arch")
 *   || ARCH_NOT_KNOWN
 * ) ;
 * 
 * // Scala
 * (
 *   java.lang.System.getProperty("os.arch")
 *   ?? archs.archUnknown
 * )
 * 
 * // Python
 * return (
 *    java.lang.System.get_property("os.arch")
 *    or ARCH_NOT_KNOWN
 * )
 * 
 * // Bash
 * return (
 *    (java_lang_System_getProperty "os.arch" )
 *    or (ARCH_NOT_KNOWN )
 * )
 * 
 * ```
 * 
 */
trait ClassNamesCompiler extends
   AnyRef
{

   extension (name0: ow.Type) {

      /**
       * 
       * compile a/the function-body-level references to the `namespace`
       * 
       * ```
       * // JS
       * return (
       *   java.lang.System.getProperty("os.arch")
       *   || ARCH_NOT_KNOWN
       * ) ;
       * 
       * // Scala
       * (
       *   java.lang.System.getProperty("os.arch")
       *   ?? archs.archUnknown
       * )
       * 
       * // Python
       * return (
       *    java.lang.System.get_property("os.arch")
       *    or ARCH_NOT_KNOWN
       * )
       * 
       * // Bash
       * return (
       *    (java_lang_System_getProperty "os.arch" )
       *    or (ARCH_NOT_KNOWN )
       * )
       * 
       * ```
       * 
       */
      def compileInlineLevelRef(): String

   }
   
}















