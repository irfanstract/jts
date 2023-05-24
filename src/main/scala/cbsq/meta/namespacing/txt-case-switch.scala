package cbsq.meta.namespacing

















export cbsq.meta.util.IntendedLocale





   
opaque type IdentList <: Matchable 
   = IndexedSeq[String]

/* the canonical form shall be the all-uppercased form */


/**
 * 
 * an instance, given the *ident-list*.
 * may perform checking/validation.
 * 
 */
def byLowerCasedIdentifiers(v: IndexedSeq[String])(using IntendedLocale) : IdentList = {
   import scala.language.unsafeNulls

   v
   .map(_.toUpperCase(summon[IntendedLocale] ) )
   
}





// private 
extension (words: IdentList) {
         
         def concatToCamelCase()(using locale: IntendedLocale): String = {
            
                  import scala.language.unsafeNulls

                  import scala.jdk.CollectionConverters.*

                  words
                  .map(_.toLowerCase(locale ) )
                  .map(w => {
                     w.toIndexedSeq
                     .zipWithIndex
                     .map({
                        case (chr, i) =>
                           if i <= 0 then 
                              chr.toUpper
                           else chr
                     })
                     .mkString("")
                  })
                  .mkString("")
                  .zipWithIndex
                  .map({
                     case (chr, i) =>
                        if i <= 0 then
                           chr.toLower
                        else chr
                  })
                  .mkString("")
            
         }

}
















































































