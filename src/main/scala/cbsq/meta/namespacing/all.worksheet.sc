



import cbsq.meta.util.IntendedLocale


import cbsq.meta.namespacing.byLowerCasedIdentifiers

import cbsq.meta.namespacing.concatToCamelCase




import scala.language.unsafeNulls




given IntendedLocale = {
   IntendedLocale(java.util.Locale.ROOT)
}



val s001 = byLowerCasedIdentifiers(IndexedSeq("reachability", "fence" ) )

val s002 = s001.concatToCamelCase()

