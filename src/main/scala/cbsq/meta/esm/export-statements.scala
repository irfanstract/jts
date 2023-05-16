package cbsq.meta.esm
















type ExportAllAssignmentKindEnum = (
   ExportAllAssignmentKindEnumImpl[?]
)

trait ExportAllAssignmentKindEnumImpl[V <: (Product & Matchable)] {

   /**
    * 
    * denotes the `export = Bar ;` variant
    * 
    */
   val whenExportsBeThis : V

   /**
    * 
    * denotes the `export default Bar ;` variant
    * 
    */
   val whenExportDefaultBeThis : V

   /**
    * 
    * denotes the `declare module global { ... }` variant(s)
    * 
    */
   val OfDeclareGlobal: (Nothing => V )

   /**
    * 
    * every possible instance
    * 
    */
   type CV = V

}


























