open System

open Mono.Cecil
open Mono.Cecil.Cil

open Chiron

type EdgeKind = Calls | Implements | Inherits | Offers | Accepts | Returns | Holds | Annotates | Exposes | Creates

type DependencyEdge = 
  { kind : EdgeKind 
    target : TypeReference }

type WeightedKind = 
  { kind : EdgeKind
    weight : int }

type TargetDependency = 
  { target : TypeReference
    kinds : WeightedKind list }

type TypeDependencies =
  { source : TypeDefinition
    dependencies : TargetDependency list }

type GraphElement = 
  { name: string
    ``namespace``: string }

  static member ToJson (x : GraphElement) = json {
    do! Json.write "name" x.name
    do! Json.write "namespace" x.``namespace``
  }

type GraphNode = 
  { source: GraphElement
    edges: GraphEdge list }
 
  static member ToJson (x : GraphNode) = json {
    do! Json.write "source" x.source
    do! Json.write "edges" x.edges
}
and GraphEdge = 
  { target: GraphElement }
  
  static member ToJson (x : GraphEdge) = json {
    do! Json.write "target" x.target
  } 

let rec typesFromTypeReference (typeRef : TypeReference) : TypeReference list = 
  match typeRef with
     | :? GenericInstanceType as genericInstanceType -> 
       let gen = 
         if genericInstanceType.HasGenericArguments then
           genericInstanceType.GenericArguments |> Seq.filter (fun arg -> not arg.IsGenericParameter) |> Seq.collect (fun arg -> arg |> typesFromTypeReference) |> Seq.toList
         else
           []
       let result = [ typeRef ] @ gen
       result
     | :? ArrayType as arrayType ->
       // Unwrap array
       typesFromTypeReference arrayType.ElementType
     | :? TypeDefinition as typeDefinition ->
       [ typeRef ]
     | _ ->
       [ typeRef ]

let typesReferredByCallInstruction (inst : Instruction) : DependencyEdge list = 
  let maybeDeclaringType : TypeReference option = 
    match inst.Operand with
    | :? GenericInstanceMethod as gim -> 
      Some gim.DeclaringType
    | :? MethodDefinition as mdef ->
      let tref = mdef.DeclaringType :> TypeReference
      Some tref
    | :? MethodReference as mref ->
      Some mref.DeclaringType 
    | _ ->
      printfn "something entirely different or similar?"
      None
  match maybeDeclaringType with
  | Some declaringType -> declaringType |> typesFromTypeReference |> List.map (fun tgt -> { kind = Calls; target = tgt })
  | None -> []

let typesReferredByNewobjInstruction (inst : Instruction) : DependencyEdge list = 
  let maybeDeclaringType : TypeReference option = 
    match inst.Operand with
    | :? GenericInstanceMethod as gim -> 
      Some gim.DeclaringType
    | :? MethodDefinition as mdef ->
      let tref = mdef.DeclaringType :> TypeReference
      Some tref
    | :? MethodReference as mref ->
      Some mref.DeclaringType 
    | _ ->
      printfn "something entirely different or similar?"
      None
  match maybeDeclaringType with
  | Some declaringType -> declaringType |> typesFromTypeReference |> List.map (fun tgt -> { kind = Creates; target = tgt })
  | None -> []

let typeReferredByInstruction (inst : Instruction) : DependencyEdge list = 
   match inst.OpCode.Code with
   | Code.Call -> 
     typesReferredByCallInstruction inst
   | Code.Calli -> 
     typesReferredByCallInstruction inst
   | Code.Callvirt -> 
     typesReferredByCallInstruction inst
   | Code.Newobj ->
     typesReferredByNewobjInstruction inst
   | _ ->
     []

let typesReferredByField (fieldDef : FieldDefinition) : DependencyEdge list = 
  let fieldType = fieldDef.FieldType
  fieldType |> typesFromTypeReference |> List.map (fun tgt -> { kind = Holds; target = tgt })

let typesReferredByParameter (p : ParameterDefinition) : DependencyEdge list = 
  let typeRefs = typesFromTypeReference p.ParameterType
  typeRefs |> List.map (fun tr -> { kind = Accepts; target = tr})

let typesReferredByMethodParameters (methodDef : MethodDefinition) : DependencyEdge list = 
  methodDef.Parameters |> Seq.collect (fun p -> typesReferredByParameter p) |> Seq.toList

let typesReferredByMethodReturnType (methodDef : MethodDefinition) : DependencyEdge list = 
  methodDef.ReturnType |> typesFromTypeReference |> List.map (fun tr -> { kind = Returns; target = tr })

let typesReferredByMethodBody (methodDef : MethodDefinition) : DependencyEdge list = 
  let result = methodDef.Body.Instructions |> Seq.collect (fun inst -> typeReferredByInstruction inst) |> Seq.toList
  result

let typesReferredByMethod (methodDef : MethodDefinition) : DependencyEdge list = 
  let fromParameters = typesReferredByMethodParameters methodDef
  let fromReturnType = typesReferredByMethodReturnType methodDef
  let fromMethodBody = if methodDef.HasBody then typesReferredByMethodBody methodDef else []
  let result = fromParameters @ fromReturnType @ fromMethodBody
  result

let typesReferredByProperty (propDef : PropertyDefinition) : DependencyEdge list = 
  let fromPropertyAccessor (methodDef : MethodDefinition) = 
    match methodDef with
    | null -> []
    | md when md.HasBody -> typesReferredByMethodBody methodDef
    | md -> []
  let fromGet = propDef.GetMethod |> fromPropertyAccessor
  let fromSet = propDef.SetMethod |> fromPropertyAccessor
  let fromPropType = propDef.PropertyType |> typesFromTypeReference |> List.map (fun tr -> { kind = Exposes; target = tr })
  let result = fromPropType @ fromGet @ fromSet
  result

let collapseEdges (edges : DependencyEdge list) : WeightedKind list = 
  edges |> List.groupBy (fun e -> e.kind) |> List.map (fun (kind, group) -> { kind = kind; weight = group |> List.length })

let typesReferredByClass (classDef : TypeDefinition) : TypeDependencies =
  let fromInterfaces : DependencyEdge list = 
    classDef.Interfaces 
    |> Seq.collect (fun interfaceRef -> [ { kind = EdgeKind.Implements; target = interfaceRef } ] ) 
    |> Seq.toList
  let fromBaseClass : DependencyEdge list = 
    match classDef.BaseType with
    | null -> []
    | t -> typesFromTypeReference t |> List.map (fun tgt -> { kind = EdgeKind.Inherits; target = tgt })
  let fromMethods = classDef.Methods |> Seq.collect (fun methodDef -> typesReferredByMethod methodDef) |> Seq.toList
  let fromProperties = classDef.Properties |> Seq.collect (fun propDef -> typesReferredByProperty propDef) |> Seq.toList
  let fromFields = classDef.Fields |> Seq.collect (fun fieldDef -> typesReferredByField fieldDef) |> Seq.toList
  let edges = fromInterfaces @ fromBaseClass @ fromMethods @ fromProperties @ fromFields
  let deps = edges |> List.groupBy (fun edge -> edge.target.FullName) |> List.map (fun (tgt, edges) -> { target = (edges |> List.head |> (fun it -> it.target)); kinds = collapseEdges edges })
  { source = classDef
    dependencies = deps }

let typesReferredByInterface (interfaceDef : TypeDefinition) : TypeDependencies =
  let fromInterfaces : DependencyEdge list = 
    interfaceDef.Interfaces 
    |> Seq.collect (fun interfaceRef -> [ { kind = EdgeKind.Implements; target = interfaceRef } ] ) 
    |> Seq.toList
  let fromMethods = interfaceDef.Methods |> Seq.collect (fun methodDef -> typesReferredByMethod methodDef) |> Seq.toList
  let fromProperties = interfaceDef.Properties |> Seq.collect (fun propDef -> typesReferredByProperty propDef) |> Seq.toList
  let fromFields = interfaceDef.Fields |> Seq.collect (fun fieldDef -> typesReferredByField fieldDef) |> Seq.toList
  let edges = fromInterfaces @ fromMethods @ fromProperties @ fromFields
  let deps = edges |> List.groupBy (fun edge -> edge.target.FullName) |> List.map (fun (tgt, edges) -> { target = (edges |> List.head |> (fun it -> it.target)); kinds = collapseEdges edges })
  { source = interfaceDef
    dependencies = deps }

let (|ClassDefinition|_|) (typeDef : TypeDefinition) =
   if typeDef.IsClass then Some (ClassDefinition typeDef) else None

let (|InterfaceDefinition|_|) (typeDef : TypeDefinition) =
   if typeDef.IsInterface then Some (InterfaceDefinition typeDef) else None

let (|EnumDefinition|_|) (typeDef : TypeDefinition) =
   if typeDef.IsEnum then Some (EnumDefinition typeDef) else None

let stripDots (name : string) : string = 
  name.Replace(".", "")

let stripTick (name : string) : string = 
  name.Replace("`", "")

let dotFriendly (name : string) : string = 
  name |> stripDots |> stripTick

let getLinesForType (typeDeps : TypeDependencies) = 
  match typeDeps with
  | { source = s; dependencies = deps } ->
    let srcName = s.FullName |> dotFriendly
    let edges = deps |> List.map (fun it -> sprintf "%s -> %s;" srcName it.target.FullName |> dotFriendly)
    let shape = if s.IsEnum then "diamond" else if s.IsInterface then "ellipse" else "box"
    let node = sprintf "%s [label = \"%s\" shape = \"%s\"];" srcName (s.Name |> dotFriendly) shape
    node :: edges

let getModuleTypes (m : ModuleDefinition) = 
  m.Types |> Seq.filter (fun typeDef -> not <| typeDef.FullName.Equals("<Module>"))

let getAssemblyTypes (assemblyPath : string) = 
  let asm = AssemblyDefinition.ReadAssembly(assemblyPath)
  asm.Modules |> Seq.collect (fun m -> getModuleTypes m)

[<EntryPoint>]
let main argv = 
    let modules = argv |> Seq.filter (fun it -> it.EndsWith(".dll") || it.EndsWith(".exe"))
    let excludes = argv |> Seq.except modules |> Seq.filter (fun it -> it.StartsWith("!")) |> Seq.map (fun it -> it.Substring(1))

    let exclude (t : TypeDefinition) : bool =
      (excludes |> Seq.contains t.Name) || (excludes |> Seq.contains t.FullName)
    let types = modules |> Seq.collect (fun dll -> getAssemblyTypes dll) |> Seq.filter (fun t -> not (exclude t))

    let allClasses = types |> Seq.filter (fun typeDef -> typeDef.IsClass) |> Seq.toList
    let genClasses = allClasses |> Seq.filter (fun typeDef -> typeDef.HasCustomAttributes && typeDef.CustomAttributes |> Seq.exists (fun a -> a.AttributeType.Name = "CompilerGeneratedAttribute"))
    let enums = types |> Seq.filter (fun typeDef -> typeDef.IsEnum) |> Seq.toList
    let interfaces = types |> Seq.filter (fun typeDef -> typeDef.IsInterface) |> Seq.toList
    let classes = allClasses |> Seq.except genClasses |> Seq.except enums |> Seq.except interfaces |> Seq.toList

    let typeList = classes @ interfaces @ enums
    let typeNames = typeList |> List.map (fun t -> t.FullName)

    let classTypeDependencies = classes |> Seq.map (fun cd -> typesReferredByClass cd) |> Seq.toList
    let interfaceTypeDependencies = interfaces |> Seq.map (fun def -> typesReferredByInterface def) |> Seq.toList
    let enumTypeDependencies = enums |> Seq.map (fun def -> { source = def; dependencies = [] }) |> Seq.toList

    let formatWeightedKind wk = 
      match wk with
      | {kind = k; weight = w} -> (k, w)

    let filteredClassTypeDependencies = 
      classTypeDependencies 
      |> List.map (fun { source = s; dependencies = deps } -> { source = s; dependencies = deps |> List.filter (fun d -> typeNames |> List.contains d.target.FullName) |> List.filter (fun d -> not (d.target.FullName.Equals(s.FullName)) ) })

    let filteredInterfaceTypeDependencies = 
      interfaceTypeDependencies 
      |> List.map (fun { source = s; dependencies = deps } -> { source = s; dependencies = deps |> List.filter (fun d -> typeNames |> List.contains d.target.FullName) |> List.filter (fun d -> not (d.target.FullName.Equals(s.FullName)) ) })

    let allTypeDeps = filteredClassTypeDependencies @ filteredInterfaceTypeDependencies @ enumTypeDependencies
    let lines = allTypeDeps |> List.collect (fun typeDef -> typeDef |> getLinesForType)
    let indentedLines = lines |> List.map (fun line -> sprintf "  %s" line)
    let oneBigLine = indentedLines |> String.concat "\n"
    let allLines = "digraph g {" :: (indentedLines @ [ "}" ])
    let graph = allLines |> String.concat Environment.NewLine
    printfn "%s" graph

    let toGraphElement (typeDef : TypeDefinition) : GraphElement = 
      { name = typeDef.Name 
        ``namespace`` = typeDef.Namespace }

    let toEdge (d : TargetDependency) : GraphEdge = 
      { target = { name = d.target.Name; ``namespace`` = d.target.Namespace } }

    let toEdges (deps : TargetDependency list) : GraphEdge list = 
      deps |> List.map toEdge
      
    let nodes = allTypeDeps |> List.map (fun { source = s; dependencies = ds } -> { source = toGraphElement s; edges = toEdges ds })

    let json = nodes |> Json.serialize |> Json.formatWith JsonFormattingOptions.Compact

    //printfn "%s" json

    0
