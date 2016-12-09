open Mono.Cecil
open Mono.Cecil.Cil

type EdgeKind = Calls | Implements | Inherits | Offers | Accepts | Returns | Holds

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

//type Dependency = 
//  { source : TypeDefinition
//    kind : EdgeKind
//    target : TypeReference }

type Edge<'T> = 
  { kind : EdgeKind 
    tgt : 'T }
and 
  TypeNode = 
  { name : string
    ``namespace`` : string 
    typeDef : TypeDefinition
    edges : Edge<TypeNode> list }

let typeReferredByInstruction (inst : Instruction) : Edge<string> option = 
   match inst.OpCode.Code with
   | Code.Call -> 
     match inst.Operand with
     | :? GenericInstanceMethod as gim -> 
       printfn "generic instance method"
       //gim.GenericArguments |> printfn 
     | :? MethodDefinition as mdef ->
       printfn "method definition"
     | :? MethodReference as mref ->
       printfn "method reference"
     | _ ->
       printfn "something entirely different or similar?"
   | _ ->
     ()
   None

let rec typesFromTypeReference (typeRef : TypeReference) : TypeReference list = 
  match typeRef with
     | :? GenericInstanceType as genericInstanceType -> 
       // printfn "generic instance type with element type %s" genericInstanceType.ElementType.FullName
       [ typeRef ]
     | :? ArrayType as arrayType ->
       // Unwrap array
       typesFromTypeReference arrayType.ElementType
     | :? TypeDefinition as typeDefinition ->
       //printfn "type definition"
       [ typeRef ]
     | _ ->
       [ typeRef ]

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
  let flops = methodDef.Body.Instructions |> Seq.map (fun inst -> typeReferredByInstruction inst) |> Seq.toList
  []

let typesReferredByMethod (methodDef : MethodDefinition) : DependencyEdge list = 
  let fromParameters = typesReferredByMethodParameters methodDef
  let fromReturnType = typesReferredByMethodReturnType methodDef
  let fromMethodBody = if methodDef.HasBody then typesReferredByMethodBody methodDef else []
  let result = fromParameters @ fromReturnType @ fromMethodBody
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
  let fromFields = classDef.Fields |> Seq.collect (fun fieldDef -> typesReferredByField fieldDef) |> Seq.toList
  let edges = fromInterfaces @ fromBaseClass @ fromMethods @ fromFields
  let deps = edges |> List.groupBy (fun edge -> edge.target.FullName) |> List.map (fun (tgt, edges) -> { target = (edges |> List.head |> (fun it -> it.target)); kinds = collapseEdges edges })
  { source = classDef
    dependencies = deps }

let typesReferredByInterfaceDefinition (interfaceDef : TypeDefinition) : Edge<string> list = 
  let fromMethods = interfaceDef.Methods |> Seq.collect (fun methodDef -> typesReferredByMethod methodDef) |> Seq.toList
  let fromFields = interfaceDef.Fields |> Seq.collect (fun fieldDef -> typesReferredByField fieldDef) |> Seq.toList
  []

let (|ClassDefinition|_|) (typeDef : TypeDefinition) =
   if typeDef.IsClass then Some (ClassDefinition typeDef) else None

let (|InterfaceDefinition|_|) (typeDef : TypeDefinition) =
   if typeDef.IsInterface then Some (InterfaceDefinition typeDef) else None

let (|EnumDefinition|_|) (typeDef : TypeDefinition) =
   if typeDef.IsEnum then Some (EnumDefinition typeDef) else None

let getTypeDependencies (typeDef : TypeDefinition) : Edge<string> list = 
   match typeDef with
   | ClassDefinition cd -> []
   | InterfaceDefinition id -> typesReferredByInterfaceDefinition id
   | EnumDefinition ed -> []
   | _ -> []

[<EntryPoint>]
let main argv = 
    let dll = argv.[0]
    let asm = AssemblyDefinition.ReadAssembly(dll)
    let types = asm.MainModule.Types

    //types |> Seq.iter (fun typeDef -> printfn "%A" typeDef.Name; typeDef.CustomAttributes |> Seq.iter (fun attr -> printfn "  -> %A" attr.AttributeType.Name))
    let allClasses = types |> Seq.filter (fun typeDef -> typeDef.IsClass) |> Seq.toList
    let genClasses = allClasses |> Seq.filter (fun typeDef -> typeDef.HasCustomAttributes && typeDef.CustomAttributes |> Seq.exists (fun a -> a.AttributeType.Name = "CompilerGeneratedAttribute"))
    let classes = allClasses |> Seq.except genClasses |> Seq.toList
    let interfaces = types |> Seq.filter (fun typeDef -> typeDef.IsInterface) |> Seq.toList
    let enums = types |> Seq.filter (fun typeDef -> typeDef.IsEnum) |> Seq.toList

    printfn "Classes"
    printfn "-------"
    classes |> Seq.iter (fun it -> printfn "%s" it.Name)
    printfn ""

    let foo = classes |> Seq.map (fun cd -> typesReferredByClass cd) |> Seq.toList

    let formatWeightedKind wk = 
      match wk with
      | {kind = k; weight = w} -> (k, w)

    foo |> List.iter (fun it -> printfn "type definition: %A" it.source; it.dependencies |> List.iter (fun d -> printfn " -> %s %A" d.target.FullName (d.kinds |> List.map (fun k -> formatWeightedKind k))))

    printfn "lol %d" foo.Length

    printfn "Interfaces"
    printfn "----------"
    interfaces |> Seq.iter (fun it -> printfn "%s" it.Name)
    printfn ""

    printfn "Enums"
    printfn "-----"
    enums |> Seq.iter (fun it -> printfn "%s" it.Name)
    printfn ""

    let addTypeDependencyEdges typeNode typeNodes = 
      typeNode

    let typeNodes = types |> Seq.map (fun typeDef -> { name = typeDef.Name 
                                                       ``namespace`` = typeDef.Namespace
                                                       typeDef = typeDef
                                                       edges = List.empty })

    let typeNodes' = typeNodes |> Seq.map (fun typeNode -> addTypeDependencyEdges typeNode typeNodes)

    0 // return an integer exit code
