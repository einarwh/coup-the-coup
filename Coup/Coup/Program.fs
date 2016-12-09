open Mono.Cecil
open Mono.Cecil.Cil

type EdgeKind = Calls | Inherits | Offers | Accepts | Holds

type Edge = 
  { kind : EdgeKind 
    target : TypeNode }
and 
  TypeNode = 
  { name : string
    ``namespace`` : string 
    typeDef : TypeDefinition
    edges : Edge list }

let typeReferredByInstruction (inst : Instruction) : Edge option = 
   match inst.OpCode.Code with
   | Code.Call -> 
     match inst.Operand with
     | :? GenericInstanceMethod as gim -> 
       printfn "generic instance method"
     | :? MethodDefinition as mdef ->
       printfn "method definition"
     | :? MethodReference as mref ->
       printfn "method reference"
     | _ ->
       printfn "something entirely different or similar?"
   | _ ->
     ()
   None

let typesReferredByField (fieldDef : FieldDefinition) (typeNodes : TypeNode list) : Edge list = 
  let fieldType = fieldDef.FieldType
  []

let typesReferredByMethod (methodDef : MethodDefinition) (typeNodes : TypeNode list) : Edge list = 
  printfn "types referred by method."
  printfn "has body? %s" <| if methodDef.HasBody then "true" else "false"
  let methodBody = methodDef.Body
  let lol = 
    if methodDef.HasBody then
      methodBody.Instructions |> Seq.map (fun inst -> typeReferredByInstruction inst) |> Seq.toList
    else
      List.empty
  let returnType = methodDef.ReturnType
  []

let typesReferredByInterfaceReference (interfaceRef : TypeReference) (typeNodes : TypeNode list) : Edge list = 
  []

let typesReferredByClass (classDef : TypeDefinition) (typeNodes : TypeNode list) : Edge list =
  printfn "!!!types referred by class"
  let fromInterfaces = classDef.Interfaces |> Seq.collect (fun interfaceRef -> typesReferredByInterfaceReference interfaceRef typeNodes) |> Seq.toList
  printfn "class %s implements %d interfaces" classDef.Name fromInterfaces.Length
  let fromMethods = classDef.Methods |> Seq.collect (fun methodDef -> typesReferredByMethod methodDef typeNodes) |> Seq.toList
  let fromFields = classDef.Fields |> Seq.collect (fun fieldDef -> typesReferredByField fieldDef typeNodes) |> Seq.toList
  fromInterfaces @ fromMethods @ fromFields

let typesReferredByInterfaceDefinition (interfaceDef : TypeDefinition) (typeNodes : TypeNode list) : Edge list = 
  let fromMethods = interfaceDef.Methods |> Seq.collect (fun methodDef -> typesReferredByMethod methodDef typeNodes) |> Seq.toList
  let fromFields = interfaceDef.Fields |> Seq.collect (fun fieldDef -> typesReferredByField fieldDef typeNodes) |> Seq.toList
  fromMethods @ fromFields

let (|ClassDefinition|_|) (typeDef : TypeDefinition) =
   if typeDef.IsClass then Some (ClassDefinition typeDef) else None

let (|InterfaceDefinition|_|) (typeDef : TypeDefinition) =
   if typeDef.IsInterface then Some (InterfaceDefinition typeDef) else None

let (|EnumDefinition|_|) (typeDef : TypeDefinition) =
   if typeDef.IsEnum then Some (EnumDefinition typeDef) else None

let getTypeDependencies (typeDef : TypeDefinition) (typeNodes : TypeNode list) : Edge list = 
   match typeDef with
   | ClassDefinition cd -> []
   | InterfaceDefinition id -> typesReferredByInterfaceDefinition id typeNodes
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

    let foo = classes |> Seq.map (fun cd -> typesReferredByClass cd []) |> Seq.toList

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
