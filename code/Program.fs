open Parser
open Evaluator
open AST
open System
open Shapes

[<EntryPoint>]
let main args = 
  if args.Length <> 1 then
    usage()
    0
  else
    let file = args[0]
    let text = IO.File.ReadAllText file
    match parse text with
      | Some ast ->
          let svg = eval ast
          printfn "%s" svg
          0
      | None ->
          printfn "Invalid program."
          1


  