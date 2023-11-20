open Parser
open Evaluator
open AST
open System

[<EntryPoint>]
let main args = 
  let points = generatePoints 6
  // List.map (fun c -> 
  //   let x = c.x
  //   let y = c.y
  //   printfn "%d" x
  //   printfn "%d" y
  //   0) (points) |> ignore


  // let shape = { pts = points; color = Red}
  // let canvas : Canvas = [shape;]

  // // 10 shapes, rainbow scheme, 6 edges
  // let expr = Expression(10, Preset(Rainbow), 6)

  // let csz = CANVAS_SZ |> string
  // let s =
  //   "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
  //   " xmlns=\"http://www.w3.org/2000/svg\"" +
  //   " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
  //   (evalCanvas (evalExpr expr))
  //   + "</svg>\n"
  // printfn "%s" s

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