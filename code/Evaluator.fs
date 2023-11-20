module Evaluator
open AST
open System

let r = new Random()
let CANVAS_SZ = 400

let evalColor (color: Color) : string =
    match color with
    | Red -> "rgb(255,0,0)"
    | Orange -> "rgb(255,127,0)"
    | Yellow -> "rgb(255,255,0)"
    | Green -> "rgb(0,255,0)"
    | Blue -> "rgb(0,0,255)"
    | Indigo -> "rgb(75,0,130)"
    | Purple -> "rgb(148,0,211)"
    | G1 -> "rgb(0,0,0)"
    | G2 -> "rgb(50,50,50)"
    | G3 -> "rgb(100,100,100)"
    | G4 -> "rgb(150,150,150)"
    | G5 -> "rgb(200,200,200)"
    | G6 -> "rgb(250,250,250)"

let rec evalScheme (scheme: Scheme) : Color list =
    match scheme with
    | Preset(Greyscale) -> [ G1; G2; G3; G4; G5; G6 ]
    | Preset(Rainbow) -> [ Red; Orange; Yellow; Green; Blue; Indigo; Purple]
    | Colors([]) -> []
    | Colors(x::xs) -> x :: evalScheme (Colors(xs))

let rec generatePoints (e: int) : Coordinate list = 
    match e with
    | 0 -> []
    | n -> { x = r.Next(CANVAS_SZ); y = r.Next(CANVAS_SZ); } :: generatePoints (e - 1)

let createShape (maxE: int) (scheme: Scheme): Shape = 
    let cs = evalScheme scheme
    let n = cs.Length
    let i = r.Next(n)
    let c = cs[i]
    let numEdges = r.Next(3, maxE)
    { pts = (generatePoints numEdges); color = c }

let rec evalExpr (expr: Expr) : Canvas = 
    match expr with
    | Expression(0, s, e) -> []
    | Expression(n, s, e) -> (createShape e s) :: (evalExpr (Expression(n-1, s, e)))

let evalShape (shape: Shape) : string = 
    let ptstr = List.fold (fun acc e -> acc + string e.x + ", " + string e.y + " ") "" shape.pts
    let color = string shape.color
    "<polygon fill=\"" + color + "\" points=\"" + ptstr + "\"/>\n"

let rec evalCanvas (canvas: Canvas) : string =
    match canvas with
    | [] -> ""
    | s::ss -> (evalShape s) + (evalCanvas ss)

let eval (expr: Expr) : string = 
    let csz = CANVAS_SZ |> string
    let scheme = 
        match expr with
        | Expression(_, scheme, _) -> scheme
    let cs = evalScheme scheme
    let n = cs.Length
    let i = r.Next(n)
    let c = string cs[i]
    "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
    "<rect width=\"" + csz + "\" height=\"" + csz + "\"" +
    " fill=\"" + c + "\" />" +
    (evalCanvas (evalExpr expr))
    + "</svg>\n"