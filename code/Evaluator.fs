module Evaluator
open AST
open System
open Shapes

// prevent color overlay (check all points in new shape and see if already colored)
// Num data type?

// done:
// should check values are valid
// make a minimum angle bw points(will have to construct shape point by point)
// make unique background color
// fix parsing of color lists

// questions:
// how to do color check??
// generate points first then randomly add lines then do color check
// maintain list of shapes
// as you build each shape, check every shape below it
// if adjacent(one of either shape's points is in the other)
// if covers(all previous shape within new shape), ignore
// if no colors left, then just pick any color

let mutable shapes: Shape List = []

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

let checkExpr (expr: Expr) : bool =
    match expr with
    | Expression(n, s, e) ->
        n > 0 && e >= 3

let rec evalScheme (scheme: Scheme) : Color list =
    match scheme with
    | Preset(Greyscale) -> [ G1; G2; G3; G4; G5; G6 ]
    | Preset(Rainbow) -> [ Red; Orange; Yellow; Green; Blue; Indigo; Purple]
    | Colors([]) -> []
    | Colors(x::xs) -> x :: evalScheme (Colors(xs))

let rec validColors (colors: Color List) (bColors: Color List) = 
    match colors.Length with
    | 0 -> []
    | n -> 
        let color = colors[0]
        if (not (List.contains color bColors)) then
            color::(validColors colors[1..] bColors)
        else
            validColors colors[1..] bColors

let getColorList (idealColors: Color List) (scheme: Scheme): Color List = 
    if idealColors.Length = 0 then
        evalScheme scheme
    else
        idealColors
let rec createShape (maxE: int) (scheme: Scheme) (bc: Color): Shape = 
    let numEdges = r.Next(3, maxE)
    let pts = generatePoints numEdges numEdges
    let placeholder = { pts = pts; color = Red }
    let bcs: Color List = forbiddenColors placeholder shapes [bc]
    printfn "%A" bcs
    let cs = getColorList (validColors (evalScheme scheme) (bcs)) scheme
    let n = cs.Length
    let i = r.Next(n)
    let c = cs[i]
    let newShape = { pts = pts; color = c }
    shapes <- newShape::shapes
    newShape
    

let rec evalExpr (expr: Expr) (bc: Color): Canvas = 
    match expr with
    | Expression(0, s, e) -> []
    | Expression(n, s, e) -> (createShape e s bc) :: (evalExpr (Expression(n-1, s, e)) bc)

let evalShape (shape: Shape) : string = 
    let ptstr = List.fold (fun acc e -> acc + string e.x + ", " + string e.y + " ") "" shape.pts
    let color = evalColor shape.color
    "<polygon fill=\"" + color + "\" points=\"" + ptstr + "\"/>\n"

let rec evalCanvas (canvas: Canvas) : string =
    match canvas with
    | [] -> ""
    | s::ss -> (evalShape s) + (evalCanvas ss)

let eval (expr: Expr) : string = 
    if not (checkExpr expr) then
        printfn "Invalid program"
        ""
    else 
        let csz = CANVAS_SZ |> string
        let scheme = 
            match expr with
            | Expression(_, scheme, _) -> scheme
        let cs = evalScheme scheme
        let n = cs.Length
        let i = r.Next(n)
        let c = evalColor cs[i]
        "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
        " xmlns=\"http://www.w3.org/2000/svg\"" +
        " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
        "<rect width=\"" + csz + "\" height=\"" + csz + "\"" +
        " fill=\"" + c + "\" />" +
        (evalCanvas (evalExpr expr cs[i]))
        + "</svg>\n"