module Parser
open AST
open Combinator



(*
 *   <expr> ::= <num> shapes, scheme: <scheme>, max <num>
 *    <num> ::= (is any positive integer)
 * <scheme> ::= <preset>
             |  <color>^{4+}
 *  <color> ::= red | green | blue | purple
 *)

let usage() =
    printfn "Usage: dotnet run <filename.txt>"
    printfn "A picture will be generated based on the instructions in the file"
    exit 1

let pad p = pbetween pws0 p pws0

let pNum = (pmany1 pdigit) |>> (fun e -> List.fold (fun acc n -> acc + string n) "" e) |>> int
let pShapes = pleft (pNum) (pstr " shapes, scheme: ")
let pColor : Parser<Scheme>= 
    pmany1 (
    (pstr "red" |>> (fun _ -> Red)) <|>
    (pstr "green" |>> (fun _ -> Green)) <|>
    (pstr "blue" |>> (fun _ -> Blue)) <|>
    (pstr "purple" |>> (fun _ -> Purple))) |>> (fun l -> Colors(l))
let pPreset : Parser<Scheme> = 
    (pstr "greyscale" |>> (fun _ -> Preset(Greyscale))) <|>
    (pstr "rainbow" |>> (fun _ -> Preset(Rainbow)))
let pScheme : Parser<Scheme> = pPreset <|> pColor 
// let pShapesScheme = pseq pShapes pScheme
let pMaxEdges = pbetween (pstr ", max ") pNum (pstr " edges")

let expr, exprImpl = recparser()

exprImpl := (pseq (pseq pShapes pScheme (fun (n, s) -> (n, s))) (pMaxEdges) (fun (t, n) -> 
    let (x, s) = t
    Expression(x, s, n)))

let grammar = pleft expr peof

let parse (input: string) : Expr option =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None