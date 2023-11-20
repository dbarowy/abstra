module AST

type Preset = 
| Greyscale
| Rainbow

type Color =
| Red
| Orange
| Yellow
| Green
| Blue
| Indigo
| Purple
| G1
| G2
| G3
| G4
| G5
| G6

type Scheme = 
| Colors of Color list
| Preset of Preset

type Expr = Expression of int * Scheme * int

type Coordinate = { x: int; y: int }
type Shape = { pts: Coordinate list; color: Color }
type Canvas = Shape list