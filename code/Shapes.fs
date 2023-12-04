module Shapes
open AST
open System
open MathNet.Numerics.Distributions

let r = new Random()
let CANVAS_SZ = 800
let avgLen = 200
let lenDist = new Normal(avgLen, 20)


// want angle to be between avgAngle / 3 and 180
let checkAngle (midPt) (A) (B) (n): bool =
    let angle = abs(atan2 (float (A.y - midPt.y)) (float (A.x - midPt.x)) - atan2 (float (B.y - midPt.y)) (float (B.x - midPt.x)))
    let avgAngle : float = ((float (n - 2)) * 180.0 / float n) * (Math.PI / 180.0)
    (angle > (avgAngle / 3.0) && angle < (Math.PI))


let rec generateNextPoint (midPt : Coordinate) (A : Coordinate) (n: int): Coordinate =
    // in radians
    let avgAngle : float = ((float (n - 2)) * 180.0 / float n) * Math.PI / 180.0
    let angleDist = new Normal(avgAngle, 0.4)
    let ang = angleDist.Sample()
    let len = lenDist.Sample()
    if (ang > (avgAngle / 3.0) && ang < (Math.PI)) && (len > 0 && len < CANVAS_SZ) then
        let prevAngle = atan2 (float(A.y - midPt.y)) (float(A.x - midPt.x))
        let newX = midPt.x + int(len * cos(ang + prevAngle))
        let newY = midPt.y + int(len * sin(ang + prevAngle))
        { x = newX; y = newY }
    else
        generateNextPoint midPt A n

let generateSecondPoint (A : Coordinate) : Coordinate =
    let angle = float(r.Next(0, 180)) * Math.PI / 180.0
    let len = lenDist.Sample();
    let newX = A.x + int(len * cos(angle))
    let newY = A.y + int(len * cos(angle))
    { x = newX; y = newY }

let rec generatePoints (e: int) (totalE): Coordinate list = 
    match e with
    | 0 -> []
    | 1 -> 
        { x = r.Next(CANVAS_SZ); y = r.Next(CANVAS_SZ); } :: generatePoints (e - 1) (totalE)
    | 2 -> 
        let prevPts = generatePoints (1) (totalE)
        (generateSecondPoint prevPts[0]) :: prevPts
    | n -> 
        let prevPts = generatePoints (e - 1) (totalE)
        let midPt = prevPts[0]
        let A = prevPts[1]
        (generateNextPoint midPt A totalE) :: prevPts
    

        
