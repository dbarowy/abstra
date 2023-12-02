module Shapes
open AST
open System
open MathNet.Numerics.Distributions

let r = new Random()
let CANVAS_SZ = 800


// want angle to be between avgAngle / 3 and 180
let checkAngle (midPt) (A) (B) (n): bool =
    let angle = abs(atan2 (float (A.y - midPt.y)) (float (A.x - midPt.x)) - atan2 (float (B.y - midPt.y)) (float (B.x - midPt.x)))
    let avgAngle : float = ((float (n - 2)) * 180.0 / float n) * (Math.PI / 180.0)
    (angle > (avgAngle / 3.0) && angle < (Math.PI))


let rec generateNextPoint (midPt : Coordinate) (A : Coordinate) (n: int): Coordinate =
    // in radians
    let avgAngle : float = ((float (n - 2)) * 180.0 / float n) * Math.PI / 180.0
    let angleDist = new Normal(avgAngle, 0.1)
    let lenDist = new Normal(100, 20)
    let ang = angleDist.Sample()
    let len = lenDist.Sample()
    if (ang > (avgAngle / 3.0) && ang < (Math.PI)) && (len > 0 && len < CANVAS_SZ) then
        let prevAngle = atan2 (float(A.y - midPt.y)) (float(A.x - midPt.x))
        let newX = midPt.x + int(len * cos(ang + prevAngle))
        let newY = midPt.y + int(len * sin(ang + prevAngle))
        { x = newX; y = newY }
    else
        generateNextPoint midPt A n



let rec generateLastPoint (pA) (pB) (nA) (nB) (n)= 
    let x = r.Next(CANVAS_SZ)
    let y = r.Next(CANVAS_SZ)
    let newPt = { x = x; y = y }
    let avgAngle : float = ((float (n - 2)) * 180.0 / float n) * (Math.PI / 180.0)
    if (checkAngle pB pA newPt n) &&
        (checkAngle nB nA newPt n) then
        newPt
    else 
        generateLastPoint pA pB nA nB n

let rec generatePoints (e: int) (totalE): Coordinate list = 
    match e with
    | 0 -> []
    | 1 -> 
        { x = r.Next(CANVAS_SZ); y = r.Next(CANVAS_SZ); } :: generatePoints (e - 1) (totalE)
    | 2 -> 
        { x = r.Next(CANVAS_SZ); y = r.Next(CANVAS_SZ); } :: generatePoints (e - 1) (totalE)
    // | _ when e = totalE ->
    //     let otherPts = generatePoints (e - 1) (totalE)
    //     let prevLineB = otherPts[0]
    //     let prevLineA = otherPts[1]
    //     let nextLineB = otherPts[totalE - 2]
    //     let nextLineA = otherPts[totalE - 3]
    //     (generateLastPoint prevLineA prevLineB nextLineA nextLineB totalE) :: otherPts
    | n -> 
        let prevPts = generatePoints (e - 1) (totalE)
        let midPt = prevPts[0]
        let A = prevPts[1]
        (generateNextPoint midPt A totalE) :: prevPts
    

        
