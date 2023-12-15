module Shapes
open AST
open System
open MathNet.Numerics.Distributions

let r = new Random()
let CANVAS_SZ = 800
let avgLen = 400
let lenDist = new Normal(avgLen, 20)

let direction (a: Coordinate) (b: Coordinate) (c: Coordinate): int = 
    let x = (b.y - a.y) * (c.x - b.x) - (b.x - a.x) * (c.y - b.y)
    if (x = 0) then
        0
    elif (x < 0) then
        2
    else
        1

let intersect (lA: Line) (lB: Line): bool = 
    let d1 = direction lA.p1 lA.p2 lB.p1
    let d2 = direction lA.p1 lA.p2 lB.p2
    let d3 = direction lB.p1 lB.p2 lA.p1
    let d4 = direction lB.p1 lB.p2 lA.p2
    if (d1 <> d2 && d3 <> d4) then
        true
    else 
        false

let rec lineIntersetsWithShape (line: Line) (pts: Coordinate List): bool = 
    match pts.Length with
    | 0 -> false
    | 1 -> false
    | n -> 
        let line2 = { p1 = pts[0]; p2 = pts[1] }
        if (intersect line line2) then
            true
        else
            lineIntersetsWithShape line pts[1..]

let rec isAdjacent (ptsA: Coordinate List) (ptsB: Coordinate List): bool = 
    match ptsA.Length with
    | 0 -> false
    | 1 -> false
    | n -> 
        let line = { p1 = ptsA[0]; p2 = ptsA[1] }
        if (lineIntersetsWithShape line ptsB) then
            true
        else
            isAdjacent ptsA[1..] ptsB
        
        
let rec forbiddenColors (newShape: Shape) (shapes: Shape List) (forbiddenCs: Color List): Color List = 
    match shapes.Length with
    | 0 -> forbiddenCs
    | n -> 
        let shape = shapes[0]
        if (isAdjacent ((newShape.pts[(newShape.pts.Length - 1)])::newShape.pts) ((shape.pts[((shape.pts).Length - 1)])::shape.pts)) && (not (List.contains shape.color forbiddenCs)) then
            shape.color :: forbiddenColors newShape shapes[1..] (forbiddenCs)
        else
            forbiddenColors newShape shapes[1..] forbiddenCs


// want angle to be between avgAngle / 3 and 180
let checkAngle (midPt) (A) (B) (n): bool =
    let angle = abs(atan2 (float (A.y - midPt.y)) (float (A.x - midPt.x)) - atan2 (float (B.y - midPt.y)) (float (B.x - midPt.x)))
    let avgAngle : float = ((float (n - 2)) * 180.0 / float n) * (Math.PI / 180.0)
    (angle > (avgAngle / 3.0) && angle < (Math.PI))


let rec generateNextPoint (midPt : Coordinate) (A : Coordinate) (n: int): Coordinate =
    // in radians
    let avgAngle : float = ((float (n - 2)) * 180.0 / float n) * Math.PI / 180.0
    let angleDist = new Normal(avgAngle, 0.2)
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
    

        
