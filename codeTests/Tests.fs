namespace codeTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Shapes
open Parser
open Evaluator

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestParser () =
        let text = "4 shapes, scheme: red blue purple green, max 3 edges"
        match parse text with
        | Some ast ->
            let svg = ast
            let test = Expression(4, Colors [Red; Blue; Purple; Green], 3)
            Assert.AreEqual(svg, test)
        | None ->
            printfn "Invalid program."
            Assert.Fail()

    [<TestMethod>]
    member this.testInterpreter () =
        let expr = Expression(4, Colors [Red; Blue; Purple; Green], 3)
        let svg = eval expr
        ()

    [<TestMethod>]
    member this.TestPointGenerator() =
        let pts = generatePoints 6 6
        Assert.AreEqual(6, pts.Length)

    [<TestMethod>]
    member this.TestInIntersects () =
        let line1 = {p1 = { x=2; y=2 }; p2 = { x=4; y=4 }}
        let line2 = {p1 = { x=3; y=2 }; p2 = { x=3; y=4 }}
        let line3 = {p1 = { x=5; y = 5 }; p2 = { x=5; y=10 }}
        Assert.IsTrue(intersect line1 line2)
        Assert.IsFalse(intersect line1 line3)


