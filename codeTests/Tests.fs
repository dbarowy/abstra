namespace codeTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Shapes

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestInIntersects () =
        let line1 = {p1 = { x=2; y=2 }; p2 = { x=4; y=4 }}
        let line2 = {p1 = { x=3; y=2 }; p2 = { x=3; y=4 }}
        let line3 = {p1 = { x=5; y = 5 }; p2 = { x=5; y=10 }}
        Assert.IsTrue(intersect line1 line2)
        Assert.IsFalse(intersect line1 line3)


