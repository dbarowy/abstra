namespace codeTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open AST
open Shapes

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestInShape () =
        let pts = [ { x=2; y=2 }; { x=3; y=10 }; { x=9; y=8 }; { x = 11; y=4} ]
        let shape: Shape = { color = Red; pts =pts }
        let ptIn = { x=4; y=5 }
        Assert.IsTrue(inShape ptIn shape)
        let ptLeft = { x=1; y=5 }
        Assert.IsFalse(inShape ptLeft shape)
        let ptAbove = {x = 5; y = 20}
        Assert.IsFalse(inShape ptAbove shape)


