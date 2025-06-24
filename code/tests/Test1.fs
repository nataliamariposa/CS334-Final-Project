namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open AST
open Evaluator

[<TestClass>]
type Test1 () =
    let rec findSlnDirectory (dir: string) : string =
        let slnFiles = System.IO.Directory.GetFiles(dir, "*.sln")
        if slnFiles.Length > 0 then dir
        else
            let parent = System.IO.Directory.GetParent(dir)
            if obj.ReferenceEquals(parent, null) then
                raise (System.Exception(sprintf "Failed to locate .sln file starting from %s" dir))
            else
                findSlnDirectory parent.FullName

    let readTestFile (filename: string) : string =
        let cwd = System.IO.Directory.GetCurrentDirectory()
        let slnDir = findSlnDirectory cwd
        let path = System.IO.Path.Combine(slnDir, "tests", "testdata", filename)
        if System.IO.File.Exists(path) then
            System.IO.File.ReadAllText(path)
        else
            raise (System.IO.FileNotFoundException(sprintf "Test file not found at expected location: %s" path))


//tests the parser (test1)
    [<TestMethod>]
    member this.SimpleParserTest () =
        let input = "player 1 {\"dealer\", [(diamond, 1),(diamond, 2)]}"
        let result = parse input
        match result with 
        | Some actual_ast ->
            let expected_ast = Seq [
                PlayerDef { 
                    IdNum = 1 
                    Deck = { 
                        Name = "dealer" 
                        Cards = [
                            { Suit = Diamond; Number = 1}; 
                            { Suit = Diamond; Number = 2}]}}]
            Assert.AreEqual<Expr>(expected_ast, actual_ast)
        | None -> Assert.IsTrue(false)

//tests the evaluator (test2)
    [<TestMethod>]
    member this.TestMethodPassing () =
        let input = readTestFile "test2.boul"
        let result = parse input
        match result with
        | Some actual_ast ->
            let actual_result, _ = eval actual_ast Map.empty
            let expected_result = Num 3
            Assert.AreEqual<Expr>(expected_result, actual_result)
        | None -> Assert.IsTrue(false)

//tests the sum function (test3)
    [<TestMethod>]
    member this.SumFunctionTest () =
        let input = readTestFile "test3.boul"
        let result = parse input
        match result with
        | Some actual_ast ->
            let actual_result, _ = eval actual_ast Map.empty
            let expected_result = Num 3
            Assert.AreEqual<Expr>(expected_result, actual_result)
        | None -> Assert.IsTrue(false)

//tests the hasCards function (test4)
    [<TestMethod>]

    member this.HasCardsFunctionTest () =
        let input = readTestFile "test4.boul"
        let result = parse input
        match result with
        | Some actual_ast ->
            let actual_result, _ = eval actual_ast Map.empty
            let expected_result = Bool false
            Assert.AreEqual<Expr>(expected_result, actual_result)
        | None -> Assert.IsTrue(false)

//tests the firstcard function (test5)
    [<TestMethod>]

    member this.FirstCardFunctionTest () =
        let input = readTestFile "test5.boul"
        let result = parse input
        match result with
        | Some actual_ast ->
            let actual_result, _ = eval actual_ast Map.empty
            let expected_result: Expr = CardDef { Suit = Diamond; Number = 1}
            Assert.AreEqual<Expr>(expected_result, actual_result)
        | None -> Assert.IsTrue(false)