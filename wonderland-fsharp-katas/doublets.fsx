// See the file doublets.md for detailed information.

open System
open System.IO

let wordsPath = Path.Combine (__SOURCE_DIRECTORY__,"resources","words.txt")
let words = File.ReadAllLines wordsPath

type Word = string

let areWordsLinked (w1: Word) (w2: Word) =
    let matchingChars = 
        [0 .. w1.Length - 1] 
        |> List.fold (fun acc i -> if w1.[i] = w2.[i] then acc + 1 else acc) 0
    matchingChars = w1.Length - 1

let doublets (w1:Word,w2:Word) = 
    if w1.Length = w2.Length then
        let wordsOfLength = 
            words 
            |> Array.toList
            |> List.filter (fun w -> w.Length = w1.Length)
        let findNextWord (prevWord,currWord) = 
            if currWord <> w2 then
                wordsOfLength
                |> List.tryFind (fun w -> w <> prevWord && areWordsLinked currWord w)
                |> Option.map (fun w -> (currWord, (currWord, w)))
            else None
        List.append (Seq.unfold findNextWord (w1,w1) |> Seq.toList) [w2]
    else []

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

//areWordsLinked "head" "heal"

let tests () =

    test <@ doublets ("head", "tail") = ["head"; "heal"; "teal"; "tell"; "tall"; "tail"] @>
    test <@ doublets ("door", "lock") = ["door"; "boor"; "book"; "look"; "lock"] @>
    test <@ doublets ("bank", "loan") = ["bank"; "bonk"; "book"; "look"; "loon"; "loan"] @>
    test <@ doublets ("wheat", "bread") = ["wheat"; "cheat"; "cheap"; "cheep"; "creep"; "creed"; "breed"; "bread"] @>

    test <@ doublets ("ye", "freezer") = [] @>

// run the tests
tests ()
