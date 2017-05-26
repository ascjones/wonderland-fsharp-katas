// See the file alphabet-cipher.md for detailed information.

open System

type Message = string
type Keyword = string

let charToInt (c: char) =
    System.Convert.ToInt16 c

let intToChar (i: int16) =
    System.Convert.ToChar i

let a = charToInt 'a'
let z = charToInt 'z'

let toCharCode (c: char) =
    charToInt c - a

let toCharInts (s: string) =
    s.ToCharArray() |> Array.map toCharCode

let repeatKey (key: Keyword) (fillCount: int) =
    [|0 .. (fillCount - 1)|]
    |> Array.map (fun i ->
        key.ToCharArray()
        |> Array.item (i % key.Length)
        |> toCharCode)

let mapChars f (key: Keyword) (msg: string) =
    Array.zip (toCharInts msg) (repeatKey key msg.Length)
    |> Array.map (f >> intToChar)
    |> String

let encode (key:Keyword) (message:Message) : Message =
    let encodeChar (msgCode, keyCode) =
        ((keyCode + msgCode) % 26s) + a
    mapChars encodeChar key message

let decode (key:Keyword) (message:Message) : Message =
    let decodeChar (msgCode, keyCode) =
        ((msgCode - keyCode + 26s) % 26s) + a
    mapChars decodeChar key message

let decipher (cipher:Message) (message:Message) : Keyword =
    "decypherme"

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
