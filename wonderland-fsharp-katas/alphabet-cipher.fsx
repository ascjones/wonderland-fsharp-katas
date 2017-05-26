// See the file alphabet-cipher.md for detailed information.

open System

type Message = string
type Keyword = string

let charToInt (c: char) =
    Convert.ToInt16 c

let a = charToInt 'a'

let toCharCode (c: char) =
    charToInt c - a

let fromCharCode i =
    Convert.ToChar (i + a)

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
    |> Array.map (f >> fromCharCode)
    |> String

// sample grid of chars mapped to integers, a = 0, z = 25
//
//  key char ->    0 1 2 3
//              0  0 1 2 3
//              1  1 2 3 0
//              2  2 3 0 1
//              3  3 0 1 2
//  msg char -- ^

let encode (key:Keyword) (message:Message) : Message =
    let encodeChar (msgCode, keyCode) =
        ((keyCode + msgCode) % 26s)
    mapChars encodeChar key message

let decode (key:Keyword) (message:Message) : Message =
    let decodeChar (msgCode, keyCode) =
        ((msgCode - keyCode + 26s) % 26s)
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
