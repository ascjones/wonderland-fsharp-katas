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
    Convert.ToChar ((i % 26s) + a)

let toCharInts (s: string) =
    s.ToCharArray() |> Array.map toCharCode

let repeatKey (key: Keyword) (msg: Message) =
    [|0 .. (msg.Length - 1)|]
    |> Array.map (fun i ->
        key.ToCharArray()
        |> Array.item (i % key.Length))

let mapChars f (key: Keyword) (msg: string) =
    repeatKey key msg
    |> Array.map toCharCode
    |> Array.zip (toCharInts msg)
    |> Array.map (f >> fromCharCode)

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
        keyCode + msgCode
    mapChars encodeChar key message |> String

let decode (key:Keyword) (message:Message) : Message =
    let decodeChar (msgCode, keyCode) =
        msgCode - keyCode + 26s
    mapChars decodeChar key message |> String

let decipher (cipher:Message) (msg:Message) : Keyword =
    let decipherChar (cipherCode, msgCode) =
        msgCode - cipherCode + 26s
    let repeatedKey =
        mapChars decipherChar cipher msg // todo: extract mapChars without key repeat
    let rec extractKey acc = function
        | [] -> acc
        | x :: xs ->
            let key = Array.append acc [|x|]
            if repeatKey (String key) msg = repeatedKey then key
            else extractKey key xs
    extractKey [||] (repeatedKey |> Array.toList) |> String

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
