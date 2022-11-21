(*

<expr> ::=
    | <deg><dur><expr>
    | base<freq><qual><expr>
    | <empty>
<deg> ::=
    | tonic
    | supertonic
    | mediant
    | subdominant
    | dominant
    | submediant
    | leading tone
<qual> ::= major | minor
<freq> ::=
    | <num>hz
<dur> ::=
    | <num>sec
<num> ::=
    | <d><num>
    | <d>
<d> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9


sample program:

base 440 hz major
tonic 1 sec
supertonic 1 sec
dominant 1 sec
tonic 1 sec

*)


module Parser

open AST
open Combinator
open System

let pad p = pbetween pws0 pws0 p
let expr,exprImpl = recparser()
let num = pmany1 pdigit |>> stringify |>> int
let basedef = pad ( pstr "base" )
let quality =
    pad (
        (pstr "major" |>> (fun _ -> Major)) <|>
        (pstr "minor" |>> (fun _ -> Minor))
    )
let degree =
    pad (
        (pstr "tonic" |>> (fun _ -> Tonic)) <|>
        (pstr "supertonic" |>> (fun _ -> Supertonic)) <|>
        (pstr "mediant" |>> (fun _ -> Mediant)) <|>
        (pstr "subdominant" |>> (fun _ -> Subdominant)) <|>
        (pstr "dominant" |>> (fun _ -> Dominant)) <|>
        (pstr "submediant" |>> (fun _ -> Submediant)) <|>
        (pstr "subtonic" |>> (fun _ -> Subtonic)) <|>
        (pstr "leadingtone" |>> (fun _ -> LeadingTone))
    )

let key = pseq (pleft num (pad ( pstr "hz" ) )) quality (fun (f,q) -> Key(f, q))

let chord = pseq degree num (fun (d,n) -> Chord(d, n))


exprImpl :=
    (pseq chord expr (fun (chord,chords) -> chord :: chords)) <|>
    presult []

let staff = pseq (pright basedef key) expr (fun (k,cs) -> Staff(k, cs))

let grammar = pleft staff peof
