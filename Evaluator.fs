module Evaluator

open AST

// let evalQuality(q: Quality): string =
//     match q with
//     | Major ->
//         "int t;\n" +
//         "int st;\n" + 
//         "int m;\n" +
//         "int sd;\n" +
//         "int d;\n" + 
//         "int sm;\n" + 
//         "int sbt;\n" +
//         "int lt;\n" +
//         "base => t\n" +
//         "base * 9/8 => st;\n" + 
//         "base * 5/4 => m;\n" +
//         "base * 4/3 => sd;\n" +
//         "base * 3/2 => d;\n" +
//         "base * 5/3 => sm;\n" +
//         "base * 15/8 => sbt\n" +
//         "base * 15/8 => lt\n"
//     | Minor -> 
//         "int t;\n" +
//         "int st;\n" + 
//         "int m;\n" +
//         "int sd;\n" +
//         "int d;\n" + 
//         "int sm;\n" + 
//         "int sbt;\n" +
//         "int lt;\n" +
//         "base => t\n" +
//         "base * 9/8 => st;\n" + 
//         "base * 6/5 => m;\n" +
//         "base * 4/3 => sd;\n" +
//         "base * 3/2 => d;\n" +
//         "base * 5/3 => sm;\n" +
//         "base * 8/5 => sbt\n" +
//         "base * 15/8 => lt\n"

let evalKey(k: Key): string =
    let freq,qual = k
    let freqS = freq |> string
    freqS + " => base;\n" +
    match qual with
    | Major ->
        "int t;\n" +
        "int st;\n" + 
        "int m;\n" +
        "int sd;\n" +
        "int d;\n" + 
        "int sm;\n" + 
        "int sbt;\n" +
        "int lt;\n" +
        "base => t;\n" +
        "base * 9/8 => st;\n" + 
        "base * 5/4 => m;\n" +
        "base * 4/3 => sd;\n" +
        "base * 3/2 => d;\n" +
        "base * 5/3 => sm;\n" +
        "base * 15/8 => sbt;\n" +
        "base * 15/8 => lt;\n"
    | Minor -> 
        "int t;\n" +
        "int st;\n" + 
        "int m;\n" +
        "int sd;\n" +
        "int d;\n" + 
        "int sm;\n" + 
        "int sbt;\n" +
        "int lt;\n" +
        "base => t;\n" +
        "base * 9/8 => st;\n" + 
        "base * 6/5 => m;\n" +
        "base * 4/3 => sd;\n" +
        "base * 3/2 => d;\n" +
        "base * 8/5 => sm;\n" +
        "base * 8/5 => sbt;\n" +
        "base * 16/9 => lt;\n"

let evalChord(c: Chord): string =
    let deg,dur = c
    let durs = dur |> string
    match deg with
    | Tonic -> 
        "t => root.freq;\n" +
        "m => third.freq;\n" +
        "d => fifth.freq;\n" +
        "root => dac;\n" +
        "third => dac;\n" +
        "fifth => dac;\n" +
        durs + "::second => now;\n" +
        "root =< dac;\n" +
        "third =< dac;\n" +
        "fifth =< dac;\n"
    | Supertonic ->
        "st => root.freq;\n" +
        "sd => third.freq;\n" +
        "sm => fifth.freq;\n" +
        "root => dac;\n" +
        "third => dac;\n" +
        "fifth => dac;\n" +
        durs + "::second => now;\n" +
        "root =< dac;\n" +
        "third =< dac;\n" +
        "fifth =< dac;\n"
    | Mediant ->
        "m => root.freq;\n" +
        "d => third.freq;\n" +
        "sbt => fifth.freq;\n" +
        "root => dac;\n" +
        "third => dac;\n" +
        "fifth => dac;\n" +
        durs + "::second => now;\n" +
        "root =< dac;\n" +
        "third =< dac;\n" +
        "fifth =< dac;\n"
    | Subdominant ->
        "sd => root.freq;\n" +
        "sm => third.freq;\n" +
        "t => fifth.freq;\n" +
        "root => dac;\n" +
        "third => dac;\n" +
        "fifth => dac;\n" +
        durs + "::second => now;\n" +
        "root =< dac;\n" +
        "third =< dac;\n" +
        "fifth =< dac;\n"
    | Dominant ->
        "d => root.freq;\n" +
        "lt => third.freq;\n" +
        "st => fifth.freq;\n" +
        "root => dac;\n" +
        "third => dac;\n" +
        "fifth => dac;\n" +
        durs + "::second => now;\n" +
        "root =< dac;\n" +
        "third =< dac;\n" +
        "fifth =< dac;\n"
    | Submediant ->
        "sm => root.freq;\n" +
        "t => third.freq;\n" +
        "m => fifth.freq;\n" +
        "root => dac;\n" +
        "third => dac;\n" +
        "fifth => dac;\n" +
        durs + "::second => now;\n" +
        "root =< dac;\n" +
        "third =< dac;\n" +
        "fifth =< dac;\n"
    | Subtonic ->
        "lt => root.freq;\n" +
        "st => third.freq;\n" +
        "sd => fifth.freq;\n" +
        "root => dac;\n" +
        "third => dac;\n" +
        "fifth => dac;\n" +
        durs + "::second => now;\n" +
        "root =< dac;\n" +
        "third =< dac;\n" +
        "fifth =< dac;\n"
    | LeadingTone ->
        "lt => root.freq;\n" +
        "st => third.freq;\n" +
        "sd => fifth.freq;\n" +
        "root => dac;\n" +
        "third => dac;\n" +
        "fifth => dac;\n" +
        durs + "::second => now;\n" +
        "root =< dac;\n" +
        "third =< dac;\n" +
        "fifth =< dac;\n"

let rec evalChords(cs: Chords): string = 
    match cs with
    | []    -> ""
    | s::ss -> (evalChord s) + (evalChords ss)

let eval(st: Staff): string =
    let key,cs = st
    "int major;\n"+
    "SinOsc root => dac;\n" +
    "SinOsc third => dac;\n" + 
    "SinOsc fifth => dac;\n" +
    "int base;\n" + evalKey key + evalChords cs
