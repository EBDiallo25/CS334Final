module AST

(* AST *)
type Quality =
| Major
| Minor
type Degree =
| Tonic
| Supertonic
| Mediant
| Subdominant
| Dominant 
| Submediant 
| Subtonic
| LeadingTone
type Chord = Degree * int
type Chords = Chord list
type Key = int * Quality 
type Staff = Key * Chords