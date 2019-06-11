signature MUSIC =
sig
    datatype pitchClass = Ces | C | Cis | Des | D | Dis | Es | E | Eis | Fes | F | Fis | Ges | G | Gis | As | A | Ais | B | H | His
    type octave = int
    type pitch = pitchClass * octave
    type dur = Ratio.ratio
    datatype note = Rest of dur | Note of pitch * dur
    datatype music = Prim of note | Serial of music * music | Parallel of music * music | Tempo of Ratio.ratio * music | Trans of int * music
    type absPitch = int
    val pcToInt : pitchClass -> int
    val absPitch : pitch -> absPitch
    val pitch : absPitch -> pitch
end

structure Music : MUSIC =
struct
datatype pitchClass = Ces | C | Cis | Des | D | Dis | Es | E | Eis | Fes | F | Fis | Ges | G | Gis | As | A | Ais | B | H | His
type octave = int
type pitch = pitchClass * octave
type dur = Ratio.ratio
datatype note = Rest of dur | Note of pitch * dur
datatype music = Prim of note | Serial of music * music | Parallel of music * music | Tempo of Ratio.ratio * music | Trans of int * music
type absPitch = int
fun pcToInt pc = case pc of Ces => ~1
			  | C => 0
			  | Cis => 1
			  | Des => 1
			  | D => 2
			  | Dis => 3
			  | Es => 3
			  | E => 4
			  | Eis => 5
			  | Fes => 4
			  | F => 5
			  | Fis => 6
			  | Ges => 6
			  | G => 7
			  | Gis => 8
			  | As => 8
			  | A => 9
			  | Ais => 10
			  | B => 10
			  | H => 11
			  | His => 12
fun absPitch (pc, oct) = 12 * oct + pcToInt pc
fun pitch ap = (List.nth ([C, Des, D, Es, E, F, Ges, G, As, A, B, H], ap mod 12), ap div 12)
end
