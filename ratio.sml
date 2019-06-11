nonfix *~ /~ +~ -~ <~ >~

signature RATIO =
sig
    type ratio = int * int
    exception DivideByZero
    val fromIntPair : int * int -> ratio
    val +~ : ratio * ratio -> ratio
    val -~ : ratio * ratio -> ratio
    val *~ : ratio * ratio -> ratio
    val /~ : ratio * ratio -> ratio
    val >~ : ratio * ratio -> bool
    val <~ : ratio * ratio -> bool
end

structure Ratio : RATIO =
struct

type ratio = int * int
exception DivideByZero

fun fromIntPair (num, 0) = raise DivideByZero
  | fromIntPair (0, den) = (0, 1)
  | fromIntPair (num, den) =
    let fun gcd (x, y) = if x = y then x
		 else if x > y then gcd (x - y, y)
		 else gcd (x, y - x)
	val g = if den > 0 then gcd (abs num, abs den)
		else ~(gcd (abs num, abs den))
    in (num div g, den div g)
    end
fun +~ ((x, y), (z, w)) = fromIntPair (w * x + y * z, y * w)
fun -~ ((x, y), (z, w)) = fromIntPair (w * x - y * z, y * w)
fun *~ ((x, y), (z, w)) = fromIntPair (x * z, y * w)
fun /~ ((x, y), (z, w)) = fromIntPair (x * w, y * z)
fun >~ ((x, y), (z, w)) = w * x > y * z
fun <~ ((x, y), (z, w)) = w * x < y * z

end

infix 7 *~ /~
infix 6 +~ -~
infix 5 >~ <~	
