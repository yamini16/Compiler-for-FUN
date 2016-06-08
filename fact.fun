fun fact (n:int):int = if n = 1 then 1 else n * (fact (n-1))

fun main (arg:int):int = let a = fact (4) in (printint(a);a)
