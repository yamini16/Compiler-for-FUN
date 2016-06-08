fun fib (n:int):int = (if (n = 0) then 0 else (if (n = 1) then 1 else (let a = fib(n-1) in (let b = fib(n-2) in (a+b)))))

fun main (arg:int):int = let a = fib (4) in (printint(a);a)
