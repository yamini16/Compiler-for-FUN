fun change (n:int ref):<> = n := (!n + 1)

fun main (arg:int):int = let a = ref 5 in (change(a); printint(!a);!a)
