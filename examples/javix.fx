
/*
    javix.fx is a custom file that will contain instruction to compiler
    from Fopix To Javix
*/
/*
val a = 42
val x = 10
val y = 1 + 2
val z = x + y
val bimp = if z = 13 then (if y = 3 then (if x = 10 then 1 else a) else a / 2) else 0*/

val block = new [2]
eval block[0] := 1
eval block[1] := 1
val ret = block[0] - block[1]

/*
def fact(x) = if x = 0 then 1 else x * fact(x-1)
val res = fact(10)
*/
/*
    @TODO
    Functions
*/
/*
def f(x,y,z) = x + y + z
val a = f(1, 2 * 5, 3 + fact(10))
*/
/* + other instructions not compiled yet (see fopixToJavix.ml) */
