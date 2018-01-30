
/*
    javix.fx is a custom file that will contain instruction to compiler
    from Fopix To Javix
*/

val x = 10
val y = 1 + 2
/*
    @bug @fixme
    This following line does not compile properly
    I have this exception
    Failure("No Javix variable binded to this Fopix var")
*/
/*val z = x + y*/

/*
    @TODO
    If then else
*/
/* val bt = if 1 = 1 then 1 else 2 */
/* val bf = if 1 = 0 then 1 else 2 */

/*
    @TODO
    Functions
*/
/* def f(x,y) = x + y */
/* val a = f(1, 2) */

/* + other instructions not compiled yet (see fopixToJavix.ml) */
