/* An example of function passed as argument */

def double (x) = 2*x

def nil () =
  val bk = new [1] in
  bk[0]:=0; /* tag */
  bk end

def cons (x,l) =
  val bk = new [3] in
  bk[0]:=1; /* tag */
  bk[1]:=x;
  bk[2]:=l;
  bk end

def map (f,l) =
  if l[0] = 0
  then nil()
  else cons(?(f)(l[1]),map(f,l[2]))
  end

def sum(l) =
  if l[0] = 0
  then 0
  else l[1] + sum(l[2])
  end

val example = cons(1,cons(2,cons(3,nil())))
val res = sum(map(&double,example)) /* 12 */
