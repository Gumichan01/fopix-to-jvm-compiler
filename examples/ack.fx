def ack (n,m) =
  if n = 0 then m+1
  else if m = 0 then ack (n-1,1)
  else ack (n-1, ack(n,m-1))
val res = ack (3,4)
/* answer: 125 */