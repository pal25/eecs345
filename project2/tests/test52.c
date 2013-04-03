description: Side effects in nested if stmt should work
value: 12

var x = 1;
while (true) {
  x = x + 1;
  if (x > 10 && x % 2 == 0)
   break;
}
return x;