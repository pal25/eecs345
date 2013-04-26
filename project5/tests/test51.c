description: Side effects in while stmt should work
value: 21

var x = 0;
while ((x = x + 1) < 21)
  x = x;
return x;