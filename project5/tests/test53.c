description: Boolean values in while stmt should work
value: 32

var x = 0;
var y = 10;
while (!(x >= y) || !(y > 25)) {
  x = x + 2;
  y = y + 1;
}
return x;