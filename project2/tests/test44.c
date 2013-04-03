description: Break should work inside basic while loops
value: -1

var x = 0;
while (x < 10) {
  x = x - 1;
  break;
  x = x + 100;
}
return x;