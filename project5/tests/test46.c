description: Nested if blocks should work
value: 2

var x = 1;
var y = x + 1;
if (x < y) {
  var z = 10;

  if (x < z) {
    var swap = y;
    y = x;
    x = swap;
  }
}
return x;