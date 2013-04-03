description: Basic variable scope should work
value: ERROR

var x = 10;
var y = 4;
if (x < y) {
  var min = x;
}
else {
  var min = y;
}
return min;