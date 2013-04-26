description: Side assignments in while stmt should work.
value: 10

var x = 0;
var y = 0;

while(10 > (x = y)) {
    y = y + 1;
 }

return x;
