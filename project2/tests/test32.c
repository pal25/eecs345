description: Blocks should work arbitrarily nested
value: 8

var x = 0;
var y = 10;

while(x < y) {
    if(x > 5) {
	y = y - 1;
	x = x + 1;
    } else {
	x = x + 1;
    }
 }

return x;
