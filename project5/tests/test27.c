description: Break should work in while loops
value: 5

var x = 1;
var y = 10;
while (x < y)
    if (x == 5)
	break;
    else
	x = x + 1;
return x;
