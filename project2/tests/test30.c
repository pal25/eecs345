description: Blocks should work for arbitrary while stmts
value: 260

var a = 0;
var b = 10;
while (b != 0) {
    a = a + b * 5;
    b = b - 1;

    if (b == 2)
	return a;
 }
return a;
