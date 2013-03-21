description: Continue should work in while loops
value: 6

var a = 0;
var b = 0;
while(a < 12) {
    if(a % 2 == 0) {
	a = a + 1;
	continue;
    }
    a = a + 1;
    b = b + 1;
 }

return b;
	
