description: Really strange assignments work in while 
value: 20

var a = 0;
var b = 0;
var c = 10;
while((a = a + b * 2) < (c = c + b)) {
    b = b + 1;
 }

return a;
	
