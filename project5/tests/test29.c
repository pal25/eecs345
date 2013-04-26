description: Begin should work for if stmts
value: 2

var x = 1;
var y = 3;
var count = 0;
if(x > y) {
    x = x + 1;
    return x;
 } else {
    y = y - 1;
    return y;
 }
return y;
