;description: Falsiness should also work
;value: 21

var x = 100 % 2 == 0;
var y = 10 >= 20;
var z;
if (x || y)
  z = 21;
else
  z = 22;
return z;
