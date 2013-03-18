;description: Boolean operater (not !) should work
;value: 21

var x = 10;
var y = 20;
var z = 20 >= 10;
if (!z || false)
  z = !z;
else
  z = 21;
return z;
