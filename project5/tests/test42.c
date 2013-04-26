description: Test basic blocks
value: 20

var x = 10;
{
  var y = 2;
  var z = x * y;
  x = z;
}
return x;