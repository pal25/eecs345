description: GCD from the assignment
value: 7

var x = 14;
var y = 3 * x - 7;
gcd(a,b) {
  if (a < b) {
    var temp = a;
    a = b;
    b = temp;
  }
  var r = a % b;
  while (r != 0) {
    a = b;
    b = r;
    r = a % b;
  }
  return b;
}
main () {
  return gcd(x,y);
}
