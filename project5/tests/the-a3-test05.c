description: GCD from the assignment
value: 7

static var x = 14;
static var y = 3 * x - 7;
static gcd(a,b) {
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
static main () {
  return gcd(x,y);
}
