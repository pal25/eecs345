description: Function without a return statement
value: 35

static var x = 0;
static var y = 0;

static setx(a) {
  x = a;
}

static sety(b) {
  y = b;
}

static main() {
  setx(5);
  sety(7);
  return x * y;
}
