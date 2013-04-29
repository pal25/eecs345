description: Changing global variables
value: 45

static var x = 1;
static var y = 10;
static var r = 0;

static main() {
  while (x < y) {
     r = r + x;
     x = x + 1;
  }
  return r;
}
