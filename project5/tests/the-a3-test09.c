description: Function in global variable declaration
value: 5

static var x = 4;

static increment(x) {
  return x + 1;
}
static var y = increment(x);

static main() {
    return y;
}
