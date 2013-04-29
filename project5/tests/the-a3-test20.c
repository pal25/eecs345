description: Function calls that ignore return value
value: 2

static var count = 0;

static f(a,b) {
  count = count + 1;
  a = a + b;
  return a;
}

static main() {
  f(1, 2);
  f(3, 4);
  return count;
}
