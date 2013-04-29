description: Call-by-reference works with multiple functions
value: 6

static ref2(&x) {
  x = x + 1;
}
static ref(&x) {
  x = x + 1;
  ref2(x);
}
static main() {
  var x = 4;
  ref(x);
  return x;
}
