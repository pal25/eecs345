description: Call-by-reference works
value: 5

static ref(&x) {
  x = x + 1;
}
static main() {
  var x = 4;
  ref(x);
  return x;
}

