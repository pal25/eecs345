description: Multiple function calls in an expression
value: 20

static fact(n) {
  var f = 1;
  while (n > 1) {
    f = f * n;
    n = n - 1;
  }
  return f;
}

static binom(a, b) {
  var val = fact(a) / (fact(b) * fact(a-b));
  return val;
}

static main() {
  return binom(6,3);
}
