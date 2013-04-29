description: Mismatched parameters and arguments
value: ERROR

static f(a) {
  return a*a;
}

static main() {
  return f(10, 11, 12);
}
