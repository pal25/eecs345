description: Functions with same variable, different scope
value: 8

static other() {
  var x = 5;
  return x + 3;
}

static main() {
  var x = 4;
  return other();
}
