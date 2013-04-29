description: Calling another function
value: 4

static other(x) {
    return x + 1;
}
static main() {
  var y = 3;
  return other(y);
}
