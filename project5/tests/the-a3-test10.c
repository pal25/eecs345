description: Call-by-reference on non-variable should explode
value: ERROR

static ref(&x) {
  return 7;
}
static main() {
    var y = 10;
    ref(y + 1);
    return 1;
}
