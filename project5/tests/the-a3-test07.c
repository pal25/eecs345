description: Passing in true or false
value: 7

static other(x) {
  if (x)
      return 1;
  else
      return 7;
}

static main() {
  return other(false);
}
