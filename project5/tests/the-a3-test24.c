description: Boolean parameters and return values
value: 'true

static minmax(a, b, min) {
  if (min && a < b || !min && a > b)
    return true;
  else
    return false;
}

static main() {
  return (minmax(10, 100, true) && minmax(5, 3, false));
}
