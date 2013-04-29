description: Recursive factorial from the assignment
value: 720

static factorial (x) {
  if (x == 0)
    return 1;
  else
    return x * factorial(x - 1);
}

static main () {
  return factorial(6);
}
