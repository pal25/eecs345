description: Recursive factorial from the assignment
value: 720

factorial (x) {
  if (x == 0)
    return 1;
  else
    return x * factorial(x - 1);
}

main () {
  return factorial(6);
}
