description: Functions with same variable, different scope
value: 8

other() {
  var x = 5;
  return x + 3;
}

main() {
  var x = 4;
  return other();
}
