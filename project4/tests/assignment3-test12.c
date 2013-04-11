description: Call-by-reference works with multiple functions
value: 6

ref2(&x) {
  x = x + 1;
}
ref(&x) {
  x = x + 1;
  ref2(x);
}
main() {
  var x = 4;
  ref(x);
  return x;
}
