description: Call-by-reference works
value: 5

ref(&x) {
  x = x + 1;
}
main() {
  var x = 4;
  ref(x);
  return x;
}

