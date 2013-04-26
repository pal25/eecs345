description: Call-by-reference on non-variable should explode
value: ERROR

ref(&x) {
  return 7;
}
main() {
    var y = 10;
    ref(y + 1);
    return 1;
}
