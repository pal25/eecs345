description: Function in global variable declaration
value: 5

var x = 4;

increment(x) {
  return x + 1;
}
var y = increment(x);

main() {
    return y;
}
