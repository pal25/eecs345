description: Mutating variables inside an equality
value: 4

class Main {
  static main() {
    var x;
    var y;
    if ((x = 4) == 3)
      y = 3 + 2;
    return x;
  }
}
