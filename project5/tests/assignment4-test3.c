description: Mutating variable state inside if statements
value: 'false

class Main {
  static main() {
    var x = 5;
    if (5 == (x = 4))
      return true;
    else
      return false;
  }
}  
