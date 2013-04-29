description: Functions changing global variables
value: 1

static var global = 3;
static other() {
    global = 1;
}
static main() {
  other();
  return global;
}
