description: Function parameters hiding globals
value: 1

static min(x, y, z) {
  if (x < y) {
    if (x < z)
      return x;
    else if (z < x)
      return z;
  }
  else if (y > z)
    return z;
  else
    return y;
}

static var x = 10;
static var y = 20;
static var z = 30;

static var min1 = min(x,y,z);
static var min2 = min(z,y,x);

static main() {
  var min3 = min(y,z,x);

  if (min1 == min3)
    if (min1 == min2)
      if (min2 == min3)
        return 1;
  return 0;
}
