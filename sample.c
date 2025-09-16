int main() {
  int a = 5;
  int b = 0;

target:
  a += 0;
  int c = 1;

  b += c;
  if (a-- > 0) {
    int inc = 1;
    c += inc;
    goto target;
    int c = 10;  // not the same c as two lines earlier
  }

  return b;
}
