int main() {
  int a = 5;
  int b = 0;

target:
  a += 0;
  int c = 1;

  b += c;
  if (a-- > 0) {
    c += 1;
    goto target;
  }

  return b;
}
