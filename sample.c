int main() {
  int a = -4 * 4;
  int b = a + 40;
  int c;
  int d = a + (c = b + 1);
  return c + d;
}
