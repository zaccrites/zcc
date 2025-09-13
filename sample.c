int main() {
  int a = 10;  // a=10
  int b = a++;  // a=11, b=10
  int c = ++b - 4;  // a=11, b=11, c=7
  b--;  // a=11, b=10, c=7
  return a + b + c;

}
