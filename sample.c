
int g(int a, int b, int c, int d, int e, int f, int g, int h);

int f(int x, int y) {
  return (x + y) + g(1, 2, 3, 4, 5, 6, 7, 8);
}

int g(int a, int b, int c, int d, int e, int f, int g, int h) {
  return a + b + c + d + e + f + g + h;
}



int putchar(int c);

int print_hello_world() {
  putchar(40 + 32);
  putchar(110 - 9);
  putchar(100 + 8);
  putchar(54 << 1);
  putchar(333 / 3);
  putchar(40 | 0x04);
  putchar(0x20);
  putchar(87);
  putchar(111);
  putchar(114);
  putchar(108);
  putchar(100);
  putchar(33);
  putchar(10);
  return 0;
}


int main() {
  print_hello_world();
  return 5 + f(9, 10);
}

