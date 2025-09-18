int main() {
  int a = 1 + 2;
  int acc = 0;
  switch (a) {
    case 1:
      return 1;
    case 2:
      return 2 + 2;
    case 3:
      {
        int x = 2;
        acc += 1;
        acc += x;
        break;
      }
    case 4:
    case 5:
      acc = 5;
      break;
    default:
      acc = 60;
      break;
  }
  return acc;
}
