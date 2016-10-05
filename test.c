int number = 0xA34B;
int i=0;
while (i<4) {
  val = number % 16;
  if (val < 10) {
    printf("%c", val + '0');
  } else {
    printf("%c", val - 10 + 'A');
  }
  number = number / 16;
i++; }
