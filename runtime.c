#include <stdio.h>

void printInt(long n) asm("printInt");
void printString(char *n) asm("printString");

void printInt(long n)
{
  printf("%ld\n", n);
}

void printString(char *s)
{
  printf("%s\n", s);
}

extern void cs132start() asm("cs132start");

int main()
{
  cs132start();
}
