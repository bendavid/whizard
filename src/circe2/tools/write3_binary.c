#include <stdio.h>
void write3_ (double *x, double *y, double *w) 
{
  double buf[3]; buf[0] = *x; buf[1] = *y; buf[2] = *w;
  if (fwrite (buf, sizeof (double), 3, stdout) != 3) {
    fprintf (stdin, "write3: fwrite failed!\n");
    exit (1);
  }
}
