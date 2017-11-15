#include <assert.h>
#include <stdio.h>
#include <sys/resource.h>

static unsigned correct_result = 0;
typedef unsigned (*func_t)(unsigned, unsigned);

extern unsigned __attribute__((optimize("O0")))
  offset_imul(unsigned x, unsigned y);

extern unsigned __attribute__((optimize("O0")))
  offset_shift_add(unsigned x, unsigned y);

static unsigned correct(const unsigned x, const unsigned y)
{
  return x + y*320;
}

static double rusage()
{
  struct rusage ru;
  getrusage(RUSAGE_SELF, &ru);
  return ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1000000.0;
}

static unsigned __attribute__((optimize("O0")))
  calc(const size_t its, func_t func)
{
  unsigned check = 0xaaaaaaaa;

  for ( size_t n=0; n<its; ++n )
    check ^= func(n, n+1);

  return check;
}

static void
  __attribute__((optimize("O0")))
  best(const size_t its, double* best, func_t func)
{
  const double start = rusage();
  const unsigned result = calc(its, func);
  assert(result == correct_result);
  const double secs = rusage() - start;

  if ( secs < *best )
    *best = secs;
}

int main()
{
  double tc=9999, ti=9999, ts=9999;

  const size_t its = 1000000000;
  correct_result = calc(its, correct);

  for (;;) {
    best(its, &tc, correct);
    best(its, &ts, offset_shift_add);
    best(its, &ti, offset_imul);
    printf("correct %fs, shift+add %fs, imul %fs\n", tc, ts, ti);
  }
}
