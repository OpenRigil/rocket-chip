#include "mmio.h"

#define MMM_STATUS  0x2000
#define MMM_CONTROL 0x2004
#define MMM_PPRIME  0x2008
#define MMM_P       0x200C
#define MMM_A       0x2010
#define MMM_B       0x2014
#define MMM_BP      0x2018
#define MMM_OUT     0x201C

volatile uint64_t tohost;
volatile uint64_t fromhost;

static void do_tohost(uint64_t tohost_value)
{
  while (tohost)
    fromhost = 0;
  tohost = tohost_value;
}

static void cputchar(int x)
{
  do_tohost(0x0101000000000000 | (unsigned char)x);
}

static void cputstring(const char* s)
{
  while (*s)
    cputchar(*s++);
}

static void terminate(int code)
{
  do_tohost(code);
  while (1);
}

void main(void)
{
  //uint32_t result, ref = 52569216, p = 393685561, a = 80906835, b = 339262562;
  uint32_t result, ref = 52602, p = 120209, a = 83618, b = 19519;
  //120209 83618 19519 139728 27366 52602
  uint32_t bp = b + p;

  // wait for peripheral to be ready
  //while ((reg_read8(GCD_STATUS) & 0x2) == 0) ;

  reg_write8(MMM_PPRIME, 1);
  reg_write32(MMM_P, p);
  reg_write32(MMM_A, a);
  reg_write32(MMM_B, b);
  reg_write32(MMM_BP, bp);

  // send ready signal
  reg_write8(MMM_CONTROL, 2);

  // wait for peripheral to complete
  while ((reg_read8(MMM_STATUS) & 0x1) == 0) ;

  result = reg_read32(MMM_OUT);

  //cputstring("Hello from HTIF");
  if (result != ref) {
    terminate(841);
  }
  terminate(1);
}
