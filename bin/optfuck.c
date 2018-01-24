/* optfuck.c --- Brainfuck with input as command line arguments.

   Copyright (C) 2016, 2017 Göktuğ Kayaalp <self |at| gkayaalp {dot} com>.

   Permission to use, copy, modify, and/or distribute this software
   for any purpose with or without fee is hereby granted, provided
   that the above copyright notice and this permission notice appear
   in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
   WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
   AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
   CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
   OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
   NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
   CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

   Hello world example:
   $ cc -std=c99 -o of optfuck.c
   $ ./of -B -B -B -B -B -B -B -B -j -D -B -B -B -B -j\
          -D -B -B -D -B -B -B -D -B -B -B -D -B -d -d\
          -d -d -b -J -D -B -D -B -D -b -D -D -B -j -d\
          -J -d -b -J -D -D -o -D -b -b -b -o -B -B -B\
          -B -B -B -B -o -o -B -B -B -o -D -D -o -d -b\
          -o -d -o -B -B -B -o -b -b -b -b -b -b -o -b\
          -b -b -b -b -b -b -b -o -D -D -B -o -D -B -B -o 

   The interpreter is created with help from
   <https://github.com/kgabis/brainfuck-c/>.

   Though I used a 4-byte integer as the representation for the
   instruction.  The higher 3 bits represent the instruction, (see op*
   macros), and the rest is the argument to it.

   Options help:
   	-D increment data pointer       -d decrement data pointer
	-B incr byte at data pointer    -b decr byte at data pointer
	-o write byte at data pointer   -g read byte from stdin
	-j if the byte at data pointer is 0, jump after matching
	   `-J'
	-J if the byte at data pointer is nonzero, jump after
           matching `-j'

   Lots of options with a single `-' does not work.  Because I didn't
   bother.
 */

#include<stdio.h>
#include<stdint.h>
#include<strings.h>

#define MEMSIZ (4096)
#define PRGSIZ (4096)
#define STKSIZ (4096)
unsigned short S[STKSIZ]; /* The stack. */
unsigned int P = 0;	  /* The stack pointer. */
#define PUS(x) (S[P++]=x) /* Push to the stack. */
#define POP()  (S[--P])	  /* Pop the stack. */
#define EMPT() (P==0)	  /* Stack empty? */
#define FULL() (P==STKSIZ)	/* Stack full? */

#define die(s) {puts(s);return 1;}

#define opincdp  (0)
#define opdecdp  (1)
#define opincval (2)
#define opdecval (3)
#define opputval (4)
#define opgetval (5)
#define opjmpfwd (6)
#define opjmpbck (7)

#define opshift (29)
#define opmask(x) (x<<opshift)

int main(int argc, char **argv)
{
	if(1==argc){ printf(
	"Options help:\n"
	"\t-D increment data pointer       -d decrement data pointer\n"
	"\t-B incr byte at data pointer    -b decr byte at data pointer\n"
	"\t-o write byte at data pointer   -g read byte from stdin\n"
	"\t-j if the byte at data pointer is 0, jump after matching\n"
	"\t   `-J'\n"
	"\t-J if the byte at data pointer is nonzero, jump after\n"
	"\t   matching `-j'\n"
	"\nData pointer starts out as 0 (zero), and the byte array is\n"
	"bzeroed.  The program is limited to %d instructions, though\n"
	"probably ARG_MAX would be more restrictive than that.\n"
	"Negative values and negative data pointer is not allowed.\n\n"
	"Copyright (C) Göktuğ Kayaalp, ISC Licence (see the source).\n",
	PRGSIZ);
	}

	uint8_t mem[MEMSIZ];	/* The memory. */
	uint32_t prg[PRGSIZ];	/* The program. */
	uint32_t dp, jp, i; 	/* Data pointer, jump pointer. */
	dp = 0; bzero(mem,MEMSIZ); bzero(prg,PRGSIZ); bzero(S,STKSIZ);

	/* Compile */
	if(argc>PRGSIZ) die("input bigger than max prog size!")
	for(i=0;i<(argc-1);i++){
		switch(argv[i+1][1]){
		case 'D': prg[i]=(opincdp<<opshift);  break;
		case 'd': prg[i]=(opdecdp<<opshift);  break;
		case 'B': prg[i]=(opincval<<opshift); break;
		case 'b': prg[i]=(opdecval<<opshift); break;
		case 'o': prg[i]=(opputval<<opshift); break;
		case 'g': prg[i]=(opgetval<<opshift); break;
		case 'j':
			if(FULL()) die("-j: stack full");
			PUS(i);
			prg[i]=(opjmpfwd<<opshift);
			break;
		case 'J':
			if(EMPT()) die("unmatched -J");
			jp = POP();
			prg[i]=(opjmpbck<<opshift) | jp;
			prg[jp]|=i;
			break;
		default: die("syntax error");
		}
	}
	//return 0;
	if(!EMPT()) die("syntax error: unbalanced -j/-J");

	/* Eval */
	for(i=0;i<(argc-1);i++){
		switch(prg[i]>>opshift){
		case opincdp:
			dp++;
			if(dp>PRGSIZ) die("segfault");
			break;
		case opdecdp:
			if(dp==0) die("segfault");
			dp--;
			break;
		case opincval:
			if(mem[dp]==UINT8_MAX) die("data out of range");
			mem[dp]++;
			break;
		case opdecval:
			if(mem[dp]==0) die("data out of range");
			mem[dp]--;
			break;
		case opputval: putchar(mem[dp]); break;
		case opgetval: mem[dp]=(uint32_t)getchar(); break;
		case opjmpfwd:
			if(!mem[dp])i=(prg[i])^(opmask(opjmpfwd));
			break;
		case opjmpbck:
			if(mem[dp]) i=(prg[i])^(opmask(opjmpbck));
			break;
		default: die("unrecognised bytecode!");
		}
	}

	return 0;
}
