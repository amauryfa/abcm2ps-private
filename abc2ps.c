/*
 * abcm2ps: a program to typeset tunes written in abc format using PostScript
 *
 * Copyright (C) 1998-2000 Jean-François Moine
 *
 * Adapted from abc2ps-1.2.5:
 *  Copyright (C) 1996,1997  Michael Methfessel
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  The author can be contacted as follows:
 *
 * Jean-François Moine
 *	mailto:moinejf@free.fr
 * or	mailto:Jean-Francois.Moine@bst.bsf.alcatel.fr
 *
 * Original page:
 *	http://moinejf.free.fr/
 *
 * Original abc2ps:
 *  Michael Methfessel
 *  msm@ihp-ffo.de
 *  Institute for Semiconductor Physics, PO Box 409,
 *  D-15204 Frankfurt (Oder), Germany
 *
 */

/* Main program abcm2ps.c */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include <time.h>
#include <string.h>

#include "abcparse.h"
#include "abc2ps.h"

/* -- global variables -- */

struct ISTRUCT info, default_info;
unsigned char deco_glob[128], deco_tune[128];
struct SYMBOL *sym;		/* (points to the symbols of the current voice) */

struct FORMAT dfmt;		/* format at start of tune */
struct FORMAT cfmt;		/* current format for output */

char fontnames[MAXFONTS][STRLFMT];	/* list of needed fonts */
int  nfontnames;

char mbf[501];			/* mini-buffer for one line */
float bposy;			/* current position in buffered data */
int use_buffer;			/* 1 if lines are being accumulated */

char page_init[201];		/* initialization string after page break */
int tunenum;			/* number of current tune */
int nsym;			/* number of symbols in line */
int pagenum;			/* current page in output file */

float posx,posy;		/* overall scale, position on page */

int insert_btype;		/* needed to split bars across lines */

int verbose;			/* verbosity, global and within tune */
int in_page;

				/* switches modified by flags: */
int gmode;			/* switch for glue treatment */
int pagenumbers;		/* write page numbers ? */
int epsf;			/* for EPSF postscript output */
int choose_outname;		/* 1 names outfile w. title/fnam */
int break_continues;		/* ignore continuations ? */

char outf[STRL1];		/* output file name */
char infostr[STRL1];		/* title string in PS file */

int  file_initialized;		/* for output file */
FILE *fout;			/* output file */
int nepsf;			/* counter for epsf output files */

/* -- local variables -- */

static struct FORMAT sfmt;	/* format after initialization */
static int include_xrefs;	/* to include xref numbers in title */
static int one_per_page;	/* new page for each tune ? */
static int write_history;	/* write history and notes ? */
static float alfa_c;		/* max compression allowed */
static int bars_per_line;	/* bars for auto linebreaking */
static int encoding;		/* latin encoding number */
static int continue_lines;	/* flag to continue all lines */
#ifdef unix
static char dirsep = '/';
#else
static char dirsep = '\\';
#endif
static int deco_old;		/* abc2ps decorations */
static int help_me;		/* need help ? */
static int landscape;		/* flag for landscape output */
static float lmargin;		/* left margin */
static float indent;		/* 1st line indentation */
static int music_only;		/* no vocals if 1 */
static int pretty;		/* for pretty but sprawling layout */
static float scalefac; 		/* scale factor for symbol size */
static float staffsep; 		/* staff separation */
static char styf[STRL1];	/* layout style file name */
static char styd[STRL1];	/* layout style directory */
static float swidth;		/* staff width */
static int  ninf;		/* number of input file names */
char in_file[MAXINF][STRL1];	/* list of input file names (should be static - see subs.c) */
static char *sel[MAXINF];	/* associated selectors */

/* memory arena (for clrarena & getarena) */
static struct str_a {
	char	str[4096 - 12];	/* memory area */
	char	*p;		/* pointer in area */
	struct str_a *n;	/* next area */
	int	r;		/* remaining space in area */
} *str_r, *str_p;		/* root and current area pointers */

/* -- local functions -- */

static void cutext(char *fid);
static void do_filter(struct abctune *t,
		      char *sel);
static void do_select(struct abctune *t,
		      int first_tune,
		      int last_tune);
static void getext(char *fid,
		   char *ext);
static void init_ops(void);
static int parse_args(int ac,
		      char *av[]);
static char *read_file(char *fname);
static int set_page_format(void);
static void usage(void);
static void write_version(void);

static void clrarena(void);

/* -- main program -- */
int main(int argc,
	 char *argv[])
{
	char ext[41];
	int j;

	/* -- set default options and parse arguments -- */
	init_ops();
	verbose = VERBOSE0;
	if (parse_args(argc, argv) != 0)
		exit(1);
	printf("abcm2ps, version " VERSION " (" VDATE ")\n");

	/* -- set the page format -- */
	nfontnames = 0;
	if (!set_page_format())
		exit(3);
	if (help_me == 2) {
		print_format(&cfmt);
		exit(0);
	}

	/* -- help printout -- */
	if (argc <= 1)
		help_me = 1;
	if (help_me == 1) {
		usage();
		exit(0);
	}

	if (ninf == 0)
		rx("No input file specified", "");

	if (epsf)
		cutext(outf);

	/* -- initialize -- */
	posx = cfmt.leftmargin;
	posy = cfmt.pageheight - cfmt.topmargin;
	page_init[0] = '\0';
	abc_init((void *(*)(int sz)) getarena,	/* alloc */
		 0,	/* free */
		 sizeof(struct SYMBOL) - sizeof(struct abcsym),
		 0);	/* don't keep comments */

	/* -- loop over files in list -- */
	if (ninf == 0)
		printf("++++ No input files\n");
	for (j = 0; j < ninf; j++) {
		struct abctune *t;
		char *file;

		getext(in_file[j], ext);
		/*  skip .ps and .eps files */
		if (!strcmp(ext, "ps") || !strcmp(ext, "eps"))
			continue;

		/* read the file into memory */
		if ((file = read_file(in_file[j])) == 0) {
#ifdef unix
			perror("read_file");
#endif
			printf("++++ Cannot read input file '%s'\n",
				in_file[j]);
			continue;
		}

/*fixme*/
		dfmt = sfmt;
		strcpy(infostr, in_file[j]);

		memset(&default_info, 0, sizeof default_info);
		default_info.title[0] = "(notitle)";
		memcpy(&info, &default_info, sizeof info);
		reset_deco(deco_old);
		memcpy(&deco_tune, &deco_glob, sizeof deco_tune);
		if (!epsf) {
			if (choose_outname)
				strcpy(outf, in_file[j]);
			strext(outf, "ps");
			open_output_file(outf);
		}
		printf("%s: ", in_file[j]);
		if (verbose >= 3)
			printf("\n");
		clrarena();
		t = abc_parse(file);
		free(file);

		if (sel[j]) {
			do_filter(t, sel[j]);
		} else	do_select(t,
				  -32000,
				  (int) ((unsigned) (~0) >> 1));
		/*abc_free(t);	(useless) */
		printf("\n");
	}

	close_output_file();
	return 0;
}

/* -- cut off extension on a file identifier -- */
static void cutext(char *fid)
{
	char *p;

	if ((p = strrchr(fid, dirsep)) == 0)
		p = fid;
	if ((p = strrchr(p, '.')) != 0)
		*p = '\0';
}

/* -- do filtering on an input file -- */
static void do_filter(struct abctune *t, char *sel)
{
	int cur_sel;
	int end_sel;
	int n;

	for (;;) {
		sscanf(sel, "%d%n", &cur_sel, &n);
		sel += n;
		if (*sel == '-') {
			sel++;
			end_sel = (int) ((unsigned) (~0) >> 1);	/* MAXINT?? */
			sscanf(sel, "%d%n", &end_sel, &n);
			sel += n;
		} else	end_sel = cur_sel;
		do_select(t, cur_sel, end_sel);
		if (*sel != ',')
			break;
		sel++;
	}
}

/* -- do a tune selection -- */
static void do_select(struct abctune *t,
		      int first_tune,
		      int last_tune)
{
	while (t != 0) {
		struct abcsym *s;
		int print_tune;

		print_tune = 0;
		for (s = t->first_sym; s != 0; s = s->next) {
			if (s->type == ABC_T_INFO
			    && s->text[0] == 'X') {
				int i;

				if (sscanf(s->text, "X:%d", &i) == 1
				    && i >= first_tune
				    && i <= last_tune)
					print_tune = 1;
				break;
			}
		}
		if (print_tune
		    || !t->client_data) {
			do_tune(t, !print_tune);
			t->client_data = 1;	/* treated */
		}
		t = t->next;
	}
}

/* -- get extension on a file identifier -- */
static void getext(char *fid,
		   char *ext)
{
	char *p;

	if ((p = strrchr(fid, dirsep)) == 0)
		p = fid;
	if ((p = strrchr(p, '.')) != 0) {
		strcpy(ext, p + 1);
	} else	ext[0] = '\0';
}

/* -- init_ops -- */
static void init_ops(void)
{
	one_per_page	= -1;
	landscape	= -1;
	scalefac	= -1.0;
	lmargin		= -1.0;
	indent		= -1.0;
	swidth		= -1.0;
	write_history   = -1;
	staffsep	= -1;
	break_continues = -1;
	continue_lines  = -1;
	include_xrefs   = -1;
	music_only	= -1;
	alfa_c		= -1.0;
	encoding	= -1;

	pagenumbers	= 0;
	styf[0] = '\0';

	strcpy(styd, DEFAULT_FDIR);
	strcpy(outf, OUTPUTFILE);
	pretty		= 0;
	epsf		= 0;
	choose_outname	= 0;
	gmode		= G_FILL;
}

/* -- ops_into_fmt -- */
void ops_into_fmt(struct FORMAT *fmt)
{
	if (landscape >= 0)
		fmt->landscape = landscape;
	if (scalefac >= 0)
		fmt->scale = scalefac;
	if (lmargin >= 0)
		fmt->leftmargin = lmargin;
	if (indent >= 0)
		fmt->indent = indent;
	if (swidth >= 0)
		fmt->staffwidth = swidth;
	if (continue_lines >= 0)
		fmt->continueall = continue_lines;
	if (break_continues >= 0)
		fmt->breakall = break_continues;
	if (write_history >= 0)
		fmt->writehistory = write_history;
	if (bars_per_line >= 0)
		fmt->barsperstaff = bars_per_line;
	if (encoding >= 0)
		fmt->encoding = encoding;
	if (include_xrefs >= 0)
		fmt->withxrefs = include_xrefs;
	if (staffsep >= 0)
		fmt->staffsep = staffsep;
	if (one_per_page >= 0)
		fmt->oneperpage = one_per_page;
	if (music_only >= 0)
		fmt->musiconly = music_only;
	if (alfa_c >= 0)
		fmt->maxshrink = alfa_c;
}

/* -- parse_args: parse list of arguments, interpret flags -- */
static int parse_args(int ac,
		      char *av[])
{
	int i, m, k, j, ok, got_value;
	char c, aaa[201], ext[41];

	help_me = 0;
	ninf = 0;

	for (i = 1; i < ac; i++) {
		if (av[i][0] == '+') {	/* switch off flags with '+' */
			m = 1;
			k = strlen(av[i]);
			while (m < k) {
				switch (av[i][m]) {
				case 'B': bars_per_line = 0; break;
				case 'b': break_continues = 0; break;
				case 'c': continue_lines = 0; break;
				case 'E': epsf = 0; break;
				case 'F': strcpy(styf, ""); break;
				case 'l': landscape = 0; break;
				case 'M': music_only = 0; break;
				case 'N': pagenumbers = 0; break;
				case 'n': write_history = 0; break;
				case 'O':
					choose_outname = 0;
					strcpy(outf, OUTPUTFILE);
					break;
				case 'p': pretty = 0; break;
				case 'x': include_xrefs = 0; break;
				case '1': one_per_page = 0; break;
				default:
					printf("++++ Cannot switch off flag: +%c\n",
					       av[i][m]);
					return 1;
				}
				m++;
			}
			continue;
		}

		if (av[i][0] == '-') {	     /* interpret a flag with '-'*/
			m = 1;
			k = strlen(av[i]);
			while (m < k) {
				switch (c = av[i][m]) {	/* simple flags */
				case 'b': break_continues = 1; continue_lines = 0; break;
				case 'c': continue_lines = 1; break_continues = 0; break;
				case 'E': epsf = 1; break;
				case 'H': help_me = 2; break;
				case 'h': help_me = 1; break;
				case 'l': landscape = 1; break;
				case 'M': music_only = 1; break;
				case 'N': pagenumbers = 1; break;
				case 'n': write_history = 1; break;
				case 'C':
				case 'R':
				case 'S':
				case 'T':
				case 'o':
					printf("'-%c' is obsolete - flag ignored\n",
					       c);
					break;
				case 'P': pretty = 2; break;
				case 'p': pretty = 1; break;
				case 'u': deco_old = 1; break;
				case 'V':
					write_version();
					exit(0);
				case 'x': include_xrefs = 1; break;
				case '1': one_per_page = 1; break;

				case 'e':	/* filtering */
					if ((j = ninf - 1) < 0)
						j = 0;
					if (sel[j] != 0) {
						printf("++++Too many '-e'\n");
						return 1;
					}
					sel[j] = &av[i][m + 1];
					if (sel[j][0] == '\0') {
						if (i >= ac) {
							printf("++++ No filter in '-e'");
							return 1;
						}
						i++;
						sel[j] = av[i];
					}
					m = k;
					break;
				case 'a':	/* flags with parameter.. */
				case 'B':
				case 'D':
				case 'd':
				case 'F':
				case 'g':
				case 'I':
				case 'j':
				case 'L':
				case 'm':
				case 'O':
				case 's':
				case 'v':
				case 'w':
				case 'Y':
					strcpy(aaa, &av[i][m+1]);
					if (aaa[0] != '\0'
					    && strchr("gO", c)) {	/* no sticky arg */
						printf("++++ Incorrect usage of flag -%c\n",
						       c);
						return 1;
					}

					got_value = 1;			/* check for value */
					if (aaa[0] == '\0') {
						i++;
						if (i >= ac || av[i][0] == '-')
							got_value = 0;
						else	strcpy(aaa, av[i]);
					}

					if (got_value
					    && strchr("BjLsvY", c)) {	    /* check num args */
						ok = 1;
						for (j = 0; j < strlen(aaa); j++)
							if (!strchr("0123456789.", aaa[j])) {
								ok = 0;
								break;
							}
						if (!ok) {
							printf("++++ Invalid parameter <%s> for flag -%c\n",
							       aaa, c);
							return 1;
						}
					}

					if (!got_value) {	/* check value was given */
						printf("++++ Missing parameter after flag -%c\n",
						       c);
						return 1;
					}
					switch (c) {
					case 'a':
						sscanf(aaa, "%f", &alfa_c);
						if (alfa_c > 1.05 || alfa_c < -0.01) {
							printf("++++ Bad parameter for flag -a: %s\n",
							       aaa);
							return 1;
						}
						break;
					case 'B':
						sscanf(aaa, "%d", &bars_per_line);
						continue_lines = 0;
						break;
					case 'D':
						strcpy(styd, aaa);
						break;
					case 'd':
						staffsep = scan_u(aaa);
						break;
					case 'F':
						strcpy(styf, aaa);
						break;
					case 'g':
						if (abbrev(aaa, "shrink", 2))
							gmode = G_SHRINK;
						else if (abbrev(aaa, "stretch", 2))
							gmode = G_STRETCH;
						else if (abbrev(aaa, "space", 2))
							gmode = G_SPACE;
						else if (abbrev(aaa, "fill", 2))
							gmode = G_FILL;
						else {
							printf("++++ Bad parameter for flag -g: %s\n",
							       aaa);
							return 1;
						}
						break;
					case 'I':
						indent = scan_u(aaa);
						break;
					case 'j':
						sscanf(aaa, "%d", &measure_nb);
						break;
					case 'L':
						sscanf(aaa, "%d", &encoding);
						if (encoding < 0 || encoding > 6) {
							printf("++++ Bad encoding value %d - changed to 0\n",
								encoding);
							encoding = 0;
						}
						break;
					case 'O':
						if (!strcmp(aaa, "=")) {
							choose_outname = 1;
						} else {
							getext(aaa, ext);
							if (strcmp(ext, "ps")
							    && strcmp(ext, "eps")
							    && strcmp(ext, "")) {
								printf("Wrong extension for output file: %s\n",
								       aaa);
								return 1;
							}
							strcpy(outf, aaa);
							strext(outf, "ps");
							choose_outname = 0;
						}
						break;
					case 's':
						sscanf(aaa, "%f", &scalefac);
						break;
					case 'v':
						sscanf(aaa, "%d", &verbose);
						break;
					case 'w':
						swidth = scan_u(aaa);
						break;
					case 'm':
						lmargin = scan_u(aaa);
						break;
					case 'Y':
						/*fixme ??*/
						break;
					}
					m = k;		/* stop */
					break;
				default:
					printf("++++ Unknown flag: -%c\n", c);
					return 1;
				}
				m++;
			}
			if (k == 1) {
				if (ninf >= MAXINF) {
					printf("++++ Too many input files, max is %d\n",
					       MAXINF);
					return 1;
				}
				in_file[ninf][0] = '\0';	/* stdin */
				ninf++;
			}
			continue;
		}

		if (strstr(av[i], ".fmt"))	/* implicit -F */
			strcpy(styf, av[i]);
		else {
			if (ninf >= MAXINF) {
				printf("++++ Too many input files, max is %d\n",
				       MAXINF);
				return 1;
			}
			strcpy(in_file[ninf], av[i]);
			ninf++;
		}
	}
	return 0;
}

/* -- read an input file into 'file' -- */
static char *read_file(char *fname)
{
	int fsize;
	char new_file[256];
	FILE *fin;
	char *p;
	char *file;

	if (fname[0] == '\0') {
		fin = stdin;
		strcpy(fname, "stdin");
		file = p = malloc(8096);
		while (fgets(p, 256, fin)) {
			p += strlen(p);
			if (p - file >= 8094 - 256) {
				printf("input file too big - max: 8096");
				break;
			}
		}
	} else {
		if ((fin = fopen(fname, "rb")) == 0) {
			char ext[41];

			getext(fname, ext);
			if (ext[0] != '\0')
				return 0;
			sprintf(new_file, "%s.abc", fname);
			if ((fin = fopen(new_file, "rb")) == 0)
				return 0;
		}
		if (fseek(fin, 0L, SEEK_END) < 0) {
			fclose(fin);
			return 0;
		}
		fsize = ftell(fin);
		rewind(fin);
		if ((file = malloc(fsize + 2)) == 0) {
			fclose(fin);
			return 0;
		}

		if (fread(file, 1, fsize, fin) != fsize) {
			fclose(fin);
			free(file);
			return 0;
		}
		file[fsize] = '\0';
		fclose(fin);
	}
	return file;
}

/* -- set_page_format --- */
static int set_page_format(void)
{
	int i,j;

	if (pretty == 1)
		set_pretty_format(&cfmt);
	else if (pretty == 2)
		set_pretty2_format(&cfmt);
	else	set_standard_format(&cfmt);

	i = read_fmt_file("fonts.fmt", styd, &cfmt);
	j = 0;
	if (styf[0] != '\0') {
		strext(styf, "fmt");
		j = read_fmt_file(styf, styd, &cfmt);
		if (j == 0) {
			printf("\n++++ Cannot open file: %s\n",
			       styf);
			return 0;
		}
		strcpy(cfmt.name, styf);
	}
	if (i || j)
		printf("\n");
	ops_into_fmt(&cfmt);

	make_font_list(&cfmt);
	sfmt = cfmt;
	dfmt = cfmt;
	return 1;
}

/* -- set extension on a file identifier -- */
void strext(char *fid,
	    char *ext)
{
	char *p, *q;

	if ((p = strrchr(fid, dirsep)) == 0)
		p = fid;
	if ((q = strrchr(p, '.')) == 0)
		strcat(p, ".");
	else	q[1] = '\0';
	strcat(p, ext);
}

/* -- display usage -- */
static void usage(void)
{
	printf("Usage: abcm2ps [options] files [file_options] ..\n"
	       "  - typeset tunes in Postscript.\n"
	       "where:\n"
	       " files   input files in abc format, or '-'\n"
	       " options:\n"
	       "     -E      produce EPSF output, one tune per file\n"
	       "     -O fff  set outfile name to fff\n"
	       "     -O =    make outfile name from infile/title\n"
	       "     -u      old decoration definitions\n"
	       "     -v nn   set verbosity level to nn\n"
	       "     -V      show program version\n"
	       "     -h      show this command summary\n"
	       "     -e pattern\n"
	       "             list of tunes to select\n"
	       "     -L n    set char encoding to Latin number n\n"
	       "  Formatting:\n"
	       "     -H      show the format parameters\n"
	       "     -p      pretty output (looks better, needs more space)\n"
	       "     -P      select second predefined pretty output style\n"
	       "     -s xx   set scale factor for symbol size to xx\n"
	       "     -w xx   set staff width (cm/in/pt)\n"
	       "     -m xx   set left margin (cm/in/pt)\n"
	       "     -d xx   set staff separation (cm/in/pt)\n"
	       "     -I xx   indent 1st line (cm/in/pt)\n"
	       "     -x      include xref numbers in output\n"
	       "     -n      include notes and history in output\n"
	       "     -N      write page numbers\n"
	       "     -1      write one tune per page\n"
	       "     -l      landscape mode\n"
	       "     -g xx   set glue mode to shrink|space|stretch|fill\n"
	       "     -F foo  read format from \"foo.fmt\"\n"
	       "     -D bar  look for format files in directory \"bar\"\n"
	       "     -j n    Number the measures every n bars or on the left if 0\n"
	       "  Line breaks:\n"
	       "     -a xx   set max shrinkage to xx (between 0 and 1)\n"
	       "     -b      break at all line ends (ignore continuations)\n"
	       "     -c      continue all line ends (append '\\')\n"
	       "     -B bb   put line break every bb bars\n");
}

/* -- write_version -- */
static void write_version(void)
{
	printf("abcm2ps v" VERSION " (%s, %s) compiled %s\n",
		VDATE, style, __DATE__);

	if (strlen(DEFAULT_FDIR) > 0)
		printf("Default format directory %s\n", DEFAULT_FDIR);
}

/* -- arena routines -- */
static void clrarena(void)
{
	str_p = 0;
}

/* The area is 4 bytes aligned to handle correctly int and pointers access
 * on some machines as Sun Sparc.
 * It works on 32 bits machines, but may not work on 64 bits ones. */
char *getarena(int len)
{
	char *p;

	len = (len + 3) & ~3;		/* align at 32 bits boundary */
	if (str_p == 0
	    || str_p->r < len) {
		if (str_p == 0) {
			if (str_r == 0) {
				str_r = calloc(1, sizeof *str_r);
				str_p = str_r;
			} else	str_p = str_r;
		} else {
			if (str_p->n == 0)
				str_p->n = calloc(1, sizeof *str_r);
			str_p = str_p->n;
		}
		str_p->p = str_p->str;
		str_p->r = sizeof str_p->str;
	}
	p = str_p->p;
	str_p->p += len;
	str_p->r -= len;
	return p;
}

