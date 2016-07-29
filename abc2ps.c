/*
 * abcm2ps: a program to typeset tunes written in abc format using PostScript
 *
 * Copyright (C) 1998-2003 Jean-Fran�ois Moine
 *
 * Adapted from abc2ps-1.2.5:
 *  Copyright (C) 1996,1997  Michael Methfessel
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Jean-Fran�ois Moine
 *	mailto:moinejf@free.fr
 *
 * Original page:
 *	http://moinejf.free.fr/
 *
 * Original abc2ps:
 *	Michael Methfessel
 *	msm@ihp-ffo.de
 *	Institute for Semiconductor Physics, PO Box 409,
 *	D-15204 Frankfurt (Oder), Germany
 */

/* Main program abcm2ps.c */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#include <string.h>
#include <ctype.h>

#include "abcparse.h"
#include "abc2ps.h"

/* -- global variables -- */

struct ISTRUCT info, default_info;
unsigned char deco_glob[256], deco_tune[256];
struct SYMBOL *sym;		/* (points to the symbols of the current voice) */

char page_init[201];		/* initialization string after page break */
int tunenum;			/* number of current tune */
int pagenum = 1;		/* current page in output file */

int in_page;

				/* switches modified by flags: */
int pagenumbers;		/* write page numbers ? */
int epsf;			/* for EPSF postscript output */
int choose_outname;		/* 1 names outfile w. title/fnam */

char outf[STRL1];		/* output file name */
int  file_initialized;		/* for output file */
FILE *fout;			/* output file */

int s_argc;			/* command line arguments */
char **s_argv;

struct WHISTLE_S whistle_tb[MAXWHISTLE];
int nwhistle;

/* -- local variables -- */

static int include_xrefs = -1;	/* to include xref numbers in title */
static int one_per_page = -1;	/* new page for each tune */
static int splittune = -1;	/* tune may be splitted */
static int write_history = -1;	/* write history and notes */
static float alfa_c = -1.0;	/* max compression allowed */
static int bars_per_line = -1;	/* bars for auto linebreaking */
static int encoding = -1;	/* latin encoding number */
static int continue_lines = -1;	/* flag to continue all lines */
static int deco_old;		/* abc2ps decorations */
static int landscape = -1;	/* flag for landscape output */
static float lmargin = -1.0;	/* left margin */
static float indent = -1.0;	/* 1st line indentation */
static int music_only = -1;	/* no vocals if 1 */
static int flatbeams = -1;	/* flat beams when bagpipe */
static int graceslurs = -1;	/* slurs in grace notes */
static float scalefac = -1.0;	/* scale factor for symbol size */
static float staffsep = -1.0;	/* staff separation */
static char *styd = DEFAULT_FDIR; /* format search directory */
static int def_fmt_done = 0;	/* default format read */
static float swidth = -1.0;	/* staff width */
static int measurenb = -1;	/* measure numbering (-1: none, 0: on the left, or every n bars) */
static int measurebox = -1;	/* display measure numbers in a box */
static int measurefirst = -1;	/* first measure number */
static int printtempo = -1;	/* print the tempo indications */
static char *sel;		/* current input file selector */
char *in_fname;			/* current input file name */

/* memory arena (for clrarena, lvlarena & getarena) */
#define MAXAREAL 4		/* max area levels:
				 * 0; global, 1: tune, 2: output, 3: output line */
static int str_level;		/* current arena level */
static struct str_a {
	char	str[4096];	/* memory area */
	char	*p;		/* pointer in area */
	struct str_a *n;	/* next area */
	int	r;		/* remaining space in area */
} *str_r[MAXAREAL], *str_c[MAXAREAL];	/* root and current area pointers */

/* -- local functions -- */

static void cutext(char *fid);
static void do_filter(struct abctune *t,
		      char *sel);
static void do_select(struct abctune *t,
		      int first_tune,
		      int last_tune);
static void output_file(void);
static void read_def_format(void);
static char *read_file(void);
static void set_page_format(void);
static void usage(void);
static void whistle_parse(char *p);
static void write_version(void);

/* -- main program -- */
int main(int argc,
	 char **argv)
{
	int j;
	char *p;
	char c, *aaa;

	/* -- set default options and parse arguments -- */
	fprintf(stderr, "abcm2ps-" VERSION " (" VDATE ")\n");
	if (argc <= 1)
		usage();

	/* -- initialize -- */
	strcpy(outf, OUTPUTFILE);
	clrarena(0);			/* global dexription */
	clrarena(1);			/* tune description */
	clrarena(2);			/* tune generation */
	clrarena(3);			/* line generation */
	clear_buffer();
	abc_init((void *(*)(int size)) getarena, /* alloc */
		 0,				/* free */
		(void (*)(int level)) lvlarena, /* new level */
		 sizeof(struct SYMBOL) - sizeof(struct abcsym),
		 0);			/* don't keep comments */
	set_format();
	s_argc = argc;
	s_argv = argv;

	/* parse the arguments - as soon as a file ends, it is treated */
	while (--argc > 0) {
		argv++;
		p = *argv;
		if (*p == '+') {	/* switch off flags with '+' */
			while (*++p != '\0') {
				switch (*p) {
				case 'B': bars_per_line = 0; break;
				case 'c': continue_lines = 0; break;
				case 'E': epsf = 0; break;
				case 'F': def_fmt_done = 1; break;
				case 'G': graceslurs = 1; break;
				case 'j':
				case 'k': measurenb = -1; break;
				case 'l': landscape = 0; break;
				case 'M': music_only = 0; break;
				case 'N': pagenumbers = 0; break;
				case 'n': write_history = 0; break;
				case 'O':
					choose_outname = 0;
					strcpy(outf, OUTPUTFILE);
					break;
				case 'Q': printtempo = 0; break;
				case 'x': include_xrefs = 0; break;
				case 'W': nwhistle = 0; break;
				case '0': splittune = 0; break;
				case '1': one_per_page = 0; break;
				default:
					fprintf(stderr,
						"++++ Cannot switch off flag: +%c\n",
						*p);
					severity = 1;
					break;
				}
			}
			continue;
		}

		if (*p == '-') {	     /* interpret a flag with '-'*/
			if (p[1] == '\0') {
				if (in_fname != 0)
					output_file();
				in_fname = "";		/* read from stdin */
				continue;
			}
			if (p[1] == '-') {
				p += 2;
				if (--argc <= 0) {
					fprintf(stderr,
						"++++ No argument for '--'\n");
					return 2;
				}
				argv++;
				interpret_format_line(p, *argv);
				continue;
			}
			while (*++p != '\0') {
				switch (c = *p) {

					/* simple flags */
				case 'c': continue_lines = 1; break;
				case 'E': epsf = 1; break;
				case 'f': flatbeams = 1; break;
				case 'G': graceslurs = 0; break;
				case 'H': 
					if (fout == 0)
						set_page_format();
					print_format();
					return 0;
				case 'h': usage(); break;
				case 'l': landscape = 1; break;
				case 'M': music_only = 1; break;
				case 'n': write_history = 1; break;
/*				case 'b': */
				case 'C':
				case 'g':
				case 'P':
				case 'p':
				case 'R':
				case 'S':
				case 'T':
				case 'o':
				case 'v':
					fprintf(stderr,
						"'-%c' is obsolete - option ignored\n",
						c);
					break;
				case 'Q': printtempo = 1; break;
				case 'u': deco_old = 1; break;
				case 'V':
					write_version();
					return 0;
				case 'x': include_xrefs = 1; break;
				case '0': splittune = 1; break;
				case '1': one_per_page = 1; break;

				case 'e':	/* filtering */
					if (sel != 0) {
						fprintf(stderr,
							"++++ Too many '-e'\n");
						return 2;
					}
					sel = p + 1;
					if (sel[0] == '\0') {
						if (--argc <= 0) {
							fprintf(stderr,
								"++++ No filter in '-e'\n");
							return 2;
						}
						argv++;
						sel = *argv;
					} else {
						while (p[1] != '\0')	/* stop */
							p++;
					}
					break;

					/* flag with optional parameter */
				case 'N':
					if (p[1] == 0
					    && (argc <= 1
					 	|| !isdigit(argv[1][0]))) {
						pagenumbers = 2; /* old behaviour */
						break;
					}
					/* fall thru */
					/* flags with parameter.. */
				case 'a':
				case 'B':
				case 'b':
				case 'D':
				case 'd':
				case 'F':
				case 'I':
				case 'j':
				case 'k':
				case 'L':
				case 'm':
				case 'O':
				case 's':
				case 'w':
				case 'W':
					aaa = p + 1;
					if (*aaa == '\0') {
						aaa = *++argv;
						if (--argc <= 0 || *aaa == '-') {
							fprintf(stderr,
								"++++ Missing parameter after flag -%c\n",
								c);
							return 2;
						}
					} else {
						while (p[1] != '\0')	/* stop */
							p++;
					}

					if (strchr("BbfjkLNsv", c)) {	    /* check num args */
						for (j = 0; j < strlen(aaa); j++)
							if (!strchr("0123456789.",
								    aaa[j])) {
								if (aaa[j] == 'b'
								    && aaa[j+1] == '\0'
								    && (c == 'j'
									|| c == 'k'))
									break;
								fprintf(stderr,
									"++++ Invalid parameter <%s> for flag -%c\n",
									aaa, c);
								return 2;
							}
					}

					switch (c) {
					case 'a':
						sscanf(aaa, "%f", &alfa_c);
						if (alfa_c > 1. || alfa_c < 0) {
							fprintf(stderr,
								"++++ Bad parameter for flag -a: %s\n",
								aaa);
							return 2;
						}
						break;
					case 'B':
						sscanf(aaa, "%d", &bars_per_line);
						continue_lines = 0;
						break;
					case 'b':
						sscanf(aaa, "%d", &measurefirst);
						break;
					case 'D':
						styd = aaa;
						break;
					case 'd':
						staffsep = scan_u(aaa);
						break;
					case 'F':
						read_def_format();
						if (read_fmt_file(aaa, styd) < 0) {
							fprintf(stderr,
								"++++ Cannot open format file: %s\n",
								aaa);
							severity = 1;
						}
						break;
					case 'I':
						indent = scan_u(aaa);
						break;
					case 'j':
					case 'k':
						sscanf(aaa, "%d", &measurenb);
						if (aaa[strlen(aaa) - 1] == 'b')
							measurebox = 1;
						else	measurebox = 0;
						break;
					case 'L':
						sscanf(aaa, "%d", &encoding);
						if ((unsigned) encoding > MAXENC) {
							fprintf(stderr,
								"++++ Bad encoding value %s - reset to 0\n",
								aaa);
							encoding = 0;
						}
						break;
					case 'm':
						lmargin = scan_u(aaa);
						break;
					case 'N':
						sscanf(aaa, "%d", &pagenumbers);
						if (pagenumbers < 0 || pagenumbers > 4) {
							fprintf(stderr,
								"++++ '-N' value %s - changed to 2\n",
								aaa);
							pagenumbers = 2;
						}
						break;
					case 'O':
						if (!strcmp(aaa, "=")) {
							choose_outname = 1;
							break;
						}
						strcpy(outf, aaa);
						choose_outname = 0;
						break;
					case 's':
						sscanf(aaa, "%f", &scalefac);
						break;
					case 'w':
						swidth = scan_u(aaa);
						break;
					case 'W':
						whistle_parse(aaa);
						break;
					}
					break;
				default:
					fprintf(stderr,
						"++++ Unknown flag: -%c\n", c);
					severity = 1;
					break;
				}
			}
			continue;
		}

		if (strstr(p, ".fmt")) {	/* implicit -F */
			read_def_format();
			if (read_fmt_file(p, styd) < 0) {
				fprintf(stderr,
					"++++ Cannot open format file: %s\n",
					p);
				severity = 1;
			}
			continue;
		}

		if (in_fname != 0)
			output_file();
		in_fname = p;
	}

	if (in_fname != 0)
		output_file();

	if (!epsf && fout == 0) {
		fprintf(stderr, "No input file specified\n");
		return 1;
	}

	close_output_file();

	return severity;
}

/* -- output the current ABC file -- */
static void output_file(void)
{
	struct abctune *t;
	char *file;

	/* read the file into memory */
	if ((file = read_file()) == 0) {
#if defined(unix) || defined(__unix__)
		perror("read_file");
#endif
		fprintf(stderr, "++++ Cannot read input file '%s'\n",
			in_fname);
		severity = 1;
		return;
	}

	/* initialize if not already done */
	if (fout == 0)
		set_page_format();

	memset(&default_info, 0, sizeof default_info);
	default_info.title[0] = "(notitle)";
	memcpy(&info, &default_info, sizeof info);
	reset_deco(deco_old);
	memcpy(&deco_tune, &deco_glob, sizeof deco_tune);
	if (!epsf) {
		if (choose_outname) {
			strcpy(outf, in_fname);
			strext(outf, "ps");
		}
		open_output_file();
	} else	cutext(outf);
	fprintf(stderr, "File %s\n", in_fname);
	clrarena(1);
	lvlarena(0);
	t = abc_parse(file);
	free(file);

	if (sel) {
		do_filter(t, sel);
		sel = 0;
	} else	do_select(t,
			  -32000,
			  (int) ((unsigned) (~0) >> 1));
	/*abc_free(t);	(useless) */
}

/* -- cut off extension on a file identifier -- */
static void cutext(char *fid)
{
	char *p;

	if ((p = strrchr(fid, DIRSEP)) == 0)
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
			end_sel = (int) ((unsigned) (~0) >> 1);
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
		    || !t->client_data) {	/* (parse the global symbols) */
			clrarena(2);
			do_tune(t, !print_tune);
			t->client_data = 1;	/* treated */
		}
		t = t->next;
	}
}

/* -- return the file extension -- */
static char *getext(char *fid)
{
	char *p;

	if ((p = strrchr(fid, DIRSEP)) == 0)
		p = fid;
	if ((p = strrchr(p, '.')) != 0)
		return p + 1;
	return 0;
}

/* -- copy the command line options to the current format -- */
void ops_into_fmt(void)
{
	struct FORMAT *fmt;

	fmt = &cfmt;
	if (landscape >= 0)
		fmt->landscape = landscape;
	if (scalefac >= 0)
		fmt->scale = scalefac;
	if (lmargin >= 0)
		fmt->leftmargin = lmargin;
	if (indent >= 0)
		fmt->indent = indent;
	if (swidth >= 0) {
		fmt->rightmargin = fmt->pagewidth - swidth - fmt->leftmargin;
		if (fmt->rightmargin < 0)
			fprintf(stderr, "Warning: staffwidth too big\n");
	}
	if (continue_lines >= 0)
		fmt->continueall = continue_lines;
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
	if (splittune >= 0)
		fmt->splittune = splittune;
	if (music_only >= 0)
		fmt->musiconly = music_only;
	if (graceslurs >= 0)
		fmt->graceslurs = graceslurs;
	if (flatbeams >= 0)
		fmt->flatbeams = flatbeams;
	if (measurenb >= 0)
		fmt->measurenb = measurenb;
	if (measurebox >= 0)
		fmt->measurebox = measurebox;
	if (measurefirst >= 0)
		fmt->measurefirst = measurefirst;
	if (printtempo >= 0)
		fmt->printtempo = printtempo;
	if (alfa_c >= 0)
		fmt->maxshrink = alfa_c;
}

/* -- read the default format -- */
static void read_def_format(void)
{
	if (def_fmt_done)
		return;
	def_fmt_done = 1;
	read_fmt_file("default.fmt", styd);
}

/* -- read an input file -- */
static char *read_file(void)
{
	int fsize;
	FILE *fin;
	char *file;

	if (in_fname[0] == '\0') {
		int offset;

		in_fname = "stdin";
		fsize = 8096;
		file = malloc(fsize);
		offset = 0;
		for (;;) {
			if (fread(&file[offset], 1, 8096, stdin) != 8096)
				break;
			fsize += 8096;
			file = realloc(file, fsize);
			offset += 8096;
		}
		if (ferror(stdin) != 0) {
			free(file);
			return 0;
		}
	} else {
		if ((fin = fopen(in_fname, "rb")) == 0) {
			char new_file[256];

			if (getext(in_fname) != 0)
				return 0;
			sprintf(new_file, "%s.abc", in_fname);
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
static void set_page_format(void)
{
	read_def_format();
	ops_into_fmt();
	make_font_list();
}

/* -- set extension on a file identifier -- */
void strext(char *fid,
	    char *ext)
{
	char *p, *q;

	if ((p = strrchr(fid, DIRSEP)) == 0)
		p = fid;
	if ((q = strrchr(p, '.')) == 0)
		strcat(p, ".");
	else	q[1] = '\0';
	strcat(p, ext);
}

/* -- display usage and exit -- */
static void usage(void)
{
	printf(	"ABC to Postscript translator.\n"
		"Usage: abcm2ps [options] file [file_options] ..\n"
		"where:\n"
		" file        input ABC file, or '-'\n"
		" options and file_options:\n"
		"  .output file options:\n"
		"     -E      produce EPSF output, one tune per file\n"
		"     -O fff  set outfile name to fff\n"
		"     -O =    make outfile name from infile/title\n"
		"  .output formatting:\n"
		"     -s xx   set scale factor to xx\n"
		"     -w xx   set staff width (cm/in/pt)\n"
		"     -m xx   set left margin (cm/in/pt)\n"
		"     -d xx   set staff separation (cm/in/pt)\n"
		"     -a xx   set max shrinkage to xx (between 0 and 1)\n"
		"     -F foo  read format from \"foo.fmt\"\n"
		"     -D bar  look for format files in directory \"bar\"\n"
		"  .output options:\n"
		"     -l      landscape mode\n"
		"     -I xx   indent 1st line (cm/in/pt)\n"
		"     -x      include xref numbers in output\n"
		"     -M      don't output the lyrics\n"
		"     -n      include notes and history in output\n"
		"     -N n    set page number mode to n=\n"
		"             0=off 1=left 2=right 3=even left,odd right 4=even right,odd left\n"
		"     -1      write one tune per page\n"
		"     -G      no slur in grace notes\n"
		"     -j n[b] number the measures every n bars (or on the left if n=0)\n"
		"             if 'b', display in a box\n"
		"     -k n[b] same as '-j' (abc2ps compatibility)\n"
		"     -b n    set the first measure number to n\n"
		"     -f      have flat beams when bagpipe tunes\n"
		"     -W vk   output a whistle tablature for voice 'v', key 'k'\n"
		"  .line breaks:\n"
		"     -c      auto line break\n"
		"     -B n    break every n bars\n"
		"  .input file selection/options:\n"
		"     -e pattern\n"
		"             xref list of tunes to select\n"
		"     -u      abc2ps implicit decorations\n"
		"     -L n    set char encoding to Latin number n\n"
		"  .help/configuration:\n"
		"     -V      show program version\n"
		"     -h      show this command summary\n"
		"     -H      show the format parameters\n");
	exit(0);
}

/* -- parse the whistle command ('-Wvk') -- */
static void whistle_parse(char *p)
{
	short pitch;
	char *q;
	static char notes_tb[14] = "CDEFGABcdefgab";
	static char pitch_tb[14] = {60, 62, 64, 65, 67, 69, 71,
				    72, 74, 76, 77, 79, 81, 83};

	if (nwhistle >= MAXWHISTLE) {
		fprintf(stderr, "++++ Too many '-W'\n");
		return;
	}
	if ((unsigned) (*p - '0') > 9) {
		fprintf(stderr, "++++ Bad voice number in '-W'\n");
		return;
	}
	whistle_tb[nwhistle].voice = *p++ - '0' - 1;
	pitch = 0;
	if (*p == '^' || *p == '_') {
		if (*p++ == '^')
			pitch++;
		else	pitch--;
	}
	if ((q = strchr(notes_tb, *p)) == 0) {
		fprintf(stderr, "++++ Bad note name in '-W'\n");
		return;
	}
	pitch += pitch_tb[q - notes_tb];
	p++;
	if (*p == '#' || *p == 'b') {
		if (*p++ == '#')
			pitch++;
		else	pitch--;
	}
	while (*p == '\'' || *p == ',') {
		if (*p++ == '\'')
			pitch += 12;
		else	pitch -= 12;
	}
	whistle_tb[nwhistle].pitch = pitch;
	nwhistle++;
}

/* -- write the program version -- */
static void write_version(void)
{
	fprintf(stderr, "Compiled: " __DATE__ "\n"
	       "Options:"
#ifdef BSTEM_DOWN
		" BSTEM_DOWN"
#endif
#ifdef A4_FORMAT
		" A4_FORMAT"
#endif
#ifdef CLEF_TRANSPOSE
		" CLEF_TRANSPOSE"
#endif
#ifdef DECO_IS_ROLL
		" DECO_IS_ROLL"
#endif
#if !defined(BSTEM_DOWN) && !defined(A4_FORMAT) && !defined(CLEF_TRANSPOSE) \
     && !defined(DECO_IS_ROLL)
		" NONE"
#endif
		"\n");

	if (strlen(DEFAULT_FDIR) > 0)
		fprintf(stderr,
			"Default format directory: %s\n", DEFAULT_FDIR);
}

/* -- arena routines -- */
void clrarena(int level)
{
	struct str_a *a_p;

	if ((a_p = str_r[level]) == 0) {
		str_r[level] = a_p = malloc(sizeof *str_r[0]);
		a_p->n = 0;
	}
	str_c[level] = a_p;
	a_p->p = a_p->str;
	a_p->r = sizeof a_p->str;
}

void lvlarena(int level)
{
	str_level = level;
}

/* The area is 8 bytes aligned to handle correctly int and pointers access
 * on some machines as Sun Sparc. */
char *getarena(int len)
{
	char *p;
	struct str_a *a_p;

	a_p = str_c[str_level];
	len = (len + 7) & ~7;		/* align at 64 bits boundary */
	if (a_p->r < len) {
		if (a_p->n == 0) {
			a_p->n = malloc(sizeof *str_r[0]);
			a_p->n->n = 0;
		}
		str_c[str_level] = a_p = a_p->n;
		a_p->p = a_p->str;
		a_p->r = sizeof a_p->str;
	}
	p = a_p->p;
	a_p->p += len;
	a_p->r -= len;
	return p;
}
