/*  
 *  This file is part of abc2ps, Copyright (C) 1996,1997 Michael Methfessel
 *  Modified for abcm2ps, Copyright (C) 1998,1999 Jean-François Moine
 *  See file abc2ps.c for details.
 */

#include <stdio.h>
#include <math.h>
#include <time.h>
#include <string.h>

#include "abcparse.h"
#include "abc2ps.h"

/*  subroutines connected with page layout  */

/* -- fontspec -- */
static void fontspec(struct FONTSPEC *f,
		     char *name,
		     float size)
{
	strcpy(f->name, name);
	f->size = size;
	f->swfac = 1.0;
	if (strcmp(f->name, "Times-Bold") == 0)
		f->swfac = 1.05;
	else if (strcmp(f->name, "Helvetica-Bold") == 0)
		f->swfac = 1.15;
	else if (strstr(f->name, "Helvetica")
		 || strstr(f->name,"Palatino"))
		f->swfac = 1.10;
}

/* -- add_font -- */
/* checks font list, adds font if new */
static int add_font(struct FONTSPEC *f)
{
	int fnum;

	for (fnum = nfontnames; --fnum >= 0; )
		if (strcmp(f->name, fontnames[fnum]) == 0)
			return fnum;		/* already there */

	if (nfontnames >= MAXFONTS) {
		fprintf(stderr, "++++ Too many fonts\n");
		exit(1);
	}
	fnum = nfontnames;
	strcpy(fontnames[fnum], f->name);
	if (verbose >= 10)
		printf("New font %s at %d\n", f->name, fnum);
	nfontnames++;
	return fnum;
}

/* -- make_font_list -- */
void make_font_list(struct FORMAT *f)
{
	if (verbose >= 10)
		printf("Adding fonts from format..\n");
	add_font(&f->titlefont);
	add_font(&f->subtitlefont);
	add_font(&f->composerfont);
	add_font(&f->partsfont);
	add_font(&f->vocalfont);
	add_font(&f->textfont);
	add_font(&f->wordsfont);
	add_font(&f->gchordfont);
}

/* -- set_standard_format -- */
void set_standard_format(struct FORMAT *f)
{
	memset(f, 0, sizeof *f);
	strcpy(f->name, "standard");
	f->pageheight	= PAGEHEIGHT;
	f->staffwidth	= STAFFWIDTH;
	f->leftmargin	= LEFTMARGIN;
	f->topmargin	= 1.0 * CM;
	f->botmargin	= 1.0 * CM;
	f->topspace	= 0.8 * CM;
	f->titlespace 	= 0.2 * CM;
	f->subtitlespace = 0.1 * CM;
	f->composerspace = 0.2 * CM;
	f->musicspace	= 0.2 * CM;
	f->partsspace	= 0.3 * CM;
	f->staffsep	= 46.0 * PT;
	f->vocalspace	= 23.0 * PT;
	f->textspace	= 0.5 * CM;
/*	f->indent	= 0.0 * CM; */
/*	f->wordsspace	= 0.0 * CM; */
	f->scale	= 0.70;
	f->maxshrink	= 0.65;
/*	f->landscape	= 0; */
/*	f->titleleft	= 0; */
	f->stretchstaff	= 1;
/*	f->stretchlast	= 0; */
/*	f->continueall	= 0; */
/*	f->breakall	= 0; */
/*	f->writehistory	= 0; */
/*	f->withxrefs	= 0; */
/*	f->oneperpage	= 0; */
/*	f->musiconly	= 0; */
/*	f->titlecaps	= 0; */
/*	f->barsperstaff	= 0; */
/*	f->encoding	= 0; */
	f->lineskipfac	= 1.1;
	f->parskipfac	= 0.4;
	fontspec(&f->titlefont,	"Times-Roman", 15.0);
	fontspec(&f->subtitlefont, "Times-Roman", 12.0);
	fontspec(&f->composerfont, "Times-Italic", 11.0);
	fontspec(&f->partsfont,	"Times-Roman", 11.0);
	fontspec(&f->tempofont,	"Times-Bold", 10.0);
	fontspec(&f->vocalfont,	"Times-Bold", 13.0);
	fontspec(&f->textfont,	"Times-Roman", 12.0);
	fontspec(&f->wordsfont,	"Times-Roman", 12.0);
	fontspec(&f->gchordfont, "Helvetica", 12.0);
	if (verbose >= 10)
		printf("Loading format \"%s\"\n", f->name);
}

/* -- set_pretty_format -- */
void set_pretty_format(struct FORMAT *f)
{
	memset(f, 0, sizeof *f);
	strcpy(f->name, "pretty");
	f->pageheight	= PAGEHEIGHT;
	f->staffwidth	= STAFFWIDTH;
	f->leftmargin	= LEFTMARGIN;
	f->topmargin	= 1.0 * CM;
	f->botmargin	= 1.0 * CM;
	f->topspace	= 0.8 * CM;
	f->titlespace 	= 0.4 * CM;
	f->subtitlespace = 0.1 * CM;
	f->composerspace = 0.25 * CM;
	f->musicspace	= 0.25 * CM;
	f->partsspace	= 0.3 * CM;
	f->staffsep	= 50.0 * PT;
	f->vocalspace	=  23.0 * PT;
	f->textspace	= 0.5 * CM;
/*	f->indent	= 0.0 * CM; */
/*	f->wordsspace	= 0.0 * CM; */
	f->scale	= 0.8;
	f->maxshrink	= 0.55;
/*	f->landscape	= 0; */
/*	f->titleleft	= 0; */
	f->stretchstaff = 1;
/*	f->stretchlast	= 0; */
/*	f->continueall	= 0; */
/*	f->breakall	= 0; */
/*	f->writehistory	= 0; */
/*	f->withxrefs	= 0; */
/*	f->oneperpage	= 0; */
/*	f->musiconly	= 0; */
/*	f->titlecaps	= 0; */
/*	f->barsperstaff = 0; */
/*	f->encoding	= 0; */
	f->lineskipfac	= 1.1;
	f->parskipfac	= 0.1;
	fontspec(&f->titlefont,	"Times-Roman", 18.0);
	fontspec(&f->subtitlefont,  "Times-Roman", 15.0);
	fontspec(&f->composerfont,  "Times-Italic", 12.0);
	fontspec(&f->partsfont,	"Times-Roman", 12.0);
	fontspec(&f->tempofont,	"Times-Bold", 10.0);
	fontspec(&f->vocalfont,	"Times-Bold", 14.0);
	fontspec(&f->textfont,	"Times-Roman", 10.0);
	fontspec(&f->wordsfont,	"Times-Roman", 10.0);
	fontspec(&f->gchordfont, "Helvetica", 12.0);
}

/* -- set_pretty2_format -- */
void set_pretty2_format(struct FORMAT *f)
{
	memset(f, 0, sizeof *f);
	strcpy(f->name, "pretty2");
	f->pageheight	= PAGEHEIGHT;
	f->staffwidth	= STAFFWIDTH;
	f->leftmargin	= LEFTMARGIN;
	f->topmargin	= 1.0 * CM;
	f->botmargin	= 1.0 * CM;
	f->topspace	= 0.8 * CM;
	f->titlespace	= 0.4 * CM;
	f->subtitlespace = 0.1 * CM;
	f->composerspace = 0.3 * CM;
	f->musicspace	= 0.25 * CM;
	f->partsspace	= 0.2 * CM;
	f->staffsep	= 55.0 * PT;
	f->vocalspace	= 23.0 * PT;
	f->textspace	= 0.2 * CM;
/*	f->indent	= 0.0 * CM; */
/*	f->wordsspace	= 0.0 * CM; */
	f->scale	= 0.70;
	f->maxshrink	= 0.55;
/*	f->landscape	= 0; */
	f->titleleft	= 1;
	f->stretchstaff	= 1;
/*	f->stretchlast	= 0; */
/*	f->continueall	= 0; */
/*	f->breakall	= 0; */
/*	f->writehistory = 0; */
/*	f->withxrefs	= 0; */
/*	f->oneperpage	= 0; */
/*	f->musiconly	= 0; */
/*	f->titlecaps	= 0; */
/*	f->barsperstaff	= 0; */
/*	f->encoding	= 0; */
	f->lineskipfac	= 1.1;
	f->parskipfac	= 0.1;
	fontspec(&f->titlefont,	"Helvetica-Bold", 16.0);
	fontspec(&f->subtitlefont, "Helvetica-Bold", 13.0);
	fontspec(&f->composerfont, "Helvetica",	10.0);
	fontspec(&f->partsfont,	"Times-Roman", 12.0);
	fontspec(&f->tempofont,	"Times-Bold", 10.0);
	fontspec(&f->vocalfont,	"Times-Bold", 13.0);
	fontspec(&f->textfont,	"Times-Roman", 10.0);
	fontspec(&f->wordsfont,	"Times-Roman", 10.0);
	fontspec(&f->gchordfont, "Helvetica", 12.0);
}

/* -- print_format -- */
void print_format(struct FORMAT *f)
{
	char yn[2][5]={"no","yes"};

	printf("\nFormat \"%s\":\n", f->name);
	printf("  pageheight    %.2fcm\n", f->pageheight/CM);
	printf("  staffwidth    %.2fcm\n", f->staffwidth/CM);
	printf("  topmargin     %.2fcm\n", f->topmargin/CM);
	printf("  botmargin     %.2fcm\n", f->botmargin/CM);
	printf("  leftmargin    %.2fcm\n", f->leftmargin/CM);
	printf("  topspace      %.2fcm\n", f->topspace/CM);
	printf("  titlespace    %.2fcm\n", f->titlespace/CM);
	printf("  subtitlespace %.2fcm\n", f->subtitlespace/CM);
	printf("  composerspace %.2fcm\n", f->composerspace/CM);
	printf("  musicspace    %.2fcm\n", f->musicspace/CM);
	printf("  partsspace    %.2fcm\n", f->partsspace/CM);
	printf("  wordsspace    %.2fcm\n", f->wordsspace/CM);
	printf("  textspace     %.2fcm\n", f->textspace/CM);
	printf("  indent        %.2fcm\n", f->indent/CM);
	printf("  vocalspace    %.1fpt\n", f->vocalspace);
	printf("  staffsep      %.1fpt\n", f->staffsep);
	printf("  scale         %.2f\n",  f->scale);
	printf("  maxshrink     %.2f\n",  f->maxshrink);
	printf("  titlefont     %s %.1f\n", f->titlefont.name, f->titlefont.size);
	printf("  subtitlefont  %s %.1f\n", f->subtitlefont.name, f->subtitlefont.size);
	printf("  composerfont  %s %.1f\n", f->composerfont.name, f->composerfont.size);
	printf("  partsfont     %s %.1f\n", f->partsfont.name, f->partsfont.size);
	printf("  tempofont     %s %.1f\n", f->tempofont.name, f->tempofont.size);
	printf("  vocalfont     %s %.1f\n", f->vocalfont.name, f->vocalfont.size);
	printf("  gchordfont    %s %.1f\n", f->gchordfont.name, f->gchordfont.size);
	printf("  textfont      %s %.1f\n", f->textfont.name, f->textfont.size);
	printf("  wordsfont     %s %.1f\n", f->wordsfont.name, f->wordsfont.size);
	printf("  lineskipfac   %.1f\n", f->lineskipfac);
	printf("  parskipfac    %.1f\n", f->parskipfac);
	printf("  barsperstaff  %d\n",	f->barsperstaff);
	if (f->encoding != 0)
		printf("  encoding      ISOLatin%d\n", f->encoding);
	else	printf("  encoding      ASCII\n");
	printf("  landscape     %s\n", yn[f->landscape]);
	printf("  titleleft     %s\n", yn[f->titleleft]);
	printf("  titlecaps     %s\n", yn[f->titlecaps]);
	printf("  musiconly     %s\n", yn[f->musiconly]);
	printf("  stretchstaff  %s\n", yn[f->stretchstaff]);
	printf("  stretchlast   %s\n", yn[f->stretchlast]);
	printf("  writehistory  %s\n", yn[f->writehistory]);
	printf("  continueall   %s\n", yn[f->continueall]);
	printf("  breakall      %s\n", yn[f->breakall]);
	printf("  oneperpage    %s\n", yn[f->oneperpage]);
	printf("  withxrefs     %s\n", yn[f->withxrefs]);
}

/* -- g_logv: read a logical variable -- */
static int g_logv(char *l)
{
	char t[31];

	if (*l == 0)
		return 1;
	get_str(t, l, sizeof t);
	if (!strcmp(t, "1") || !strcmp(t, "yes") || !strcmp(t, "true"))
		return 1;
	if (!strcmp(t, "0") || !strcmp(t, "no") || !strcmp(t, "false"))
		return 0;
	printf("++++ Unknown logical \"%s\" in line: %s\n", t, l);
	exit(3);
}

/* -- g_fltv: read a float variable, no units -- */
static float g_fltv(char *l)
{
	float v;

	sscanf(l, "%f", &v);
	return v;
}

/* -- g_intv: read an int variable, no units -- */
static int g_intv(char *l)
{
	int v;

	sscanf(l, "%d", &v);
	return v;
}

/* -- g_fspc: read a font specifier -- */
static void g_fspc(char *l,
		   struct FONTSPEC *fn)
{
	char	fname[STRLFMT];
	float	fsize;
	char *p;

	fsize = fn->size;
	p = get_str(fname, l, sizeof fname);
	if (*p != '\0')
		fsize = g_fltv(p);
	fontspec(fn,
		 strcmp(fname, "*") != 0 ? fname : fn->name,
		 fsize);
	if (!file_initialized)
		add_font(fn);
}

/* -- read a line with a format directive, set in format struct f -- */
int interpret_format_line(char *l,
			  struct FORMAT *f)
{
	char *p;
	char w[81];

	p = get_str(w, l, sizeof w);
	if (w[0] == '\0'
	    || w[0] == '%')
		return 0;
	if (verbose >= 6)
		printf("Interpret format line: %s\n", l);
	if (!strcmp(w, "end"))
		return 1;

	if	(!strcmp(w, "pageheight"))	f->pageheight = scan_u(p);
	else if (!strcmp(w, "staffwidth"))	f->staffwidth = scan_u(p);
	else if (!strcmp(w, "topmargin"))	f->topmargin = scan_u(p);
	else if (!strcmp(w, "botmargin"))	f->botmargin = scan_u(p);
	else if (!strcmp(w, "leftmargin"))	f->leftmargin = scan_u(p);
	else if (!strcmp(w, "topspace"))	f->topspace = scan_u(p);
	else if (!strcmp(w, "wordsspace"))	f->wordsspace = scan_u(p);
	else if (!strcmp(w, "titlespace"))	f->titlespace = scan_u(p);
	else if (!strcmp(w, "subtitlespace"))	f->subtitlespace = scan_u(p);
	else if (!strcmp(w, "composerspace"))	f->composerspace = scan_u(p);
	else if (!strcmp(w, "musicspace"))	f->musicspace = scan_u(p);
	else if (!strcmp(w, "partsspace"))	f->partsspace = scan_u(p);
	else if (!strcmp(w, "staffsep"))	f->staffsep = scan_u(p);
	else if (!strcmp(w, "vocalspace"))	f->vocalspace = scan_u(p);
	else if (!strcmp(w, "textspace"))	f->textspace = scan_u(p);
	else if (!strcmp(w, "indent"))		f->indent = scan_u(p);

	else if (!strcmp(w, "scale"))		f->scale = g_fltv(p);
	else if (!strcmp(w, "maxshrink"))	f->maxshrink = g_fltv(p);
	else if (!strcmp(w, "lineskipfac"))	f->lineskipfac = g_fltv(p);
	else if (!strcmp(w, "parskipfac"))	f->parskipfac = g_fltv(p);

	else if (!strcmp(w, "barsperstaff"))	f->barsperstaff = g_intv(p);
	else if (!strcmp(w, "encoding")) {
		f->encoding = g_intv(p);
		if (f->encoding < 0 || f->encoding > 6) {
			printf("++++ Bad encoding value %d - changed to 0\n",
				f->encoding);
			f->encoding = 0;
		}
	} else if (!strcmp(w, "titleleft"))	f->titleleft = g_logv(p);
	else if (!strcmp(w, "titlecaps"))	f->titlecaps = g_logv(p);
	else if (!strcmp(w, "landscape"))	f->landscape = g_logv(p);
	else if (!strcmp(w, "musiconly"))	f->musiconly = g_logv(p);
	else if (!strcmp(w, "stretchstaff"))	f->stretchstaff = g_logv(p);
	else if (!strcmp(w, "stretchlast"))	f->stretchlast = g_logv(p);
	else if (!strcmp(w, "continueall"))	f->continueall = g_logv(p);
	else if (!strcmp(w, "breakall"))	f->breakall = g_logv(p);
	else if (!strcmp(w, "writehistory"))	f->writehistory = g_logv(p);
	else if (!strcmp(w, "withxrefs"))	f->withxrefs = g_logv(p);
	else if (!strcmp(w, "oneperpage"))	f->oneperpage = g_logv(p);

	else if (!strcmp(w, "titlefont"))	g_fspc(p, &f->titlefont);
	else if (!strcmp(w, "subtitlefont"))	g_fspc(p, &f->subtitlefont);
	else if (!strcmp(w, "vocalfont"))	g_fspc(p, &f->vocalfont);
	else if (!strcmp(w, "partsfont"))	g_fspc(p, &f->partsfont);
	else if (!strcmp(w, "tempofont"))	g_fspc(p, &f->tempofont);
	else if (!strcmp(w, "textfont"))	g_fspc(p, &f->textfont);
	else if (!strcmp(w, "composerfont"))	g_fspc(p, &f->composerfont);
	else if (!strcmp(w, "wordsfont"))	g_fspc(p, &f->wordsfont);
	else if (!strcmp(w, "gchordfont"))	g_fspc(p, &f->gchordfont);

	else if (!strcmp(w, "font")) {
		struct FONTSPEC tempfont;
		int fnum;

		get_str(w, p, sizeof w);
		for (fnum = nfontnames; --fnum >= 0; ) {
			if (!strcmp(w, fontnames[fnum]))
				break;
		}
		if (fnum < 0) {
			if (file_initialized) {
				printf("++++ Cannot predefine when output file open: %s\n",
				       l);
				exit(3);
			}
			tempfont.size = 12.0;
			g_fspc(p, &tempfont);
		}
	} else {
		if (verbose >= 5)
			printf("Ignore format line: %s\n", l);
		return 2;
	}
	return 0;
}

/* -- read_fmt_file -- */
int read_fmt_file(char filename[],
		  char dirname[],
		  struct FORMAT *f)
{
	FILE *fp;
	char line[BSIZE], fname[201];

	strcpy(fname, filename);
	if ((fp = fopen(fname, "r")) == NULL) {
		if (dirname[0] == '\0')
			return 0;
#ifdef WIN32
                sprintf(fname, "%s\\%s", dirname, filename);
#else
                sprintf(fname, "%s/%s", dirname, filename);
#endif
		if ((fp = fopen(fname, "r")) == NULL)
			return 0;
	}

	if (verbose >= 4)
		printf("Reading format file %s:\n", fname);
/*	printf("%s .. ", fname);*/
	for (;;) {
		line[0] = '\0';
		if (!fgets(line, BSIZE, fp))
			break;
		line[strlen(line) - 1] = '\0';
		if (interpret_format_line(line, f))
			break;
	}
	fclose(fp);
	return 1;
}
