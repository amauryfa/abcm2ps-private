/*
 *  This file is part of abc2ps, Copyright (C) 1996,1997  Michael Methfessel
 *  Modified for abcm2ps, Copyright (C) 1998-2000 Jean-François Moine
 *  See file abc2ps.c for details.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <ctype.h>

#include "abcparse.h"
#include "abc2ps.h" 

struct STAFF staff_tb[MAXSTAFF];	/* staff table */
int nstaff;			/* (0..MAXSTAFF-1) */

struct VOICE_S voice_tb[MAXVOICE];	/* voice table */
int nvoice;			/* (0..MAXVOICE-1) */
static struct VOICE_S *curvoice;	/* current voice while parsing */

static char seq_tb[9] = {	/* sequence indexed by symbol type */
	SQ_EXTRA,
	SQ_EXTRA, SQ_NOTE, SQ_NOTE, SQ_BAR,
	SQ_CLEF, SQ_SIG, SQ_SIG, SQ_SIG
};

static char *add_wd(char *str);
static char *def_voice(unsigned char *p, int *p_voice);
static void get_clef(struct SYMBOL *s);
static void get_key(struct SYMBOL *s);
static void get_meter(struct SYMBOL *s);
static void get_voice(struct SYMBOL *s);
static void new_line(void);
static void new_note(struct SYMBOL *s);
static void process_pscomment(struct SYMBOL *s);
static void sym_link(struct SYMBOL *s);

/*  subroutines connected with parsing the input file  */

/* -- returns a new symbol at end of list -- */
struct SYMBOL *add_sym(struct VOICE_S *p_voice,
		       int type)
{
	struct SYMBOL *s;

	s = (struct SYMBOL *) getarena(sizeof (struct SYMBOL));
	memset(s, 0, sizeof *s);
	if (p_voice->sym != 0) {
		p_voice->last_symbol->next = s;
		s->prev = p_voice->last_symbol;
	} else	p_voice->sym = s;
	p_voice->last_symbol = s;

	s->type = type;
	s->seq = seq_tb[type];
	s->voice = p_voice->voice;
	s->staff = p_voice->staff;
	return s;
}

/* -- insert a symbol after a reference one -- */
struct SYMBOL *ins_sym(int type,
		       struct SYMBOL *s,	/* previous symbol */
		       int voice)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *new_s, *next;

	curvoice = p_voice = &voice_tb[voice];
	p_voice->last_symbol = s;
	next = s->next;
	new_s = add_sym(p_voice, type);
	if ((new_s->next = next) != 0)
		next->prev = new_s;
	return new_s;
}

/* -- get staves definition (%%staves) -- */
static void get_staves(unsigned char *p)
{
	int	voice, staff;
	unsigned char flags, flags2;
	unsigned char vtb[MAXVOICE];
#define OPEN_BRACE 0x01
#define CLOSE_BRACE 0x02
#define OPEN_BRACKET 0x04
#define CLOSE_BRACKET 0x08
#define OPEN_PARENTH 0x10
#define CLOSE_PARENTH 0x20

	/* define the voices */
	memset(vtb, 0, sizeof vtb);
	flags = 0;
	voice = -1;
	staff = -1;
	while (*p != '\0') {
		switch (*p) {
		case ' ':
		case '\t':
			break;
		case '[':
			if (flags & (OPEN_BRACKET | OPEN_BRACE | OPEN_PARENTH))
				goto err;
			flags |= OPEN_BRACKET;
			voice = -1;
			break;
		case ']':
			if (voice == -1)
				goto err;
			vtb[voice] |= CLOSE_BRACKET;
			break;
		case '{':
			if (flags & (OPEN_BRACKET | OPEN_BRACE | OPEN_PARENTH))
				goto err;
			flags |= OPEN_BRACE;
			voice = -1;
			break;
		case '}':
			if (voice == -1)
				goto err;
			vtb[voice] |= CLOSE_BRACE;
			break;
		case '(':
			if (flags & OPEN_PARENTH)
				goto err;
			flags |= OPEN_PARENTH;
			voice = -1;
			break;
		case ')':
			if (voice == -1)
				goto err;
			vtb[voice] |= CLOSE_PARENTH;
			break;
		default:
			if (!isalnum(*p))
				goto err;
			p = def_voice(p, &voice);
			vtb[voice] = flags;
			flags = 0;
			continue;
		}
		p++;
	}

	/* check for errors */
	flags = CLOSE_BRACKET | CLOSE_BRACE | CLOSE_PARENTH;	/* bad flags */
	flags2 = flags;
	for (voice = 0; voice <= nvoice; voice++) {
		if (vtb[voice] & flags)
			goto err;
		if (vtb[voice] & CLOSE_PARENTH) {
			if (vtb[voice] & flags)
				goto err;
			flags = flags2;
		}
		if (vtb[voice] & OPEN_BRACKET) {
			flags &= ~CLOSE_BRACKET;
			flags |= OPEN_BRACKET | OPEN_BRACE;
		} else if (vtb[voice] & CLOSE_BRACKET) {
			flags &= ~(OPEN_BRACKET | OPEN_BRACE);
			flags |= CLOSE_BRACKET;
		} else if (vtb[voice] & OPEN_BRACE) {
			flags &= ~CLOSE_BRACE;
			flags |= OPEN_BRACKET | OPEN_BRACE;
		} else if (vtb[voice] & CLOSE_BRACE) {
			flags &= ~(OPEN_BRACKET | OPEN_BRACE);
			flags |= CLOSE_BRACE;
		}
		if (vtb[voice] & OPEN_PARENTH) {
			flags2 = flags;
			flags &= ~CLOSE_PARENTH;
		}
	}

	/* define the staves */
	memset(staff_tb, 0, sizeof staff_tb);
	for (voice = 0; voice <= nvoice; voice++) {
		int v;

		flags = vtb[voice];
		if (flags & CLOSE_PARENTH) {
			if (flags & CLOSE_BRACE)
				voice_tb[voice - 1].second = 1;
			else	voice_tb[voice].second = 1;
			staff_tb[staff].nvoice++;
		} else {
			if (++staff >= MAXSTAFF) {
				wng("Too many staves", "");
				return;
			}
			staff_tb[staff].nvoice = 1;
		}
		voice_tb[voice].staff = staff;
		if (flags & OPEN_BRACKET)
			staff_tb[staff].bracket = 1;
		if (flags & CLOSE_BRACKET)
			staff_tb[staff].bracket_end = 1;
		if (flags & OPEN_BRACE) {
			for (v = voice + 1; v <= nvoice; v++)
				if (vtb[v] & CLOSE_BRACE)
					break;
			switch (v - voice) {
			case 1:
				if (flags & OPEN_PARENTH)
					goto err;
				break;
			case 2:
				if (flags & OPEN_PARENTH
				    || (vtb[voice + 1] & OPEN_PARENTH))
					break;
				voice++;
				voice_tb[voice].second = 1;
				voice_tb[voice].floating = 1;
				voice_tb[voice].staff = staff;
				staff_tb[staff].nvoice++;
				break;
			case 3:
				if (flags & OPEN_PARENTH
				    && (vtb[voice + 2] & OPEN_PARENTH))
					break;
				if (flags & OPEN_PARENTH
				    || (vtb[voice + 1] & OPEN_PARENTH)
				    || (vtb[voice + 2] & OPEN_PARENTH))
					goto err;
				/* '{a b c d}' -> '{(a b) (c d)}' */
				vtb[voice] |= OPEN_PARENTH;
				flags  |= OPEN_PARENTH;
				vtb[voice + 1] |= CLOSE_PARENTH;
				vtb[voice + 2] |= OPEN_PARENTH;
				vtb[voice + 3] |= CLOSE_PARENTH;
				break;
			default:
				goto err;
			}
			staff_tb[staff].brace = 1;
		}
		if (flags & CLOSE_BRACE)
			staff_tb[staff].brace_end = 1;
		if (flags & OPEN_PARENTH) {
			if (!(vtb[voice + 1] & CLOSE_PARENTH))
				wng("Cannot handle yet more than 2 voices per staff",
				    "");
		}
	}
	nstaff = staff;
	return;

	/* when error, let one voice per staff */
err:
	wng("%%%%staves error", "");
	for (voice++; voice <= nvoice; voice++) {
		staff_tb[staff].nvoice = 1;
		voice_tb[voice].staff = ++staff;
	}
	nstaff = staff;
}

/* -- def_voice: define a voice by name -- */
/* the voice is created if it does not exist */
static char *def_voice(unsigned char *p,
		       int *p_voice)
{
	char *name;
	char sep;
	int voice;

	name = p;
	while (isalnum(*p))
		p++;
	sep = *p;
	*p = '\0';

	if (voice_tb[0].name == 0)
		voice = 0;		/* first voice */
	else {
		for (voice = 0; voice <= nvoice; voice++) {
			if (strcmp(name, voice_tb[voice].name) == 0)
				goto done;
		}
		if (voice >= MAXVOICE) {
			wng("Too many voices", "");
			voice--;
		}
	}
	nvoice = voice;
        voice_tb[voice].name = add_wd(name);
	voice_tb[voice].staff = nstaff;
	voice_tb[voice].sym = 0;
	voice_tb[voice].clef = TREBLE;
done:
	*p_voice = voice;
	*p = sep;
	return p;
}

/* -- initialize the general tune characteristics of all potential voices -- */
/*fixme: should copy the voice 0 to the other ones */
static void voice_init(void)
{
	struct VOICE_S *p_voice;
	int	i;

	for (i = 0, p_voice = voice_tb;
	     i < MAXVOICE;
	     i++, p_voice++) {
		p_voice->sym = 0;
		p_voice->voice = i;
	}
}

/* -- trim_title: move trailing "The" to front -- */
static void trim_title(unsigned char *p)
{
	unsigned char *q;
	int l;

	l = strlen(p);
	q = p + l - 3;
	if (strcmp(q, "The") != 0)
		return;
	q--;
	while (isspace(*q))
		q--;
	if (*q != ',')
		return;
	l = q - p;
	memmove(p + 4, p, l);
	memcpy(p, "The ", 4);
	p[l + 4] = '\0';
}

/* -- identify info line, store in proper place	-- */
static char *state_txt[4] = {
	"global", "header", "tune", "embedded"
};
static void info_field(struct SYMBOL *s)
{
	struct ISTRUCT *inf;
	char info_type = s->as.text[0];
	unsigned char *p = &s->as.text[2];

	/* change global or local */
	inf = s->as.state == ABC_S_GLOBAL ? &default_info : &info;

	while (isspace(*p))
	       p++;

	switch (info_type) {
	case 'A':
		return;
	case 'B':
		inf->book = p;
		return;
	case 'C':
		if (inf->ncomp >= NCOMP)
			wng("Too many composer lines","");
		else {
			inf->comp[inf->ncomp] = p;
			inf->ncomp++;
		}
		return;
	case 'D':
		add_text(p, TEXT_D);
		return;
	case 'E':
	case 'F':
	case 'G':
		return;
	case 'H':
		add_text(p, TEXT_H);
		return;
	case 'I':
		return;
	case 'K':
		get_key(s);
		if (s->as.state != ABC_S_HEAD)
			return;
		if (verbose >= 3)
			printf("---- start %s (%s) ----\n",
			       info.xref, info.title[0]);
		fflush(stdout);
		check_margin(cfmt.leftmargin);
		write_heading();
		insert_btype = -1;
		voice_init();			/* initialize all the voices */
		reset_gen();
		curvoice = &voice_tb[0];	/* and switch to the 1st one */
		return;
	case 'L':
		return;
	case 'M':
		get_meter(s);
		return;
	case 'N':
		add_text(p, TEXT_N);
		return;
	case 'O':
		inf->orig = p;
		return;
	case 'P':
		inf->parts = p;
		goto pqt;
	case 'Q':
		if (curvoice->voice != 0)	/* tempo only for first voice */
			return;
		switch (s->as.state) {
		case ABC_S_GLOBAL:
		case ABC_S_HEAD:
			inf->tempo = s;
			break;
		default:
			sym_link(s);
			s->type = TEMPO;
			break;
		}
		return;
	case 'R':
		inf->rhyth = p;
		return;
	case 'S':
		inf->src = p;
		return;
	case 'T':
		if (inf->ntitle >= 3) {
			wng("Too many T:", "");
			break;
		}
		inf->title[inf->ntitle] = p;
		trim_title(p);
		inf->ntitle++;
	pqt:				/* common to P, T */
		if (s->as.state != ABC_S_TUNE)
			return;
		output_music();
		switch (info_type) {
		case 'P':
			write_parts();
			break;
/*		case 'T': */
		default:
			write_inside_title();
			break;
		}
		voice_init();
		reset_gen();
		curvoice = &voice_tb[0];
		return;
	case 'U': {
		unsigned char *deco;

		deco = s->as.state == ABC_S_GLOBAL ? deco_glob : deco_tune;
		deco[s->as.u.user.symbol] = s->as.u.user.value;
		return;
	}
	case 'u':
		return;
	case 'V':
		get_voice(s);
		return;
	case 'w':
		if (s->as.state != ABC_S_TUNE)
			break;
		return;
	case 'W':
		add_text(p, TEXT_W);
		return;
	case 'X':
		if (!epsf)
			write_buffer(fout);	/* flush stuff left from %% lines */
		PUT1("\n\n%% --- tune %d ---\n", ++tunenum);
		if (!epsf)
			bskip(cfmt.topspace);
		memcpy(&info, &default_info, sizeof info);
		info.xref = p;
		memcpy(&deco_tune, &deco_glob, sizeof deco_tune);
		return;
	case 'Z':
		add_text(p, TEXT_Z);
		return;
	}
	{
		char error_msg[50];

		sprintf(error_msg,
			"%s info '%c:' not treated",
			state_txt[(int) s->as.state], info_type);
		wng(error_msg, "");
	}
}

/* -- set head type, dots, flags for note -- */
void identify_note(int len,
		  int *p_head,
		  int *p_dots,
		  int *p_flags)
{
	int head, dots, flags;
	int base;

	head = H_FULL;
	flags = 0;
	base = CROTCHET;
	if (len >= CROTCHET) {
		if (len >= SEMIBREVE) {
			if (len >= BREVE)
				base = BREVE;
			else	base = SEMIBREVE;
			head = H_OVAL;
		} else if (len >= MINIM) {
			base = MINIM;
			head = H_EMPTY;
		}	
	} else if (len >= QUAVER) {
		base = QUAVER;
		flags = 1;
	} else if (len >= SEMIQUAVER) {
		base = SEMIQUAVER;
		flags = 2;
	} else if (len >= QUAVER / 4) {		/* demisemiquaver */
		base = QUAVER / 4;
		flags = 3;
	} else if (len >= QUAVER / 8) {
		base = QUAVER / 8;
		flags = 4;
	} else	wng("Cannot identify head for note", "");

	dots = 0;
	if (len == base)
		;
	else if (2 * len == 3 * base)
		dots = 1;
	else if (4 * len == 7 * base)
		dots = 2;
	else if (8 * len == 15 * base)
		dots = 3;
	else	wng("Cannot handle note length for note", "");

	*p_head = head;
	*p_dots = dots;
	*p_flags = flags;
}

/* -- add a text -- */
static char *add_wd(char *str)
{
	char *rp;
	int l;

	if ((l = strlen(str)) == 0)
		return 0;
	rp = getarena(l + 1);
	strcpy(rp, str);
	return rp;
}

/* -- bar ('|' found in music) -- */
static void bar(struct SYMBOL *s)
{
#if 0
	/*fixme: if '#if 1', there are repeat indications on the next voices
	 * correct stuff is: have insert_btype/insert_v per voice...
	 */
	struct VOICE_S *p_voice = curvoice;

	if (bar_type == B_INVIS
	    && p_voice->last_symbol->type == BAR) {
		s = p_voice->last_symbol;
	} else {
#endif
		sym_link(s);
		s->type = BAR;

		/* the bar must be before a key signature */
		if (s->prev != 0
		    && s->prev->type == KEYSIG) {
			struct SYMBOL *s3;

			s3 = s->prev;
			curvoice->last_symbol = s3;
			s3->next = 0;
			s3->prev->next = s;
			s->prev = s3->prev;
			s->next = s3;
			s3->prev = s;
		}
#if 0
	}
#endif
	if (s->as.u.bar.eoln)
		new_line();
}

/* -- do a tune -- */
void do_tune(struct abctune *at,
	     int header_only)
{
	struct abcsym *as;
	struct {
		int seq;
	} vtb[MAXVOICE];
	int voice;
	int seq;

	/* initialize */
/*fixme: some job is to move to 'X:'*/
	memset(voice_tb, 0, sizeof voice_tb);
	voice_init();		/* initialize all the voices */
	cfmt = dfmt;
	init_pdims();
	clear_text();
	nvoice = 0;
	nstaff = 0;
	memset(&staff_tb[0], 0, sizeof staff_tb[0]);
	curvoice = &voice_tb[0];
	clear_buffer();
	use_buffer = 1;

	/* scan the tune */
	memset(vtb, 0, sizeof vtb);
	voice = seq = 0;
	for (as = at->first_sym; as != 0; as = as->next) {
		struct SYMBOL *s = (struct SYMBOL *) as;

		if (header_only
		    && as->state != ABC_S_GLOBAL)
			break;
		switch (as->type) {
		case ABC_T_INFO:
			if (header_only
			    && (s->as.text[0] == 'X'
				|| s->as.text[0] == 'T'))
				break;
			vtb[voice].seq = seq;
			info_field(s);
			voice = curvoice->voice;
			seq = vtb[voice].seq;
			break;
		case ABC_T_PSCOM:
			vtb[voice].seq = seq;
			process_pscomment(s);
			voice = curvoice->voice;
			seq = vtb[voice].seq;
			break;
		case ABC_T_REST:
		case ABC_T_NOTE:
			new_note(s);
			seq = 0;
			break;
		case ABC_T_BAR:
			bar(s);
			break;
		case ABC_T_CLEF:
			get_clef(s);
			break;
		}
		s->seq = seq_tb[s->type];
		if (s->type != 0
		    && s->type != NOTE
		    && s->type != REST) {
			if (s->seq <= seq)
				s->seq = seq + 1;
			seq = s->seq;
		}
	}

	output_music();
	put_words(fout);
	if (cfmt.writehistory)
		put_history(fout);
	if (epsf) {
		FILE *feps;
		char fnm[81], finf[81];

		close_output_file();
		if (choose_outname) {
			epsf_title(info.title[0], fnm);
			strcat(fnm, ".eps");
		} else {
			nepsf++;
			sprintf(fnm, "%s%03d.eps", outf, nepsf);
		}
/*fixme: should not be in_file[0]??*/
		sprintf(finf, "%s (%s)", in_file[0], info.xref);
		if ((feps = fopen(fnm, "w")) == NULL)
			rx("Cannot open output file ", fnm);
		init_ps(feps, finf, 1,
			cfmt.leftmargin - 5,
			posy + bposy - 5,
			cfmt.leftmargin + cfmt.staffwidth + 5,
			cfmt.pageheight - cfmt.topmargin);
		init_epsf(feps);
		write_buffer(feps);
		printf("\n[%s] %s", fnm, info.title[0]);
		close_epsf(feps);
		fclose(feps);
		in_page = 0;
		init_pdims();
	} else {
		buffer_eob(fout);
		write_buffer(fout);
		if (verbose == 0 && tunenum % 10 == 0)
			printf(".");
		if (verbose == 2)
			printf("%s - ", info.title[0]);
	}

	if (!epsf) {
		buffer_eob(fout);
		write_buffer(fout);
	}
}

/* -- get a clef definition (in K: or V:) -- */
static void get_clef(struct SYMBOL *s)
{
	struct VOICE_S *p_voice = curvoice;
	int clef = s->as.u.clef.clef;

	if (p_voice->sym == 0)
		staff_tb[(int) p_voice->staff].clef = clef;	/* initial clef */
	else {
		struct SYMBOL *s2;

		for (s2 = p_voice->last_symbol;
		     ;
		     s2 = s2->prev) {
			if (s2->type == CLEF
			    || s2->type == NOTE
			    || s2->type == REST
			    || s2->type == BAR)
				break;
		}
		if (s2->type == CLEF) {
			s2->u = clef;
			s2->v = 1;
			if (s2 == p_voice->sym)
				staff_tb[(int) p_voice->staff].clef = clef;
		} else if (p_voice->clef != clef) {
			sym_link(s);
			s->type = CLEF;
			s->u = clef;
			s->v = 1;

			/* the clef change must be before a key signature */
			if (s->prev->type == KEYSIG) {
				struct SYMBOL *s3;

				s3 = s->prev;
				p_voice->last_symbol = s3;
				s3->next = 0;
				s3->prev->next = s;
				s->prev = s3->prev;
				s->next = s3;
				s3->prev = s;
			}

			/* the clef change must be before a bar */
			if (s2->type == BAR) {
				if (p_voice->last_symbol == s)
					p_voice->last_symbol = s->prev;
				s->prev->next = s->next;
				s2->prev->next = s;
				s->prev = s2->prev;
				s->next = s2;
				s2->prev = s;
			}
		}
	}
	p_voice->clef = clef;		/* current clef */
	p_voice->forced_clef = 1;	/* don't change */
}

/* -- get a key signature definition (K:) -- */
static void get_key(struct SYMBOL *s)
{
	struct VOICE_S *p_voice;
	int i;

	switch (s->as.state) {
	case ABC_S_GLOBAL:
		break;
	case ABC_S_HEAD:
		for (i = MAXVOICE, p_voice = voice_tb;
		     --i >= 0;
		     p_voice++)
			p_voice->p_key = s;
		break;
	case ABC_S_TUNE:
		if (curvoice->sym == 0) {
			curvoice->p_key = s;
			break;
		}
		/* fall thru */
	case ABC_S_EMBED:
		if (s->as.u.key.empty)
			break;		/* clef only */
		sym_link(s);
		s->type = KEYSIG;
		break;
	}
}

/* -- set meter from M: -- */
static void get_meter(struct SYMBOL *s)
{
	switch (s->as.state) {
	case ABC_S_GLOBAL:
		break;
	case ABC_S_HEAD:
		if (curvoice->voice != 0)
			break;
		curvoice->p_meter = s;
		break;
	case ABC_S_TUNE:
		if (curvoice->voice != 0)
			break;
		if (curvoice->sym == 0)
			curvoice->p_meter = s;
		else	{
			sym_link(s);
			s->type = TIMESIG;
		}
		break;
	case ABC_S_EMBED:
		sym_link(s);
		s->type = TIMESIG;
		break;
	}
}

/* -- treat a 'V:' -- */
static void get_voice(struct SYMBOL *s)
{
	int voice, old_nvoice;
	char t[32];

	old_nvoice = nvoice;
	def_voice(s->as.u.voice.name, &voice);
	if (voice > old_nvoice) {	/* new voice */
		if (nstaff >= MAXSTAFF - 1) {
			wng("Too many staves", "");
			return;
		}
		memset(&staff_tb[++nstaff], 0, sizeof staff_tb[0]);
		voice_tb[voice].staff = nstaff;
	}

	/* switch to this voice */
	curvoice = &voice_tb[voice];

	/* if some name has changed, update */
	if (s->as.u.voice.fname != 0) {
		tex_str(t, s->as.u.voice.fname, sizeof t,
			&voice_tb[voice].nmw);
		voice_tb[voice].nm = add_wd(t);
	}
	if (s->as.u.voice.nname != 0) {
		tex_str(t, s->as.u.voice.nname, sizeof t,
			&voice_tb[voice].snmw);
		voice_tb[voice].snm = add_wd(t);
	}
}

/* -- newline (explicit '\\' or \n) -- */
static void new_line(void)
{
	struct VOICE_S *p_voice = curvoice;

	if (p_voice->sym != 0) {
		struct SYMBOL *s;

		s = p_voice->last_symbol;
		if (curvoice->voice == 0
		    && !cfmt.barsperstaff
		    && !cfmt.continueall)
			s->eoln = 1;
	}
}

/* -- new note or rest (while parsing a music line) -- */
static void new_note(struct SYMBOL *s)
{
	int k;

	sym_link(s);
	s->type = s->as.type == ABC_T_NOTE ? NOTE : REST;
	s->nhd = s->as.u.note.nhd;
	memcpy(s->pits, s->as.u.note.pits, sizeof s->pits);

	{
		int head, dots, flags;

		identify_note(s->as.u.note.lens[0],
			     &head, &dots, &flags);
		s->head = head;
		s->dots = dots;
		s->flags = flags;
	}
	if (s->as.u.note.eoln)
		new_line();

	/* convert the decorations */
	for (k = s->as.u.note.dc.n; --k >= 0; ) {
		unsigned char deco;

		deco = s->as.u.note.dc.t[k];
		if (deco < 128) {
			deco = deco_tune[deco];
			if (deco == 0
			    && s->as.u.note.dc.t[k] != 0)
				printf("Decoration '%c' not defined\n",
				       s->as.u.note.dc.t[k]);
			s->as.u.note.dc.t[k] = deco;
		}
	}

	/* change the lyric words */
/* Use '^' to mark a '-' between syllables - hope nobody needs '^' ! */
	if (s->as.u.note.ly) {
		struct lyrics *ly = s->as.u.note.ly;

		for (k = 0; k < MAXLY; k++) {
			char *p;

			if ((p = ly->w[k]) == 0)
				continue;
			if (*p == '-' && p[1] == '\0') {
				*p = '^';
				continue;
			}
			while (*p != '\0') {
				if (*p == '~')
					*p = ' ';
				else if (*p == '-') {
					if (p[-1] == '\\') {
						int l = strlen(p);

						memmove(p - 1, p, l + 1);
						continue;
					}
					*p = '^';
				}
				p++;
			}
		}
	}
}

/* -- process a pseudo-comment (%%) -- */
static void process_pscomment(struct SYMBOL *s)
{
	unsigned char *p;
	int i;
	char *q;
	char w[81];
	float h1;
	/*specific to begintext / endtext*/
	static int in_text_block, job, add_final_nl;

	p = s->as.text;
	if (in_text_block) {
		if (strncmp(p, "%%endtext", 9) == 0) {
			if (job != SKIP)
				write_text_block(fout, job, s->as.state);
			in_text_block = 0;
			return;
		}
		if (*p == '%' && p[1] == '%')
			p += 2;
		if (job != SKIP) {
			if (*p == '\0') {
				write_text_block(fout, job, s->as.state);
				ntxt = 0;
			} else	add_to_text_block(p,
						  add_final_nl);
		}
		return;
	}

	p += 2;				/* skip %% */
	q = p;
	for (i = 0; i < sizeof w - 1; i++) {
		if (*p == '\0' || isspace(*p))
			break;
		w[i] = *p++;
	}
	w[i] = '\0';
	while (isspace(*p))
		p++;
	if (strcmp(w, "begintext") == 0) {
		if (epsf && s->as.state != ABC_S_HEAD)
			return;
		job = OBEYLINES;
		if (*p == '\0'
		    || strncmp(p, "obeylines", 9) == 0)
			;
		else if (strncmp(p, "align", 5) == 0)
			job = ALIGN;
		else if (strncmp(p, "skip", 4) == 0)
			job = SKIP;
		else if (strncmp(p, "ragged", 6) == 0)
			job = RAGGED;
		else	rx("bad argument for begintext: ", p);
		add_final_nl = 0;
		if (job == OBEYLINES)
			add_final_nl = 1;
		output_music();
		buffer_eob(fout);
		set_font(&cfmt.textfont);
		ntxt = 0;
		in_text_block = 1;
		return;
	}

	if (strcmp(w, "text") == 0 || strcmp(w, "center") == 0) {
		if (epsf && s->as.state == ABC_S_GLOBAL)
			return;
		output_music();
		set_font(&cfmt.textfont);
		ntxt = 0;
		add_to_text_block(p, 1);
		if (w[0] == 't')
			write_text_block(fout, OBEYLINES, s->as.state);
		else	write_text_block(fout, OBEYCENTER, s->as.state);
		buffer_eob(fout);
		return;
	}
	if (strcmp(w, "sep") == 0) {
		float h2, len, lwidth;

		output_music();
		lwidth = cfmt.staffwidth;
		h1 = h2 = len = 0;
		if (*p != '\0') {
			h1 = scan_u(p);
			while (*p != '\0' && !isspace(*p))
				p++;
			while (isspace(*p))
				p++;
		}
		if (*p != '\0') {
			h2 = scan_u(p);
			while (*p != '\0' && !isspace(*p))
				p++;
			while (isspace(*p))
				p++;
		}
		if (*p != '\0')
			len = scan_u(p);
		if (h1 < 1)
			h1 = 0.5 * CM;
		if (h2 < 1)
			h2 = h1;
		if (len < 1)
			len = 3.0 * CM;
		bskip(h1);
		PUT2("%.1f %.1f sep0\n",
		     (lwidth - len) * 0.5,
		     (lwidth + len) * 0.5);
		bskip(h2);
		buffer_eob(fout);
		return;
	}
	if (strcmp(w, "vskip") == 0) {
		output_music();
		h1 = scan_u(p);
		if (h1 < 1)
			h1 = 0.5 * CM;
		bskip(h1);
		buffer_eob(fout);
		return;
	}
	if (strcmp(w, "newpage") == 0) {
		output_music();
		write_buffer(fout);
		use_buffer = 0;
		write_pagebreak(fout);
		return;
	}
	if (strcmp(w, "staves") == 0) {
		if (s->as.state == ABC_S_TUNE) {
			output_music();
			voice_init();
			reset_gen();
			nvoice = 0;		/* only one voice */
			voice_tb[0].name = 0;	/* not defined */
		}
		get_staves(p);
		curvoice = &voice_tb[0];
		return;
	}
	interpret_format_line(q, &cfmt);
	ops_into_fmt(&cfmt);
	if (s->as.state == ABC_S_GLOBAL)
		dfmt = cfmt;
}

/* -- link a symbol in a voice -- */
static void sym_link(struct SYMBOL *s)
{
	struct VOICE_S *p_voice = curvoice;

	memset((&s->as) + 1, 0, sizeof (struct SYMBOL) - sizeof (struct abcsym));
	if (p_voice->sym != 0) {
		p_voice->last_symbol->next = s;
		s->prev = p_voice->last_symbol;
	} else	p_voice->sym = s;
	p_voice->last_symbol = s;

	s->voice = p_voice->voice;
	s->staff = p_voice->staff;
}
