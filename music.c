/*  
 * This file is part of abcm2ps.
 * Copyright (C) 1998-2000 Jean-François Moine
 * Adapted from abc2ps, Copyright (C) 1996,1997 Michael Methfessel
 * See file abc2ps.c for details.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>
#include <ctype.h>

#include "abcparse.h" 
#include "abc2ps.h" 

struct BEAM {			/* packages info on one beam */
	struct SYMBOL *s1, *s2;
	float a, b;
	float x, y, t;
	int stem;
};

struct VOICE_P {		/* used by set_sym_glue */
	struct SYMBOL *s0;	/* start of bar */
	char	nplet;
	char	nn;
	char	ncount;
	short	bars;
};

static struct ENDINGS {		/* where to draw endings */
	float a,b;		/* start and end position */
	char *v;		/* value to display */
	short staff;		/* staff */
} ending[20];
static int num_ending;		/* number of endings to draw */
static int mes1, mes2;		/* to count measures in an ending */
static int insert_meter;	/* flag to insert time signature */
static char *insert_v;

static float alfa_last, beta_last;	/* for last short short line.. */
static int mline;			/* number music lines in current tune */

static int vbnp, vbnx;		/* used in set_spacing */
static int vnbp, vnbx;

static struct SYMBOL *tssym;	/* time sorted list of symbols */
static struct SYMBOL *tsnext;	/* next line when cut */

int measure_nb = -1;		/* measure numbering (-1: no, 0: on the left, or every n bars) */
static int nbar;

#include "style.h"		/* globals to define layout style */
char *style = STYLE;

static void set_nplet(struct SYMBOL *s);

#define AT_LEAST(a,b)  { float tmp = b; if((a)<tmp) a=tmp; }

/*  subroutines connected with output of music	*/

/* -- Sets the prefered width for a note depending on the duration -- */
/* Return value is default space on right and left side. 
 * Function is determined by pseudo-logarithmic values.
 * Return value is 1.0 for a crotchet. */
static float nwidth(int len)
{
	float p, x;

	x = (float) len / (float) (BASE_LEN / 4);
	if (len >= BASE_LEN / 4) {
		if (len >= BASE_LEN / 2) {
			if (len >= BASE_LEN)
				p = 0.054 * x + 1.744;
			else	p = 0.17 * x + 1.24;
		} else	p = 0.6 * x + 0.4;
	} else {
		if (len < BASE_LEN / 8) {
			if (len < BASE_LEN / 16)
				p = 0.4 * x + 0.41;
			else	p = 0.46 * x + 0.395;
		} else	p = 0.75 * x + 0.25;
	}
/*printf(" nwidth 1/%d w: %.3f\n", BASE_LEN/len, p);*/
	return p;
}

/* -- next_note, prev_note -- */
static struct SYMBOL *next_note(struct SYMBOL *k)
{
	for (k = k->next; k != 0; k = k->next) {
		if (k->type == NOTE || k->type == REST)
			return k;
	}
	return 0;
}

static struct SYMBOL *prev_note(struct SYMBOL *k)
{
	for (k = k->prev; k != 0; k = k->prev) {
		if (k->type == NOTE || k->type == REST)
			return k;
	}
	return 0;
}

/* -- preceded_by_note -- */
static struct SYMBOL *preceded_by_note(struct SYMBOL *s)
{
	do {
		s = s->prev;
	} while (s->type == INVISIBLE);
	if (s->type == NOTE
	    || s->type == REST)
		return s;
	return 0;
}

/* -- show sym properties set by parser -- */
static void print_syms(struct SYMBOL *sym)
{
	int t,j,y;
	struct SYMBOL *s;
	struct abcsym *as;
static char bsym[10] = {'-', '1' ,'2', '3', '4', '5', '6', '7', '8', '9'};
static char *acc_tb[] = {"", "^", "=", "_", "^^", "__"};
static char *clef_tb[7] = {"TREBLE", "ALTO1", "ALTO2", "ALTO", "ALTO4", "BASS3", "BASS"};
static char *bar_tb[8] = {"invisible", "single", "double", "thin-thick",
			  "thick-thin", "left repeat", "right repeat", "double repeat"};

	printf("\n------- Symbol list -------\n"
	       "word   slur  eol description\n");

	for (s = sym; s != 0; s = s->next) {
		int word_end, slur_st, slur_end;

		as = &s->as;
		t = s->type;
		if (t == NOTE
		    || t == REST) {
			word_end = as->u.note.word_end;
			slur_st = as->u.note.slur_st;
			slur_end = as->u.note.slur_end;
		} else {
			word_end = slur_st = slur_end = 0;
		}
		printf(" %c %c   %c %c   %c  ",
			bsym[(unsigned) s->word_st], bsym[word_end],
			bsym[slur_st], bsym[slur_end],
			bsym[(unsigned) s->eoln] );
		switch (t) {
		case NOTE:
			printf("NOTE ");
		case REST:
			if (t == REST)
				printf("REST ");
			if (s->nhd > 0)
				printf(" [");
			for (j = 0; j <= s->nhd; j++) {
				y = 3 * (s->pits[j] - 18);
				printf(" %s%2d-%-2d", acc_tb[(unsigned) as->u.note.accs[j]],
				       y, as->u.note.lens[j]);
			}
			if (s->nhd > 0)
				printf(" ]");
			if (as->u.note.p_plet)
				printf(" (%d:%d:%d",
				       as->u.note.p_plet,
				       as->u.note.q_plet,
				       as->u.note.r_plet);
			if (s->as.text != 0)
				printf(" \"%s\"", s->as.text);
			if (as->u.note.dc.n > 0) {
				printf(" deco ");
				for (j = 0; j < as->u.note.dc.n; j++) {
					unsigned char c;

					c = as->u.note.dc.t[j];
					if (c == 0)
						printf("(none)");
					if (c < 128)
						printf("%c", c);
					else	printf("!%s!", deco_tb[c]);
				}
			}
			if (as->u.note.gr) {
				printf(" grace ");
				for (j = 0; j < as->u.note.gr->n; j++) {
					if (j > 0)
						printf("-");
					printf("%s%d",
					       acc_tb[(unsigned) as->u.note.gr->a[j]],
					       as->u.note.gr->p[j]);
				}
			}
			break;

		case BAR:
			printf("BAR  ======= %s", bar_tb[s->as.u.bar.type]);
			if (s->as.text)
				printf(", ending %s", s->as.text);
			break;

		case CLEF:
			printf("CLEF  %s", clef_tb[s->u]);
			break;

		case TIMESIG:
			printf("TIMESIG ");
			if (as->u.meter.flag == 1)
				printf("C");
			else if (as->u.meter.flag == 2)
				printf("C|");
			else if (as->u.meter.top != 0)
				printf("%s/%d", as->u.meter.top, as->u.meter.m2);
			else	printf("%d/%d", as->u.meter.m1, as->u.meter.m2);
			break;

		case KEYSIG:
			printf("KEYSIG  %d ", s->as.u.key.sf);
			if (s->as.u.key.sf > 0)
				printf("sharps");
			else if (s->as.u.key.sf < 0)
				printf("flats");
			printf(" from %d", s->u);
			break;
		case TEMPO:
			printf("TEMPO ");
			if (s->as.u.tempo.str != 0)
				printf("'%s' ", s->as.u.tempo.str);
			printf("%d=%d",
			       s->as.u.tempo.length, s->as.u.tempo.value);
			break;

		case INVISIBLE:
			printf("INVIS");
			break;

		default:
			printf("UNKNOWN");
			break;
		}
		printf("\n");
	}
	printf("\n");
}

/* -- set_head_directions -- */
/* decide whether to shift heads to other side of stem on chords */
/* also position accidentals to avoid too much overlap */
static void set_head_directions(struct SYMBOL *s)
{
	int i, n, sig, d, da, shift, nac;
	int i1, i2, m;
	float dx, xmn;
	struct note *note;

	note = &s->as.u.note;
	n = note->nhd;
	sig = s->stem > 0 ? 1 : -1;
	for (i = 0; i <= n; i++) {
		s->shhd[i] = 0;
		s->shac[i] = 8;
		if (s->head == H_OVAL)
			s->shac[i] += 3;
	}
	s->xmx = 0;
	if (n == 0)
		return;

	shift = 0;				/* shift heads */

	i1 = 1;
	i2 = n + 1;
	if (sig < 0) {
		i1 = n - 1;
		i2 = -1;
	}
	for (i = i1; i != i2; i += sig) {
		d = s->pits[i] - s->pits[i - sig];
		if (d < 0)
			d = -d;
		if (d >= 2 || d == 0) 
			shift = 0;
		else {
			shift = 1 - shift;
			if (shift) {
				dx = 7.8;
				if (s->head == H_EMPTY)
					dx = 7.8;
				else if (s->head == H_OVAL)
					dx = 10.0;
				if (s->stem < 0)
					s->shhd[i] = -dx;
				else
					s->shhd[i] = dx;
			}
		}
		if (s->shhd[i] > s->xmx)
			s->xmx = s->shhd[i];
	}

	shift = 0;			/* shift accidentals */
	for (i = n + 1; --i >= 0; ) {
		xmn = 0;		/* left-most pos of a close head */
		nac = 99;		/* relative pos of next acc above */
		for (m = 0; m <= n; m++) {
			float xx;

			xx = s->shhd[m];
			d = s->pits[m] - s->pits[i];
			da = d > 0 ? d : -d;
			if (da <= 5 && xx < xmn)
				xmn = xx;
			if (d > 0 && da < nac && note->accs[m])
				nac = da;
		}
		s->shac[i] = 8.5 - xmn + s->shhd[i];	/* aligns accidentals in column */
		if (s->head == H_EMPTY)
			s->shac[i] += 1.0;
		else if (s->head == H_OVAL)
			s->shac[i] += 3.0;
		if (note->accs[i]) {
			if (nac >= 6)			/* no overlap */
				shift = 0;
			else if (nac >= 4) {		/* weak overlap */
				if (shift == 0)
					shift++;
				else	shift--;
			} else {			/* strong overlap */
				switch (shift) {
				case 0: shift = 2; break;
				case 1: shift = 3; break;
				case 2: shift = 1; break;
				case 3: shift = 0; break;
				}
			}
#if 0
			while (shift >= 4)
				shift -= 4;
#endif
			s->shac[i] += 3. * shift;
		}
	}
}

/* -- define the clef for a staff -- */
/* this function is called only once for the whole tune */
static void set_clef(int voice)
{
	struct SYMBOL *s;
	int min, max;
	int note_found;
	struct SYMBOL *last_bar;
	int clef;
	int chg;
	struct SYMBOL *a;

	min = max = 16;			/* 'C' */

	/* count the number of notes upper and lower than 'C' */
	for (s = voice_tb[voice].sym;
	     s != 0;
	     s = s->next) {
		int xp;

		if (s->type != NOTE)
			continue;
		xp = s->nhd;
		if (s->pits[xp] > max)
			max = s->pits[xp];
		else if (s->pits[0] < min)
			min = s->pits[0];
		if ((a = s->a) != 0
#if 0
		    /*fixme*/
		    && !voice_tb[(unsigned) a->voice].floating
#endif
		   ) {
			if (a->pits[a->nhd] > max)
				max = a->pits[a->nhd];
			else if (a->pits[0] < min)
				min = a->pits[0];
		}
	}
	if (min >= 13)			/* all upper than 'G,' --> treble clef */
		return;
	if (max <= 19) {		/* all lower than 'F' --> bass clef */
		staff_tb[(unsigned) voice_tb[voice].staff].clef = BASS;
#if 0
		voice_tb[voice].sym->u = BASS;
#endif
		return;
	}

	/* set clef changes */
	last_bar = 0;
	if (max + min >= 2 * 16)
		clef = TREBLE;
	else	{
		clef = BASS;
		staff_tb[(unsigned) voice_tb[voice].staff].clef = clef;
#if 0
		voice_tb[voice].sym->u = clef;
#endif
	}
	chg = 0;
	note_found = 0;
	for (s = voice_tb[voice].sym;
	     s != 0;
	     s = s->next) {
		if (s->type != NOTE) {
			if (s->type == BAR
			    && note_found)
				last_bar = s;
			continue;
		}
#if 1
		a = s->a;
#else
		/*fixme: not useful: the floating voice is on the right staff*/
		if ((a = s->a) != 0
		    && voice_tb[(unsigned) s->a->voice].floating)
			a = 0;
#endif
		note_found = 1;
		if (clef == TREBLE) {
			int xp = s->nhd;

			if (s->pits[xp] <= 12		/* 'F,' */
			    || (a != 0
				&& a->pits[0] <= 12)
			    || (s->pits[xp] <= 14	/* 'A,' */
				&& s->next != 0
#if 0
				&& s->next->type == NOTE
#endif
				&& s->next->pits[(unsigned) s->next->nhd] <= 14))
				chg++;
		} else {
			if (s->pits[0] >= 20		/* 'G' */
			    || (a != 0
				&& a->pits[0] >= 20)

			    || (s->pits[0] >= 19	/* 'F' */
				&& s->next != 0
#if 0
				&& s->next->type == NOTE
#endif
				&& s->next->pits[0] >= 19))
				chg++;
		}
		if (chg) {
			chg = 0;
			if (clef == TREBLE)
				clef = BASS;
			else	clef = TREBLE;
			if (last_bar == 0) {
				staff_tb[(unsigned) voice_tb[voice].staff].clef = clef;
#if 0
				voice_tb[voice].sym->u = clef;
#endif
			} else {
				struct SYMBOL *new_s, *first_bar;
				int time, seq;

				/* the clef change must be before the bar */
				/*fixme: may appear inside a measure */
				new_s = ins_sym(CLEF, last_bar->prev, voice);
				new_s->u = clef;
				new_s->v = 1;
				time = last_bar->time;
				seq = last_bar->seq;
				for (first_bar = last_bar->ts_prev;
				     first_bar != 0;
				     first_bar = first_bar->ts_prev)
					if (first_bar->time != time
					    || first_bar->seq != seq)
						break;
				new_s->ts_next = first_bar->ts_next;
				new_s->ts_next->ts_prev = new_s;
				first_bar->ts_next = new_s;
				new_s->ts_prev = first_bar;
				new_s->time = time;
				new_s->seq = SQ_CLEF;
				last_bar = s->next;
			}
		}
	}
}

/* -- sort the symbols by time -- */
/* this function is called only once for the whole tune */
static void def_tssym(void)
{
	int voice;
	struct SYMBOL *s, *t, *prev_sym;
	struct {
		struct SYMBOL *s;
		short staff_chg;
		short selected;
	} vtb[MAXVOICE];
	int time, bars;

	memset(vtb, 0, sizeof vtb);

	/* sort the symbol by time */
	prev_sym = 0;
	tssym = 0;
	for (voice = 0; voice <= nvoice; voice++) {
		if ((s = voice_tb[voice].sym) == 0) {
			int v;

			printf(">>> voice %s is empty\n", voice_tb[voice].name);
			if (voice != nvoice) {
				memmove(&voice_tb[voice],
					&voice_tb[voice + 1],
					sizeof voice_tb[0] * (nvoice - voice));
			}
			nvoice--;
			voice--;
			for (v = voice; v <= nvoice; v++) {
				for (s = voice_tb[v].sym;
				     s != 0;
				     s = s->next)
					s->voice = v;
			}
			continue;
		}
		s->ts_prev = prev_sym;
		if (prev_sym != 0)
			prev_sym->ts_next = s;
		else	tssym = s;
		prev_sym = s;
		vtb[voice].s = s->next;
	}
	bars = 0;			/* (for errors) */
	time = 0;
	for (;;) {
		int seq;

		/* search the closest next time/sequence */
		time += 100000;
		seq = 0;
		t = 0;
		for (voice = 0; voice <= nvoice; voice++) {
			if ((s = vtb[voice].s) == 0
			    || s->time > time)
				continue;
			if (s->time < time
			    || s->seq < seq) {
				time = s->time;
				seq = s->seq;
				t = s;
			}
		}
		if (t == 0)
			break;		/* echu (finished) */

		/* warn about incorrect number of notes / measures */
		t = 0;
		for (voice = 0; voice <= nvoice; voice++) {
			vtb[voice].selected = 0;
			if ((s = vtb[voice].s) != 0
			    && s->time == time
			    && s->seq == seq) {
				vtb[voice].selected = 1;
				if (s->type == BAR
				    && s->as.u.bar.type != B_INVIS
				    && t == 0)
					t = s;
			}
		}

		if (t != 0) {
			int ko = 0;

			bars++;
			for (voice = 0; voice <= nvoice; voice++) {
				if ((s = vtb[voice].s) == 0)
					continue;
				if (s->type == NOTE
				    || s->type == REST
				    || s->time != time) {
					ko = 1;
					break;
				}
			}
			if (ko) {
				printf(">>> line %d - too many notes in measure %d "
				       "for voice %s\n",
				       s != 0 ? s->as.linenum : 0, bars,
				       voice_tb[(int) s->voice].name);
				for (voice = 0; voice <= nvoice; voice++) {
					if ((t = vtb[voice].s) == 0)
						continue;
					if (t->type == BAR)
						t->time = time + s->len;
				}
				bars--;
				continue;
			}
		}

		/* set the staff of the floating voices */
		for (voice = 0; voice <= nvoice; voice++) {
			struct SYMBOL *u;
			int d1, d2;

			if (!voice_tb[voice].floating
			    || !vtb[voice].selected)
				continue;
			s = vtb[voice].s;
			t = vtb[voice + 1].s;
			if (t == 0)
				continue;
			if (s->type != NOTE
			    && s->type != REST) {
				if (vtb[voice].staff_chg)
					s->staff = t->staff;
				continue;
			}

			u = vtb[voice - 1].s;
			if (u == 0)
				continue;
			d1 = u->pits[0] - s->pits[0];
			d2 = s->pits[0] - t->pits[0];
			if (!vtb[voice].staff_chg) {
				int xp = s->nhd;

				if (d2 < 0
				    || (s->pits[xp] <= 13	/* G, */
					&& d2 < 7)
				    || d1 > 7)
					vtb[voice].staff_chg = 1;
			} else {
				if (d1 < 0
				    || (s->pits[0] >= 19	/* F */
					&& d1 < 7)
				    || d2 > 7)
					vtb[voice].staff_chg = 0;
			}
			if (vtb[voice].staff_chg)
				s->staff = t->staff;
		}

		/* associate the symbols on a same staff */
		for (voice = 0; voice <= nvoice; voice++) {
			if (!vtb[voice].selected)
				continue;
			s = vtb[voice].s;
			if (s->type != NOTE
			    && s->type != REST)
				continue;
			if (voice > 0
			    && (t = vtb[voice - 1].s) != 0) {
				if (!vtb[voice - 1].selected)
					t = t->prev;
				if (s->staff == t->staff) {
					if (s->a == 0)
						s->a = t;
					if (t->a == 0)
						t->a = s;
				}
			}
			if (voice < nvoice
			    && (t = vtb[voice + 1].s) != 0) {
				if (!vtb[voice + 1].selected)
					t = t->prev;
				if (s->staff == t->staff) {
					if (s->a == 0)
						s->a = t;
					if (t->a == 0)
						t->a = s;
				}
			}
		}

		/* set the time linkage */
		for (voice = 0; voice <= nvoice; voice++) {
			if (!vtb[voice].selected)
				continue;
			s = vtb[voice].s;
			s->ts_prev = prev_sym;
			prev_sym->ts_next = s;
			prev_sym = s;
			if ((vtb[voice].s = s->next) != 0)
				s->next->time = time + s->len;
		}
	}
}

/* -- do left indentation -- */
static float set_indent(void)
{
	int voice, staff;
	float w;
	float more_shift;

	/*fixme: split big lines*/
	w = 0;
	if (mline == 1) {

		/* first line: use main name */
		for (voice = 0; voice <= nvoice; voice++) {
			if (w < voice_tb[voice].nmw)
				w = voice_tb[voice].nmw;
		}

		/* if no name, indent the first line when many lines */
		if (w == 0)
			return tsnext == 0
				? 0
				: cfmt.indent;
	} else {

		/* other lines: use subname */
		for (voice = 0; voice <= nvoice; voice++) {
			if (w < voice_tb[voice].snmw)
				w = voice_tb[voice].snmw;
		}
		if (w == 0)
			return 0;
	}
	more_shift = 0;
	for (staff = 0; staff <= nstaff; staff++) {
		if (staff_tb[staff].brace
		    || staff_tb[staff].bracket) {
			more_shift = 10.;
			break;
		}
	}
	return cfmt.vocalfont.swfac * cfmt.vocalfont.size * (w + 4 * cwid(' '))
		+ more_shift;
}

/* -- set staves and stems when multivoice -- */
/* this function is called only once per tune */
/* it supposes that the first symbol of each voice is the clef */
/* (this code is limited to 2 voices per staff) */
static void set_multi(void)
{
	int i2;
	int staff;
	int voice;
	char staff_clef[MAXSTAFF];
	struct SYMBOL *s;

	/* set the note/rest length, set a pitch for all symbols */
	for (voice = 0; voice <= nvoice; voice++) {
		int pitch;
		struct SYMBOL *sym;

		sym = voice_tb[voice].sym;

		pitch = 22;				/* 'B' - if no note! */
		for (s = sym; s != 0; s = s->next) {
			if (s->type == NOTE) {
				pitch = s->pits[0];
				break;
			}
		}
		for (s = sym; s != 0; s = s->next) {
			if (s->type == NOTE
			    || s->type == REST) {
				if (s->as.u.note.p_plet != 0)	/* start of a n-plet */
					set_nplet(s);
				if (s->len == 0)
					s->len = s->as.u.note.lens[0];
			}
			if (s->type == NOTE) {
				pitch = s->pits[0];
				if (s->prev->type != NOTE)
					s->prev->pits[0] = (s->prev->pits[0]
							    + pitch) / 2;
			} else	s->pits[0] = pitch;
		}
	}

	/* sort the symbols by time */
	def_tssym();

	/* set the clefs */
	for (voice = 0; voice <= nvoice; voice++) {
		if (!voice_tb[voice].second		/* if not a secondary voice */
		    && !voice_tb[voice].forced_clef)	/* and no explicit clef */
			set_clef(voice);		/* set the main clef */
	}

	/* set the starting clefs */
	for (voice = 0; voice <= nvoice; voice++)
		voice_tb[voice].sym->u = staff_tb[(unsigned) voice_tb[voice].staff].clef;

	/* adjust the pitch of the notes */
	for (s = tssym; s != 0; s = s->ts_next) {
		int delta;

		staff = s->staff;
		switch (s->type) {
		case CLEF:
			staff_clef[staff] = s->u;
			continue;
		default:
			continue;
		case NOTE:
		case REST:
			break;
		}
		delta = 2 * staff_clef[staff];
		if (delta != 0) {
			for (i2 = s->nhd + 1; --i2 >= 0; )
				s->pits[i2] += delta;
			if (s->as.u.note.gr) {
				for (i2 = s->as.u.note.gr->n; --i2 >= 0; )
					s->as.u.note.gr->p[i2] += delta;
				}
		}
	}

	/* set the stems direction (in 'multi') */
	/* for the rests, 'u' is used to adjust the height in the staff */
	for (voice = 0; voice <= nvoice; voice++) {
		s = voice_tb[voice].sym;
		for (;;) {
			struct SYMBOL *t, *s2;
			int multi;
			int h;

			/* look in the measure if any associated note */
			multi = 0;
			for (t = s;
			     t != 0 && t->type != BAR;
			     t = t->next) {
				if ((s2 = t->a) != 0
				    && !s2->as.u.note.invis) {
					multi = s->voice < s2->voice
						? 1 : -1;
					break;
				}
			}
			if (t == 0)
				break;
			if (t->type == BAR) {
				if ((s = t->next) == 0)
					break;
				continue;
			}

			for (;
			     s != 0 && s->type != BAR;
			     s = s->next) {
				if ((s2 = s->a) != 0)
					multi = s->voice < s2->voice
						? 1 : -1;
				s->multi = multi;
				if (s->type != REST)
					continue;
/*fixme: the rest may be over many notes - see rest centering*/
				if (s2 != 0
				    && s2->type == REST
				    && (s->as.u.note.invis
					|| s2->as.u.note.invis)) {
					h = 12.;	/* as if 1 voice */
				} else if (multi > 0) {
					if (s2 != 0
					    && s2->type == NOTE) {
						h = 3 * ((s2->pits[(unsigned) s2->nhd]
							  & ~3) - 18) + 12;
						if (h < 18)
							h = 18;
					} else	h = 18;
				} else {
					if (s2 != 0
					    && s2->type == NOTE) {
						h = 3 * ((s2->pits[0]
							  & ~3) - 18) - 12;
						if (h > 6)
							h = 6;
					} else	h = 6;
				}
				s->u = h;
			}
			if (s == 0)
				break;
			if ((s = s->next) == 0)
				break;
		}
	}
}

/* -- change the length of the n-plets -- */
static void set_nplet(struct SYMBOL *s)
{
	struct SYMBOL *t;
	int l, r, a, b, n, lplet;

	l = 0;
	a = SEMIBREVE;
	t = s;
	r = s->as.u.note.r_plet;
	while (r > 0) {
		if (t->type == NOTE
		    || t->type == REST) {
			l += t->as.u.note.lens[0];
			if (t->as.u.note.lens[0] < a)
				a = t->as.u.note.lens[0];
			r--;
		}
		if ((t = t->next) == 0) {
			wng("Not enough notes in a n-plet", "");
			break;
		}
	}
	n = l / a;
	lplet = (l * s->as.u.note.q_plet) / s->as.u.note.p_plet;
	r = s->as.u.note.r_plet;
	while (r > 0) {
		if (s->type == NOTE
		    || s->type == REST) {
			b = s->as.u.note.lens[0] / a;
			l = (lplet * b) / n;
			lplet -= l;
			n -= b;
			s->len = l;
			r--;
		}
		if ((s = s->next) == 0)
			break;
	}

	/* set the beam break */
	if (s != 0) {
		for (s = s->prev; s != 0; s = s->prev) {
			if (s->type == NOTE) {
				s->beam_break = 1;
				break;
			}
		}
	}
}

/* -- set the y offset of the staves and return the whole height -- */
static float set_staff(void)
{
	int	i;
	float	y = 0;
	float	staffsep;

	staffsep = 0.5 * cfmt.staffsep;
	for (i = nstaff; i >= 0; i--) {

		/* bottom */
		if (staff_tb[i].nvocal > 0
		    && !cfmt.musiconly) {
			float yword;

			yword = cfmt.vocalspace;
			if (cfmt.vocalfont.size - staff_tb[i].botpos > yword)
				yword = cfmt.vocalfont.size - staff_tb[i].botpos;
			y += staffsep
				+ yword
				+ 1.1 * cfmt.vocalfont.size * (staff_tb[i].nvocal - 1);
		} else {
			if (-staff_tb[i].botpos > staffsep)
				y += -staff_tb[i].botpos;
			else	y += staffsep;
		}
		staff_tb[i].y = y;
		y += 24.;
		if (i == 0)
			staffsep = 0.5 * cfmt.staffsep;
		else	staffsep = 0.5 * 0.8 * cfmt.staffsep;

		/* top */
		if (staff_tb[i].toppos > staffsep)
			y += staff_tb[i].toppos;
		else	y += staffsep;
	}
	return y;
}

/* -- set symbol characteristics -- */
/* this function is called only once per tune */
static void set_sym_chars(void)
{
	int np, m, pav;
	struct SYMBOL *s;

	/* set the vertical offsets */
	for (s = tssym; s != 0; s = s->ts_next) {
		switch (s->type) {
		case REST:
			/* if 'multi' is not null, 'u' contains the desired 
			 * vertical position (see set_multi) */
			if (s->multi == 0)
				s->y = 12;
			else	s->y = s->u;
			break;
		case NOTE:
			pav = 0;
			np = s->nhd;
			for (m = 0; m <= np; m++)
				pav += s->pits[m];
			s->y = 3 * (s->pits[0] - 18);
			s->ymn = s->y;
			s->ymx = 3 * (s->pits[np] - 18);
			s->yav = 3 * ((pav / (np + 1)) - 18);
			break;
		}
	}
}

/* -- decide on beams and on stem directions -- */
/* this routine is called only once per tune */
static void set_beams(struct SYMBOL *sym)
{
	int start_flag;
	struct SYMBOL *s, *lastnote;
	int beam, laststem, stem, lasty;

	/* separate words at notes without flags */
	start_flag = 1;
	lastnote = 0;
	for (s = sym; s != 0; s = s->next) {
		switch (s->type) {
		case REST:
			if (s->as.u.note.word_end)
				s->as.u.note.word_end = 0;
			else if (!s->as.u.note.eoln)
				continue;
			/* fall thru */
		default:
			if (lastnote != 0) {
				lastnote->as.u.note.word_end = 1;
				start_flag = 1;
				lastnote = 0;
			}
			continue;
		case NOTE:
			break;
		}
		if (s->flags == 0) {
			if (lastnote != 0)
				lastnote->as.u.note.word_end = 1;
			s->word_st = 1;
			start_flag = s->as.u.note.word_end = 1;
		} else {
			if (start_flag)
				s->word_st = 1;
			start_flag = s->as.u.note.word_end;
		}
		lastnote = s;
	}

#if CUT_NPLETS
	/* separate words before and after n-plet */
	{
		int num, nplet;

		num = nplet = 0;
		lastnote = 0;
		for (s = sym; s != 0; s = s->next) {
			if (s->type != NOTE && s->type != REST)
				continue;
			num++;
			if (nplet && num == nplet) {
				if (lastnote != 0)
					lastnote->as.u.note.word_end = 1;
				s->word_st = 1;
			}
			if (s->as.u.note.p_plet) {
				nplet = s->as.u.note.r_plet;
				num = 0;
				if (lastnote != 0)
					lastnote->as.u.note.word_end = 1;
				s->word_st = 1;
			}
			lastnote = s;
		}
	}
#endif

	/* set stem directions; near middle, use previous direction */
	beam = 0;
	stem = 0;				/* (compilation warning) */
	laststem = 0;
	lasty = 0;
	for (s = sym; s != 0; s = s->next) {
		if (s->type != NOTE) {
			laststem = 0;
			continue;
		}
		if ((s->stem = s->multi) == 0) {

			/* not set by set_multi() (voice alone on the staff) */
			if (s->len < SEMIBREVE)
				s->stem = s->yav >= 12 ? -1 : 1;
#ifndef BSTEM_DOWN
			if (laststem != 0
			    && s->yav > 11
			    && s->yav < 13) {
				int dy;

				dy = s->yav - lasty;
				if (dy > -7 && dy < 7)
					s->stem = laststem;
			}
#endif
			/* notes in a beam have the same stem direction */
			if (s->word_st
			    && !s->as.u.note.word_end) {	/* start of beam */
				int avg;
				struct SYMBOL *t;
				int n;

				avg = s->yav;
				n = 1;
				for (t = s->next; t != 0; t = t->next) {
					if (t->type == NOTE) {
						avg += t->yav;
						n++;
					}
					if (t->as.u.note.word_end)
						break;
				}
				avg /= n;
				stem = 1;
				if (avg >= 12) {
					stem = -1;
#ifndef BSTEM_DOWN
					if (avg == 12
					    && laststem != 0)
						stem = laststem;
#endif
				}
				beam = 1;
			}
			if (beam)
				s->stem = stem;
		} else {			/* stem set by set_multi */
			if (s->word_st
			    && !s->as.u.note.word_end) {	/* start of beam */
				beam = 1;
				stem = s->stem;
			}
		}

		if (s->as.u.note.word_end)
			beam = 0;
		if (voice_tb[(unsigned char) s->voice].bagpipe)
			s->stem = -1;
		if (s->len >= SEMIBREVE
		    || (s->len >= CROTCHET && s->len < MINIM
			&& s->as.u.note.stemless))
			s->stem = 0;
		laststem = s->stem;
		if (s->len >= MINIM)
			laststem = 0;
		lasty = s->yav;
	}
}

/* -- set a shift when notes overlap -- */
static void set_overlap(void)
{
	struct SYMBOL *s;

	for (s = tssym; s != 0; s = s->ts_next) {
		struct SYMBOL *s2;
		int d, m;

		if (s->multi <= 0		/* no associated symbol or 1st voice */
		    || (s2 = s->a) == 0
		    || s->time != s2->time
		    || s->seq != s2->seq
		    || s->type != NOTE
		    || s2->type != NOTE)
			continue;

		/*fixme: does not work when chords*/
		d = s->pits[0] - s2->pits[0];
		if (d < 0
		    || d == 1
		    || (d == 0
			&& s->len != s2->len
			/*fixme: same for QUAVER-SEMIQUAVER, etc..*/
			&& (s->len != CROTCHET
			    || (s2->len != QUAVER && s2->len != SEMIQUAVER))
			&& ((s->len != QUAVER && s->len != SEMIQUAVER)
			    || s2->len != CROTCHET))) {
			float d1, d2, x2, dy1, dy2;

			/*fixme: does not work when both notes have accidentals*/
			d1 = d2 = x2 = dy1 = 0;
			/* the dot of the 2nd voice should be lower */
			dy2 = -3;
			/* if the 1st voice is below the 2nd one, shift the 1st voice (lowest) */
			if (d < 0) {
				if (d == -1)
					d1 = 7.;
				else	d1 = 4.;
			/* and shift the dot of the 2nd voice if any */
				if (s2->dots > 0)
					x2 = d1;
			/* the dot of the 1st voice must be lower */
				dy1 = -3;
				dy2 = 0;
			/* if the upper note is dotted but not the lower one,
			 * shift the 1st voice */
			} else if (s->dots > 0) {
				if (s2->dots == 0)
					d1 = 8.;
			/* if both notes are dotted, have a bigger shift on the 2nd voice */
				else	d2 = 12. + 3.5 * s2->dots;
			/* if the upper note if MINIM or higher, shift the 1st voice */
			} else if (s->head != H_FULL
				 && s->len > s2->len)
				d1 = 8.;
			/* else shift the 2nd voice */
			else	d2 = 8.;
			/* do the shift, and have a minimal space */
			if (d1 > 0) {
				for (m = s->nhd + 1; --m >= 0; ) {
					s->shhd[m] += d1;
					s->shac[m] += d1;
				}
				s->xmx += d1;
				s2->xmx += x2;
				s->wr += d1 + 2;
				s2->wr += d1 + 2;
			} else /*if (d2 > 0)*/ {
				for (m = s2->nhd + 1; --m >= 0; ) {
					s2->shhd[m] += d2;
					s2->shac[m] += d2;
				}
				s2->xmx += d2;
				s->wr += d2 + 2;
				s2->wr += d2 + 2;
			}
			s->doty = dy1;
			s2->doty = dy2;
		} else if (d == 2) {
			if (s->head == H_OVAL && s2->head != H_OVAL) {
				for (m = s2->nhd + 1; --m >= 0; )
					s2->shac[m] += 3;
			} else if (s->head != H_OVAL && s2->head == H_OVAL) {
				for (m = s->nhd + 1; --m >= 0; )
					s->shac[m] += 3;
			}
		}
	}
}

/* -- set the stem lengths -- */
/* and set the top and bottom positions of the staves */
static void set_stems(struct SYMBOL *sym)
{
	struct SYMBOL *s;

	for (s = sym; s != 0; s = s->next) {
		int staff;
		float slen, ymin, ymax;

		/*fixme: may have gchord in bar*/
		if (s->type != NOTE)
			continue;

		/* shift notes in chords (need stem direction to do this) */
		set_head_directions(s);

		/* set height of stem end, without considering beaming for now */
		slen = STEM;
		if (s->nhd > 0)
			slen = STEM_CH;
		if (s->flags == 3)
			slen += 4;
		else if (s->flags == 4)
			slen += 9;
		if (s->stem > 0) {
			if (s->flags > 2)
				slen -= 1;
			if (s->pits[s->nhd] > 26) {
				slen -= 2;
				if (s->pits[s->nhd] > 28)
					slen -= 2;
			}
			s->y = ymin = s->ymn;
			s->ys = ymax = s->ymx + slen;
		} else if (s->stem < 0) {
			if (s->pits[0] < 18) {
				slen -= 2;
				if (s->pits[0] < 16)
					slen -= 2;
			}
			s->y = ymax = s->ymx;
			s->ys = ymin = s->ymn - slen;
		} else {
			s->ys = s->y = ymin = ymax = s->ymx;
		}

		/* add vertical staff space on top for guitar chords */
		if (s->as.text != 0) {
			char *gch;

			ymin += cfmt.gchordfont.size * 0.5;
			gch = s->as.text;
			while ((gch = strstr(gch, "\\n")) != 0) {
				ymin += cfmt.gchordfont.size;
				gch++;
			}
		}

		/* adjust staff top and bottom */
		/*fixme: have space for the decorations */
		staff = s->staff;
		if (staff_tb[staff].toppos < ymax + 5. - 24.)
			staff_tb[staff].toppos = ymax + 5. - 24.;
		if (staff_tb[staff].botpos > ymin - 5.)
			staff_tb[staff].botpos = ymin - 5.;
	}
}

/* -- set widths and prefered space -- */
/* This routine sets the minimal left and right widths wl,wr
   so that successive symbols are still separated when
   no extra glue is put between them. It also sets the prefered
   spacings pl,pr for good output and xl,xr for expanded layout.
   All distances in pt relative to the symbol center.
   Here is also set the number of lyric lines of the staves. */
static void set_sym_widths(struct SYMBOL *sym)
{
	int j, m, k;
	float xx, w, swfac, spc;
	char t[81];
	struct SYMBOL *s;

	swfac = cfmt.vocalfont.swfac;

	for (s = sym; s != 0; s = s->next) {
		struct SYMBOL *prev;

		switch (s->type) {
		case INVISIBLE:		/* empty space; shrink,space,stretch from u,v,w */
			s->wl = s->wr = 0.5 * s->u;
			s->pl = s->pr = 0.5 * s->v;
			s->xl = s->xr = 0.5 * s->w;
			break;

		case NOTE:
		case REST:
			s->xr = s->pr = nwidth(s->len);
			if (s->head == H_EMPTY) {
				s->wl = 6.0;
				s->wr = 14.0;
			} else if (s->head == H_OVAL) {
				s->wl = 8.0;
				s->wr = 18.0;
			} else
				s->wl = s->wr = 4.5;

			/* room for shifted heads and accidental signs */
			for (m = 0; m <= s->nhd; m++) {
				xx = s->shhd[m];
				AT_LEAST(s->wr, xx + 6.);
				AT_LEAST(s->wl, -xx + 6.);
				if (s->as.u.note.accs[m]) {
					xx -= s->shac[m];
					AT_LEAST(s->wl, -xx + 3.);
				}
			}
			s->xl = s->wl;
			s->pl = s->wl;

			/* room for slide */
			for (k = s->as.u.note.dc.n; --k >= 0; ) {
				if (s->as.u.note.dc.t[k] == D_slide)
					s->wl += 10.;
			}

			/* room for grace notes */
			if (s->as.u.note.gr) {
				xx = GSPACE0 + 1.;
				if (s->as.u.note.gr->a[0])
					xx += 3.5;
				for (j = 1; j < s->as.u.note.gr->n; j++) {
					xx += GSPACE;
					if (s->as.u.note.gr->a[j])
						xx += 4.;
				}
				s->wl += xx;
				s->pl += xx;
				s->xl += xx;
			}

			/* space for flag if stem goes up on standalone note */
			if (s->word_st && s->as.u.note.word_end
			    && s->stem > 0 && s->flags > 0)
				AT_LEAST(s->wr, 12.);

			/* leave room for dots */
			if (s->dots > 0) {
				AT_LEAST(s->wr, 12. + s->xmx);
				if (s->dots >= 2)
					s->wr += 3.5;
				if (s->head == H_EMPTY)
					s->wr += 1;
				else if (s->head == H_OVAL)
					s->wr += 2;

				/* special case: standalone with up-stem and flags */
				if (s->flags && s->stem > 0
				    && s->word_st
				    && s->as.u.note.word_end
				    && !(s->y % 6)) {
					s->wr += DOTSHIFT;
					s->pr += DOTSHIFT;
					s->xr += DOTSHIFT;
				}
			}

			/* extra space when down stem follows up stem */
			if ((prev = preceded_by_note(s)) != 0) {
				if (prev->stem > 0 && s->stem < 0)
					AT_LEAST(s->wl, 7);

				/* make sure helper lines don't overlap */
				if (s->y > 27 && prev->y > 27)
					AT_LEAST(s->wl, 7.5);
			}

			/* leave room for guitar chord */
			if (s->as.text != 0) {
				struct SYMBOL *k;
				char *gchlower;

				/* special case: guitar chord under ending 1 or 2 */
				/* leave some room to the left of the note */
				if ((k = s->prev) != 0
				    && k->type == BAR
				    && k->as.text != 0
				    && isdigit((unsigned char) k->as.text[0]))
					AT_LEAST(s->wl, 18.);

				/* rest is same for all guitar chord cases */
				tex_str(t, s->as.text, sizeof t, &w);

				/* may have many lines in guitar chord */
				if ((gchlower = strstr(t, "\\n")) != 0) {

					/* stacked chord/figured bass */
					float wlower;
					char *p;

					w = 0;
					p = t;
					do {
						*gchlower = '\0';
						wlower = 0;
						for (; *p; p++)
							wlower += cwid(*p);
						if (wlower > w)
							w = wlower;
						p += 2;		/* skip "\n" */
					} while ((gchlower = strstr(p, "\\n")) != 0);
					wlower = 0;
					for (; *p; p++)
						wlower += cwid(*p);
					if (wlower > w)
						w = wlower;
				}
				xx = cfmt.gchordfont.size * w;
				spc = xx * GCHPRE;
				if (spc > 8.0)
					spc = 8.0;
				/* fixme: may have a bar with gchord*/
				k = prev_note(s);
				if (k != 0 && k->as.text != 0)
					AT_LEAST(s->wl, spc);
				k = next_note(s);
				if (k != 0 && k->as.text != 0)
					AT_LEAST(s->wr, xx - spc);
			}

			/* leave room for vocals under note */
			/* and update the number of lyric line per staff */
			/*fixme: pb when lyrics of 2 voices in the same staff */
			if (!cfmt.musiconly
			    && s->as.u.note.ly) {
				struct lyrics *ly = s->as.u.note.ly;
				int nlyric = 0;
				int staff;

				for (j = 0; j < MAXLY; j++) {
					struct SYMBOL *k;

					if (ly->w[j] == 0)
						continue;
					nlyric = j + 1;
					tex_str(t, ly->w[j], sizeof t, &w);
					xx = swfac * cfmt.vocalfont.size
						* (w + 2 * cwid(' '));
					if ((k = s->next) != 0
					    && (k->type == REST || k->type == NOTE)
					    && (k->as.u.note.ly == 0
						|| k->as.u.note.ly->w[j] == 0))
						xx -= 20.;	/*fixme: which width?*/
					if (isdigit(ly->w[j][0])) {
						float shift;

						shift = 5. * cfmt.vocalfont.size
							* swfac * cwid('1');
						AT_LEAST(s->wl, shift);
						AT_LEAST(s->wr, xx - shift);
					} else {
						AT_LEAST(s->wl, xx * VOCPRE);
						AT_LEAST(s->wr, xx * (1.0 - VOCPRE));
					}
				}
				staff = voice_tb[(int) s->voice].staff;
				if (staff_tb[staff].nvocal < nlyric)
					staff_tb[staff].nvocal = nlyric;

			}

			AT_LEAST(s->pl, s->wl);
			AT_LEAST(s->xl, s->wl);
			AT_LEAST(s->pr, s->wr);
			AT_LEAST(s->xr, s->wr);
			break;

		case BAR:
			switch (s->as.u.bar.type) {
			case B_SINGLE:
				s->wl = s->wr = 3; break;
			case B_DOUBLE:
				s->wl = 7; s->wr = 4; break;
			case B_LREP:
				s->wl = 5; s->wr = 12; break;
			case B_RREP:
				s->wl = 12; s->wr = 5; break;
			case B_DREP:
				s->wl = s->wr = 12; break;
			case B_THICK_THIN:
				s->wl = 3; s->wr = 9; break;
			case B_THIN_THICK:
				s->wl = 9; s->wr = 3; break;
			case B_INVIS:
				s->wl = s->wr = 0; break;
			}
			s->pl = s->xl = s->wl;
			s->pr = s->xr = s->wr;
			/*fixme: have room for gchord*/
			break;

		case CLEF:
			s->wl = s->wr = s->xl = 12;
			s->pl = s->pr = s->xr = 12;
			break;

		case KEYSIG: {
			int n1, n2;
			int esp = 0;

			n1 = s->as.u.key.sf;	/* new key sig */
			n2 = s->as.u.key.old_sf; /* old key */
			if (n1 * n2 >= 0) {
				if (n1 < 0 || n2 < 0) {
					n1 = -n1;
					n2 = -n2;
				}
				if (n2 > n1)
					n1 = n2;
			} else {
				n1 -= n2;
				if (n1 < 0)
					n1 = -n1;
				esp = 3;	/* see draw_keysig */
			}
			if (n1 > 0) {
				s->wl = 5;
				s->wr = 5 * n1 + esp + 5;
				s->pl = s->xl = s->wl;
				s->pr = s->xr = s->wr;
#if 0
			} else {
				s->wl = s->pl = s->xl = 3;
				s->wr = s->pr = s->xr = 3;
#endif
			}
			break;
		}

		case TIMESIG:
			if (s->as.u.meter.top != 0)
				s->wl = 8 + 4 * (strlen(s->as.u.meter.top) - 1);
			else	s->wl = 8;
			s->wr = s->wl + 4;
			s->pl = s->xl = s->wl;
			s->pr = s->xr = s->wr;
			break;

		default:
			printf(">>> line %d - cannot set width for sym type %d\n",
			       s->as.linenum, s->type);
			/* fall thru */
		case TEMPO:		/* no space */
			s->wl = s->wr = s->xl = 0;
			s->pl = s->pr = s->xr = 0;
			break;
		}
	}
}

/* -- set the end of a piece of tune -- */
/* tsnext is the beginning of the next line */
static void set_piece(struct SYMBOL *s)
{
	int voice;

	/* if last line, do nothing */
	if ((tsnext = s->ts_next) == 0)
		return;

	/* if the key signature changes on the next line,
	 * put it at the end of the current line */
	if (tsnext->type == KEYSIG) {
		for (s = s->ts_next; s->ts_next != 0; s = s->ts_next)
			if (s->ts_next->type != KEYSIG)
				break;
		if ((tsnext = s->ts_next) == 0)
			return;
	}
	s->ts_next = 0;

	/* set end of voices */
	for (voice = nvoice + 1 ; --voice >= 0; ) {
		for (s = tsnext->ts_prev; s != 0; s = s->ts_prev)
			if (s->voice == voice) {
				s->next = 0;
				break;
			}
	}
}

/* -- set the basic spacing before symbol -- */
static void set_spacing(struct SYMBOL *s,
			struct VOICE_P *voice_p)
{
	struct SYMBOL *prev;
	float	w0, w1, w2, nw1, nw2;

	prev = s->prev;
	s->shrink = prev->wr + s->wl;
	s->space = prev->pr + s->pl;
	s->stretch = prev->xr + s->xl;

	prev = preceded_by_note(s);
	switch (s->type) {
	case NOTE:
	case REST:
		nw1 = nwidth(s->len);
		if (prev != 0) {		/* two notes behind each other */
			nw2 = nwidth(prev->len);
			w1 = lnnp * nw2;
			w2 = lnnp * nw1;
			AT_LEAST(s->space,
				 bnnp * w1 + (1 - bnnp) * 0.5 * (w1 + w2));
			w1 = lnnx * nw2;
			w2 = lnnx * nw1;
			AT_LEAST(s->stretch,
				 bnnx * w1 + (1 - bnnx) * 0.5 * (w1 + w2));

			/* squeeze notes a bit if big jump in pitch */
			if (s->type == NOTE
			    && prev->type == NOTE) {
				int dy;
				float fac;

				dy = s->y - prev->y;
				if (dy < 0)
					dy =- dy;
				fac = 1. - 0.010 * dy;
				if (fac < 0.9)
					fac = 0.9;
				s->space *= fac;
				s->stretch *= fac;
			}
		} else {			/* note at start of bar */
			w1 = lbnp * nw1;
			w0 = lbnp * nwidth(vbnp);
			AT_LEAST(s->space,
				 bbnp * w1 + (1 - bbnp) * w0 + s->prev->pr);
			w1 = lbnx * nw1;
			w0 = lbnx * nwidth(vbnx);
			AT_LEAST(s->stretch,
				 bbnx * w1 + (1 - bbnx) * w0 + s->prev->xr);
			voice_p->s0 = s;	/* remember start of bar */
			voice_p->nn = 0;	/* count of notes in bar */
		}

		if (!s->word_st) {		/* reduce spacing within a beam */
			s->space *= fnnp;
			s->stretch *= fnnx;
		}

		if (s->as.u.note.p_plet > 0)		 /* reduce spacing in n-plet */
			voice_p->nplet = s->as.u.note.r_plet - 1;
		else if (voice_p->nplet > 0) {
			voice_p->nplet--;
			s->space *= gnnp;
			s->stretch *= gnnx;
		}

		voice_p->nn++;
		voice_p->ncount++;
		if (voice_p->bars < 0)
			voice_p->bars = 0;
		break;

	default:
		break;

	case BAR: {
		if (prev == 0)
			break;

		/* end of bar reached */
		nw2 = nwidth(prev->len);
		w1 = lnbp * nw2;
		w0 = lnbp * nwidth(vnbp);
		AT_LEAST(s->space,
			 bnbp * w1 + (1 - bnbp) * w0 + s->pl);
		w1 = lnbx * nw2;
		w0 = lnbx * nwidth(vnbx);
		AT_LEAST(s->stretch,
			 bnbx * w1 + (1 - bnbx) * w0 + s->xl);

		if (voice_p->nn == 1		/* only one note in measure */
		    && nvoice == 0) {		/* (and only 1 voice in the tune) */
			/* special treatment only if length at least one-half measure */
			/*fixme: to see when multi-voices*/
#if 0
			if (2 * prev->len >= (meter1 * BASE_LEN) / meter2) {
#endif
				w0 = 0.5 * ln0p * nw2;
				voice_p->s0->space =
					bn0p * w0 + (1 - bn0p) * voice_p->s0->space;
				s->space  = bn0p * w0 + (1 - bn0p) * s->space;
				w0 = 0.5 * ln0x * nw2;
				voice_p->s0->stretch =
					bn0x * w0 + (1 - bn0x) * voice_p->s0->stretch;
				s->stretch  = bn0x * w0 + (1 - bn0x) * s->stretch;
#if 0
			}
#endif
		}
		break;
		}
	}
}

/* -- Sets the characteristics of the glue between symbols, then
	positions the symbols along the staff. If staff is overfull,
	only does symbols which fit in, returns this number. -- */
static float set_sym_glue(float width)
{
	float	alfa0, beta0;
	float	alfa, beta;
	int	cut = 0;
	int	voice, time, seq;
	struct VOICE_P voice_p[MAXVOICE];
	float	space, shrink, stretch;
	float	w;
	struct SYMBOL *s, *s2;
	int	prev_len;
	float	next_space, next_stretch, next_shrink;

	alfa0 = ALFA_X;			/* max shrink and stretch */
	if (cfmt.continueall)
		alfa0 = cfmt.maxshrink;
	if (gmode == G_SHRINK
	    || gmode == G_STRETCH)
		alfa0 = 1.0;
	else if (gmode == G_SPACE)
		alfa0 = 0.0;
	beta0 = BETA_X;
	if (cfmt.continueall)
		beta0 = BETA_C;

	{
		struct SYMBOL *p_meter;
		int m1, m2;

		if ((p_meter = voice_tb[0].p_meter) != 0) {
			m1 = p_meter->as.u.meter.m1;
			m2 = p_meter->as.u.meter.m2;
		} else {
			m1 = 4;
			m2 = 4;
		}
		vbnp = (rbnp * m1 * BASE_LEN) / m2;
		vbnx = (rbnx * m1 * BASE_LEN) / m2;
		vnbp = (rnbp * m1 * BASE_LEN) / m2;
		vnbx = (rnbx * m1 * BASE_LEN) / m2;
	}

	memset(voice_p, 0, sizeof voice_p);

	/* set spacing for the first symbols (1 for each voice) */
	s = tssym;
	time = s->time;
	seq = s->seq;
	while (s != 0
	       && s->time == time
	       && s->seq == seq) {
		voice = s->voice;
		space = s->space = s->pl;
		shrink = s->shrink = s->wl;
		stretch = s->stretch = s->xl;

		/* set spacing for the next note */
		if (s->next == 0)
			bug("Only one symbol", 1);
		set_spacing(s->next, &voice_p[voice]);
		s = s->ts_next;
	}
	prev_len = 0;
	next_space = s->ts_prev->next->space;
	next_stretch = s->ts_prev->next->stretch;
	next_shrink = s->ts_prev->next->shrink;
	cut = 0;

	/* then loop over the symbols */
	while (s != 0) {
		struct SYMBOL *s3, *s4;
		float min_space, min_shrink;
		int len;

		/* get the notes at this time, set spacing
		 * and get the min shrinking */
		time = s->time;
		seq = s->seq;
		min_shrink = 0;
		len = s->time - s->ts_prev->time;
		if (prev_len > 0 && len != prev_len) {
			next_space *= (float) len / prev_len;
			next_stretch *= (float) len / prev_len;
			next_shrink *= (float) len / prev_len;
		}
		for (s2 = s; s2 != 0; s2 = s2->ts_next) {
			if (s2->time != time
			    || s2->seq != seq)
				break;

			/* if the previous symbol length is greater than
			   the length from the previous elements, adjust */
			if (s2->prev->len > len) {
				s2->space = next_space;
				s2->stretch = next_stretch;
				s2->shrink = next_shrink;
				if (s2->shrink < s2->wl + 2)
					s2->shrink = s2->wl + 2;
				/*fixme: if clef and no element before, have a null space*/
			}

			/* make sure that shrink <= space <= stretch */
			if (s2->space < s2->shrink)
				s2->space = s2->shrink;
			if (s2->stretch < s2->space)
				s2->stretch = s2->space;
			if (s2->shrink > min_shrink)
				min_shrink = s2->shrink;
		}

		/* get the smallest spacing, greater than minimal shrinking */
		s4 = s;
		min_space = 1000.;
		for (s3 = s; s3 != s2; s3 = s3->ts_next) {
			if (s3->space < min_space
			    && s3->shrink >= min_shrink) {
				min_space = s3->space;
				s4 = s3;
			}
		}

		/* set the horizontal position goal */
		space += s4->space;
		shrink += s4->shrink;
		stretch += s4->stretch;

		/* adjust spacing and advance */
		prev_len = BASE_LEN * 4;
		next_space = 1000.;
		for ( ;s != s2; s = s->ts_next) {
			voice = s->voice;
			s->space = space;
			s->shrink = shrink;
			s->stretch = stretch;

			/* if a single rest in the measure, center */
			if (s->type == BAR
			    && voice_p[voice].nn == 1
			    && s->prev->type == REST) {
				s3 = s->prev;
				s4 = s3->prev;
				s3->space = (s->space + s4->space) * 0.5;
				s3->stretch = (s->stretch + s4->stretch) * 0.5;
				s3->shrink = (s->shrink + s4->shrink) * 0.5;
				if (s3->len > CROTCHET) {
					s3->head = H_OVAL;
					if (s3->len > SEMIBREVE)
						s3->len = BREVE;
					s3->dots = 0;
				}
			}

			if (verbose > 21)
				printf("glue [%d] %d (%.1f,%.1f,%.1f)"
				       " %d %d\n",
				       voice, s->type,
				       s->shrink, s->space, s->stretch,
				       voice_p[voice].bars, voice_p[voice].ncount);

			/* set the spacing for the next note
			   and keep info about the shortest element */
			if (s->len < prev_len)
				prev_len = s->len;
			if (s->next != 0) {
				set_spacing(s->next, &voice_p[voice]);
				if (s->len == prev_len
				    && s->next->space < next_space) {
					next_space = s->next->space;
					next_stretch = s->next->stretch;
					next_shrink = s->next->shrink;
				}
			} else {
				if (s->len == prev_len
				    && s->space < next_space) {
					next_space = s->pr;
					next_stretch = s->xr;
					next_shrink = s->wr;
				}
			}

			/* count the number of bars */
			if (s->type == BAR
			    && s->as.u.bar.type != B_INVIS
			    && voice_p[voice].ncount > 0
			    && voice_p[voice].bars >= 0)
				voice_p[voice].bars++;
		}

		/* check the total width */
		alfa = 0;
		if (space > shrink) {
			alfa = (space - width) / (space - shrink);

			if (alfa > alfa0
			    && (voice_p[voice].ncount > 1
				|| voice_p[voice].bars > 0)) {
				if (s == 0)
					break;
				/* may have a key sig change at end of line */
				/*fixme: may also have a meter change*/
				if (s->type == KEYSIG)
					continue;
				s = s->ts_prev;
				if (!cfmt.continueall || verbose > 4) {
					if (verbose <= 3)
						printf("\n");
					printf("++++ Overfull after %d bar%s in row %d voice %d\n",
					       voice_p[voice].bars,
					       voice_p[voice].bars == 1 ? "" : "s",
					       mline, voice);
				}
				cut = 1;

				/* get back to the previous bar, if any */
				for (s2 = s; s2 != 0; s2 = s2->ts_prev) {
					if (s2->type == BAR
					    || s2->type == KEYSIG)
						break;
				}

				/* (should have some note) */
				if (s2 != 0
				    && s2->time > tssym->time)
					s = s2;
				if (tsnext != 0)	/* restore the linkage */
					tsnext->ts_prev->ts_next = tsnext;
				set_piece(s);
				break;
			}
		}
	}

	/* if the last symbol is not a bar, add right space (fixme: maybe *2) */
	for (s = tssym; s->ts_next != 0; s = s->ts_next)
		;
	space = s->space;
	stretch = s->stretch;
	shrink = s->shrink;
	if (s->type != BAR) {
		shrink += s->xr;
		space += s->pr;
		stretch += s->wr;
	}

	/* set the glue, calculate final symbol positions */
	if (verbose > 9)
		printf("Output width %.2f, shrink,space,stretch %.2f, %.2f, %.2f\n",
			width, shrink, space, stretch);

	alfa = beta = 0;
	if (space > width) {
		alfa = 99;
		if (space > shrink)
			alfa = (space - width) / (space - shrink);
		else	printf("space <= shrink!\n");
	} else {
		beta = 99;
		if (stretch > space)
			beta = (width - space) / (stretch - space);
		else	printf("stretch <= space!\n");
	}
	if (verbose > 12)
		printf("start with alfa=%.2f, b=%.2f\n", alfa, beta);

	if (gmode == G_SHRINK) {
		alfa = 1;		/* force minimal spacing */
		beta = 0;
	} else if (gmode == G_STRETCH) {
		alfa = 0;		/* force stretched spacing */
		beta = 1;
	} else if (gmode == G_SPACE) {
		alfa = beta = 0;	/* force natural spacing */
	}

	if (alfa > alfa0) {
		alfa = alfa0;
		beta = 0;
	}

	if (beta > beta0) {
		if (!cfmt.continueall || cut) {		/*??*/
			if (verbose <= 3)
				printf("\n");
			printf("++++ Underfull (%.0fpt of %.0fpt) in row %d\n",
				(beta0 * stretch + (1 - beta0) * space) * cfmt.scale,
				cfmt.staffwidth, mline);
		}
		alfa = 0;
		if (!cfmt.stretchstaff)
			beta = 0;
		if (!cfmt.stretchlast
		    && tsnext == 0
		    && beta >= beta_last) {
			alfa = alfa_last;	/* shrink underfull last line same as previous */
			beta = beta_last;
		}
	}

	if (verbose > 12)
		printf("now alfa=%.3f, beta=%.3f\n", alfa, beta);
	w = alfa * shrink + beta * stretch + (1 - alfa - beta) * space;
	if (verbose >= 3) {
		if (alfa > 0)
			printf("Shrink staff %.0f%%",  100*alfa);
		else if (beta > 0)
			printf("Stretch staff %.0f%%", 100*beta);
		else	printf("No shrink or stretch");
		printf(" to width %.0f (%.0f,%.0f,%.0f)\n",
		       w, shrink, space, stretch);
	}
	if (w > width + 0.1) {			/* (+ 0.1 for floating point precision error) */
		printf("++++ Overfull in row %d\n",
		       mline);
		alfa = (space - width) / (space - shrink);
		w = width;
	}

	for (s = tssym; s != 0; s = s->ts_next) {
		s->x = alfa * s->shrink + beta * s->stretch
			+ (1 - alfa - beta) * s->space;
		if (verbose > 22)
			printf("pos[%d]: type=%d  pos=%.2f\n",
			       s->voice, s->type, s->x);
	}


	/* add small random shifts to positions */
	if (nvoice == 0) {
		for (s = voice_tb[0].sym; s->next != 0; s = s->next) {
			if (s->type == NOTE || s->type == REST) {
				float w1, w2;

				w1 = s->x - s->prev->x;
				w2 = s->next->x - s->x;
				if (w2 < w1)
					w1 = w2;
				s->x += RANFAC * ranf(-w1, w1);
			}
		}
	}

	alfa_last = alfa;
	beta_last = beta;

	return w;
}

/* -- draw the name/subname of the voices -- */
static void draw_vname(void)
{
	int voice, staff;
	int n;
	struct {
		struct VOICE_S *v[4];
		int nv;
		int nl;
	} staff_d[MAXSTAFF], *staff_p;
	float y;
	struct VOICE_S *v;

	memset(staff_d, 0, sizeof staff_d);
	n = 0;
	for (voice = nvoice + 1 ; --voice >= 0; ) {
		v = &voice_tb[voice];
		staff = v->staff;
		if (staff_tb[staff].brace_end)
			staff--;
		staff_p = &staff_d[staff];
		if (mline == 1) {
			if (v->nm == 0)
				continue;
			if (strlen(v->nm) > 32)
				staff_p->nl++;
		} else {
			if (v->snm == 0)
				continue;
			if (strlen(v->snm) > 8)
				staff_p->nl++;
		}
		staff_p->v[staff_p->nv] = v;
		staff_p->nv++;
		staff_p->nl++;
		n++;
	}
	if (n == 0)
		return;
	set_font(&cfmt.vocalfont);
	for (staff = nstaff + 1; --staff >= 0; ) {
		staff_p = &staff_d[staff];
		if (staff_p->nl == 0)
			continue;
		y = staff_tb[staff].y + 12. - 9. * (staff_p->nl - 1)
			- cfmt.vocalfont.size * 0.3;
		if (staff_tb[staff].brace)
			y -= (staff_tb[staff].y - staff_tb[staff+1].y) * 0.5;
		for (voice = 0; voice < staff_p->nv; voice++) {
			v = staff_p->v[voice];
			/*fixme: truncate*/
			PUT2("0 %.1f M (%s) show\n",
			     y,
			     mline == 1 ? v->nm : v->snm);
			y += 18.;
		}
	}
}

/* -- draw the staves and the left side -- */
static void draw_staff(float realwidth,
		       float indent)
{
	int i;

	if (indent != 0) {
		draw_vname();		/* draw the voices name/subnames */
		PUT1("%.2f 0 translate ", indent);	/* do indentation */
	}

	/* draw the staves */
	for (i = nstaff; i >= 0; i--) {
		PUT2("0 %.2f moveto %.2f staff\n",
		     staff_tb[i].y, realwidth);
	}

	/* measure numbering */
	if (measure_nb == 0) {
		set_font(&cfmt.composerfont);
		PUT2("0 %.1f M (%d) show\n",
		     staff_tb[0].y + 24. + 16., nbar);
	} else if (measure_nb > 0
		   && nbar % measure_nb == 0) {
		set_font(&cfmt.composerfont);
		PUT2("22 %.1f M (%d) show\n",
		     staff_tb[0].y + 24. + 6., nbar);
	}

	if (nstaff == 0)
		return;

	PUT2("%.1f 0 %.1f bar\n",
	     staff_tb[0].y - staff_tb[nstaff].y + 24.,
	     staff_tb[nstaff].y);
	for (i = 0; i <= nstaff; i++) {
		float y;

		if (staff_tb[i].brace) {
			y = staff_tb[i].y + 24.;
			PUT2("%.1f 0 %.1f brace\n",
			     y - staff_tb[++i].y, y);
		} else if (staff_tb[i].bracket) {
			y = staff_tb[i++].y + 24.;
			while (!staff_tb[i].bracket_end)
				i++;
			PUT2("%.1f 0 %.1f bracket\n",
			     y - staff_tb[i].y, y);
		}
	}
}

/* -- draw_timesig -- */
static void draw_timesig(float x,
			 struct SYMBOL *s)
{
	int	j;

	if (s->as.u.meter.flag == 1)
		for (j = nstaff; j >= 0; j--)
			PUT2("%.1f %.1f csig\n", x, staff_tb[j].y);
	else if (s->as.u.meter.flag == 2)
		for (j = nstaff; j >= 0; j--)
			PUT2("%.1f %.1f ctsig\n", x, staff_tb[j].y);
	else if (s->as.u.meter.top != 0)
		for (j = nstaff; j >= 0; j--)
			PUT4("%.1f %.1f (%s) (%d) tsig\n",
			     x, staff_tb[j].y, s->as.u.meter.top, s->as.u.meter.m2);
	else	for (j = nstaff; j >= 0; j--)
			PUT4("%.1f %.1f (%d) (%d) tsig\n",
			     x, staff_tb[j].y, s->as.u.meter.m1, s->as.u.meter.m2);
}

/* -- draw_keysig -- */
static void draw_keysig(int voice,
			float x,
			struct SYMBOL *s)
{
	int i;
	static char sharp_tb[7] = {24, 15, 27, 18, 9, 21, 12};
	static char flat_tb[7] = {12, 21, 9, 18, 6, 15, 3};
	static signed char sharp_cl[7] = {0, -15, -9, -3, -18, -12, -6};
	static signed char flat_cl[7] = {0, 6, -9, -3, 3, -12, -6};

	if (!voice_tb[voice].second) {
		int old_sf = s->as.u.key.old_sf;
		int staff = voice_tb[voice].staff;
		float staffb = staff_tb[staff].y;
		int clef = staff_tb[staff].clef;
		int shift, clef_shift;

		/* if flats to sharps, or sharps to flats, put neutrals */
		if (s->as.u.key.sf == 0
		    || old_sf * s->as.u.key.sf < 0) {

			/* old sharps */
			clef_shift = sharp_cl[clef];
			for (i = 0; i < old_sf; i++) {
				if ((shift = sharp_tb[i] + clef_shift) < -3)
					shift += 21;
				PUT2("%.1f %.1f nt0 ", x, staffb + shift);
				x += 5;
			}

			/* old flats */
			clef_shift = flat_cl[clef];
			for (i = 0; i > old_sf; i--) {
				if ((shift = flat_tb[-i] + clef_shift) < -3)
					shift += 21;
				PUT2("%.1f %.1f nt0 ", x, staffb + shift);
				x += 5;
			}
			if (s->as.u.key.sf != 0)
				x += 3;		/* extra space */
		}

		/* new sharps */
		clef_shift = sharp_cl[clef];
		for (i = 0; i < s->as.u.key.sf; i++) {
			if ((shift = sharp_tb[i] + clef_shift) < -3)
				shift += 21;
			PUT2("%.1f %.1f sh0 ", x, staffb + shift);
			x += 5;
		}

		/* new flats */
		clef_shift = flat_cl[clef];
		for (i = 0; i > s->as.u.key.sf; i--) {
			if ((shift = flat_tb[-i] + clef_shift) < -3)
				shift += 21;
			PUT2("%.1f %.1f ft0 ", x, staffb + shift);
			x += 5;
		}

		/* if less sharps or flats, add neutrals */
		/* sharps */
		if (s->as.u.key.sf > 0) {
			clef_shift = sharp_cl[clef];
			for (i = s->as.u.key.sf; i < old_sf; i++) {
				if ((shift = sharp_tb[i] + clef_shift) < -3)
					shift += 21;
				PUT2("%.1f %.1f nt0 ", x, staffb + shift);
				x += 5;
			}
		/* flats */
		} else if (s->as.u.key.sf < 0) {
			clef_shift = flat_cl[clef];
			for (i = s->as.u.key.sf; i > old_sf; i--) {
				if ((shift = flat_tb[-i] + clef_shift) < -3)
					shift += 21;
				PUT2("%.1f %.1f nt0 ", x, staffb + shift);
				x += 5;
			}
		}
		PUT0("\n");
	}

	/* memorize the current keysig of the voice */
	voice_tb[voice].p_key = s;
	voice_tb[voice].bagpipe = s->as.u.key.bagpipe;
}

/* -- draw_bar1 -- */
static void draw_bar1(float x,
		      float y,
		      float h,
		      struct SYMBOL *s)
{
	switch (s->as.u.bar.type) {
	case B_SINGLE:
		PUT3("%.1f %.1f %.1f bar\n", h, x, y); break;
	case B_DOUBLE:
		PUT3("%.1f %.1f %.1f dbar\n", h, x, y); break;
	case B_LREP:
	case B_THICK_THIN:
		PUT3("%.1f %.1f %.1f fbar1\n", h, x, y); break;
	case B_RREP:
	case B_THIN_THICK:
		PUT3("%.1f %.1f %.1f fbar2\n", h, x, y); break;
	case B_DREP:
		PUT3("%.1f %.1f %.1f fbar1 ", h, x - 1, y);
		PUT3("%.1f %.1f %.1f fbar2\n", h, x + 1, y); break;
	case B_INVIS: break;
	default:
		printf(">>> line %d - cannot draw bar type %d\n",
		       s->as.linenum, s->as.u.bar.type);
		break;
	}
}

/* -- draw_bar -- */
static void draw_bar(float x,
		     struct SYMBOL *s)
{
	int	staff;
	float	stafft, y;
	int	dotsb = 0;
	int	dotsa = 0;
	int	bracket;

	bracket = 0;
	stafft = staff_tb[0].y + 24.;	/* top of upper staff */
	for (staff = 0; staff < nstaff; staff++) {
		if (staff_tb[staff].bracket)
			bracket = 1;
		if (staff_tb[staff].bracket_end)
			bracket = 0;
		if (!bracket
		    && staff_tb[staff].nvocal > 0) {
			y = staff_tb[staff].y;
			draw_bar1(x, y, stafft - y, s);
			stafft = staff_tb[staff + 1].y + 24.;
		}
	}
	y = staff_tb[nstaff].y;
	draw_bar1(x, y, stafft - y, s);

	switch (s->as.u.bar.type) {
	case B_LREP:
		dotsb = 10;
		break;
	case B_RREP:
		dotsa = 10;
		break;
	case B_DREP:
		dotsb = 9;
		dotsa = 9;
		break;
	default:
		break;
	}
	if (dotsb != 0) {
		for (staff = nstaff + 1; --staff >= 0; )
			PUT2(" %.1f %.1f rdots ",
			     x + dotsb, staff_tb[staff].y);
		PUT0("\n");
	}
	if (dotsa != 0) {
		for (staff = nstaff + 1; --staff >= 0; )
			PUT2(" %.1f %.1f rdots ",
			     x - dotsa, staff_tb[staff].y);
		PUT0("\n");
	}
	if (s->as.u.bar.dc.n > 0) {
		float top2;

		PUT1("/x %.2f def", x);
		draw_decorations(s, &top2);	/* add decorations */
		PUT0("\n");
	}
}

/* -- remember where to draw endings -- */
static void update_endings(struct SYMBOL *s)
{
	float x;
	int i;

	x = s->x;
	if (num_ending > 0) {
		i = num_ending - 1;
		if (ending[i].v[0] == '1')
			mes1++;
		else {
			mes2++;
			if (mes2 == mes1)
				ending[i].b = x;
		}
	}

	if (s->as.text != 0
	   && isdigit((unsigned char) s->as.text[0])) {
		struct ENDINGS *e;

		if (num_ending > 0)
			if (ending[num_ending - 1].v[0] == '1')
				ending[num_ending - 1].b = x - 3;
		e = &ending[num_ending];
		e->a = x;
		e->b = -1;
		e->v = s->as.text;
		e->staff = s->staff;
		if (e->v[0] == '1')
			mes1 = 0;
		else	mes2 = 0;
		num_ending++;
	}
}

/* -- draw_endings -- */
static void draw_endings(void)
{
	int i;

	for (i = 0; i < num_ending; i++) {
		float	staffb;
		struct ENDINGS *e;

		e = &ending[i];
		staffb = staff_tb[e->staff].y;
		if (e->b < 0) {
			PUT4("%.1f %.1f (%s) %.1f end2\n",
			     e->a, e->a + 50, e->v, staffb);
		} else if (e->v[0] == '1') {
			PUT4("%.1f %.1f (%s) %.1f end1\n",
			     e->a, e->b, e->v, staffb);
		} else {
			PUT4("%.1f %.1f (%s) %.1f end2\n",
			     e->a, e->b, e->v, staffb);
		}
	}
	num_ending = 0;
}

/* -- draw_rest -- */
static void draw_rest(float x,
		      float yy,
		      struct SYMBOL *s,
		      float *gchy)
{
	int i;
	float dotx, doty, top2;
	float staffb;

	staffb = staff_tb[(unsigned) s->staff].y;	/* bottom of staff */

	*gchy = 38. + staffb;
	if (s->as.u.note.invis) {
		if (s->as.u.note.dc.n > 0) {
			PUT1("/x %.2f def", x);
			draw_decorations(s, &top2);	/* add decorations */
			PUT0("\n");
		}
		return;
	}

	PUT2("%.2f %.0f ", x, yy + staffb);

	switch (s->head) {
	case H_OVAL:
		PUT0(s->len >= BREVE ? "r0" : "r1");
		if (yy < -6		/* add one helper line */
		    /*fixme:add upper helper line when breve*/
		    || yy >= 24) {
			PUT1(" %.1f hl", yy + 6. + staffb);
		}
		dotx = 8;
		doty = -3;
		break;
	case H_EMPTY:
		PUT0("r2");
		if (yy <= -6		/* add one helper line */
		    || yy >= 30) {
			PUT1(" %.1f hl", yy + staffb);
		}
		dotx = 8;
		doty = 3;
		break;
	default: {
		char *p;

		switch (s->flags) {
		case 0: p = "r4"; break;
		case 1: p = "r8"; break;
		case 2: p = "r16"; break;
		case 3: p = "r32"; break;
		default: p = "r64"; break;
		}
		PUT0(p);
		dotx = 6.5;
		if ((int) yy % 6)
			doty = 0;		/* dots */
		else	doty = 3;
		}
		break;
	}

	for (i = 0; i < s->dots; i++) {
		PUT2(" %.1f %.1f dt", dotx, doty);
		dotx += 3.5;
	}

	if (s->as.u.note.dc.n > 0)
		draw_decorations(s, &top2);	/* add decorations */

	PUT0("\n");
}

/* -- draw_gracenotes -- */
static void draw_gracenotes(float x,
			    float w,
			    float d,
			    struct SYMBOL *s)
{
	int i, n, ii, m;
	float xg[20], lg, px[20], py[20], xx;
	int yg[20], yy;
	float x0, y0, x1, y1, x2, y2, x3, y3, bet1, bet2, dy1, dy2, dx, fac, facx;
	float a, b, staffb;
static char *acc2_tb[] = { "", "gsh0", "gnt0", "gft0", "gds0", "gdf0" };

	if (s->as.u.note.gr == 0)
		return;
	n = s->as.u.note.gr->n;

	staffb = staff_tb[(unsigned) s->staff].y;	/* bottom of staff */
	facx = 0.3;
	fac = d / w - 1;
	if (fac < 0)
		fac = 0;
	fac = 1. + (fac * facx) / (fac + facx);

	dx = 0;
	for (m = 0; m <= s->nhd; m++) {	/* room for accidentals */
		float dd;

		dd = -s->shhd[m];
		if (s->as.u.note.accs[m])
			dd = -s->shhd[m] + s->shac[m];
		if (s->as.u.note.accs[m] == A_FT || s->as.u.note.accs[m] == A_NT)
			dd -= 2;
		if (dx < dd)
			dx = dd;
	}

	xx = x - fac * (dx + GSPACE0);
	for (i = n; --i >= 0; ) {		/* set note positions */
		yg[i] = 3 * (s->as.u.note.gr->p[i] - 18);
		if (i == n - 1) {		/* some subtle shifts.. */
			if (yg[i] >= s->ymx)
				xx += 1;	/* gnote above a bit closer */
			if (n == 1 && yg[i] < s->ymn - 7)
				xx -= 2;	/* below with flag further */
		}

		if (i < n - 1
		    && yg[i] > yg[i+1] + 8)
			xx += fac * 1.8;

		xg[i] = xx;
		xx -= fac * GSPACE;
		if (s->as.u.note.gr->a[i])
			xx -= 3.5;
	}

	if (n > 1) {
		float s1, delta;
		float sx, sy, sxx, sxy, lmin;

		s1 = sx = sy = sxx = sxy = 0;	/* linear fit through stems */
		for (i = 0; i < n; i++) {
			px[i] = xg[i] + GSTEM_XOFF;
			py[i] = yg[i] + GSTEM;
			s1 += 1;
			sx += px[i];
			sy += py[i];
			sxx += px[i] * px[i];
			sxy += px[i] * py[i];
		}
		delta = s1 * sxx - sx * sx;	/* beam fct: y=ax+b */
		a = (s1 * sxy - sx * sy) / delta;
		if (a > BEAM_SLOPE)
			a = BEAM_SLOPE;
		else if (a < -BEAM_SLOPE)
			a = -BEAM_SLOPE;
		b = (sy - a * sx) / s1;

		if (voice_tb[(unsigned) s->voice].bagpipe) {
			a = 0;
			b = 35;
		}

		lmin = 100;			/* shift to get min stems */
		for (i = 0; i < n; i++) {
			px[i] = xg[i] + GSTEM_XOFF;
			py[i] = a * px[i] + b;
			lg = py[i] - yg[i];
			if (lg < lmin)
				lmin = lg;
		}
		if (lmin < 10)
			b += 10 - lmin;
	}

	for (i = 0; i < n; i++) {		/* draw grace notes */
		int acc, y;

		if (n > 1) {
			px[i] = xg[i] + GSTEM_XOFF;
			py[i] = a * px[i] + b;
			lg = py[i] - yg[i];
			PUT3("%.1f %.1f %.1f gnt ",
			     xg[i], yg[i] + staffb, lg);
		} else {
			lg = GSTEM;
			PUT4("%.1f %.1f %.1f gn1%s ",
			     xg[i], yg[i] + staffb, lg,
			     s->as.u.note.gr->sappo ? "s" : "");
		}

		if ((acc = s->as.u.note.gr->a[i]) != 0)
			PUT3("%.1f %.1f %s ",
			     xg[i] - 4.5, yg[i] + staffb, acc2_tb[acc]);

		y = yg[i];		/* helper lines */
		if (y <= -6) {
			if (y % 6)
				PUT2("%.1f %.1f ghl ",
				     xg[i], y+3 + staffb);
			else	PUT2("%.1f %.1f ghl ",
				     xg[i], y + staffb);
		}
		if (y >= 30) {
			if (y % 6)
				PUT2("%.1f %.1f ghl ",
				     xg[i], y-3 + staffb);
			else	PUT2("%.1f %.1f ghl ",
				     xg[i], y + staffb);
		}
	}

	if (n > 1) {				/* beam */
		PUT5("%.1f %.1f %.1f %.1f gbm%d ",
		     px[0], py[0] + staffb,px[n-1],
		     py[n - 1] + staffb,
		     voice_tb[(unsigned) s->voice].bagpipe ? 3 : 2);
	}

	bet1 = 0.2;				/* slur */
	bet2 = 0.8;
	yy = 1000;
	for (i = n; --i >= 0; )
		if (yg[i] <= yy) {
			yy = yg[i];
			ii = i;
		}
	x0 = xg[ii];
	y0 = yg[ii] - 5;
	if (ii > 0) {
		x0 -= 4;
		y0 += 1;
	}
	x3 = x - 1;
	y3 = s->ymn - 5;
	dy1 = (x3 - x0) * 0.4;
	if (dy1 > 3)
		dy1 = 3;
	dy2 = dy1;

	if (yg[ii] > s->ymn + 7){
		x0 = xg[ii] - 1;
		y0 = yg[ii] - 4.5;
		y3 = s->ymn + 1.5;
		x3 = x - dx - 5.5;
		dy2 = (y0 - y3) * 0.2;
		dy1 = (y0 - y3) * 0.8;
		bet1 = 0.0;
	}

	if (y3 > y0 + 4) {
		y3 = y0 + 4;
		x0 = xg[ii] + 2;
		y0 = yg[ii] - 4;
	}

	x1 = bet1 * x3 + (1 - bet1) * x0;
	y1 = bet1 * y3 + (1 - bet1) * y0 - dy1;
	x2 = bet2 * x3 + (1 - bet2) * x0;
	y2 = bet2 * y3 + (1 - bet2) * y0 - dy2;

	PUT4(" %.1f %.1f %.1f %.1f",
	     x1, y1 + staffb, x2, y2 + staffb);
	PUT4(" %.1f %.1f %.1f %.1f gsl\n",
	     x3, y3 + staffb, x0, y0 + staffb);
}

/* -- draw m-th head with accidentals and dots -- */
static void draw_basic_note(float x,
			    float w,
			    float d,
			    struct SYMBOL *s,
			    int m)
{
	int y;
	float staffb;
static char *acc_tb[] = { "", "sh", "nt", "ft", "dsh", "dft" };

	staffb = staff_tb[(unsigned) s->staff].y;	/* bottom of staff */

	y = 3 * (s->pits[m] - 18);		/* height on staff */

	PUT2("%.1f %.1f ",			/* draw head */
	     x + s->shhd[m],
	     (float) y + staffb);
	switch (s->head) {
	case H_OVAL:
		if (s->as.u.note.lens[0] >= BREVE)
			PUT0("HDD");
		else	PUT0("HD");
		break;
	case H_EMPTY:
		PUT0("Hd"); break;
	default:
		PUT0("hd"); break;
	}
	if (s->shhd[m]) {
		int yy;

		yy = 0;
		if (y >= 30) {
			yy = y;
			if (yy % 6)
				yy -= 3;
		}
		if (y <= -6) {
			yy = y;
			if (yy % 6)
				yy += 3;
		}
		if (yy)
			PUT1(" %.1f hl", (float) yy + staffb);
	}

	if (s->dots) {				/* add dots */
		int i;
		float dotx;
		int doty;

		dotx = 8. + s->xmx - s->shhd[m];
		if (y % 6)
			doty = 0;
		else	{
			if ((doty = s->doty) == 0)	/* defined when voices overlap */
				doty = 3;
			if (s->flags && s->stem > 0
			    && s->word_st
			    && s->as.u.note.word_end
			    && s->nhd == 0)
				dotx += DOTSHIFT;
		}
		if (s->head == H_EMPTY)
			dotx += 1;
		else if (s->head == H_OVAL)
			dotx += 2;
		for (i = 0; i < s->dots; i++) {
			PUT2(" %.1f %d dt", dotx, doty);
			dotx += 3.5;
		}
	}

	if (s->as.u.note.accs[m]) {			/* add accidentals */
		float dx, add, fac;

		add = 0.3 * (d - w - 3.);
		fac = 1. + add / s->wl;
		if (fac < 1.)
			fac = 1.;
		else if (fac > 1.2)
			fac = 1.2;
		dx = fac * s->shac[m];
		PUT2(" %.1f %s", dx, acc_tb[(unsigned) s->as.u.note.accs[m]]);
	}
}

/* -- draw_note -- */
static float draw_note(float x,
		       float w,
		       float d,
		       struct SYMBOL *s,
		       int fl,
		       float *gchy)
{
	char	c;
	int	y, i, m, ma;
	float	yc, slen, slen0, top, top2;
	float	staffb;

	slen0 = STEM;
	staffb = staff_tb[(unsigned) s->staff].y;

	draw_gracenotes(x, w, d, s);		/* draw grace notes */

	c = s->stem > 0 ? 'u' : 'd';
	slen = s->stem * (s->ys - s->y);

	/* draw the master note - can be only the first or the last note */
	if (s->stem > 0)
		ma = 0;
	else	ma = s->nhd;
	draw_basic_note(x, w, d, s, ma);	/* draw note head */
	if (s->stem)				/* add stem */
		PUT2(" %.1f s%c", slen, c);
	if (fl && s->flags > 0)			/* add flags */
		PUT3(" %.1f f%d%c", slen, s->flags, c);
	top = draw_decorations(s, &top2);	/* add decorations */

	y = s->ymn;				/* lower helper lines */
	if (y <= -6) {
		for (i = -6; i >= y; i -= 6)
			PUT1(" %.1f hl", (float) i + staffb);
		if (s->head == H_OVAL)
			PUT0("1");
	}
	y = s->ymx;				/* upper helper lines */
	if (y >= 30) {
		for (i = 30; i <= y; i += 6)
			PUT1(" %.1f hl", (float) i + staffb);
		if (s->head == H_OVAL)
			PUT0("1");
	}

	/* draw the other notes */
	for (m = 0; m <= s->nhd; m++) {
		if (m == ma)
			continue;
		PUT0(" ");
		draw_basic_note(x, w, d, s, m);		/* draw note heads */
		/* fixme: shall this be there ? */
		if (m > 0 && s->pits[m] == s->pits[m - 1]) {	/* unions */
			char cc;

			if (c == 'u')
				cc = 'd';
			else	cc = 'u';
			if (s->stem)
				PUT2(" %.2f s%c", slen0, cc);
			if (s->flags > 0)
				PUT3(" %.1f f%d%c", slen0,s->flags, cc);
		}
	}

	*gchy = 38 + staffb;
	if (s->as.text != 0) {		/* position guitar chord */
		yc = 38;
		if (yc < y + 8)
			yc = y + 8;
		if (yc < s->ys + 4)
			yc = s->ys + 4;
#if 0 /*fixme*/
		/*fixme: already counted? */
		for (k = 0; k < s->as.u.note.dc.n; k++) {
			int deco;

			deco = s->as.u.note.dc.t[k];
			/*fixme: other deco?*/
			if (deco == D_turn && yc < y + 12)
				yc = y + 12;
		}
#endif
		if (yc < top2)
			yc = top2;
		*gchy = yc + staffb;
	}

	PUT0("\n");

	return top;
}

/* -- up/down shift needed to get k*6 -- */
static float rnd6(float x)
{
	int ix, iy, ir;

	ix = x + 600.999 - 3.0;
	ir = ix % 6;
	iy = ix - 600;
	if (ir > 0)
		iy += 6 - ir;
	return (float) iy - x;
}

/* -- b_pos -- */
static float b_pos(int stem,
		   int flags,
		   float b)
{
	float d1, d2, add;
	float top, bot;

	if (stem > 0) {
		top = b;
		bot = b - (flags - 1) * BEAM_SHIFT - BEAM_DEPTH;
		if (bot > 26)
			return b;
	} else {
		bot = b;
		top = b + (flags - 1) * BEAM_SHIFT + BEAM_DEPTH;
		if (top < -2)
			return b;
	}

	d1 = rnd6(top - BEAM_OFFSET);
	d2 = rnd6(bot + BEAM_OFFSET);
	add = d1;
	if (d1 * d1 > d2 * d2)
		add = d2;

	return b + add;
}

/* -- calculate_beam -- */
static void calculate_beam(struct SYMBOL *s1,
			   struct BEAM *bm)
{
	struct SYMBOL *s, *s2;
	int i, notes, flags;
	float x,y,ys,a,b,max_stem_err,stem_err,min_stem,slen,yyg,yg,try;
	float s0, sx, sy, sxx, sxy, syy, delta, hh, a0;
/*	int two_staves; */
  
	s2 = 0;			/* find first and last note in beam */
	for (s = s1; s != 0; s = s->next)
		if (s->type == NOTE
		    && s->as.u.note.word_end) {
			s2 = s;
			break;
		}
	if (s2 == 0)
		return;

	notes = flags = 0;		/* set x positions, count notes and flags */
/*	two_staves = 0; */
	for (s = s1; ; s = s->next) {
		s->xs = s->x;
		if (s->type == NOTE) {  
			s->xs += s->stem * STEM_XOFF;
			if (s->nhd == 0)
				s->xs += s->shhd[0];
			if (s->flags > flags)
				flags = s->flags;
			notes++;
		}
		if (s == s2)
			break;
#if 0
		if (s->staff != s->next->staff)
			two_staves = 1;
#endif
	}

	s0 = sx = sy = sxx = sxy = syy = 0;	/* linear fit through stem ends */
	for (s = s1; ; s = s->next) {
		int stem_len;

		if (s->type != NOTE) {
			if (s == s2)
				break;
			continue;
		}
		x = s->xs;
		stem_len = STEM * s->stem;
#if 0 /*fixme: test*/
		if (s->stem > 0) {
			if (s->pits[0] > 26) {		/* > 'f' */
				stem_len -= 2;
				if (s->pits[0] > 28)
					stem_len -= 2;
			}
		} else {
			if (s->pits[0] < 18) {		/* < 'E' */
				stem_len += 2;
				if (s->pits[0] < 16)
					stem_len += 2;
			}
		}
		if (two_staves) {
			if (s->stem > 0)
				stem_len += (26 - s->pits[0]) / 2;
			else	stem_len -= (18 - s->pits[0]) / 2;
		}
#endif
		y = s->ymx + stem_len + staff_tb[(unsigned) s->staff].y;
		s0 += 1; sx += x; sy += y;
		sxx += x * x; sxy += x * y; syy += y * y;
		if (s == s2)
			break;
	}

	delta = s0 * sxx - sx * sx;	/* beam fct: y=ax+b */
	a = (s0 * sxy - sx * sy) / delta;
	b = (sy - a * sx) / s0;

	/* the next few lines modify the slope of the beam */
	if (notes >= 3) {
		hh = syy - a * sxy - b * sy;	/* flatten if notes not in line */
		if (hh > 0
		    && hh / (notes - 2) > 0.5)
			a *= BEAM_FLATFAC;
	}

	if (a >= 0)
		a = BEAM_SLOPE * a / (BEAM_SLOPE + a);	/* max steepness for beam */
	else	a = BEAM_SLOPE * a / (BEAM_SLOPE - a);

	/* to decide if to draw flat etc. use normalized slope a0 */
	a0 = a * (s2->xs - s1->xs) / (20 * (notes - 1));

	if (a0 < BEAM_THRESH && a0 > -BEAM_THRESH)
		a = 0;			/* flat below threshhold */

	b = (sy - a * sx) / s0;		/* recalculate b for new slope */

/*  if (flags>1) b=b+2*stem;*/		/* leave a bit more room if several beams */

	if (voice_tb[(unsigned) s->voice].bagpipe) {
		b = -11 + staff_tb[(unsigned) s->staff].y;
		a = 0;
	}

/*fixme: have a look again*/
	max_stem_err = 0;		/* check stem lengths */
	for (s = s1; ; s = s->next) {
		if (s->type != NOTE) {
			if (s == s2)
				break;
			continue;
		}
		if (s->nhd == 0) {
			min_stem = STEM_MIN;
			if (s->flags == 2)
				min_stem = STEM_MIN2;
			else if (s->flags == 3)
				min_stem = STEM_MIN3;
			else if (s->flags == 4)
				min_stem = STEM_MIN4;
		} else {
			min_stem = STEM_CH_MIN;
			if (s->flags == 2)
				min_stem = STEM_CH_MIN2;
			else if (s->flags == 3)
				min_stem = STEM_CH_MIN3;
			else if (s->flags == 4)
				min_stem = STEM_CH_MIN4;
		}
		min_stem += BEAM_DEPTH + BEAM_SHIFT * (s->flags - 1);
		ys = a * s->xs + b;
		if (s->stem > 0) {
			slen = ys - staff_tb[(unsigned) s->staff].y - s->ymx;
			if (s->pits[s->nhd] > 26) {
				min_stem -= 2;
				if (s->pits[s->nhd] > 28)
					min_stem -= 2;
			}
		} else {
			slen = s->ymn - ys + staff_tb[(unsigned) s->staff].y;
			if (s->pits[0] < 18) {
				min_stem -= 2;
				if (s->pits[0] < 16)
					min_stem -= 2;
			}
		}
		stem_err = min_stem - slen;
		if (max_stem_err < stem_err)
			max_stem_err = stem_err;
		if (s == s2)
			break;
	}

	if (max_stem_err > 0)			/* shift beam if stems too short */
		b += s1->stem * max_stem_err;

	for (s = s1->next; ; s = s->next) {	/* room for gracenotes */
		if (s->type != NOTE
		    || s->as.u.note.gr == 0) {
			if (s == s2)
				break;
			continue;
		}

		for (i = 0; i < s->as.u.note.gr->n; i++) {
			yyg = a * (s->x - GSPACE0) + b;
			yg = 3 * (s->as.u.note.gr->p[i] - 18);
			if (s->stem > 0) {
				try = yg + GSTEM - yyg - BEAM_DEPTH - 2;
				if (try > 0)
					b += try;
			} else {
				try = yg - yyg + BEAM_DEPTH + 7;
				if (try < 0)
					b += try;
			}
		}
		if (s == s2)
			break;
	}

	if (a < 0.01 && a > -0.01)	/* shift flat beams onto staff lines */
/*fixme*/
		b = b_pos(s1->stem, flags, b);

	for (s = s1; ; s = s->next) {
		if (s->type == NOTE)	/* final stems */
			s->ys = a * s->xs + b - staff_tb[(unsigned) s->staff].y;
		if (s == s2)
			break;
	}

	bm->s1 = s1;			  /* save beam parameters in struct */
	bm->s2 = s2;
	bm->a = a;
	bm->b = b;
/*fixme*/
	bm->stem = s1->stem;
	bm->t = s1->stem * BEAM_DEPTH;
	return;
}

/* -- rest_under_beam -- */
static float rest_under_beam(struct SYMBOL *s0,
			     struct BEAM *bm)
{
	struct SYMBOL *s, *s1, *s2;
	float y;
	int nf;
	float x;

	x = s0->x;
	s1 = bm->s1;
	s2 = bm->s2;
	nf = 1;
	for (s = s1; ; s = s->next) {
		if (s->type == NOTE && s->flags > nf)
			nf = s->flags;
		if (s == s2)
			break;
	}

	y = bm->a * x + bm->b - staff_tb[(unsigned) s0->staff].y;
	if (s->stem > 0) {
		y -= BEAM_DEPTH + (nf - 1) * BEAM_SHIFT;
		y -= s0->head != H_FULL ? 4 : 9;
		if (y > 12)
			y = 12;
	} else {
		y += BEAM_DEPTH + (nf - 1) * BEAM_SHIFT;
		y += s0->head != H_FULL ? 4 : 11;
		if (y < 12)
			y = 12;
	}

	if (s0->head != H_FULL) {
		int iy;

		iy = (y + 3.0) / 6.0;
		y = 6 * iy;
	}

	return y;
}

/* -- draw number on a beam -- */
static void draw_beam_num(struct BEAM *bm,
			  int num,
			  float xn)
{
	float yn;

	if (bm->stem < 0)
		yn = -12.;
	else	yn = 4.;
	yn += bm->a * xn + bm->b;

	PUT3("%.1f %.1f (%d) bnum\n", xn, yn, num);
}

/* -- draw a single beam -- */
static void draw_beam(float x1,
		      float x2,
		      float dy,
		      struct BEAM *bm)
{
	float y1, y2;

	y1 = bm->a * x1 + bm->b - bm->stem * dy;
	y2 = bm->a * x2 + bm->b - bm->stem * dy;
	PUT5("%.1f %.1f %.1f %.1f %.1f bm\n", x1, y1, x2, y2, bm->t);
}

/* -- draw the beams for one word -- */
static void draw_beams(struct BEAM *bm)
{
	struct SYMBOL *s, *s1, *s2;
	int i, maxfl, shift;

	s1 = bm->s1;
	s2 = bm->s2;
	maxfl = 1;

	/* make first beam over whole word */
	for (s = s1; ; s = s->next) {	/* numbers for nplets on same beam */
		int p;

		if ((s->type == NOTE || s->type == REST)
		    && (p = s->as.u.note.p_plet) > 0) {
			struct SYMBOL *s3;
			int r;

			r = s->as.u.note.r_plet;
			s3 = s;
			while (--r > 0) {
				s3 = s3->next;
				if (s3 == s2
				    && r > 1) {
					s3 = 0;
					break;
				}
			}
			if (s3 != 0) {
				float xn;

				xn = 0.5 * (s->xs + s3->xs);
				draw_beam_num(bm, p, xn);
				s->as.u.note.p_plet = 0;
			}
		}
		if (s->flags > maxfl)
			maxfl = s->flags;
		if (s == s2)
			break;
	}

	draw_beam(s1->xs, s2->xs, 0.0, bm);

	/* other beams with two or more flags */
	shift = 0;
	for (i = 2; i <= maxfl; i++) {
		struct SYMBOL *k1, *k2;
		int inbeam;

		k1 = k2 = s1;
		inbeam = 0;
		shift += BEAM_SHIFT;
		for (s = s1; ; s = s->next) {
			if (s->type != NOTE) {
				if (s == s2)	/* (not useful) */
					break;
				continue;
			}
			if (!inbeam && s->flags >= i) {
				k1 = s;
				inbeam = 1;
			}
			if (inbeam && (s->flags < i
				       || s == s2
				       || s->beam_break)) {
				float x1, x2;

				if (s->flags >= i
				    && (s == s2 || s->beam_break))
					k2 = s;
				x1 = k1->xs;
				x2 = k2->xs;
				if (k1 == k2) {
					if (k1 == s1)
						x1 += BEAM_STUB;
					else	x1 -= BEAM_STUB;
				}
				draw_beam(x1, x2, shift, bm);
				inbeam = 0;
			}
			k2 = s;
			if (s == s2)
				break;
		}
	}
}

/* -- return min or max, depending on s -- */
static float extreme(int s,
		     float a,
		     float b)
{
	if (s > 0)
		return a > b ? a : b;
	return a > b ? b : a;
}

/* -- draw_bracket -- */
static void draw_bracket(int p,
			 struct SYMBOL *s1,
			 struct SYMBOL *s2)
{
	struct SYMBOL *sy;
	float x1, x2, y1, y2, xm, ym, s, s0, xx, yy, yx, dy;
	float staffb, st1, st2;

/*fixme: to optimize*/
    if (s1->multi >= 0) {

	/* sole or upper voice */
	x1 = s1->x - 4.;
	x2 = s2->x + 4.;
	y1 = s1->ymx + 10;
	y2 = s2->ymx + 10;
	st1 = staff_tb[(unsigned) s1->staff].y;
	st2 = staff_tb[(unsigned) s2->staff].y;

	if (s1->stem > 0) {
		y1 = s1->ys + 4.;
		x1 += 3.;
	}
	if (s2->stem > 0) {
		y2 = s2->ys + 4.;
		x2 += 3.;
	}

	if (y1 < 30.)
		y1 = 30.;
	if (y2 < 30.)
		y2 = 30.;

	xm = 0.5 * (x1 + x2);
	ym = 0.5 * (y1 + st1 + y2 + st2);

	s = (y2 + st2 - y1 - st1) / (x2 - x1);
	s0 = (s2->ymx + st2 - s1->ymx - st1) / (x2 - x1);
	if (s0 > 0) {
		if (s < 0)
			s = 0;
		else if (s > s0)
			s = s0;
	} else {
		if (s > 0)
			s = 0;
		else if (s < s0)
			s = s0;
	}
	if (s * s < 0.1 * 0.1)
		s = 0;

	/* shift up bracket if needed */
	dy = 0;
	for (sy = s1; ; sy = sy->next) {
		if (sy->type != NOTE
		    && sy->type != REST) {
			if (sy == s2)
				break;
			continue;
		}
		staffb = staff_tb[(unsigned) sy->staff].y;
		xx = sy->x;
		yy = ym + (xx - xm) * s;
		yx = sy->ymx + staffb + 10.;
		if (sy->stem > 0)
			yx = sy->ys + staffb + 5.;
		if (yx - yy > dy)
			dy = yx - yy;
		if (sy == s2)
			break;
	}

	ym += dy;
	y1 = ym + s * (x1 - xm);
	y2 = ym + s * (x2 - xm);

	xx = xm - 6.;
	yy = ym + s * (xx - xm);
	PUT4("%.1f %.1f %.1f %.1f hbr ", x1, y1, xx, yy);

	xx = xm + 6.;
	yy = ym + s * (xx - xm);
	PUT4("%.1f %.1f %.1f %.1f hbr ", x2, y2, xx, yy);

    } else {	/* lower voice of the staff */
/*fixme: think to all that again..*/
	x1 = s1->x - 8.;
/*fixme: the note may be shifted to the right*/
	x2 = s2->x;
	y1 = s1->ys - 8;
	y2 = s2->ys - 8;
	st1 = staff_tb[(unsigned) s1->staff].y;
	st2 = staff_tb[(unsigned) s2->staff].y;

	if (y1 > -6.)
		y1 = -6.;
	if (y2 > -6.)
		y2 = -6.;

	xm = 0.5 * (x1 + x2);
	ym = 0.5 * (y1 + st1 + y2 + st2);

	s = (y2 + st2 - y1 - st1) / (x2 - x1);
	s0 = (s2->ymn + st2 - s1->ymn - st1) / (x2 - x1);
	if (s0 > 0) {
		if (s < 0)
			s = 0;
		else if (s > s0)
			s = s0;
	} else {
		if (s > 0)
			s = 0;
		else if (s < s0)
			s = s0;
	}
	if (s * s < 0.1 * 0.1)
		s = 0;

	/* shift down bracket if needed */
	dy = 0;
	for (sy = s1; ; sy = sy->next) {
		if (sy->type != NOTE
		    && sy->type != REST) {
			if (sy == s2)
				break;
			continue;
		}
		staffb = staff_tb[(unsigned) sy->staff].y;
		xx = sy->x;
		yy = ym + (xx - xm) * s;
		yx = sy->ymn + staffb - 6.;
		if (sy->stem < 0)
			yx = sy->ys + staffb - 5.;
		if (yx - yy < dy)
			dy = yx - yy;
		if (sy == s2)
			break;
	}

	ym += dy;
	y1 = ym + s * (x1 - xm);
	y2 = ym + s * (x2 - xm);

	xx = xm - 6.;
	yy = ym + s * (xx - xm);
	PUT4("%.1f %.1f %.1f %.1f moveto lineto 0 3 rlineto stroke ",
	     x1, y1, xx, yy);

	xx = xm + 6.;
	yy = ym + s * (xx - xm);
	PUT4("%.1f %.1f %.1f %.1f moveto lineto 0 3 rlineto stroke ",
	     x2, y2, xx, yy);
    } /* lower voice */

	yy = 0.5 * (y1 + y2);
	PUT3("%.1f %.1f (%d) bnum\n", xm, yy - 4., p);
}

/* -- draw_nplet_brackets  -- */
static void draw_nplet_brackets(struct SYMBOL *sym)
{
	struct SYMBOL *s, *s1;
	int r;

	for (s = sym; s != 0; s = s->next) {
		if ((s->type == NOTE
		     || s->type == REST)
		    && s->as.u.note.p_plet > 0) {
			r = s->as.u.note.r_plet;
			for (s1 = s; s1 != 0; s1 = s1->next) {
				if ((s1->type == NOTE
				     || s1->type == REST)
				    && --r <= 0)
					break;
			}
			draw_bracket(s->as.u.note.p_plet, s, s1);
		}
	}
}

/* -- decide whether slur goes up or down -- */
static int slur_direction(struct SYMBOL *k1,
			  struct SYMBOL *k2)
{
	struct SYMBOL *s;
	int are_stems, are_downstems, y_max;

	are_stems = are_downstems = 0;
	y_max = 300;
	for (s = k1; ; s = s->next) {
		if (s->type == NOTE) {
			if (s->stem != 0) {
				are_stems = 1;
				if (s->stem < 0)
					are_downstems = 1;
			}
			if (y_max > s->ymn)
				y_max = s->ymn;
		}
		if (s == k2)
			break;
	}
	if (are_downstems
	    || (!are_stems
		&& y_max >= 12))
		return 1;
	return -1;
}

/* -- check if slur sequence in a multi-voice staff -- */
static int slur_multi(struct SYMBOL *k1,
		      struct SYMBOL *k2)
{
	for (;;) {
		if (k1->multi != 0)	/* if multi voice */
			/*fixme: may change*/
			return k1->multi;
		if (k1 == k2)
			break;
		k1 = k1->next;
	}
	return 0;
}

/* -- output slur -- */
static void output_slur(float x1,
			float y1,
			float x2,
			float y2,
			float s,
			float height,
			float shift)
{
	float alfa, beta, mx, my, xx1, yy1, xx2, yy2, dx, dy, dz, a, add;

	alfa = 0.3;
	beta = 0.45;

	/* for wide flat slurs, make shape more square */
	dy = y2 - y1;
	if (dy < 0)
		dy = -dy;
	dx = x2 - x1;
	if (dx < 0)
		dx =- dx;
	a = dy / dx;
	if (a < 0.7 && dx > 40) {
		add = 0.2 * (dx - 40) / 100;
		alfa = 0.3 + add;
		if (alfa > 0.7)
			alfa = 0.7;
	}

	/* alfa, beta, and height determine Bezier control points pp1,pp2
	 *
	 *           X====alfa===|===alfa=====X
	 *          /            |             \
	 *        pp1            |              pp2
	 *        /            height            \
	 *      beta             |                beta
	 *      /                |                 \
	 *    p1                 m                  p2
	 *
	 */

	mx = 0.5 * (x1 + x2);
	my = 0.5 * (y1 + y2);

	xx1 = mx + alfa * (x1 - mx);
	yy1 = my + alfa * (y1 - my) + height;
	xx1 = x1 + beta * (xx1 - x1);
	yy1 = y1 + beta * (yy1 - y1);

	xx2 = mx + alfa * (x2 - mx);
	yy2 = my + alfa * (y2 - my) + height;
	xx2 = x2 + beta * (xx2 - x2);
	yy2 = y2 + beta * (yy2 - y2);

	dx = 0.03 * (x2 - x1);
	if (dx > 10.0)
		dx = 10.0;
	dy = 1.0;
	dz = 0.20;
	if (x2 - x1 > 100)
		dz += 0.001 * (x2 - x1);
	if (dz > 0.6)
		dz = 0.6;

	PUT4("%.1f %.1f %.1f %.1f ", 
	     xx2 - dx, yy2 + shift + s * dy, xx1 + dx, yy1 + shift + s * dy);
	PUT3("%.1f %.1f 0 %.1f ", x1, y1 + shift + s * dz,s * dz);
	PUT4("%.1f %.1f %.1f %.1f ", xx1, yy1 + shift, xx2, yy2 + shift);
	PUT4("%.1f %.1f %.1f %.1f SL\n", x2, y2 + shift, x1, y1 + shift);

/*PUT4("%.2f %.2f %.2f %.2f ", xx1, yy1 + shift, xx2, yy2 + shift);
  PUT4("%.2f %.2f %.2f %.2f sl\n", x2, y2 + shift, x1, y1 + shift);*/
}

/* -- draw phrasing slur between two symbols -- */
/* (not a pretty routine, this) */
static void draw_phrasing(struct SYMBOL *k1,
			  struct SYMBOL *k2,
			  int level)
{
	struct SYMBOL *k;
	float x1, y1, x2, y2, yy, height, addx, addy;
	float hmin, a;
	float x, y, z, h, dx, dy;
	int s, nn;

	if (k1 == k2)
		return;
	if (nbuf + 100 > BUFFSZ)
		rx("PS output exceeds reserved space per staff",
		   " -- increase BUFFSZ1");

	if ((s = slur_multi(k1, k2)) == 0)
		s = slur_direction(k1, k2);

	nn = 0;
	for (k = k1; ; k = k->next) {
		if (k->type == NOTE
		    || k->type == REST)
			nn++;
		if (k == k2)
			break;
	}

	/* fix endpoints */
	x1 = k1->x + k1->xmx;		/* take the max right side */
	x2 = k2->x;
	if (k1->type == NOTE) {		/* here if k1 points to note */
		y1 = (s > 0 ? k1->ymx : k1->ymn) + s * 6;
		if (k1->as.u.note.word_end) {
			if (k1->stem * s > 0) {
				if (s > 0)
					x1 += 4;
				k = next_note(k1);
				if (k->stem * s > 0)
					y = k->ys;
				else	y = s > 0 ? k->ymx : k->ymn;
				if (k1->stem > 0) {
					if (y > k1->ys)
						y1 = k1->ys + s * 2;
					else if (y > y1)
						y1 = y + s * 2;
				} else {
					if (y < k1->ys)
						y1 = k1->ys + s * 2;
					else if (y < y1)
						y1 = y + s * 2;
				}
			}
		} else	y1 = extreme(s, y1, k1->ys + s * 2);
		if (s > 0 && y1 < k1->dc_top + 2.5)
			y1 = k1->dc_top + 2.5;
	} else	y1 = k1->y;

	if (k2->type == NOTE) {		/* here if k2 points to note */
		y2 = (s > 0 ? k2->ymx : k2->ymn) + s * 6;
		if (k2->word_st) {
			if (k2->stem * s > 0) {
				if (s < 0)
					x2 -= 3;
				k = prev_note(k2);
				if (k->stem * s > 0)
					y = k->ys;
				else	y = s > 0 ? k->ymx : k->ymn;
				if (k2->stem > 0) {
					if (y > k2->ys)
						y2 = k2->ys + s * 2;
					else if (y > y2)
						y2 = y + s * 2;
				} else {
					if (y < k2->ys)
						y2 = k2->ys + s * 2;
					else if (y < y2)
						y2 = y + s * 2;
				}
			}
		} else	y2 = extreme(s, y2, k2->ys + s * 2);
		if (s > 0 && y2 < k2->dc_top + 2.5)
			y2 = k2->dc_top + 2.5;
	} else	y2 = k2->y;

	if (k1->type != NOTE) {
		x1 += k1->wr;
		y1 = y2 + 1.2 * s;
		if (nn > 1) {
			if (s > 0) {
				if (y1 < 24 + 4)
					y1 = 24 + 4;
			} else {
				if (y1 > -4)
					y1 = -4;
			}
		}
	}

	if (k2->type != NOTE) {
		y2 = y1 + 1.2 * s;
		if (nn > 1) {
			if (s > 0) {
				if (y2 < 24 + 4)
					y2 = 24 + 4;
			} else {
				if (y2 > -4)
					y2 = -4;
			}
		}
	}

	/* shift endpoints */
	addx = 0.04 * (x2 - x1);
	if (addx > 3.0)
		addx = 3.0;
	addy = 0.02 * (x2 - x1);
	if (addy > 3.0)
		addy = 3.0;
	x1 += addx;
	x2 -= addx;
	y1 += s * addy + staff_tb[(unsigned) k1->staff].y;
	y2 += s * addy + staff_tb[(unsigned) k2->staff].y;

	a = (y2 - y1) / (x2 - x1);		/* slur steepness */
	if (a > SLUR_SLOPE)
		a = SLUR_SLOPE;
	else if (a < -SLUR_SLOPE)
		a = -SLUR_SLOPE;
	if (a * s > 0)
		y1 = y2 - a * (x2 - x1);
	else	y2 = y1 + a * (x2 - x1);

	/* for big vertical jump, shift endpoints */
	y = y2 - y1;
	if (y > 8)
		y = 8;
	else if (y < -8)
		y = -8;
	z = y;
	if (z < 0)
		z = -z;
	dx = 0.5 * z;
	dy = 0.3 * z;
	if (y > 0) {
		if (s > 0) {
			x2 -= dx;
			y2 -= dy;
		} else {
			x1 += dx;
			y1 += dy;
		}
	} else {
		if (s > 0) {
			x1 += dx;
			y1 -= dy;
		} else {
			x2 -= dx;
			y2 += dy;
		}
	}

	h = 0;
	for (k = k1->next; k != k2 ; k = k->next) {
		if (k->type == NOTE) {
			float staffb;

			staffb = staff_tb[(unsigned) k->staff].y;
			x = k->x;
			yy = (s > 0 ? k->ymx : k->ymn) + staffb;
			y = extreme(s,
				    yy + 6 * s,
				    k->ys + staffb + 2 * s);
			z = (y2 * (x - x1) + y1 * (x2 - x)) / (x2 - x1);
			h = extreme(s, h, y - z);
		}
	}

	y1 += 0.4 * h;
	y2 += 0.4 * h;
	h *= 0.6;

	hmin = (0.03 * (x2 - x1) + 8.) * s;
	if (nn > 3)
		hmin = s * (0.12 * (x2 - x1) + 12);
	height = extreme(s, hmin, 3.0 * h);
	height = extreme(-s, height, 50. * s);

	y = y2 - y1;
	if (y < 0)
		y = -y;
	if (s > 0 && height < 0.8 * y)
		height = 0.8 * y;
	else if (s < 0 && height > -0.8 * y)
		height = -0.8 * y;

	output_slur(x1, y1, x2, y2, s, height,
		    3. * s * level);
}

/* -- find place to terminate/start slur -- */
static struct SYMBOL *next_scut(struct SYMBOL *s)
{
	struct SYMBOL *prev;

	prev = s;
	for (s = s->next; s != 0; s = s->next) {
		if (s->type == BAR
		    && (s->as.u.bar.type == B_RREP
			|| s->as.u.bar.type == B_DREP
			|| s->as.u.bar.type == B_THIN_THICK
			|| s->as.u.bar.type == B_THICK_THIN
			|| (s->as.text != 0
			    && s->as.text[0] != '1')))
			return s;
		prev = s;
	}
	return prev;
}

static struct SYMBOL *prev_scut(struct SYMBOL *s)
{
	struct SYMBOL *sym;
	int voice;

	voice = s->voice;
	for ( ; s != 0; s = s->prev) {
		if (s->type == BAR
		    && (s->as.u.bar.type == B_LREP
			|| s->as.u.bar.type == B_DREP
			|| s->as.u.bar.type == B_THIN_THICK
			|| s->as.u.bar.type == B_THICK_THIN
			|| (s->as.text != 0
			    && s->as.text[0] != '1')))
			return s;
	}

	/* return sym before first note */
	sym = voice_tb[voice].sym;
	for (s = sym; s != 0; s = s->next) {
		if (s->type == REST
		    || s->type == NOTE)
			return s->prev;
	}
	return sym;
}

/* -- draw_chord_ties -- */
static void draw_chord_ties(struct SYMBOL *k1,
			    struct SYMBOL *k2,
			    int nslur,
			    int *mhead1,
			    int *mhead2,
			    int job)
{
	struct SYMBOL *cut;
	int i, m1, p1, p2, y;
	float s, x1, y1, x2, y2, height, addx, addy;

	for (i = 0; i < nslur; i++) {
		m1 = mhead1[i];
		p1 = k1->pits[m1];
		p2 = k2->pits[mhead2[i]];
		if ((s = slur_multi(k1, k2)) == 0) {
			if (k1->nhd == 0)
				s = slur_direction(k1, k2);
			else if (m1 == 0)	/* if bottom */
				s = -1;
			else if (m1 == k1->nhd)	/* if top */
				s = 1;
			else	s = slur_direction(k1, k2);
		}

		x1 = k1->x + k1->shhd[m1];
		x2 = k2->x;
		if (job == 2) {
			cut = next_scut(k1);
			x2 = cut->x;
			if (cut == k1)
				x2 = x1 + 30.;
		} else if (job == 1) {
			cut = prev_scut(k1);
			x1 = cut->x;
			if (cut == k1)
				x2 = x1 - 30.;
		}

		addx = 0.04 * (x2 - x1);
		if (addx > 3.0)
			addx = 3.0;
		addy = 0.02 * (x2 - x1);
		if (addy > 3.0)
			addy = 3.0;

		x1 += 3 + addx;
		x2 -= 3 + addx;
		if (s > 0 && k1->stem > 0)
			x1 += 1.5;
		else if (s < 0 && k2->stem < 0)
			x2 -= 1.5;

		y = 3 * (p1 - 18);
		y1 = y + s * (4. + addy);
		y = 3 * (p2 - 18);
		y2 = y + s * (4. + addy);

		if (s > 0 && !(y % 6) && k1->dots > 0) {
			y2 = y1 = y + s * (5.5 + addy);
			x1 -= 2;
			x2 += 2;
		}
		height = s * (0.04 * (x2 - x1) + 5.);
		output_slur(x1, y1, x2, y2, s, height,
			    staff_tb[(unsigned) k1->staff].y);
	}
}

/* -- draw slurs/ties between neighboring notes/chords -- */
static void draw_ties(struct SYMBOL *k1,
		      struct SYMBOL *k2,
		      int job)
{
	int i, j, m1, m2;
	int mhead1[MAXHD], mhead2[MAXHD], nslur, nh1, nh2;

	if (nbuf + 100 > BUFFSZ)
		rx("PS output exceeds reserved space per staff",
		   " -- increase BUFFSZ1");

	nslur = 0;
	nh1 = k1->nhd;

	if (job == 2) {			/* half slurs from last note in line */
		for (i = 0; i <= nh1; i++) {
			if (k1->as.u.note.ti1[i]) {
				mhead1[nslur] = 0;
				nslur++;
			}
			j = i + 1;
			for (m1 = 0; m1 <= nh1; m1++) {
				if (k1->as.u.note.sl1[m1] == j) {
					mhead1[nslur] = m1;
					nslur++;
					break;
				}
			}
		}
		if (nslur > 0)
			draw_chord_ties(k1, k1,
					nslur, mhead1, mhead1, job);
		return;
	}

	if (job == 1) {			/* half slurs to first note in line */
		/* (ti2 is just used in this case) */
		for (i = 0; i <= nh1; i++) {
			if (k1->as.u.note.ti2[i]) {
				mhead1[nslur] = 0;
				nslur++;
			}
			j = i + 1;
			for (m1 = 0; m1 <= nh1; m1++) {
				if (k1->as.u.note.sl2[m1] == j) {
					mhead1[nslur] = m1;
					nslur++;
					break;
				}
			}
		}
		if (nslur > 0)
			draw_chord_ties(k1, k1,
					nslur, mhead1, mhead1, job);
		return;
	}

	/* real 2-note case: set up list of slurs/ties to draw */
	nh2 = k2->nhd;
	for (i = 0; i <= nh1; i++) {
		if (k1->as.u.note.ti1[i]) {
			int pit;

			pit = k1->pits[i];
			for (m2 = 0; m2 <= nh2; m2++) {
				if (k2->pits[m2] == pit) {
					mhead1[nslur] = i;
					mhead2[nslur] = m2;
					nslur++;
					break;
				}
			}
		}
		j = i + 1;
		for (m1 = 0; m1 <= nh1; m1++) {
			if (k1->as.u.note.sl1[m1] == j) {
				for (m2 = 0; m2 <= nh2; m2++) {
					if (k2->as.u.note.sl2[m2] == j) {
						mhead1[nslur] = m1;
						mhead2[nslur] = m2;
						nslur++;
						break;
					}
				}
			}
		}
	}
	if (nslur > 0)
		draw_chord_ties(k1, k2,
				nslur, mhead1, mhead2, job);
}

/* -- draw all slurs/ties between neighboring notes -- */
static void draw_all_ties(struct SYMBOL *sym)
{
	struct SYMBOL *s1, *s2;

	for (s1 = sym; s1 != 0; s1 = s1->next) {
		if (s1->type == NOTE)
			break;
	}
	if (s1 == 0)
		return;
	draw_ties(s1, s1, 1);
  
	for (;;) {
		for (s2 = s1->next; s2 != 0; s2 = s2->next) {
			if (s2->type == NOTE)
				break;
		}
		if (s2 == 0)
			break;
		draw_ties(s1, s2, 0);
		s1 = s2;
	}
	draw_ties(s1, s1, 2);
}

/* -- draw all phrasing slurs for one staff -- */
static void draw_all_phrasings(struct SYMBOL *sym)
{
	struct SYMBOL *s, *s1, *k;
	struct SYMBOL *cut;
	int pass,num;

	for (pass = 0; ; pass++) {
		num = 0;
		for (s = sym; s != 0; s = s->next) {
			if (s->type != NOTE
			    || !s->as.u.note.slur_st)
				continue;
			k = 0;			/* find matching slur end */
			for (s1 = s->next; s1 != 0; s1 = s1->next) {
				if (s1->as.u.note.slur_end) {
					k = s1;
					break;
				}
				if (s1->as.u.note.slur_st)
					break;
			}
			if (k != 0) {
				cut = next_scut(s);
				for (s1 = cut->next; s1 != 0; s1 = s1->next)
					if (s1 == k)
						break;
				if (s1 != 0 && s1 != k) {
					draw_phrasing(s, cut, pass);
					s = prev_scut(k);
				}
				draw_phrasing(s, k, pass);
				num++;
				s->as.u.note.slur_st--;
				k->as.u.note.slur_end--;
			}
		}
		if (num == 0)
			break;
	}

	/* do unbalanced slurs still left over */
	for (s = sym; s != 0; s = s->next) {
		if (s->type != NOTE)
			continue;
		if (s->as.u.note.slur_end) {
			cut = prev_scut(s);
			draw_phrasing(cut, s, 0);
		}
		if (s->as.u.note.slur_st) {
			cut = next_scut(s);
			draw_phrasing(s, cut, 0);
		}
	}
}

/* -- check_bars -- */
static void check_bars(struct SYMBOL *sym)
{
	struct SYMBOL *s;

	/* split up unsuitable bars at staff end */
	for (s = sym; s->next != 0; s = s->next)
		;
	if (s->type == BAR) {
		if (s->as.u.bar.type == B_LREP) {
			s->as.u.bar.type = B_SINGLE;
			insert_btype = B_LREP;
			insert_v = 0;
			if (s->prev->type == BAR) {	  /* avoid consecutive bars */
				s->as.u.bar.type = s->prev->as.u.bar.type;
				s->as.text = s->prev->as.text;
				s->prev->as.u.bar.type = B_INVIS;
			}
		} else if (s->as.u.bar.type == B_DREP) {
			s->as.u.bar.type = B_RREP;
			insert_btype = B_LREP;
			insert_v = 0;
		} else if (s->as.u.bar.type == B_RREP && s->as.text != 0) {
			insert_btype = B_INVIS;
			insert_v = s->as.text;
			s->as.text = 0;
		} else if (s->as.u.bar.type == B_SINGLE && s->as.text != 0) {
			insert_btype = B_INVIS;
			insert_v = s->as.text;
			s->as.text = 0;
		}
	}

	/* merge back-to-back repeat bars */
	for (s = sym; s->next != 0; s = s->next) {
		if (s->type == BAR && s->as.u.bar.type == B_RREP) {
			if (s->next->type == BAR && s->next->as.u.bar.type == B_LREP) {
				s->type = INVISIBLE;
				s->next->as.u.bar.type = B_DREP;
				s->next->x = 0.5 * (s->x + s->next->x);
			}
		}
	}

	/* remove single bars next to another bar */
	for (s = sym->next; s->next != 0; s = s->next) {
		if ((s->type == BAR && s->as.u.bar.type == B_SINGLE)
		    && ((s->next->type == BAR && s->next->as.u.bar.type != B_INVIS)
			|| (s->prev->type == BAR && s->prev->as.u.bar.type != B_INVIS)))
			s->type = INVISIBLE;
	}

#if 0
	/* remove double bars next to another bar */
	for (s = sym->next; s->next != 0; s = s->next) {
		if (s->type == BAR && s->as.u.bar.type == B_DOUBLE) {
			if (s->next->type == BAR && s->next->as.u.bar.type != B_INVIS)
				s->type = INVISIBLE;
			if (s->prev->type == BAR && s->prev->as.u.bar.type != B_INVIS)
				s->type = INVISIBLE;
		}
	}
#endif
}

/* -- draw guitar chord/figured bass -- */
static void draw_gchord(char *p,
			struct SYMBOL *s)
{
	float gchy, w, spc, yspc;
	char *q, *r;

	/* look where to print the first line */
	gchy = s->gchy;
	yspc = cfmt.gchordfont.size;
	q = p;
	while ((q = strstr(q, "\\n")) != 0) {
		gchy += yspc;
		q += 2;		/* skip "\n" */
	}
	for (;;) {
		if ((q = strstr(p, "\\n")) != 0)
			*q = '\0';
		w = 0;
		for (r = p; *r != '\0'; r++)
			w += cwid(*r);
		spc = w * cfmt.gchordfont.size * GCHPRE;
		if (spc > 8.0)
			spc = 8.0;
		PUT3("%.1f %.1f (%s) gc ",
		     s->x - spc, gchy, p);
		if (q == 0)
			break;
		p = q + 2;	/* skip "\n" */
		gchy -= yspc;
	}
}

/* -- draw_vocals -- */
static void draw_vocals(struct SYMBOL *sym,
			int nwl)
{
	int hyflag, l, j;
	float x, x0, lastx, yword, spc, vfsize, w, swfac, lskip;
	unsigned char word[81], t[81];
	float staffb;
	int staff;

	vfsize = cfmt.vocalfont.size;
	lskip = 1.1 * vfsize;
	set_font(&cfmt.vocalfont);
	yword = cfmt.vocalspace;
	swfac = cfmt.vocalfont.swfac;

	staff = sym->staff;
	if (cfmt.vocalfont.size - staff_tb[staff].botpos > yword)
		yword = cfmt.vocalfont.size - staff_tb[staff].botpos;
	staffb = staff_tb[staff].y		/* bottom of staff */
		- yword;

	for (j = 0; j < nwl; j++) {
		struct SYMBOL *s;

		hyflag = 0;
		lastx = -10;
		for (s = sym; s != 0; s = s->next) {
			struct lyrics *ly;

			if (s->type != NOTE
			    || (ly = s->as.u.note.ly) == 0
			    || ly->w[j] == 0)
				continue;
			strncpy(word, ly->w[j], sizeof word - 1);
			word[sizeof word - 1] = '\0';
			x0 = s->x;
			l = strlen(word);

			if (hyflag) {
				tex_str(t, word, sizeof t, &w);
				spc = x0 - VOCPRE * vfsize * swfac * w - lastx;
				x = lastx + 0.5 * spc
					- 0.5 * swfac * vfsize * cwid('-');
				PUT2("%.1f %.1f whf ", x, staffb);
				hyflag = 0;
			}

			if (l > 1 && word[l - 1] == '^') {
				word[l - 1] = '\0';
				hyflag = 1;
			}

			if (l == 1 && word[0] == '_') {
				if (lastx < 0)
					lastx = s->prev->x + s->prev->wr;
				PUT3("%.1f %.1f %.1f wln ",
				     lastx + 3, x0 + 1, staffb);
				lastx = x0 + 1;
			} else if (l == 1 && word[0] == '^') {
				PUT2("%.1f %.1f whf ", x0, staffb);
				lastx = x0 + vfsize * swfac * w;
			} else {
				tex_str(t, word, sizeof t, &w);
				if (isdigit(word[0]))
/*1.2.10: was 3*/
					x0 -= 5. * vfsize * swfac * cwid('1');
				else	x0 -= VOCPRE * vfsize * swfac * w;
				PUT3("(%s) %.1f %.1f wd ", t, x0, staffb);
				lastx = x0 + vfsize * swfac * w;
			}
		}
		if (hyflag)
			PUT2("%.1f %.1f whf ", lastx + 5, staffb);
		PUT0("\n");
		staffb -= lskip;
	}
}

/* -- draw symbols at proper positions on staff -- */
static void draw_symbols(int voice)
{
	struct SYMBOL *sym;
	int nwl;
	float x, y, top, xl, d, w, gchy;
	struct BEAM bm;
	struct SYMBOL *s;

	sym = voice_tb[voice].sym;

	/* draw the symbols */
	bm.s2 = 0;
	for (s = sym; s != 0; s = s->next) {
		if (nbuf + 100 > BUFFSZ) 
			rx("PS output exceeds reserved space per staff",
			   " -- increase BUFFSZ1");
		x = s->x;
		switch (s->type) {
		case NOTE:
			xl = s->prev->x;
			d = x - xl;
			w = s->shrink;

			if (s->word_st && !s->as.u.note.word_end)
				calculate_beam(s, &bm);
			top = draw_note(x, w, d, s, bm.s2 == 0, &gchy);
			if (s == bm.s2) {
				draw_beams(&bm);
				bm.s2 = 0;
			}
			s->gchy = gchy;
			s->dc_top = top;
			break;

		case REST:
			y = s->y;
			if (bm.s2 != 0)
				y = rest_under_beam(s, &bm);
			draw_rest(x, y, s, &gchy);
			s->gchy = gchy;
			break;

		case BAR:
			update_endings(s);
			if (voice != 0)
				break;
			draw_bar(x, s);
			s->gchy = 38. + staff_tb[(unsigned) s->staff].y; /* (if gchord) */
			nbar++;
			if (!s->eoln
			    && measure_nb > 0
			    && nbar % measure_nb == 0) {
				set_font(&cfmt.composerfont);
				PUT3("%.1f %.1f M (%d) show\n",
				     x, staff_tb[0].y + 24. + 6., nbar);
			}
			break;

		case CLEF: {
			int	staff;
			float	staffb;
			char	ct = 'c';		/* clef type - def: alto */

			staff = s->staff;
			if (voice_tb[voice].second)
				continue;		/* only one clef per staff */
			staff_tb[staff].clef = s->u;	/* (for next lines) */
			staffb = staff_tb[staff].y;

			switch (s->u) {
			case BASS3:
				staffb -= 6;
			case BASS:
				ct = 'b';
				break;
			case ALTO1:
				staffb -= 12;
				break;
			case ALTO2:
				staffb -= 6;
				break;
			case ALTO4:
				staffb += 6;
			case ALTO:
				break;
			default:
				bug("unknown clef type", 0);
			case TREBLE:
				ct = 't';
				break;
			}
			PUT4("%.1f %.1f %c%cclef\n", x, staffb,
			     s->v ? 's' : ' ', ct);
			break;
		}
		case TIMESIG:
			if (voice != 0)
				break;
			draw_timesig(x, s);
			voice_tb[0].p_meter = s;
			break;

		case KEYSIG:
			draw_keysig(voice, x, s);
			break;

		case INVISIBLE:
		case TEMPO:
			break;			/* nothing */

		default:
			printf(">>> line %d - cannot draw symbol type %d\n",
				s->as.linenum, s->type);
		}
	}

	/* draw guitar chords */
	/*fixme: set font & eoln only if guitar chords in postscript*/
	set_font(&cfmt.gchordfont);

	for (s = sym; s != 0; s = s->next) {
		char t[81];

		switch (s->type) {
		case BAR:
			if (s->as.text != 0
			    && !isdigit((unsigned char) s->as.text[0]))
				break;
			continue;
		case NOTE:
		case REST:
			if (s->as.text != 0)
				break;
			continue;
		default:
			continue;
		}
		tex_str(t, s->as.text, sizeof t, &w);
		draw_gchord(t, s);
	}

	draw_nplet_brackets(sym);

	draw_all_ties(sym);

	draw_all_phrasings(sym);

	draw_endings();

	nwl = staff_tb[(unsigned) sym->staff].nvocal;
	if (nwl > 0 && !cfmt.musiconly)
		draw_vocals(sym, nwl);
}

/* -- draw the tempo(s) -- */
static void draw_tempo(float indent)
{
	struct SYMBOL *s;
	int head, dots, flags;
	float sc, dx;

	/* search if any tempo (first voice only) */
	for (s = voice_tb[0].sym; s != 0; s = s->next) {
		if (s->type == TEMPO)
			break;
	}
	if (s == 0)
		return;			/* no tempo */

	set_font(&cfmt.tempofont);
	PUT0("\n");

	for (s = voice_tb[0].sym; s != 0; s = s->next) {
		if (s->type != TEMPO)
			continue;

		/*fixme: cf left shift (-5.)*/
		PUT1("%.1f 0 M ", (s->x + indent) * cfmt.scale - 5.);

		/* draw the string, if specified */
		if (s->as.u.tempo.str != 0)
			put_str3("(",
				 s->as.u.tempo.str,
				 "  ) show\n");

		/* draw the tempo indication, if specified */
		if (s->as.u.tempo.length == 0)
			continue;
		identify_note(s->as.u.tempo.length,
			      &head, &dots, &flags);

		/* draw the note */
		sc = 0.55 * cfmt.tempofont.size / 10.0;
		PUT2("gsave %.2f %.2f scale 15 3 rmoveto currentpoint\n",
		     sc, sc);
		if (head == H_OVAL)
			PUT0("HD");
		else if (head == H_EMPTY)
			PUT0("Hd");
		else if (head == H_FULL)
			PUT0("hd");
		dx = 4.0;
		if (dots) {
			float dotx;
			int i;

			dotx = 8;
			if (flags > 0)
				dotx += 4;
			if (head == H_EMPTY)
				dotx += 1;
			else if (head == H_OVAL)
				dotx += 2;
			for (i = 0; i < dots; i++) {
				PUT1(" %.1f 0 dt", dotx);
				dx = dotx;
				dotx += 3.5;
			}
		}
		/* (16 is the stem high) */
		if (s->as.u.tempo.length < SEMIBREVE)
			PUT0(" 16 su");
		if (flags > 0) {
			PUT1(" 16 f%du", flags);
			if (dx < 6.0)
				dx = 6.0;
		}
		PUT2(" grestore %.2f 0 rmoveto ( = %d) show\n",
		     (dx + 18) * sc, s->as.u.tempo.value);
	}
#if 0
	bskip(cfmt.tempofont.size + 0.1 * CM);
#endif
}

/* -- check if any "real" symbol in the piece -- */
static int any_symbol(void)
{
	int voice;
	struct SYMBOL *s;

	for (voice = 0; voice <= nvoice; voice++) {
		for (s = voice_tb[voice].sym; s != 0; s = s->next) {
			switch (s->type) {
			case NOTE:
			case REST:
			case BAR:
				return 1;
			}
		}
	}
	return 0;
}

/* -- delete duplicate keysigs at staff start -- */
static void check_keysigs(void)
{
	int	t;
	int	voice;
	struct SYMBOL *s, *klast;

	for (voice = 0; voice <= nvoice; voice++) {
		klast = 0;
		for (s = voice_tb[voice].sym; s != 0; s = s->next) {
			t = s->type;
			if (t == NOTE || t == REST || t == BAR)
				break;
			if (t == KEYSIG) {
				if (klast != 0) {
					klast->type = INVISIBLE;
					klast->u = klast->v = klast->w = 0;
				}
				klast = s;
			}
		}
	}
}

/* -- find one line to output -- */
static void find_piece(void)
{
	struct SYMBOL *s;
	int number, time, seq, i;

	if ((number = cfmt.barsperstaff) == 0) {

		/* find the first end-of-line */
		for (s = tssym; /*s != 0*/; s = s->ts_next) {
			if (s->eoln)
				break;
			if (s->ts_next == 0) {
				/* when '\' at end of line and 'P:' */
				s->eoln = 1;
/*				bug("no eoln in piece", 0); */
				break;
			}
		}
	} else {

		/* count the measures */
		for (s = tssym; s != 0; s = s->ts_next)
			if (s->type == NOTE
			    || s->type == REST)
				break;
		for ( ; s != 0; s = s->ts_next) {
			if (s->type != BAR
			    || s->as.u.bar.type == B_INVIS)
				continue;
			if (s->prev->type == BAR)
				continue;
			if (--number <= 0)
				break;
		}
	}

	/* cut at the last symbol of the sequence */
	time = s->time + s->len;
	seq = s->seq;
	if (s->len > 0) {

		/* note or rest: cut on end time */
		for (; s->ts_next != 0; s = s->ts_next)
			if (s->ts_next->time >= time)
				break;
	} else {

		/* other symbol: cut on different sequence */
		for (; s->ts_next != 0; s = s->ts_next)
			if (s->ts_next->seq != seq)
				break;
	}
	set_piece(s);

	for (i = nstaff + 1; --i >= 0;) {
		staff_tb[i].toppos = 0;
		staff_tb[i].botpos = 0;
		staff_tb[i].nvocal = 0;
	}
}

/* -- init sym list with clef, meter, key -- */
static void init_music_line(int voice)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s, *sym;
	static	int meter_f, btype_f;

	if (verbose > 11)
		printf("init_music_line: voice:%d i_m=%d, i_b=%d\n",
		       voice, insert_meter, insert_btype);

	p_voice = &voice_tb[voice];
	sym = p_voice->sym;
	p_voice->sym = 0;

	/* add clef */
	s = add_sym(p_voice, CLEF);
	p_voice->clef = staff_tb[(unsigned) p_voice->staff].clef;
	s->u = p_voice->clef;

	/* add keysig */
	s = add_sym(p_voice, KEYSIG);
	s->seq++;
	if (p_voice->p_key != 0) {
		s->as.u.key.sf = p_voice->p_key->as.u.key.sf;
		s->as.u.key.bagpipe = p_voice->p_key->as.u.key.bagpipe;
	}
	p_voice->bagpipe = s->as.u.key.bagpipe;

	/* add time signature if needed */
	if (insert_meter
	    || (voice != 0 && meter_f)) {
		struct SYMBOL *p_meter;

		/* the time signature is taken from the voice 0 */
		if ((p_meter = voice_tb[0].p_meter) != 0) {
			if (p_meter->as.u.meter.m1 != 0) {	/* != M:none */
				s = add_sym(p_voice, TIMESIG);
				s->seq += 2;
				s->as.u.meter.m1 = p_meter->as.u.meter.m1;
				s->as.u.meter.m2 = p_meter->as.u.meter.m2;
				s->as.u.meter.flag = p_meter->as.u.meter.flag;
				s->as.u.meter.top = p_meter->as.u.meter.top;
			}
		} else {
			s = add_sym(p_voice, TIMESIG);
			s->seq += 2;
			s->as.u.meter.m1 = 4;		/* default values */
			s->as.u.meter.m2 = 4;
			s->as.u.meter.flag = 0;
			s->as.u.meter.top = 0;
		}
	}
	if (voice == 0) {
		meter_f = insert_meter;
		insert_meter = 0;
	}

	/* add tempo if any */
	if (info.tempo) {
		/*fixme: may be cleaner*/
		s = add_sym(p_voice, TEMPO);
		s->seq = s->prev->seq + 1;	/*??*/
		s->as.u.tempo.str = info.tempo->as.u.tempo.str;
		s->as.u.tempo.length = info.tempo->as.u.tempo.length;
		s->as.u.tempo.value = info.tempo->as.u.tempo.value;
		info.tempo = 0;
	}

	/* add bar if needed */
	if (insert_btype >= 0
	    || (voice != 0 && btype_f >= 0)) {
		if (verbose >= 20)
			printf("insert bar, type %d\n", insert_btype);
		s = add_sym(p_voice, BAR);
		s->seq += 3;
		s->u = insert_btype;
		s->as.text = insert_v;
	}
	if (voice == 0) {
		btype_f = insert_btype;
		insert_btype = -1;
	}
	if ((p_voice->last_symbol->next = sym) != 0)
		sym->prev = p_voice->last_symbol;
}

/* -- initialize a new line -- */
static void cut_symbols(void)
{
	int voice;
	struct SYMBOL *s, *s1;
	int done;
	int j, t;
	struct SYMBOL *vtb[MAXVOICE];

	tssym = tsnext;
	t = tssym->time;

	/* set start of voices */
	for (voice = 0; voice <= nvoice; voice++) {
		voice_tb[voice].sym = 0;	/* may have no symbol */
		for (s = tssym; s != 0; s = s->ts_next)
			if (s->voice == voice) {
				voice_tb[voice].sym = s;
				voice_tb[voice].staff = s->staff;
				s->prev = 0;
				break;
			}
	}

	/* add the first symbols of the line */
	for (voice = 0; voice <= nvoice; voice++) {
		init_music_line(voice);

	/* insert the new symbols into the sorted list */
		vtb[voice] = voice_tb[voice].sym;
	}

	done = 0;
	s = 0;
	for (j = 1; ; j++) {
		for (voice = 0; voice <= nvoice; voice++) {
			s1 = vtb[voice];
			if (s1 == 0)
				continue;
			if (s1->time != 0
			    || s1 == tsnext) {	/* (if no note/rest) */
				done = 1;
				break;
			}
			if (s == 0)
				tssym = s1;
			else	s->ts_next = s1;
			s1->ts_prev = s;
			s = s1;
			s->seq = j;
			s->time = t;
			vtb[voice] = s->next;
		}
		if (done)
			break;
	}
	s->ts_next = tsnext;
	tsnext->ts_prev = s;

	num_ending = 0;
	mes1 = mes2 = 0;
}

/* -- output for parsed symbol list -- */
void output_music(void)
{
	float	realwidth;
	float	lscale, lwidth;
	int	voice;

	if (verbose > 8) {
		for (voice = 0; voice <= nvoice; voice++) {
			char *vn;

			if ((vn = voice_tb[voice].name) != 0)
				printf("output_music: voice %d '%s'\n",
				       voice, vn);
			if (verbose > 9)
				print_syms(voice_tb[voice].sym);
		}
	}

	if (!file_initialized && !epsf) {
		init_ps(fout, infostr, 0, 0.0, 0.0, 0.0, 0.0);
		init_page(fout);
	}

#if 1
	for (voice = nvoice + 1; --voice >= 0; )
		if (voice_tb[voice].sym != 0)
			break;
	if (voice < 0)
		return;		/* no symbol at all */
#else
	if (voice_tb[0].sym == 0)
		return;
#endif

	for (voice = 0; voice <= nvoice; voice++)
		init_music_line(voice);
	num_ending = 0;
	mes1 = mes2 = 0;

	mline = 0;
	alfa_last = 0.1;
	beta_last = 0.0;

	lwidth = cfmt.staffwidth;
	lscale = cfmt.scale;
	check_margin(cfmt.leftmargin);

	/* dump buffer if not enough space for a staff line */
	check_buffer(fout, BUFFSZ1);

	set_multi();		/* set global characteristics */
	set_sym_chars();	/* set symbol characteristics */
	for (voice = 0; voice <= nvoice; voice++)
		set_beams(voice_tb[voice].sym);	/* decide on beams */
	nbar = 1;	/* for measure numbering */

	/* set all symbols */
	for (;;) {		/* loop over pieces of line for output */
		mline++;

		find_piece();

		if (any_symbol()) {
			float indent;

			if (verbose > 9)
				printf("row %d, nvoice %d nstaff %d\n",
					mline, nvoice, nstaff);
			check_keysigs();
			for (voice = 0; voice <= nvoice; voice++) {
				set_stems(voice_tb[voice].sym);
				set_sym_widths(voice_tb[voice].sym);
			}
			set_overlap();
			indent = set_indent();
			realwidth = set_sym_glue(lwidth / lscale - indent);

			PUT1("\n%% --- row %d\n", mline);
			draw_tempo(indent);
			bskip(lscale * set_staff());
			PUT3("gsave %.3f %.3f scale %.2f setlinewidth\n",
			     lscale, lscale, BASEWIDTH);
			draw_staff(realwidth, indent);
			for (voice = 0; voice <= nvoice; voice++) {
				check_bars(voice_tb[voice].sym);
				draw_symbols(voice);
			}
			PUT0("grestore\n");
			buffer_eob(fout);
		}
		if (tsnext == 0)
			break;
		cut_symbols();
	}

	/* reset the parser */
	for (voice = nvoice + 1; --voice >= 0; )
		voice_tb[voice].sym = 0;
}

/* -- reset the generator -- */
void reset_gen(void)
{
	insert_meter = 1;
}
