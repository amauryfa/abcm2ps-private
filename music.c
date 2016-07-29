/*
 * Music generator.
 *
 * This file is part of abcm2ps.
 *
 * Copyright (C) 1998-2007 Jean-Fran�ois Moine
 * Adapted from abc2ps, Copyright (C) 1996,1997 Michael Methfessel
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
 */

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#include "abcparse.h"
#include "abc2ps.h"

struct SYMBOL *tsnext;		/* next line when cut */
float realwidth;		/* real staff width while generating */

static int insert_meter;	/* insert time signature (1) and 1st line (2) */
static float alfa_last, beta_last;	/* for last short short line.. */

#define AT_LEAST(a,b)  do { float tmp = b; if(a<tmp) a=tmp; } while (0)

/* width of notes indexed by log2(note_length) */
float space_tb[NFLAGS_SZ] = {
	7, 10, 14.15, 20, 28.3,
	40,				/* crotchet */
	56.6, 80, 113, 150
};

/* upper and lower space needed by rests */
static struct {
	char u, l;
} rest_sp[NFLAGS_SZ] = {
	{16, 31},
	{16, 25},
	{16, 19},
	{10, 19},
	{10, 13},
	{10, 13},			/* crotchet */
	{7, 7},
	{10, 4},
	{10, 7},
	{10, 13}
};

/* -- decide whether to shift heads to other side of stem on chords -- */
/* also position accidentals to avoid too much overlap */
/* this routine is called only once per tune */
static void set_head_directions(struct SYMBOL *s)
{
	int i, i1, i2, i3, n, sig, d, shift;
	int p1, p2, p3, ps, m, nac;
	float dx, dx1, dx2, dx3, shmin, shmax;
	unsigned char ax_tb[MAXHD], ac_tb[MAXHD];
	static float dx_tb[4] = {
		9, 10, 12, 14
	};
	/* distance for no overlap - index: [prev acc][cur acc] */
	static char dt_tb[4][4] = {
		{5, 5, 5, 5},		/* dble sharp */
		{5, 6, 6, 6},		/* sharp */
		{5, 6, 5, 6},		/* natural */
		{5, 5, 5, 5}		/* flat */
	};

	/* special case when single note */
	if ((n = s->nhd) == 0) {
		if (s->as.u.note.accs[0] != 0) {
			dx = dx_tb[s->head];
			if (s->as.flags & ABC_F_GRACE)
				dx *= 0.7;
			s->shac[0] = dx;
		}
		return;
	}

	/* set the head shifts */
	dx = dx_tb[s->head] * 0.78;
	if (s->as.flags & ABC_F_GRACE)
		dx *= 0.5;
	i1 = 1;
	i2 = n + 1;
	sig = s->stem;
	if (sig < 0) {
		dx = -dx;
		i1 = n - 1;
		i2 = -1;
	}
	shift = 0;
	nac = 0;
	for (i = i1; i != i2; i += sig) {
		d = s->pits[i] - s->pits[i - sig];
		if (d < 0)
			d = -d;
		if (d > 3 || (d >= 2 && s->head < H_SQUARE))
			shift = 0;
		else {
			shift = !shift;
			if (shift) {
				s->shhd[i] = dx;
				nac++;
			}
		}
	}
	if (nac != 0 && sig > 0)
		s->xmx = dx;		/* shift the dots */

	/* set the accidental shifts */
	nac = 0;
	for (i = n; i >= 0; i--) {	/* from top to bottom */
		if ((i1 = s->as.u.note.accs[i]) != 0) {
			ax_tb[nac++] = i;
			if (i1 & 0xf8)
				i1 = A_SH;	/* micro-tone same as sharp */
			else if (i1 == A_DF)
				i1 = A_FT;	/* dble flat same as flat */
			else if (i1 == A_DS)
				i1 = 0;		/* (max -> 0) */
			ac_tb[i] = i1;
		}
	}
	if (nac == 0)			/* no accidental */
		return;
	dx = dx_tb[s->head];
	m = n;
	p2 = i2 = 0;			/* (compiler warning) */
	dx2 = 0;
	ps = 255;
	for (i = 0; i < nac; i++) {
		i1 = ax_tb[i];
		p1 = s->pits[i1];
		if (m >= 0) {		/* see if any head shift */
			if (ps - s->pits[i1] >= 4) {
				for (m--; m >= 0; m--) {
					if (s->shhd[m] < 0) {
						ps = s->pits[m];
						break;
					}
				}
			}
		}
		dx1 = dx;
		if (m >= 0 && s->shhd[m] < 0
		    && ps - p1 < 4 && ps - p1 > -4)
			dx1 -= s->shhd[m];
		if (s->as.flags & ABC_F_GRACE)
			dx1 *= 0.7;
		if (i == 0) {		/* no other shift for the 1st accidental */
			s->shac[i1] = dx1;
			i2 = i1;
			p2 = p1;
			dx2 = dx1;
			continue;
		}
		d = dt_tb[ac_tb[i2]][ac_tb[i1]];
		if (p2 - p1 < d) {		/* if possible overlap */
			if (s->as.u.note.accs[i1] & 0xf8) {	/* microtonal */
				shmin = 6.5;
				shmax = 9;
			} else {
				shmin = 4.5;
				shmax = 7;
			}
			if (s->as.flags & ABC_F_GRACE) {
				shmin *= 0.7;
				shmax *= 0.7;
			}
			if (i >= 2) {
				i3 = ax_tb[i - 2];
				p3 = s->pits[i3];
				d = dt_tb[ac_tb[i3]][ac_tb[i1]];
				if (p3 - p1 < d) {
					dx3 = s->shac[i3];
					if (p3 - p1 >= 4
					    && (ac_tb[i3] != A_SH || ac_tb[i1] != A_SH)) {
						if (dx1 > dx3 - shmin && dx1 < dx3 + shmin)
							dx1 = dx3 + shmin;
					} else {
						if (dx1 > dx3 - shmax && dx1 < dx3 + shmax)
							dx1 = dx3 + shmax;
					}
				}
			}
			if (p2 - p1 >= 4
			    && (ac_tb[i2] != A_SH || ac_tb[i1] != A_SH)) {
				if (dx1 > dx2 - shmin && dx1 < dx2 + shmin) {
					if (dx1 + shmin < dx2 + shmin)
						s->shac[i2] = dx1 + shmin;
					else	dx1 = dx2 + shmin;
				}
			} else {
				if (dx1 > dx2 - shmax && dx1 < dx2 + shmax) {
					if (dx1 + shmax < dx2 + shmax)
						s->shac[i2] = dx1 + shmax;
					else	dx1 = dx2 + shmax;
				}
			}
		}
		s->shac[i1] = dx1;
		i2 = i1;
		p2 = p1;
		dx2 = dx1;
	}
}

/* -- unlink a symbol -- */
static void delsym(struct SYMBOL *s)
{
	if (s->next != 0)
		s->next->prev = s->prev;
	if (s->prev != 0)
		s->prev->next = s->next;
	if (s->ts_next != 0) {
		if (s->sflags & S_SEQST) {
			s->ts_next->sflags |= S_SEQST;
			s->ts_next->shrink = s->shrink;
			s->ts_next->space = s->space;
		}
		s->ts_next->ts_prev = s->ts_prev;
	}
	if (s->ts_prev != 0)
		s->ts_prev->ts_next = s->ts_next;
	if (tsnext == s)
		tsnext = s->ts_next;
	if (tsfirst == s)
		tsfirst = s->ts_next;
}

/* -- check if voice combine may occur -- */
static int may_combine(struct SYMBOL *s)
{
	struct SYMBOL *s2;
	int nhd2;

	if ((s2 = s->ts_next) == 0)
		return 0;
	if (s2->voice == s->voice
	    || s2->staff != s->staff
	    || s2->time != s->time
	    || s2->dur != s->dur
	    || s2->ly != 0
	    || (s2->sflags & (S_SL1 | S_SL2))
	    || s2->as.u.note.slur_st != 0
	    || s2->as.u.note.slur_end != 0)
		return 0;
	if (s->type == NOTE
	    && (((s2->sflags ^ s->sflags) & S_WORD_ST)
		|| ((s2->as.flags ^ s->as.flags) & ABC_F_WORD_END)))
		return 0;
	nhd2 = s2->nhd;
	if (s->nhd + nhd2 + 1 >= MAXHD)
		return 0;
	if (!cfmt.comball && s->pits[0] <= s2->pits[nhd2] + 1)
		return 0;
	if (s2->as.u.note.dc.n != 0) {
		if (s2->as.u.note.dc.h != s2->as.u.note.dc.h
		    || memcmp(&s->as.u.note.dc, &s2->as.u.note.dc,
				sizeof s->as.u.note.dc) != 0)
			return 0;
	}
	return 1;
}

/* -- combine 2 voices -- */
static void do_combine(struct SYMBOL *s)
{
	struct SYMBOL *s2;
	int nhd, nhd2;

	nhd = s->nhd;
	s2 = s->ts_next;
	nhd2 = s2->nhd;
	if (s->type != s2->type) {	/* if note and rest */
		if (s2->type == REST) {
			delsym(s2);	/* remove the rest */
			return;
		}
		s->type = NOTE;		/* copy the note into the rest */
		nhd = -1;
		s->pits[0] = 127;
	}

	/* combine the voices */
	if (s->pits[0] >= s2->pits[nhd2]) {	/* standard case */
		if (s->pits[0] == s2->pits[nhd2])
			nhd2--;
		memcpy(&s->pits[nhd2 + 1], s->pits,
			sizeof s->pits[0] * (nhd + 1));
		memcpy(s->pits, s2->pits,
			sizeof s->pits[0] * (nhd2 + 1));

#define COMBINEV(f)\
    memcpy(&s->as.u.note.f[nhd2 + 1], s->as.u.note.f,\
	sizeof s->as.u.note.f[0] * (nhd + 1));\
    memcpy(s->as.u.note.f, s2->as.u.note.f,\
	sizeof s->as.u.note.f[0] * (nhd2 + 1))

		COMBINEV(pits);
		COMBINEV(lens);
		COMBINEV(accs);
		COMBINEV(sl1);
		COMBINEV(sl2);
		COMBINEV(ti1);
		COMBINEV(decs);
#undef COMBINEV
		nhd += nhd2 + 1;
		s->nhd = nhd;
		s->ymn = 3 * (s->pits[0] - 18) - 2;
/*fixme:should recalculate yav*/
	} else {				/* voice inverted */
/*fixme:KO if imbricated chords*/
		memcpy(&s->pits[nhd + 1], s2->pits,
			sizeof s->pits[0] * (nhd2 + 1));

#define COMBINEV(f)\
    memcpy(&s->as.u.note.f[nhd + 1], s2->as.u.note.f,\
	sizeof s->as.u.note.f[0] * (nhd2 + 1));\

		COMBINEV(pits);
		COMBINEV(lens);
		COMBINEV(accs);
		COMBINEV(sl1);
		COMBINEV(sl2);
		COMBINEV(ti1);
		COMBINEV(decs);
#undef COMBINEV
		nhd += nhd2 + 1;
		s->nhd = nhd;
/*fixme:should recalculate yav*/
		s->ymx = 3 * (s->pits[nhd] - 18) + 4;
	}
	if (s->as.u.note.ti1[0] == SL_AUTO)	/* force the tie directions */
		s->as.u.note.ti1[0] = SL_BELOW;
	if (s->as.u.note.ti1[nhd] == SL_AUTO)
		s->as.u.note.ti1[nhd] = SL_ABOVE;

	delsym(s2);			/* remove the next symbol */
}

/* -- try to combine voices */
static void combine_voices(void)
{
	struct SYMBOL *s, *s2;
	int r;

	for (s = tsfirst; s != 0; s = s->ts_next) {
		switch (s->type) {
		case TUPLET:
			if ((s2 = s->ts_next) == 0)
				break;
			r = s->as.u.tuplet.r_plet;
			if (s2->type != TUPLET
			    || s2->as.u.tuplet.r_plet != r
			    || s2->as.u.tuplet.p_plet != s->as.u.tuplet.p_plet
			    || s2->as.u.tuplet.q_plet != s->as.u.tuplet.q_plet)
				continue;
			s2 = s;
			for (;;) {
				s2 = s2->next;
				while (s2->type != NOTE && s2->type != REST)
					s2 = s2->next;
				if (!may_combine(s2))
					break;
				if (--r <= 0)
					break;
			}
			if (r > 0)
				continue;
			delsym(s->ts_next);	/* remove the tuplet */
			r = s->as.u.tuplet.r_plet;
			s2 = s;
			for (;;) {
				s2 = s2->next;
				while (s2->type != NOTE && s2->type != REST)
					s2 = s2->next;
				do_combine(s2);
				if (--r <= 0)
					break;
			}
			continue;
		default:
			continue;
		case NOTE:
			if (!(s->sflags & S_WORD_ST))
				continue;
		case REST:
			break;
		}
		if (s->ts_next == 0)
			break;
		if ((s->sflags & S_IN_TUPLET) || s->prev->type == TUPLET)
			continue;
		if (!(s->as.flags & ABC_F_WORD_END)) {
			s2 = s;
			for (;;) {
				if (!may_combine(s2)) {
					s2 = 0;
					break;
				}
				if (s2->type == REST
				    || (s2->as.flags & ABC_F_WORD_END))
					break;
				do {
					s2 = s2->next;
				} while (s2->type != NOTE && s2->type != REST);
			}
			if (s2 == 0)
				continue;
			s2 = s;
			for (;;) {
				do_combine(s2);
				if (s2->type == REST
				    || (s2->as.flags & ABC_F_WORD_END))
					break;
				do {
					s2 = s2->next;
				} while (s2->type != NOTE && s2->type != REST);
			}
		}
		if (may_combine(s))
			do_combine(s);
	}
}

/* -- insert a clef change (treble or bass) before a symbol -- */
static void insert_clef(struct SYMBOL *s,
			int clef_type)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *new_s;

	/* don't insert the clef between two bars */
	if (s->type == BAR && s->prev != 0 && s->prev->type == BAR
/*	    && s->time == s->prev->time */
			)
		s = s->prev;

	/* create the symbol */
	p_voice = &voice_tb[s->voice];
	if ((p_voice->last_symbol = s->prev) == 0)
		p_voice->sym = 0;
	p_voice->time = s->time;
	new_s = sym_add(p_voice, CLEF);
	new_s->next = s;
	s->prev = new_s;

	new_s->as.u.clef.type = clef_type;
	new_s->as.u.clef.line = clef_type == TREBLE ? 2 : 4;
	new_s->as.u.clef.stafflines = -1;
	new_s->staff = s->staff;
	new_s->u = 1;			/* small clef */
	new_s->sflags &= ~S_SECOND;


	/* link in time */
	while (!(s->sflags & S_SEQST))
		s = s->ts_prev;
	if (s->type == STAVES) {
		s = s->ts_next;
		s->sflags |= S_SEQST;
	} else if (s->ts_prev == 0 || s->ts_prev->type != CLEF)
		new_s->sflags |= S_SEQST;
	if ((new_s->ts_prev = s->ts_prev) != 0)
		new_s->ts_prev->ts_next = new_s;
	else	tsfirst = new_s;
	new_s->ts_next = s;
	s->ts_prev = new_s;
}

/* -- define the clef for a staff -- */
/* this function is called only once for the whole tune */
static void set_clef(int staff)
{
	struct SYSTEM *sy;
	struct SYMBOL *s, *last_chg;
	int clef_type, min, max, time;

	/* get the max and min pitches */
	min = max = 16;			/* 'C' */
	for (s = tsfirst; s != 0; s = s->ts_next) {
		if (s->staff != staff || s->type != NOTE)
			continue;
		if (s->pits[0] < min)
			min = s->pits[0];
		else if (s->pits[s->nhd] > max)
			max = s->pits[s->nhd];
	}

	sy = cursys;
	if (min >= 13)			/* all upper than 'G,' --> treble clef */
		return;
	if (max <= 19) {		/* all lower than 'F' --> bass clef */
		do {
			sy->staff[staff].clef.type = BASS;
			sy->staff[staff].clef.line = 4;
		} while ((sy = sy->next) != 0);
		return;
	}

	/* set clef changes */
	clef_type = TREBLE;
	last_chg = 0;
	for (s = tsfirst; s != 0; s = s->ts_next) {
		struct SYMBOL *s2, *s3, *s4;

		if (s->staff != staff || s->type != NOTE) {
			if (s->type == STAVES) {
				sy = sy->next;	/* keep the starting clef */
				sy->staff[staff].clef.type = clef_type;
				sy->staff[staff].clef.line = clef_type == TREBLE ? 2 : 4;
			}
			continue;
		}

		/* check if a clef change may occur */
		time = s->time;
		if (clef_type == TREBLE) {
			if (s->pits[0] > 12		/* F, */
			    || s->pits[s->nhd] > 20)	/* G */
				continue;
			s2 = s->ts_prev;
			if (s2 != 0
			    && s2->time == time
			    && s2->staff == staff
			    && s2->type == NOTE
			    && s2->pits[0] >= 19)	/* F */
				continue;
			s2 = s->ts_next;
			if (s2 != 0
			    && s2->staff == staff
			    && s2->time == time
			    && s2->type == NOTE
			    && s2->pits[0] >= 19)	/* F */
				continue;
		} else {
			if (s->pits[0] < 12		/* F, */
			    || s->pits[s->nhd] < 20)	/* G */
				continue;
			s2 = s->ts_prev;
			if (s2 != 0
			    && s2->time == time
			    && s2->staff == staff
			    && s2->type == NOTE
			    && s2->pits[0] <= 13)	/* G, */
				continue;
			s2 = s->ts_next;
			if (s2 != 0
			    && s2->staff == staff
			    && s2->time == time
			    && s2->type == NOTE
			    && s2->pits[0] <= 13)	/* G, */
				continue;
		}

		/* go backwards and search where to insert a clef change */
#if 1 /*fixme:test*/
		s3 = s;
#else
		if (!voice_tb[s->voice].second
		    && voice_tb[s->voice].staff == staff)
			s3 = s;
		else	s3 = 0;
#endif
		time = last_chg == 0 ? 0 : last_chg->time;
		for (s2 = s->ts_prev; s2 != last_chg; s2 = s2->ts_prev) {
			if (s2->time <= time)
				break;
			if (s2->staff != staff)
				continue;
			if (s2->type == BAR) {
#if 0 /*fixme:test*/
				if (voice_tb[s2->voice].second
				    || voice_tb[s2->voice].staff != staff)
					continue;
#endif
				s3 = s2;
				break;
			}
#if 1
			if (s2->type != NOTE)
#else
			if (s2->dur == 0)	/* neither note nor rest */
#endif
				continue;

			/* exit loop if a clef change cannot occur */
			if (s2->type == NOTE) {
				if (clef_type == TREBLE) {
					if (s2->pits[0] >= 19)		/* F */
						break;
				} else {
					if (s2->pits[s2->nhd] <= 13)	/* G, */
						break;
				}
			}

#if 1 /*fixme:test*/
#if 1
			/* have a 2nd choice on beam start */
			if ((s3->sflags & S_WORD_ST) == 0)
				s3 = s2;
#else
			/* have a 2nd choice on beam start */
			if ((s2->sflags & S_WORD_ST)
			    || (s3->sflags & S_WORD_ST) == 0)
				s3 = s2;
#endif
#else
			/* have a 2nd choice if word starts on the main voice */
			if (!voice_tb[s2->voice].second
			    && voice_tb[s2->voice].staff == staff) {
				if ((s2->sflags & S_WORD_ST)
				    || s3 == 0
				    || (s3->sflags & S_WORD_ST) == 0)
					s3 = s2;
			}
#endif
		}
		s2 = last_chg;
		last_chg = s;

		/* if first change, see if any note before */
		if (s2 == 0) {
#if 1 /*fixme:test*/
			s4 = s3;
#else
			if ((s4 = s3) == 0)
				s4 = s;
#endif
			for (s4 = s4->ts_prev; s4 != 0; s4 = s4->ts_prev) {
				if (s4->staff != staff)
					continue;
				if (s4->type == NOTE)
					break;
			}

			/* if no note, change the clef of the staff */
			if (s4 == 0) {
				if (clef_type == TREBLE) {
					clef_type = BASS;
					sy->staff[staff].clef.line = 4;
				} else {
					clef_type = TREBLE;
					sy->staff[staff].clef.line = 2;
				}
				sy->staff[staff].clef.type = clef_type;
				continue;
			}
		}

		/* no change possible if no insert point */
#if 1 /*fixme:test*/
		    else if (s3->time == s2->time)
#else
		if (s3 == 0 || s3 == s2)
#endif
			continue;

		/* insert a clef change */
		clef_type = clef_type == TREBLE ? BASS : TREBLE;
		insert_clef(s3, clef_type);
		s3->prev->staff = staff;
	}

	/* keep the starting clef of the next staff systems */
	while ((sy = sy->next) != 0) {
		sy->staff[staff].clef.type = clef_type;
		sy->staff[staff].clef.line = clef_type == TREBLE ? 2 : 4;
	}
}

/* -- insert a starting symbol into a voice -- */
static struct SYMBOL *sym_ins(struct SYMBOL *s, int type)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *new_s;

	new_s = (struct SYMBOL *) getarena(sizeof *new_s);
	memset(new_s, 0, sizeof *new_s);
	new_s->type = type;
	new_s->voice = s->voice;
	new_s->staff = s->staff;
	new_s->time = s->time;

	new_s->next = s;		/* voice linkage */
	if ((new_s->prev = s->prev) == 0) {
		p_voice = &voice_tb[s->voice];
		p_voice->sym = new_s;
	} else	new_s->prev->next = new_s;
	s->prev = new_s;

/*fixme:%%staves:more complex*/
	for (;;) {
		while (!(s->sflags & S_SEQST)) {
			if (s->type == STAVES)
				break;
			s = s->ts_prev;
		}
		if (s->type == STAVES) {
			s = s->ts_next;
			s->sflags |= S_SEQST;
			break;
		}
		if (s->ts_prev == 0 || s->ts_prev->type == type)
			break;
		switch (s->ts_prev->type) {
		case TIMESIG:
			if (type != CLEF && type != KEYSIG) {
				s = s->ts_prev;
				continue;
			}
			break;
		case KEYSIG:
			if (type != CLEF)
				break;
			/*fall thru*/
		case TUPLET:
		case GRACE:
			s = s->ts_prev;
			continue;
		}
		break;
	}
	new_s->ts_next = s;		/* vertical linkage */
	if ((new_s->ts_prev = s->ts_prev) == 0) {
		tsfirst = new_s;
		new_s->sflags |= S_SEQST;
	} else {
		new_s->ts_prev->ts_next = new_s;
		if (new_s->ts_prev->type != type
		    && new_s->ts_prev->type != STAVES)
			new_s->sflags |= S_SEQST;
	}
	s->ts_prev = new_s;
/*fixme:%%staves:should compute the spacing here*/
	return new_s;
}

/* -- set a clef in each voice and a key signature in each main voice -- */
/* this function is called only once per tune */
static void set_first_clefs(void)
{
	struct SYSTEM *sy;
	struct SYMBOL *s, *s2;
	struct key_s *curkey;
	int voice, staff;
	char need_clef[MAXVOICE];
	struct SYMBOL *key[MAXVOICE];

	memset(need_clef, 1, sizeof need_clef);
	memset(key, 0, sizeof key);
	sy = cursys;
	for (s = tsfirst; s != 0; s = s->ts_next) {
		switch (s->type) {
		case STAVES:
			staff = sy->nstaff;
			if (sy->next->nstaff < nstaff) {
				for (voice = 0; voice < MAXVOICE; voice++) {
					if (sy->voice[voice].staff > nstaff)
						need_clef[voice] = 1;
				}
			}
			sy = sy->next;
			continue;
		case KEYSIG:
			key[s->voice] = s;
			break;
		}
		voice = s->voice;
		if (!need_clef[voice])
			continue;
		need_clef[voice] = 0;
		if (s->type != CLEF
		    || s->as.u.clef.type < 0) {
			staff = sy->voice[voice].staff;
			s2 = sym_ins(s, CLEF);
			s2->u = 0;
			memcpy(&s2->as.u.clef,
				&sy->staff[staff].clef,
				sizeof s2->as.u.clef);
			if (sy->voice[voice].second) {
				s2->sflags &= ~S_SEQST;
				s2->sflags |= S_SECOND;
				s2->as.flags |= ABC_F_INVIS;
				s2->as.u.clef.invis = 1;
				need_clef[voice] = 0;
				continue;
			}
			if (s2->as.u.clef.invis)
				s2->as.flags |= ABC_F_INVIS;
			s2 = s;
		} else	s2 = s->next;
		if (s2->type == KEYSIG)
			continue;
		if (key[voice] == 0)
			curkey = &voice_tb[voice].key;
		else	curkey = &(key[voice]->as.u.key);
		if (curkey->sf == 0 && curkey->nacc == 0)
			continue;
		s2 = sym_ins(s2, KEYSIG);
		memcpy(&s2->as.u.key, curkey, sizeof s2->as.u.key);
		if (s2->as.u.key.bagpipe && s2->as.u.key.sf == 2)
			s2->u = 3;	/* K:Hp --> G natural */
	}
}

/* -- set the staff of the floating voices -- */
/* this function is called only once per tune */
static void set_float(void)
{
	struct VOICE_S *p_voice;
	int staff, staff_chg;
	struct SYMBOL *s, *s1;

	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		if (!p_voice->floating)
			continue;
		staff_chg = 0;
		staff = p_voice->staff;
		for (s = p_voice->sym; s != 0; s = s->next) {
			signed char up, down;

			if (s->type != NOTE) {
				if (staff_chg)
					s->staff++;
				continue;
			}
			if (!(s->sflags & S_FLOATING)) {
				staff_chg = 0;
				continue;
			}
			if (s->pits[0] >= 19) {		/* F */
				staff_chg = 0;
				continue;
			}
			if (s->pits[s->nhd] <= 12) {	/* F, */
				staff_chg = 1;
				s->staff++;
				continue;
			}
			up = 127;
			for (s1 = s->ts_prev; s1 != 0; s1 = s1->ts_prev) {
				if (s1->staff != staff
				    || s1->voice == s->voice)
					break;
#if 1
/*fixme:test again*/
				if (s1->type == NOTE)
#endif
				    if (s1->pits[0] < up)
					up = s1->pits[0];
			}
			if (up == 127) {
				if (staff_chg)
					s->staff++;
				continue;
			}
			if (s->pits[s->nhd] > up - 3) {
				staff_chg = 0;
				continue;
			}
			down = -127;
			for (s1 = s->ts_next; s1 != 0; s1 = s1->ts_next) {
				if (s1->staff != staff + 1
				    || s1->voice == s->voice)
					break;
#if 1
/*fixme:test again*/
				if (s1->type == NOTE)
#endif
				    if (s1->pits[s1->nhd] > down)
					down = s1->pits[s1->nhd];
			}
			if (down == -127) {
				if (staff_chg)
					s->staff++;
				continue;
			}
			if (s->pits[0] < down + 3) {
				staff_chg = 1;
				s->staff++;
				continue;
			}
			up -= s->pits[s->nhd];
			down = s->pits[0] - down;
			if (!staff_chg) {
				if (up < down + 3)
					continue;
				staff_chg = 1;
			} else {
				if (up < down - 3) {
					staff_chg = 0;
					continue;
				}
			}
			s->staff++;
		}
	}
}

/* -- set the x offset of the grace notes -- */
static float set_graceoffs(struct SYMBOL *s)
{
	struct SYMBOL *g, *next;
	int m;
	float xx, gspleft, gspinside, gspright;

	gspleft = (cfmt.gracespace >> 16) * 0.1;
	gspinside = ((cfmt.gracespace >> 8) & 0xff) * 0.1;
	gspright = (cfmt.gracespace & 0xff) * 0.1;
	xx = 0;
	g = s->grace;
	g->sflags |= S_WORD_ST;
	for ( ; ; g = g->next) {
		set_head_directions(g);
		for (m = g->nhd; m >= 0; m--) {
			if (g->as.u.note.accs[m]) {
				xx += 4;
				if (g->as.u.note.accs[m] & 0xf8)
					xx += 2;
				break;
			}
		}
		g->x = xx;

		if (g->nflags <= 0) {
			g->sflags |= S_WORD_ST;
			g->as.flags |= ABC_F_WORD_END;
		}
		next = g->next;
		if (next == 0) {
			g->as.flags |= ABC_F_WORD_END;
			break;
		}
		if (next->nflags <= 0)
			g->as.flags |= ABC_F_WORD_END;
		if (g->as.flags & ABC_F_WORD_END) {
			next->sflags |= S_WORD_ST;
			xx += gspinside / 4;
		}
		if (g->nflags <= 0)
			xx += gspinside / 4;
		if (g->y > next->y + 8)
			xx -= 1.6;
		xx += gspinside;
	}

	xx += gspleft + gspright;
	if ((next = s->next) != 0
	    && next->type == NOTE) {		/* if before a note */
		if (g->y >= (float) (3 * (next->pits[next->nhd] - 18)))
			xx -= 1;		/* above, a bit closer */
		else if ((g->sflags & S_WORD_ST)
			 && g->y < (float) (3 * (next->pits[0] - 18) - 7))
			xx += 2;	/* below with flag, a bit further */
	}

	/* return the whole width */
	return xx;
}

/* -- set the width of a symbol -- */
/* This routine sets the minimal left and right widths wl,wr
 * so that successive symbols are still separated when
 * no extra glue is put between them */
static void set_width(struct SYMBOL *s)
{
	struct SYMBOL *s2, *k;
	int i, m;
	float xx, w, wlnote, wlw;
	/* whistle space at start of line !! see /tw_head !! */
	static unsigned char pitwh[12] =
		{28, 54, 28, 54, 28, 28, 54, 28, 54, 28, 54, 28};

	switch (s->type) {
	case NOTE:
	case REST:

		/* set the note widths */
		switch (s->head) {
		case H_SQUARE:
			wlnote = 8;
			break;
		case H_OVAL:
			wlnote = 6;
			break;
		case H_EMPTY:
			wlnote = 5;
			break;
		default:
			wlnote = 4.5;
			break;
		}
		s->wr = wlnote;

		/* room for shifted heads and accidental signs */
		if (s->xmx > 0)
			s->wr += s->xmx + 4;
		s2 = s->prev;
		while (s2->type == TUPLET)
			s2 = s2->prev;
		switch (s2->type) {
		case BAR:
		case CLEF:
		case KEYSIG:
		case TIMESIG:
			wlnote += 3;
			break;
		}
		for (m = 0; m <= s->nhd; m++) {
			xx = s->shhd[m];
			if (xx < 0)
				AT_LEAST(wlnote, -xx + 5);
			if (s->as.u.note.accs[m]) {
				AT_LEAST(wlnote, s->shac[m]
					 + ((s->as.u.note.accs[m] & 0xf8)
					    ? 6.5 : 4.5));
			}
		}
		switch (s2->type) {
		case BAR:
		case CLEF:
		case KEYSIG:
		case TIMESIG:
			wlnote -= 3;
			break;
		}

		/* room for the decorations */
		if (s->as.u.note.dc.n > 0)
			wlnote += deco_width(s);

		/* space for flag if stem goes up on standalone note */
		if ((s->sflags & S_WORD_ST)
		    && (s->as.flags & ABC_F_WORD_END)
		    && s->stem > 0 && s->nflags > 0)
			AT_LEAST(s->wr, s->xmx + 12);

		/* leave room for dots and set their offset */
		if (s->dots > 0) {

			/* standalone with up-stem and flags */
			if (s->nflags > 0 && s->stem > 0
			    && s->xmx == 0 && s->doty == 0
			    && (s->sflags & S_WORD_ST)
			    && (s->as.flags & ABC_F_WORD_END)
			    && !(s->y % 6))
				s->xmx = DOTSHIFT;
			switch (s->head) {
			case H_SQUARE:
			case H_OVAL:
				s->xmx += 2;
				break;
			case H_EMPTY:
				s->xmx += 1;
				break;
			}
			AT_LEAST(s->wr, s->xmx + 12);
			if (s->dots >= 2)
				s->wr += 3.5 * (s->dots - 1);
		}

		wlw = wlnote;

		switch (s2->type) {
		case NOTE:		/* extra space when up stem - down stem */
			if (s2->stem > 0 && s->stem < 0)
				AT_LEAST(wlw, 7);

			/* make sure helper lines don't overlap */
			if ((s->y > 27 && s2->y > 27)
			    || (s->y < -3 && s2->y < -3))
				AT_LEAST(wlw, 6);

			/* have ties wide enough */
			if (s2->sflags & S_TI1)
				AT_LEAST(wlw, 14);
			break;
		case CLEF:		/* extra space at start of line */
			if ((s2->sflags & S_SECOND)
			    || s2->as.u.clef.type < 0 || s2->u)
				break;
			wlw += 8;
			break;
		case KEYSIG:
/*		case TIMESIG:	*/
			wlw += 4;
			break;
		}

		/* leave room for guitar chord */
		/* !! this sequence is tied to draw_gchord() !! */
		if (s->as.text != 0) {
			float lspc, rspc;
			char *p, *q, sep, antype;

			str_font(s->gcf);
			lspc = rspc = cwid(' ') * cfmt.font_tb[s->gcf].swfac;
			p = s->as.text;
			antype = '\0';
			sep = '\n';
			for (;;) {
				if (sep == '\n') {
					if (*p != '\0' && strchr("^_<>@", *p) != 0)
						antype = *p++;
					else	antype = '\0';
				}
				for (q = p; ; q++) {
					if (*q == '\\') {
						q++;
						if (*q == '\\' || *q == ';')
							continue;
					}
					if (*q == '\0' || *q == ';' || *q == '\n') {
						sep = *q;
						*q = '\0';
						break;
					}
				}
				w = tex_str(p);
				switch (antype) {
				case '^':		/* above */
				case '_':		/* below */
				default: {		/* default = above */
					float wl;

					wl = w * GCHPRE;
					if (wl > 8)
						wl = 8;
					if (wl > lspc)
						lspc = wl;
					w -= wl;
					if (w > rspc)
						rspc = w;
					break;
				    }
				case '<':		/* left */
					w += wlnote;
					if (w > lspc)
						lspc = w;
					break;
				case '>':		/* right */
					w += s->wr;
					if (w > rspc)
						rspc = w;
					break;
				case '@':		/* absolute */
					break;
				}
				if (sep == '\0')
					break;
				*q = sep;
				p = q + 1;
			}
/*fixme: pb when "<" only*/
			if (s2->as.text != 0)
				AT_LEAST(wlw, lspc);
/*fixme: pb when ">" only*/
			for (k = s->next; k != 0; k = k->next) {
				switch (k->type) {
				default:
					continue;
				case NOTE:
				case REST:
				case BAR:
					if (k->as.text != 0)
						AT_LEAST(s->wr, rspc);
					break;
				}
				break;
			}
		}

		/* leave room for vocals under note */
		/* related to draw_lyrics() */
		if (s->ly) {
			struct lyrics *ly = s->ly;
			struct lyl *lyl;
			float align = 0;

			if (voice_tb[s->voice].tabhead == 0)
			    for (i = 0; i < MAXLY; i++) {
				float swfac, shift;
				char *p;

				if ((lyl = ly->lyl[i]) == 0)
					continue;
				p = lyl->t;
				w = lyl->w;
				swfac = lyl->f->swfac;
				xx = w + 2 * cwid(' ') * swfac;
				if (isdigit((unsigned char) *p)
				    || p[1] == ':'
				    || p[1] == '(' || p[1] == ')') {
					float sz;

					if (p[1] == '(')
						sz = cwid((unsigned char) p[1]);
					else {
						sz = 0;
						while (*p != '\0') {
/*fixme: KO when '\ooo'*/
							if (*p == '\\') {
								p++;
								continue;
							}
							sz += cwid((unsigned char) *p);
							if (*p == ' ')
								break;
							p++;
						}
					}
					sz *= swfac;
					shift = (w - sz + 2 * cwid(' ') * swfac)
						* VOCPRE;
					if (shift > 20)
						shift = 20;
					shift += sz;
					if (isdigit((unsigned char) lyl->t[0])) {
						if (shift > align)
							align = shift;
					}
				} else if (*p == '\x02' || *p == '\x03')
					shift = 0;
				else {
					shift = xx * VOCPRE;
					if (shift > 20)
						shift = 20;
				}
				lyl->s = shift;
				AT_LEAST(wlw, shift);
				xx -= shift;
				shift = 2 * cwid(' ') * swfac;
				for (k = s->next; k != 0; k = k -> next) {
					switch (k->type) {
					case NOTE:
					case REST:
						if (k->ly == 0
						    || k->ly->lyl[i] == 0)
							xx -= 9;
						else if (k->ly->lyl[i]->t[0] == '\x02'
							 || k->ly->lyl[i]->t[0] == '\x03')
							xx -= shift;
						else	break;
						if (xx <= 0)
							break;
						continue;
					case CLEF:
					case TIMESIG:
					case KEYSIG:
						xx -= 10;
						continue;
					case TEMPO:
					case PART:
					case TUPLET:
						continue;
					default:
						xx -= 5;
						break;
					}
					break;
				}
				if (xx > s->wr)
					s->wr = xx;
			} else {
			    for (i = 0; i < MAXLY; i++) {
				if ((lyl = ly->lyl[i]) == 0)
					continue;
				lyl->s = 0;
			    }
			}
			if (align > 0) {
				for (i = 0; i < MAXLY; i++) {
					if ((lyl = ly->lyl[i]) == 0)
						continue;
					if (isdigit((unsigned char) lyl->t[0]))
						lyl->s = align;
				}
			}
		}

		if (s->dur == 0) {		/* space ('y') */
			if (s->as.u.note.lens[1] < 0)
				xx = 10;
			else	xx = (float) (s->as.u.note.lens[1] / 2);
			s->wl = s->wr = xx;
/*new fixme
			s->space = xx * 1.4; */
		}

#if 0
/*new fixme*/
		/* reduce right space when not followed by a note */
		for (k = s->next; k != 0; k = k->next) {
			switch (k->type) {
			case PART:
			case TEMPO:
				continue;
			default:
				s->pr *= 0.8;
				break;
			case NOTE:
			case REST:
			case TUPLET:
				break;
			}
			break;
		}

		/* squeeze notes a bit if big jump in pitch */
		if (s->type == NOTE
		    && s2->type == NOTE) {
			int dy;
			float fac;

			dy = s->y - s2->y;
			if (dy < 0)
				dy =- dy;
			fac = 1. - 0.01 * dy;
			if (fac < 0.9)
				fac = 0.9;
			s2->pr *= fac;

			/* stretch / shrink when opposite stem directions */
			if (s2->stem > 0 && s->stem < 0)
				s2->pr *= 1.1;
			else if (s2->stem < 0 && s->stem > 0)
				s2->pr *= 0.9;
		}
#endif
		/* if preceeded by a grace note sequence, adjust */
		if (s2->type == GRACE)
			s->wl = wlnote - 4.5;
		else	s->wl = wlw;
		break;
	case BAR:
		{
			int bar_type;

			w = 5;
			bar_type = s->as.u.bar.type;
			switch (bar_type) {
			case B_OBRA:
			case B_CBRA:
			case (B_OBRA << 4) + B_CBRA:
				w = 0;		/* invisible */
				break;
			case (B_BAR << 4) + B_COL:
			case (B_COL << 4) + B_BAR:
				w += 3 + 3 + 5;
				break;
			case (B_COL << 4) + B_COL:
				w += 5 + 3 + 3 + 3 + 5;
				break;
			default:
				for (;;) {
					switch (bar_type & 0x07) {
					case B_OBRA:
					case B_CBRA:
						w += 3;
						break;
					case B_COL:
						w += 2;
					}
					bar_type >>= 4;
					if (bar_type == 0)
						break;
					w += 3;
				}
				break;
			}
		}
		if (w != 0) {
			s->wl = w;
/*			s->space = w + 5; */
			if (s->next != 0
			    && s->next->type != TIMESIG) {
				s->wr = 8;
/*				s->pr = 8; */
			} else {
				s->wr = 5;
/*				s->pr = 5.5; */
			}
			s->shhd[0] = (w - 5) * -0.5;
		}
		if (s->as.u.bar.dc.n > 0)
			s->wl += deco_width(s);

		/* have room for the repeat numbers / guitar chord */
		if (s->as.text == 0)
			break;
		{
			int ft;
			struct FONTSPEC *f;

			ft = s->as.u.bar.repeat_bar ? REPEATFONT : s->gcf;
			str_font(ft);
			f = &cfmt.font_tb[ft];
			xx = tex_str(s->as.text) + cwid(' ') * f->swfac * 1.5;
		}
		if (!s->as.u.bar.repeat_bar) {
			if (s->prev->as.text != 0) {
				float spc;

				spc = xx * GCHPRE;
				if (spc > 8)
					spc = 8;
				AT_LEAST(s->wl, spc);
/*				s->space = s->wl; */
				xx -= spc;
			}
		}
		for (s2 = s->next; s2 != 0; s2 = s2->next) {
			switch (s2->type) {
			case PART:
			case TEMPO:
			case TUPLET:
			case GRACE:
				continue;
			case NOTE:
			case REST:
				if (s2->as.text != 0) {
					AT_LEAST(s->wr, xx);
/*new fixme
					s->pr = s->wr; */
				}
				break;
			default:
				break;
			}
			break;
		}
		break;
	case CLEF:
		if (s->as.u.clef.type < 0)
			break;
		if (!(s->as.flags & ABC_F_INVIS)) {
			s->wl = 12;
			s->wr = s->u ? 10 : 12;
		} else if (!s->u) {
			s->wl = 6;
			s->wr = 6;
		}
		break;
	case KEYSIG: {
		int n1, n2, esp;

		s->wl = 3;
		esp = 4;
		if (s->as.u.key.nacc == 0) {
			n1 = s->as.u.key.sf;	/* new key sig */
			n2 = s->u;		/* old key */
			if (n1 * n2 >= 0) {	/* if no natural */
				if (n1 < 0)
					n1 = -n1;
				if (n2 < 0)
					n2 = -n2;
				if (n2 > n1)
					n1 = n2;
			} else {
				n1 -= n2;
				if (n1 < 0)
					n1 = -n1;
				esp += 3;	/* see extra space in draw_keysig() */
			}
		} else {
			int last_acc;

			n1 = s->as.u.key.nacc;
			last_acc = s->as.u.key.accs[0];
			for (i = 1; i < n1; i++) {
				if (s->as.u.key.accs[i] != last_acc) {
					last_acc = s->as.u.key.accs[i];
					esp += 3;
				}
			}
		}
		s->wr = (float) (5 * n1 + esp);
		break;
	}
	case TIMESIG:
		/* !!tied to draw_timesig()!! */
		w = 0;
		for (i = 0; i < s->as.u.meter.nmeter; i++) {
			int l;

			l = sizeof s->as.u.meter.meter[i].top;
			if (s->as.u.meter.meter[i].top[l - 1] == '\0') {
				l = strlen(s->as.u.meter.meter[i].top);
				if (s->as.u.meter.meter[i].top[1] == '|'
				    || s->as.u.meter.meter[i].top[1] == '.')
					l--;		/* 'C|' */
			}
			if (s->as.u.meter.meter[i].bot[0] != '\0') {
				int l2;

				l2 = sizeof s->as.u.meter.meter[i].bot;
				if (s->as.u.meter.meter[i].bot[l2 - 1] == '\0')
					l2 = strlen(s->as.u.meter.meter[i].bot);
				if (l2 > l)
					l = l2;
			}
			w += 6.5 * l;
		}
		s->wl = w;
		s->wr = w + 7;
		break;
	case MREST:
		s->wl = 40 / 2 + 16;
		s->wr = 40 / 2 + 16;
		break;
	case GRACE:
		s->wl = set_graceoffs(s);
		break;
	case FMTCHG:
		if (s->u != STBRK || (s->wl = s->xmx) == 0)
			break;		/* no space */
		if (s->next == 0 || s->next->type != CLEF)
			s->wr = 8;
		else {
			s->wr = 2;
			s->next->u = 0;	/* big clef */
		}
/*new fixme
		s->space = s->wl + 4;
		s->pr = s->wr + 4; */
		break;
	case WHISTLE:
		wlw = 0;		/* avoid clash of note with TW header */
		for (s2 = tsfirst; s2 != s; s2 = s2->ts_next) {
			if (s2->shrink != 0)
				wlw += s2->shrink;
		}
		for (i = 0; i < nwhistle; i++)
			if (whistle_tb[i].voice == s->voice)
				break;
		w = pitwh[whistle_tb[i].pitch % 12];
		if (w > wlw)
			s->wl = w - wlw;
		break;
	case TEMPO:
	case STAVES:
	case PART:
	case TUPLET:		/* no space */
		break;
	default:
		bug("Cannot set width for symbol", 1);
	}
}

/* -- set the natural space -- */
static float set_space(struct SYMBOL *s)
{
	struct SYMBOL *s2;
	int i, len, l, stemdir, prev_time;
	float space;

	prev_time = s->ts_prev == 0 ? s->time : s->ts_prev->time;
	len = s->time - prev_time;		/* time skip */
	if (len == 0) {
		switch (s->type) {
		case MREST:
			return s->wl + 16;
/*fixme:do same thing at start of line*/
		case NOTE:
		case REST:
		case TUPLET:
			if (s->ts_prev->type == BAR) {
				i = 2;
				if (s->nflags < -2)
					i = 0;
				return space_tb[i];
			}
		}
		return 0;
	}
	if (s->prev != 0 && s->prev->type == MREST)
		return s->prev->wr + 16;

	if (s->type == CLEF)
		return 0;
	if (len >= CROTCHET) {
		if (len < MINIM)
			i = 5;
		else if (len < SEMIBREVE)
			i = 6;
		else if (len < BREVE)
			i = 7;
		else if (len < BREVE * 2)
			i = 8;
		else	i = 9;
	} else {
		if (len >= QUAVER)
			i = 4;
		else if (len >= SEMIQUAVER)
			i = 3;
		else if (len >= SEMIQUAVER / 2)
			i = 2;
		else if (len >= SEMIQUAVER / 4)
			i = 1;
		else	i = 0;
	}
	l = len - ((SEMIQUAVER / 8) << i);
	space = space_tb[i];
	if (l != 0) {
		if (l < 0)
			space = space_tb[0] * len / (SEMIQUAVER / 8);
		else {
			if (i >= 9)
				i = 8;
			space += (space_tb[i + 1] - space_tb[i])
				* l / len;
		}
	}
	if (s->dur == 0) {
		if (s->type == BAR) {
			if (s->as.u.bar.type & 0xf)
				space *= 0.9;	/* complex bar */
			else	space *= 0.8;
		}
		return space;
	}

	/* reduce spacing within a beam */
	if ((s->sflags & S_WORD_ST) == 0)
		space *= fnnp;

	/* decrease/increase spacing if stems in opposite directions */
/*fixme:to be done later, after x computed in sym_glue*/
	if (s->type == NOTE && s->nflags >= 0) {
		stemdir = s->stem;
		for (s2 = s->ts_prev;
		     s2 != 0 && s2->time == prev_time;
		     s2 = s2->ts_prev) {
			if (s2->nflags < 0 || s2->stem == stemdir) {
				stemdir = 0;
				break;
			}
		}
		if (stemdir != 0) {
			for (s2 = s->ts_next;
			     s2 != 0 && s2->time == s->time;
			     s2 = s2->ts_next) {
				if (s2->nflags < 0 || s2->stem != stemdir) {
					stemdir = 0;
					break;
				}
			}
			if (stemdir == 1)		/* down - up */
				space *= 0.9;
			else if (stemdir == -1)		/* up - down */
				space *= 1.11;
		}
	}
	return space;
}

/* -- set the width and space of all symbols -- */
/* this function is called once for the whole tune
 * then, once per music line up to the first sequence */
static void set_allsymwidth(void)
{
	struct SYMBOL *s, *s2, *s3;
	float new_val, shrink, space;

	/* loop on the symbol sequences */
	s = tsfirst;
	for (;;) {
		s2 = s;
		shrink = space = 0;
		do {

			/* set the minimum space before and after the symbol */
			set_width(s2);

			/* calculate the minimum space before the symbol */
/*fixme:use tables of x left and x right indexed by y*/
			if (s2->type != CLEF || s2->prev == 0)
				new_val = s2->wl;
			else	new_val = 0;	/* no space for clef changes when multi-voices */
			for (s3 = s->ts_prev; s3 != 0; s3 = s3->ts_prev) {
				if (s3->staff == s2->staff
				    && (!(s3->as.flags & ABC_F_INVIS)
					|| s3->voice == s2->voice)
				    && new_val <= s3->wr + s2->wl) {
					if (s2->ymn <= s3->ymx
					     && s2->ymx >= s3->ymn)
						new_val = s3->wr + s2->wl;
					else if (s3->wr > s2->wl) {
						if (new_val < s3->wr)
							new_val = s3->wr;
					}
				}
				if (s3->shrink != 0)
					break;
			}
			if (shrink < new_val)
				shrink = new_val;
			new_val = set_space(s2);
			if (space < new_val)
				space = new_val;
			if ((s2 = s2->ts_next) == 0)
				break;
		} while (!(s2->sflags & S_SEQST));
		s->shrink = shrink;	/* set the spaces at start of sequence */
		s->space = space;
		if ((s = s2) == 0)
			break;
	}
}

/* -- set the repeat sequences / measures -- */
/* if !xset, check and change the sequence repeat,
 *	and just check the measure repeat */
/* if xset, check and change the measure repeat */
static struct SYMBOL *set_repeat(struct SYMBOL *s,
				 int xset)
{
	struct SYMBOL *s2, *s3;
	int i, j, n, dur, staff, voice;

	staff = s->staff;
	voice = s->voice;

	/* treat the sequence repeat */
	if ((n = s->doty) < 0) {		/* number of notes / measures */
		n = -n;
		i = s->nohdix * n;		/* repeat number */
		for (s2 = s->ts_next; s2 != 0; s2 = s2->ts_next) {
			if (s2->staff != staff)
				continue;
			if (s2->voice != voice
			    && !(s2->as.flags & ABC_F_INVIS))
				goto delrep;
			if (s2->dur == 0)
				continue;
			if (--i == 0)
				break;
		}
		if (s2 == 0
		    || s2->next == 0)		/* should have some measure bar */
			goto delrep;
		s3 = s;
		for (j = s->nohdix; --j >= 0; ) {
			s2 = s3->next;
			i = n;			/* number of notes/rests */
			dur = s3->dur;
			if (dur != 0)
				i--;
			for (;;) {
				if (s2->dur != 0) {
					dur += s2->dur;
					i--;
				}
				s2 = s2->next;
				delsym(s2->prev);
				if (i == 0)
					break;
			}
			s3->type = REST;
			s3->dur = s3->as.u.note.lens[0] = dur;
			s3->sflags |= S_REPEAT | S_WORD_ST;
			s3->doty = -1;
			set_width(s3);
			if (s3->shrink != 0)
				s3->space = set_space(s3);
			s3->head = H_SQUARE;
			s3 = s2;
		}
		return s;
	}

	/* first check of the measure repeat */
	if (!xset) {
		i = s->nohdix * n;		/* repeat number */
		for (s2 = s->ts_next; s2 != 0; s2 = s2->ts_next) {
			if (s2->staff != staff)
				continue;
			if (s2->voice != voice) {
				if (!(s2->as.flags & ABC_F_INVIS))
					goto delrep;
			} else if (s2->type == BAR) {
				if (--i <= 0)
					break;
			}
		}
		if (s2 == 0)
			goto delrep;
		return s;			/* OK, treat it later */
	}

	/* second check and replace */
	i = s->nohdix * n;			/* check if NL after */
	for (s2 = s->next; s2 != 0; s2 = s2->next) {
		if (s2->sflags & S_NL)
			goto delrep;
		if (s2->type == BAR) {
			if (--i <= 0)
				break;
		}
	}

	s2 = s->prev->prev;			/* check if NL before */
	if (n == 2) {				/* if repeat 2 measures */
		for (; s2 != 0; s2 = s2->prev) {
			if (s2->type == BAR) {
				s2 = s2->prev;
				break;
			}
			if (s2->sflags & S_NL)
				goto delrep;
		}
	}
	dur = 0;
	for (; s2 != 0; s2 = s2->prev) {
		if (s2->type == BAR)
			break;
		if (s2->sflags & S_NL)
			goto delrep;
		dur += s2->dur;
	}
	if (dur == 0)
		goto delrep;	/* the previous measure is not in the music line */

	if (n == 2) {			/* repeat 2 measures (one time) */
		s3 = s;
		s2 = s->next;
		for (;;) {
			if (s2->type == BAR)
				break;
			s2 = s2->next;
			delsym(s2->prev);
		}
		s3->type = REST;
		s3->dur = s3->as.u.note.lens[0] = dur;
		s3->as.flags = ABC_F_INVIS;
		set_width(s3);
		if (s3->shrink != 0)
			s3->space = set_space(s3);
		s2->as.u.bar.len = 2;
		if (s2->shrink != 0)
			s2->space = set_space(s2);
		s3 = s2->next;
		s2 = s3->next;
		for (;;) {
			if (s2->type == BAR || s2->type == CLEF)
				break;
			s2 = s2->next;
			delsym(s2->prev);
		}
		s3->type = REST;
		s3->dur = s3->as.u.note.lens[0] = dur;
		s3->as.flags = ABC_F_INVIS;
		set_width(s3);
		if (s3->shrink != 0)
			s3->space = set_space(s3);
		if (s2->shrink != 0)
			s2->space = set_space(s2);
		return s;
	}

	/* repeat 1 measure */
	s3 = s;
	for (j = s->nohdix; --j >= 0; ) {
		s2 = s3->next;
		for (;;) {
			if (s2->type == BAR || s2->type == CLEF)
				break;
			s2 = s2->next;
			delsym(s2->prev);
		}
		s3->type = REST;
		s3->dur = s3->as.u.note.lens[0] = dur;
		s3->sflags = S_REPEAT | S_WORD_ST;
		set_width(s3);
		if (s3->shrink != 0)
			s3->space = set_space(s3);
		if (s2->shrink != 0)
			s2->space = set_space(s2);
		if (s->nohdix == 1) {
			s3->doty = 1;
			break;
		}
		s3->doty = s->nohdix - j + 1;	/* number to print above the repeat rest */
		s3 = s2->next;
	}
	return s;

delrep:					/* remove the %%repeat */
	s3 = s->ts_next;
	delsym(s);
	return s3;
}

/* -- set the beginning of a new line -- */
static struct SYMBOL *set_nl(struct SYMBOL *s)
{
	struct SYMBOL *s2;
	int time, done;

	/* if normal symbol, cut here */
	switch (s->type) {
	case CLEF:
	case KEYSIG:
	case TIMESIG:
	case FMTCHG:
	case BAR:
		break;
	case GRACE:
		if (cfmt.continueall && s->next != 0 && s->next->type != NOTE)
			break;
		/* fall thru */
	default:
		time = s->time + s->dur;
		for (s = s->ts_next; s != 0; s = s->ts_next) {
			if (s->shrink != 0
			    && s->time >= time) {
				s->sflags |= S_NL;
				break;
			}
		}
		return s;
	}

	/* get back to handle the staff breaks at end of line */
	for (; s != 0; s = s->ts_prev) {
		if (s->shrink == 0)
			continue;
		switch (s->type) {
		case CLEF:
		case KEYSIG:
		case TIMESIG:
		case FMTCHG:
			continue;
		default:
			break;
		}
		break;
	}
	done = 0;
	for (; s != 0; s = s->ts_next) {
		if (s->shrink == 0)
			continue;
		if (done < 0)
			break;
		switch (s->type) {
		case BAR:
			if (done)
				break;
			done = 1;
			continue;
		case FMTCHG:
			if (s->u != STBRK)	/* not staff break */
				break;
			if (s->doty == 0) {	/* if not forced */
				s2 = s->ts_prev;
				delsym(s);	/* remove */
				s = s2;
				continue;
			}
			done = -1;	/* keep the next symbols on the next line */
			continue;
		case TIMESIG:
			if (!cfmt.timewarn)
				break;
			continue;
		case CLEF:
			if (done)
				break;
		case KEYSIG:
			continue;
		default:
			if (!done && s->prev->type == GRACE)
				continue;
			break;
		}
		break;
	}
	if (s != 0)
		s->sflags |= S_NL;
	return s;
}

/* -- search where to cut the lines according to the staff width -- */
static struct SYMBOL *set_lines(struct SYMBOL *first,	/* first symbol */
				struct SYMBOL *last,	/* last symbol / 0 */
				float lwidth, float indent)
{
	struct SYMBOL *s, *s2;
	float x, xline, wwidth, x2, shrink, space;
	int nlines;

	/* calculate the whole size of the tune */
	wwidth = indent;
	for (s = first; s != last; s = s->ts_next) {
		if ((shrink = s->shrink) == 0)
			continue;
		if ((space = s->space) < shrink)
			wwidth += shrink;
		else	wwidth += shrink * cfmt.maxshrink
				+ space * (1 - cfmt.maxshrink);
	}

	/* loop on cutting the tune into music lines */
	s = first;
	x2 = 0;
	for (;;) {
		nlines = wwidth / lwidth + 0.999;
		if (nlines <= 1) {
			if (last != 0)
				set_nl(last);
			return 0;
		}
		xline = wwidth / nlines;
		x = indent;
		s2 = 0;
		for ( ; s != last; s = s->ts_next) {
			if ((shrink = s->shrink) == 0)
				continue;
			if ((space = s->space) < shrink)
				x += shrink;
			else	x += shrink * cfmt.maxshrink
					+ space * (1 - cfmt.maxshrink);
			if (s->type == BAR) {
				s2 = s;
				x2 = x;
			}
			if (x > xline) {
				if (s->next != 0 && s->next->type == BAR
				    && s->next->shrink != 0) {
					s2 = s->next;
					x2 = x;
				}
				break;
			}
		}
		if (s2 != 0 && x2 > xline - 200) /* go back to the previous bar */
			s = s2;
		s = set_nl(s);
		if (s == 0
		    || (last != 0 && s->time >= last->time))
			break;
		wwidth -= indent;
		for (s2 = first; s2 != s; s2 = s2->ts_next) {
			if ((shrink = s2->shrink) == 0)
				continue;
			if ((space = s2->space) < shrink)
				wwidth -= shrink;
			else	wwidth -= shrink * cfmt.maxshrink
					+ space * (1 - cfmt.maxshrink);
		}
		indent = s->shrink * -0.8; /* don't count part of the width of the next symbol */
		first = s;
	}
	return s;
}

/* -- cut the tune into music lines -- */
static void cut_tune(float lwidth, float indent)
{
	struct SYMBOL *s, *s2;
	int i, bar_time, wmeasure;
	float xmin;

	/* adjust the line width according to the starting clef
	 * and key signature */
/*fixme: may change in the tune*/
	for (s = tsfirst; ; s = s->ts_next) {
		if (s->shrink == 0)
			continue;
		if (s->type != CLEF && s->type != KEYSIG)
			break;
		lwidth -= s->shrink;
	}
	if (cfmt.continueall) {
		set_lines(s, 0, lwidth, indent);
		return;
	}

	/* if asked, count the measures and set the EOLNs */
	if ((i = cfmt.barsperstaff) != 0) {
		wmeasure = first_voice->meter.wmeasure;
		bar_time = s->time + wmeasure;
		s2 = s;
		for ( ; s != 0; s = s->ts_next) {
			switch (s->type) {
			case TIMESIG:
				wmeasure = s->as.u.meter.wmeasure;
				bar_time = s->time + wmeasure;
			default:
				continue;
			case BAR:
				break;
			}
			if (s->time < bar_time)
				continue;	/* incomplete measure */
			bar_time = s->time + wmeasure;
			if (--i > 0)
				continue;
			s->sflags |= S_EOLN;
			i = cfmt.barsperstaff;
		}
		s = s2;
	}

	/* cut at explicit end of line, checking the line width */
	xmin = indent;
	s2 = s;
	for ( ; s != 0; s = s->ts_next) {
		if (s->sflags & S_EOLN) {
			s->sflags &= ~S_EOLN;
			s = s2 = set_nl(s);
			if (s == 0)
				break;
			xmin = s->shrink;
			indent = 0;
			continue;
		}
		if (s->shrink == 0)
			continue;
		xmin += s->shrink;
		if (xmin <= lwidth)
			continue;

		/* line overfull */
		error(0, s, "Line overfull (%.0fpt of %.0fpt)",
			xmin, lwidth);
		for (s = s->ts_next; s != 0; s = s->ts_next) {
			if (s->sflags & S_EOLN)
				break;
		}
		s = s2 = set_lines(s2, s, lwidth, indent);
		if (s == 0)
			break;
		xmin = s->shrink;
		indent = 0;
	}
}

/* -- set the y values of some symbols -- */
static void set_yval(struct SYMBOL *s)
{
	switch (s->type) {
	case CLEF:
		if ((s->sflags & S_SECOND)
		    || (s->as.flags & ABC_F_INVIS)
		    || s->as.u.clef.type < 0) {
			s->ymx = s->ymn = 12;
			break;
		}
		switch (s->as.u.clef.type) {
		default:			/* treble / perc */
			s->y = -2 * 6;
			s->ymx = 24 + 12;
			s->ymn = -12;
			break;
		case ALTO:
			s->y = -3 * 6;
			s->ymx = 24 + 5;
			s->ymn = -4;
			break;
		case BASS:
			s->y = -4 * 6;
			s->ymx = 24 + 5;
			s->ymn = -3;
			break;
		}
		s->y += s->as.u.clef.line * 6;
		if (s->y > 0)
			s->ymx += s->y;
		else if (s->y < 0)
			s->ymn += s->y;
		if (s->as.u.clef.octave > 0)
			s->ymx += 12;
		else if (s->as.u.clef.octave < 0)
			s->ymn -= 12;
		break;
	default:
		s->ymx = 24 + 2;
		s->ymn = -2;
		break;
	}
}

/* -- set the pitch of the notes according to the clefs -- */
/* also set the vertical offset of the symbols */
/* it supposes that the first symbol of each voice is the clef */
/* this function is called only once per tune */
static void set_pitch(void)
{
	struct SYMBOL *s;
	int staff;
	char staff_clef[MAXSTAFF];

	for (s = tsfirst; s != 0; s = s->ts_next) {
		struct SYMBOL *g;
		int delta, np, m, pav;

		if (s->type == FMTCHG && s->u == REPEAT)
			s = set_repeat(s, 0);
		staff = s->staff;
		switch (s->type) {
		case CLEF:
			if ((s->sflags & S_SECOND)
			    || s->as.u.clef.type < 0) {
/*fixme:%%staves:can this happen?*/
				if (s->prev == 0)
					break;
				g = s->ts_prev;
				delsym(s);
				s = g;
				break;
			}
			set_yval(s);
			switch (s->as.u.clef.type) {
			default:		/* treble / perc */
				delta = 0 - 2 * 2;
				break;
			case ALTO:
				delta = 6 - 3 * 2;
				break;
			case BASS:
				delta = 12 - 4 * 2;
				break;
			}
			staff_clef[staff] = delta + s->as.u.clef.line * 2;
			break;
		default:
			set_yval(s);
			if ((g = s->grace) == 0)
				break;
			for (; g != 0; g = g->next) {
				delta = staff_clef[g->staff];
				if (delta != 0) {
					for (m = g->nhd; m >= 0; m--)
						g->pits[m] += delta;
				}
				g->ymn = 3 * (g->pits[0] - 18) - 2;
				g->ymx = 3 * (g->pits[g->nhd] - 18) + 2;
			}
			break;
		case MREST:
			s->ymx = 24 + 15;
			s->ymn = -2;
			break;
		case REST:
			s->y = 12;
			s->ymx = 12 + 8;
			s->ymn = 12 - 8;
			break;
		case NOTE:
			np = s->nhd;
			delta = staff_clef[staff];
			if (delta != 0) {
				for (m = np; m >= 0; m--)
					s->pits[m] += delta;
			}
			pav = 0;
			for (m = np; m >= 0; m--)
				pav += s->pits[m];
			s->yav = 3 * pav / (np + 1) - 3 * 18;
			s->ymx = 3 * (s->pits[np] - 18) + 4;
			s->ymn = 3 * (s->pits[0] - 18) - 4;
			break;
		}
	}
}

/* -- set the stem direction when multi-voices -- */
/* and adjust the vertical offset of the rests */
/* this function is called only once per tune */
static void set_multi(void)
{
	struct SYSTEM *sy;
	struct SYMBOL *s, *t;
	int i, j, staff, nst, rvoice, voice;
	struct {
		int nvoice;
		struct {
			int voice;
			short ymn;
			short ymx;
		} st[4];		/* (no more than 4 voices per staff) */
	} stb[MAXSTAFF];
	struct {
		signed char st1, st2;	/* (a voice cannot be on more than 2 staves) */
	} vtb[MAXVOICE];

	s = tsfirst;
	sy = cursys;
	nst = sy->nstaff;
	while (s != 0) {
		for (staff = nst; staff >= 0; staff--) {
			stb[staff].nvoice = -1;
			for (i = 4; --i >= 0; ) {
				stb[staff].st[i].voice = -1;
				stb[staff].st[i].ymx = 0;
				stb[staff].st[i].ymn = 24;
			}
		}
		for (i = 0; i < MAXVOICE; i++)
			vtb[i].st1 = vtb[i].st2 = -1;

		/* go to the next bar and get the max/min offsets */
/*fixme: the stem height is not calculated yet*/
		for (t = s;
		     t != 0 && t->type != BAR;
		     t = t->ts_next) {
			if (t->dur == 0		/* not a note or a rest */
			    || (t->as.flags & ABC_F_INVIS)) {
				if (t->type == STAVES) {
					sy = sy->next;
					for (staff = nst + 1; staff <= sy->nstaff; staff++) {
						stb[staff].nvoice = -1;
						for (i = 4; --i >= 0; ) {
							stb[staff].st[i].voice = -1;
							stb[staff].st[i].ymx = 0;
							stb[staff].st[i].ymn = 24;
						}
					}
					nst = sy->nstaff;
				}
				continue;
			}
			staff = t->staff;
#if 1
/*fixme:test*/
if (staff > nst) {
	printf("set_multi(): bad staff number\n");
}
#endif
			voice = t->voice;
			if (vtb[voice].st1 < 0)
				vtb[voice].st1 = staff;
			else if (vtb[voice].st1 == staff)
				;
			else {
				if (staff > vtb[voice].st1) {
					if (staff > vtb[voice].st2)
						vtb[voice].st2 = staff;
				} else {
					if (vtb[voice].st1 > vtb[voice].st2)
						vtb[voice].st2 = vtb[voice].st1;
					vtb[voice].st1 = staff;
				}
			}
			rvoice = sy->voice[voice].range;
			for (i = stb[staff].nvoice; i >= 0; i--) {
				if (stb[staff].st[i].voice == rvoice)
					break;
			}
			if (i < 0) {
				if (++stb[staff].nvoice >= 4)
					bug("Too many voices per staff", 1);
				for (i = 0; i < stb[staff].nvoice; i++) {
					if (rvoice < stb[staff].st[i].voice) {
						memmove(&stb[staff].st[i + 1],
							&stb[staff].st[i],
							sizeof stb[staff].st[i]
								* (stb[staff].nvoice - i));
						stb[staff].st[i].ymx = 0;
						stb[staff].st[i].ymn = 24;
						break;
					}
				}
				stb[staff].st[i].voice = rvoice;
			}
			if (t->type != NOTE)
				continue;
			if (t->ymx > stb[staff].st[i].ymx)
				stb[staff].st[i].ymx = t->ymx;
			if (t->ymn < stb[staff].st[i].ymn)
				stb[staff].st[i].ymn = t->ymn;
			if (t->sflags & S_XSTEM) {
				if (t->ts_prev->staff != staff - 1
				    || t->ts_prev->type != NOTE) {
					error(1, s, "Bad +xstem+");
					t->sflags &= ~S_XSTEM;
/*fixme:nflags KO*/
				} else {
					t->ts_prev->multi = 1;
					t->multi = 1;
					t->as.flags |= ABC_F_STEMLESS;
				}
			}
		}

		for ( ;
		     s != 0 && s->type != BAR;
		     s = s->ts_next) {
			int us, ls, not_alone, y;

			if (s->dur == 0)	/* if not note or rest */
				continue;
			staff = s->staff;
			voice = s->voice;
			if (s->multi == 0 && vtb[voice].st2 >= 0) {
				if (staff == vtb[voice].st1)
					s->multi = -1;
				else if (staff == vtb[voice].st2)
					s->multi = 1;
			}
			if (stb[staff].nvoice <= 0) { /* voice alone on the staff */
				if (s->multi != 0)
					continue;
/*fixme:could be done in set_float()*/
				if (s->sflags & S_FLOATING) {
					if (staff == voice_tb[voice].staff)
						s->multi = -1;
					else	s->multi = 1;
				}
				continue;
			}
			rvoice = sy->voice[voice].range;
			for (i = stb[staff].nvoice; i >= 0; i--) {
				if (stb[staff].st[i].voice == rvoice)
					break;
			}
			if (i < 0)
				continue;		/* voice ignored */
			if (s->multi == 0) {
				if (i == stb[staff].nvoice)
					s->multi = -1;	/* last voice */
				else {
					s->multi = 1;	/* first voice(s) */

					/* if 3 voices, and vertical space enough,
					 * have stems down for the middle voice */
					if (i != 0
					    && i + 1 == stb[staff].nvoice) {
						if (stb[staff].st[i].ymn - cfmt.stemheight
						    > stb[staff].st[i + 1].ymx)
							s->multi = -1;

						/* special case for unisson */
						if (s->ts_prev->time == s->time
						    && s->ts_prev->staff == s->staff
						    && s->pits[s->nhd] == s->ts_prev->pits[0]
						    && (s->sflags & S_WORD_ST)
						    && (s->as.flags & ABC_F_WORD_END)
						    && ((t = s->ts_next) == 0
							|| t->staff != s->staff
							|| t->time != s->time))
							s->multi = -1;
					}
				}
			}
			if (s->type != REST || s->dur == 0)
				continue;
/*fixme:do this later: the stem heights are not calculated */

			/* if one visible rest and only invisible ones
			 * at the same time on the same staff,
			 * set as if one rest only */
			if (i == 0) {		/* check the 1st rest only */
				not_alone = stb[staff].nvoice + 32;
				for (t = s; t != 0; t = t->ts_next) {
					if (t->staff != staff
					    || t->time != s->time)
						break;
					if (t->type != REST
					    || t->dur < s->dur) {
						not_alone = -1;
						break;
					}
					if (!(t->as.flags & ABC_F_INVIS)) {
						not_alone -= 32;
						continue;
					}
					rvoice = sy->voice[t->voice].range;
					for (j = stb[staff].nvoice; j >= 0; j--) {
						if (stb[staff].st[j].voice == rvoice) {
							not_alone--;
							break;
						}
					}
				}
				if (not_alone == 0) {
					if (t == 0)
						break;
					s = t->ts_prev;	/* skip the rests */
					continue;
				}
			}
			if (s->as.flags & ABC_F_INVIS)
				continue;

			/* set the rest vertical offset */
			us = rest_sp[C_XFLAGS - s->nflags].u;
			ls = rest_sp[C_XFLAGS - s->nflags].l;

			if (i == 0) {			/* first voice */
				if (stb[staff].st[0].ymx < stb[staff].st[0].ymn
				    || stb[staff].st[0].ymx >= stb[staff].st[1].ymn) {
					y = stb[staff].st[1].ymx;
					for (j = 2; j <= stb[staff].nvoice; j++) {
						if (y < stb[staff].st[j].ymx)
							y = stb[staff].st[j].ymx;
					}
					s->y = (y + ls + 3 + 12) / 6 * 6 - 12;
					if (s->y < 12)
						s->y = 12;
				} else {		/* voices inverted */
					y = (stb[staff].st[0].ymx
						+ stb[staff].st[0].ymn) / 2;
					s->y = (y - us + 24) / 6 * 6 - 24;
					if (s->y > 12)
						s->y = 12;
				}
			} else if (i == stb[staff].nvoice) { /* last voice */
				if (stb[staff].st[i].ymx < stb[staff].st[i].ymn
				    || stb[staff].st[i].ymn <= stb[staff].st[i - 1].ymx) {
					y = stb[staff].st[i - 1].ymn;
					for (j = i - 2; j >= 0; j--) {
						if (y > stb[staff].st[j].ymn)
							y = stb[staff].st[j].ymn;
					}
					s->y = (y - us + 24) / 6 * 6 - 24;
					if (s->y > 12)
						s->y = 12;
				} else {		/* voices inverted */
					y = (stb[staff].st[i].ymx
						+ stb[staff].st[i].ymn) / 2;
					s->y = (y + ls + 12) / 6 * 6 - 12;
					if (s->y < 12)
						s->y = 12;
				}
			} else {			/* middle voice */
/*fixme: may be too high*/
				s->y = (stb[staff].st[i - 1].ymn
					+ stb[staff].st[i + 1].ymx + 24)
					/ 12 * 6 - 12;
				if (((t = s->ts_next) != 0
				     && t->staff == staff
				     && t->time == s->time
				     && t->dur != 0
				     && !(t->as.flags & ABC_F_INVIS)
				     && t->ymx > s->y - ls)
				    || (s->ts_prev->staff == staff
					&& s->ts_prev->time == s->time
					&& s->ts_prev->dur != 0
					&& !(s->ts_prev->as.flags & ABC_F_INVIS)
					&& s->ts_prev->ymn < s->y + us)) {
					s->shhd[0] = 10;
					s->xmx = 10;
				}
			}
			s->ymx = s->y + us;
			if (s->ymx > stb[staff].st[i].ymx)
				stb[staff].st[i].ymx = s->ymx;
			s->ymn = s->y - ls;
			if (s->ymn < stb[staff].st[i].ymn)
				stb[staff].st[i].ymn = s->ymn;
		}

		while (s != 0 && s->type == BAR)
			s = s->ts_next;
	}
}

/* -- init the symbols at start of a music line -- */
/* this routine is called starting a tune generation,
 * and it is called later for each new music line */
static void init_music_line(void)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s, *new_s;
	int voice, staff;
	struct SYMBOL *v_sym[MAXVOICE];

	memset(v_sym, 0, sizeof v_sym);
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		voice = p_voice - voice_tb;
		if (cursys->voice[voice].range < 0)
			continue;
		if (cursys->voice[voice].second)
			p_voice->second = 1;
		else	p_voice->second = 0;
		p_voice->staff = staff = cursys->voice[voice].staff;
		if ((s = p_voice->sym) == 0)
			continue;
		v_sym[voice] = s;		/* voice symbols */
		if (s->type == CLEF) {		/* move the clefs and keysig's */
			if (s->as.u.clef.type >= 0 && !p_voice->second) {
				cursys->staff[staff].clef.type
					= s->as.u.clef.type;
				cursys->staff[staff].clef.line
					= s->as.u.clef.line;
				cursys->staff[staff].clef.octave
					= s->as.u.clef.octave;
				cursys->staff[staff].clef.invis
					= s->as.u.clef.invis;
			}
			s = s->next;
		}
		if (s != 0 && s->type == KEYSIG)
			memcpy(&p_voice->key, &s->as.u.key,
				sizeof s->as.u.key);
		p_voice->time = s->time;
	}

	/* add a clef at start of each voice */
	new_s = 0;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		voice = p_voice - voice_tb;
		if (cursys->voice[voice].range < 0
		    || (s = v_sym[voice]) == 0)
			continue;
		if (s->type == CLEF && s->as.u.clef.type >= 0) {
			s->u = 0;		/* normal clef */
#if 0
			if (cursys->staff[p_voice->staff].clef.invis)
				s->as.flags |= ABC_F_INVIS;
#endif
			v_sym[voice] = s->next;
			new_s = s;
		} else {
			new_s = sym_ins(s, CLEF);
			memcpy(&new_s->as.u.clef,
				&cursys->staff[p_voice->staff].clef,
				sizeof s->as.u.clef);
			if (new_s->next->sflags & S_SECOND)
				new_s->sflags |= S_SECOND;
			if (cursys->staff[p_voice->staff].clef.invis)
				new_s->as.flags |= ABC_F_INVIS;
			set_yval(new_s);
		}
	}

	/* add keysig */
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		voice = p_voice - voice_tb;
		if (cursys->voice[voice].range < 0
		    || cursys->voice[voice].second
		    || (s = v_sym[voice]) == 0)
			continue;
		if (s->type == KEYSIG) {
			v_sym[voice] = s->next;
			new_s = s;
			continue;
		}
		if (p_voice->key.sf != 0 || p_voice->key.nacc != 0) {
			new_s = sym_ins(v_sym[voice], KEYSIG);
			memcpy(&new_s->as.u.key, &p_voice->key, sizeof new_s->as.u.key);
			if (new_s->as.u.key.bagpipe && new_s->as.u.key.sf == 2)
				new_s->u = 3;	/* K:Hp --> G natural */
			set_yval(new_s);
		}
	}

	/* add time signature if needed */
	if (insert_meter) {
		for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
			voice = p_voice - voice_tb;
			if (cursys->voice[voice].range < 0
			    || cursys->voice[voice].second
			    || v_sym[voice] == 0)
				continue;
			if (p_voice->meter.nmeter != 0) {	/* != M:none */
				new_s = sym_ins(v_sym[voice], TIMESIG);
				memcpy(&new_s->as.u.meter, &p_voice->meter,
				       sizeof new_s->as.u.meter);
				set_yval(new_s);
			}
		}
	}

	/* add tempo if any (only one) */
	voice = first_voice - voice_tb;
	if ((s = info['Q' - 'A']) != 0
	    && v_sym[voice] != 0) {
		struct SYMBOL *s2;

/*fixme:%%staves:how to insert when no symbol in the voice?*/
		s->type = TEMPO;
		s->voice = voice;
		s->staff = first_voice->staff;
		s->time = first_voice->time;
		s->sflags |= S_SEQST;
		s2 = v_sym[voice];
		s2->prev->next = s;
		s->prev = s2->prev;
		s2->prev = s;
		s->next = s2;
		s2->ts_prev->ts_next = s;
		s->ts_prev = s2->ts_prev;
		s2->ts_prev = s;
		s->ts_next = s2;
		s2->sflags &= ~S_SEQST;
		info['Q' - 'A'] = 0;
		set_yval(s);
		new_s = s;
	}

	/* add bar if needed */
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		int i;

		if (p_voice->second || p_voice->bar_start == 0)
			continue;
		voice = p_voice - voice_tb;
		if (v_sym[voice] == 0)
			continue;
		i = 4;
		if (p_voice->bar_text == 0	/* if repeat continuation */
		    && p_voice->bar_start == B_OBRA) {
			for (s = v_sym[voice]; s != 0; s = s->next) {	/* search the end of repeat */
				if (s->type == BAR) {
					if ((s->as.u.bar.type & 0xf0)	/* if complex bar */
					    || s->as.u.bar.type == B_CBRA
					    || s->as.u.bar.repeat_bar)
						break;
					if (--i < 0)
						break;
				}
			}
			if (s == 0)
				i = -1;
			if (i >= 0 && v_sym[voice]->time == s->time)
				i = -1;		/* no note */
		}
		if (i >= 0) {
			new_s = sym_ins(v_sym[voice], BAR);
			new_s->as.u.bar.type = p_voice->bar_start & 0x7f;
			if (p_voice->bar_start & 0x80)
				new_s->as.flags |= ABC_F_INVIS;
			new_s->as.text = p_voice->bar_text;
			new_s->as.u.bar.repeat_bar = p_voice->bar_repeat;
			set_yval(new_s);
/*fixme:insert after tempo, staves and part?*/
		}
		p_voice->bar_start = 0;
		p_voice->bar_repeat = 0;
		p_voice->bar_text = 0;
	}

	/* remember where to put the whistle */
/*fixme:should be before the first note*/
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		if (!p_voice->whistle)
			continue;
		voice = p_voice - voice_tb;
		if (v_sym[voice] == 0)
			continue;
		new_s = sym_ins(v_sym[voice], WHISTLE);
		set_yval(new_s);
	}

	/* if initialization of a new music line, compute the spacing,
	 * including the first (old) sequence */
	if ((s = tsnext) != 0) {	/* (if called from cut_symbols()) */
		if ((s = new_s) != 0) {
			for (s = s->ts_next; s != 0; s = s->ts_next)
				if (s->sflags & S_SEQST)
					break;
			for (s = s->ts_next; s != 0; s = s->ts_next)
				if (s->sflags & S_SEQST)
					break;
		}
		if (s != 0)
			s->ts_prev->ts_next = 0;
		set_allsymwidth();
		if (s != 0)
			s->ts_prev->ts_next = s;
	}
}

/* -- initialize the generator -- */
/* this function is called only once per tune  */
static void set_global(void)
{
	struct SYSTEM *sy;
	struct SYMBOL *s;
	struct VOICE_S *p_voice;
	int staff;
static signed char maxpit[4] =		/* !! index = clef type !! */
		{100,
		  25,		/* e */
		  21,		/* A */
		 100};
static signed char minpit[4] =
		{-100,
		   14,		/* G, */
		   10,		/* C, */
		 -100};
static signed char delpit[4] = {0, -7, -14, 0};

	/* get the max number of staves */
	sy = cursys;
	staff = cursys->nstaff;
	while ((sy = sy->next) != 0) {
		if (sy->nstaff > staff)
			staff = sy->nstaff;
	}
	nstaff = staff;

	/* adjust the pitches if old abc2ps behaviour of clef definition */
	if (cfmt.abc2pscompat) {
		int old_behaviour, done, max, min, i;

		old_behaviour = done = 0;
		for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
			if (!p_voice->forced_clef
			    || p_voice->clef.type == PERC)
				continue;

			/* search if any pitch is too high for the clef */
			max = maxpit[p_voice->clef.type];
			min = minpit[p_voice->clef.type];
			for (s = p_voice->sym; s != 0; s = s->next) {
				switch (s->type) {
				case CLEF:
					if ((i = s->as.u.clef.type) < 0)
						continue;
					if (!s->as.u.clef.check_pitch)
						i = 0;
					max = maxpit[i];
					min = minpit[i];
				default:
					continue;
				case NOTE:
					if (s->pits[0] < min) {
						done = 1;
						break;		/* new behaviour */
					}
					if (s->pits[s->nhd] <= max)
						continue;
					old_behaviour = 1;
					done = 1;
					break;
				}
				break;
			}
			if (done)
				break;
		}
		if (old_behaviour) {
			for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
				int delta;
				struct SYMBOL *g;

				if (!p_voice->forced_clef
				    || p_voice->clef.type == PERC)
					continue;
				delta = 0;
				for (s = p_voice->sym; s != 0; s = s->next) {
					switch (s->type) {
					case CLEF:
						if ((i = s->as.u.clef.type) < 0)
							continue;
						if (!s->as.u.clef.check_pitch)
							i = 0;
						delta = delpit[i];
					default:
						continue;
					case NOTE:
					case GRACE:
						if (delta == 0)
							continue;
						break;
					}
					if (s->type == NOTE) {
						for (i = s->nhd; i >= 0; i--)
							s->pits[i] += delta;
					} else {
						for (g = s->grace; g != 0; g = g->next) {
							for (i = g->nhd; i >= 0; i--)
								g->pits[i] += delta;
						}
					}
				}
			}
		}
	}

	/* set a pitch for all symbols and the start/stop of words (beams) */
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		int pitch, start_flag;
		struct SYMBOL *sym, *lastnote;

		sym = p_voice->sym;
		pitch = 22;				/* 'B' - if no note! */
		for (s = sym; s != 0; s = s->next) {
			if (s->type == NOTE) {
				pitch = s->pits[0];
				break;
			}
		}
		while (sym != s) {
			sym->pits[0] = pitch;
			sym = sym->next;
		}
		start_flag = 1;
		lastnote = 0;
		for (s = sym; s != 0; s = s->next) {
			switch (s->type) {
			default:
				if ((s->sflags & S_EOLN) == 0)
					break;
				/* fall thru */
			case BAR:
			case MREST:
				if (lastnote != 0
				    && !(s->sflags & S_BEAM_ON)) {
					lastnote->as.flags |= ABC_F_WORD_END;
					start_flag = 1;
					lastnote = 0;
				}
				if (s->type == BAR
				    && s->next == 0
				    && s->prev->type == NOTE
				    && s->prev->dur >= BREVE)
					s->prev->head = H_SQUARE;
				break;
			case NOTE:
			case REST:
				if (s->sflags & S_TREM)
					break;
				if (s->nflags <= 0 && s->dur > 0) {
					if (lastnote != 0) {
						lastnote->as.flags |= ABC_F_WORD_END;
						lastnote = 0;
					}
					s->as.flags |= ABC_F_WORD_END;
					start_flag = 1;
					s->sflags |= S_WORD_ST;
				} else if (s->type == NOTE) {
					if (start_flag)
						s->sflags |= S_WORD_ST;
					if (s->sflags & S_EOLN)
						s->as.flags |= ABC_F_WORD_END;
					start_flag = (s->as.flags & ABC_F_WORD_END);
					lastnote = s;
				} else if ((s->as.flags & ABC_F_WORD_END)
					   || (s->sflags & S_EOLN)) {
					if (lastnote != 0) {
						lastnote->as.flags |= ABC_F_WORD_END;
						lastnote = 0;
					}
					s->as.flags &= ~ABC_F_WORD_END;
					start_flag = 1;
				}
				break;
			}
			if (s->type == NOTE) {
				pitch = s->pits[0];
				if (s->prev != 0 && s->prev->type != NOTE) {
					s->prev->pits[0] = (s->prev->pits[0]
							    + pitch) / 2;
				}
			} else	s->pits[0] = pitch;
		}
		if (lastnote != 0)
			lastnote->as.flags |= ABC_F_WORD_END;
	}

	/* set the staff of the floating voices */
	set_float();

	/* set the clefs */
	if (cfmt.autoclef) {
		for (p_voice = first_voice; p_voice; p_voice = p_voice->next)
			if (p_voice->forced_clef)
				staff_tb[p_voice->staff].forced_clef = 1;
		for (staff = 0; staff <= nstaff; staff++) {
			if (!staff_tb[staff].forced_clef)
				set_clef(staff);
		}
	}
	set_first_clefs();
	init_music_line();
	insert_meter &= ~1;		/* keep the 'first line' flag */
	set_pitch();			/* adjust the note pitches */
}

/* -- return the left indentation of the staves -- */
static float set_indent(void)
{
	int staff, voice;
	float w, maxw;
	struct VOICE_S *p_voice;
	char *p, *q;

	maxw = 0;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		voice = p_voice - voice_tb;
		if (cursys->voice[voice].range < 0)
			continue;
		if ((p = p_voice->new_name ? p_voice->nm : p_voice->snm) == 0)
			continue;
		str_font(VOICEFONT);
		for (;;) {
			if ((q = strstr(p, "\\n")) != 0)
				*q = '\0';
			w = tex_str(p);
			if (w > maxw)
				maxw = w;
			if (q == 0)
				break;
			*q = '\\';
			p = q + 2;
		}
	}

	if (maxw != 0) {
		w = 0;
		for (staff = 0; staff <= nstaff; staff++) {
			if (cursys->staff[staff].flags
					& (OPEN_BRACE2 | OPEN_BRACKET2)) {
				w = 20;
				break;
			}
			if ((cursys->staff[staff].flags
					& (OPEN_BRACE | OPEN_BRACKET))
			    && w == 0)
				w = 10;
		}
		maxw += 4 * cwid(' ') * cfmt.font_tb[VOICEFONT].swfac + w;
	}
	if (insert_meter & 2) {			/* if first music line */
		maxw += cfmt.indent;
		insert_meter &= ~2;
	}
	return maxw;
}

/* -- decide on beams and on stem directions -- */
/* this routine is called only once per tune */
static void set_beams(struct SYMBOL *sym)
{
	struct SYMBOL *s, *t;
	int beam, laststem, lasty;

	beam = 0;
	laststem = -1;
	lasty = 0;
	for (s = sym; s != 0; s = s->next) {
		if (s->type != NOTE) {
			struct SYMBOL *g;

			if ((g = s->grace) != 0) {
				if (s->stem == 0
				    && (s->stem = s->multi) == 0)
					s->stem = 1;
				for (; g != 0; g = g->next) {
					g->stem = s->stem;
					g->multi = s->multi;
				}
			}
			continue;
		}

		if (s->stem == 0		/* if not explicitly set */
		    && (s->stem = s->multi) == 0) { /* and alone on the staff */

			/* notes in a beam have the same stem direction */
			if (beam)
				s->stem = laststem;
			else if ((s->sflags & S_WORD_ST)
				   && !(s->as.flags & ABC_F_WORD_END)) { /* start of beam */
				int avg, n;

				avg = s->yav;
				n = 12;
				for (t = s->next; t != 0; t = t->next) {
					if (t->type == NOTE) {
						if (t->multi != 0) {
							avg = n - t->multi;
							break;
						}
						avg += t->yav;
						n += 12;
					}
					if (t->as.flags & ABC_F_WORD_END)
						break;
				}
				if (avg < n)
					laststem = 1;
				else if (avg > n || cfmt.bstemdown)
					laststem = -1;
				beam = 1;
				s->stem = laststem;
			} else {
				s->stem = s->yav >= 12 ? -1 : 1;
				if (s->yav == 12
				    && !cfmt.bstemdown) {
					int dy;

					dy = s->yav - lasty;
					if (dy > -7 && dy < 7)
						s->stem = laststem;
				}
			}
		} else {			/* stem set by set_multi */
			if ((s->sflags & S_WORD_ST)
			    && !(s->as.flags & ABC_F_WORD_END))	/* start of beam */
				beam = 1;
		}
		if (s->as.flags & ABC_F_WORD_END)
			beam = 0;
		laststem = s->stem;
		lasty = s->yav;
	}
}

/* -- shift the notes when voices overlap -- */
/* this routine is called only once per tune */
static void set_overlap(void)
{
	struct SYMBOL *s, *s1, *s2;
	int d, i1, i2, m, sd1, sd2, t;
	float d1, d2, dy1, dy2, noteshift;

	for (s = tsfirst; s != 0; s = s->ts_next) {
		if (s->type != NOTE
		    || (s->as.flags & ABC_F_INVIS))
			continue;

		/* search the next note at the same time on the same staff */
		s2 = s;
		for (;;) {
			if ((s2 = s2->ts_next) == 0)
				break;
			if (s2->time != s->time) {
				s2 = 0;
				break;
			}
			if (s2->type == NOTE
			    && !(s2->as.flags & ABC_F_INVIS)
			    && s2->staff == s->staff)
				break;
		}
		if (s2 == 0)
			continue;

		sd1 = sd2 = 0;
		d1 = d2 = dy1 = dy2 = 0;
		s1 = s;

		/* set the smallest interval type */
		t = 0;		/* t: interval types
				 *	0: >= 4
				 *	1: third or fourth
				 *	2: second
				 *	4: unisson
				 *	-1: unisson and different accidentals */
		{
			int dp;

			i1 = s1->nhd;
			i2 = s2->nhd;
			for (;;) {
				dp = s1->pits[i1] - s2->pits[i2];
				switch (dp) {
				case 0:
					if (s1->as.u.note.accs[i1] != s2->as.u.note.accs[i2])
						t = -1;
					else	t |= 4;
					break;
				case 1:
				case -1:
					t |= 2;
					break;
				case 2:
				case -2:
				case 3:
				case -3:
					t |= 1;
					break;
				}
				if (t < 0)
					break;
				if (dp >= 0) {
					i1--;
					if (i1 < 0)
						break;
				}
				if (dp <= 0) {
					i2--;
					if (i2 < 0)
						break;
				}
			}
		}

		if (s1->dur >= BREVE || s2->dur >= BREVE)
			noteshift = 13;
		else if (s1->dur >= SEMIBREVE || s2->dur >= SEMIBREVE)
			noteshift = 10;
		else	noteshift = 7.8;

		/* if unisson and different accidentals */
		if (t < 0) {
			if (s2->as.u.note.accs[i2] == 0) {
				d1 = noteshift + 12;
				if (s1->as.u.note.accs[i1] & 0xf8)
					d1 += 2;
				if (s2->dots)
					d1 += 6;
				for (m = s1->nhd; m >= 0; m--) {
					s1->shhd[m] += d1;
					s1->shac[m] -= d1;
				}
				s1->xmx += d1;
			} else {
				d2 = noteshift + 12;
				if (s2->as.u.note.accs[i2] & 0xf8)
					d2 += 2;
				if (s1->dots)
					d2 += 6;
				for (m = s2->nhd; m >= 0; m--) {
					s2->shhd[m] += d2;
					s2->shac[m] -= d2;
				}
				s2->xmx += d2;
			}
			s2->doty = -3;
			continue;
		}

		if (s1->stem * s2->stem > 0) {	/* if same stem direction */
			d2 = noteshift + 2;	/* shift the 2nd voice */
			if (s1->dur < CROTCHET
			    && (s1->sflags & S_WORD_ST)
			    && (s1->as.flags & ABC_F_WORD_END)) { /* if a flag */
				if (s1->stem > 0) {
					if (3 * (s1->pits[s1->nhd] - 18) > s2->ymx)
						d2 *= 0.5;
					else if (s1->pits[s1->nhd] <= s2->pits[s2->nhd])
						d2 += noteshift;
				}
			} else {			/* no flag */
				if (s1->pits[0] > s2->pits[s2->nhd] + 1)
					d2 *= 0.5;
			}
		} else if (s->stem < 0) {	/* if stem inverted, */
			s1 = s2;		/* invert the voices */
			s2 = s;
		}

		d = s1->pits[0] - s2->pits[s2->nhd];
		if (d >= 0)
			dy2 = -3;	/* the dot of the 2nd voice shall be lower */

		if (s1->head == H_SQUARE || s2->head == H_SQUARE) {
			if (s1->ymn >= s2->ymx + 4
			    || s1->ymx <= s2->ymn - 4) {
				d2 = 0;
				goto do_shift;
			}
			if (s1->stem * s2->stem > 0)	/* if same stem direction */
				goto do_shift;
		} else {
			if (s1->ymn >= s2->ymx - 2
			    || s1->ymx <= s2->ymn + 2) {
				d2 = 0;
				goto do_shift;
			}
			if (s1->stem * s2->stem > 0)	/* if same stem direction */
				goto do_shift;
			if (d >= 2)
				goto do_shift;
		}
		/* (here, voice 1 stem up and voice 2 stem down) */

		/* if unisson */
		if (t >= 4) {
			int l1, l2;

			if ((l1 = s1->dur) >= SEMIBREVE)
				goto uni_shift;
			if ((l2 = s2->dur) >= SEMIBREVE)
				goto uni_shift;
			if (s1->as.flags & s2->as.flags & ABC_F_STEMLESS)
				goto uni_shift;
			if (s1->dots != s2->dots) {
				if (cfmt.shiftunisson
				    || s1->dots * s2->dots != 0)
				goto uni_shift;
			}
			i2 = 0;
			while (i2 <= s2->nhd && s2->pits[i2] != s1->pits[0])
				i2++;
			if (i2 > s2->nhd)
				goto uni_shift;
			i1 = 0;
			while (i1 < s1->nhd && i1 + i2 < s2->nhd
			       && s2->pits[i1 + i2 + 1] == s1->pits[i1 + 1])
				i1++;
			if (i1 + i2 != s2->nhd)
				goto uni_shift;
			if (l1 == l2)
				goto same_head;
			if (l1 < l2) {
				l1 = l2;
				l2 = s1->dur;
			}
			if (l1 < MINIM) {
				if (s2->dots > 0) {
					dy2 = -3;
					goto head_2;
				}
				if (s1->dots > 0)
					goto head_1;
				goto same_head;
			}
			if (l2 < CROTCHET) {	/* (l1 >= MINIM) */
				if (cfmt.shiftunisson)
					goto uni_shift;
				if (s2->dur >= MINIM) {
					dy2 = -3;
					goto head_2;
				}
				goto head_1;
			}
			goto uni_shift;
		same_head:
			if (voice_tb[s1->voice].scale < voice_tb[s2->voice].scale)
				goto head_2;
		head_1:
			s2->nohdix = i2;	/* keep heads of 1st voice */
			for (; i2 <= s2->nhd; i2++)
				s2->as.u.note.accs[i2] = 0;
			goto do_shift;
		head_2:
			s1->nohdix = i1;	/* keep heads of 2nd voice */
			for (; i1 >= 0; i1--)
				s1->as.u.note.accs[i1] = 0;
			goto do_shift;
		}

		if (d == -1
		    && (s1->nhd == 0 || s1->pits[1] > s2->pits[s2->nhd])
		    && (s2->nhd == 0 || s1->pits[0] > s2->pits[s2->nhd - 1])) {
			if (!(s->as.flags & ABC_F_STEMLESS)) {
				d1 = noteshift;
				if (s2->dots && s1->dots == s2->dots) {
					sd2 = 1;
					dy1 = -3;
				}
			} else	d2 = noteshift;
			goto do_shift;
		}

		if (t == 1) {			/* if third or fourth only */
			if (s1->head != H_SQUARE
			    && s2->head != H_SQUARE)
				t = 0;
		}
		if (t == 0) {			/* if small overlap */
			if (s1->dur < SEMIBREVE
			    && s2->dur < SEMIBREVE) {
				if (s2->dur < CROTCHET
				    && (s2->sflags & S_WORD_ST)
				    && (s2->as.flags & ABC_F_WORD_END)	/* if flag */
				    && s1->pits[0] < s2->pits[0]
				    && 3 * (s1->pits[s1->nhd] - 18) > s2->ymn)
					d1 = noteshift;
				else	d1 = noteshift * 0.6;
				if (s2->dots)
					sd2 = 1;
			} else {
				d2 = noteshift + 1.5;
				if (s1->dots)
					sd1 = 1;
			}
			goto do_shift;
		}

	uni_shift:
		if (t >= 2) {			/* if close or unisson */
			if (s1->dots != s2->dots) {
				if (s1->dots > s2->dots) /* shift the voice with more dots */
					d1 = noteshift;
				else	d2 = noteshift;
/*fixme:if second, see if dots may be distinguished?*/
			} else if (d == 1) {
				d2 = noteshift;
				if (s1->dots)
					sd1 = 1;
			} else	d1 = noteshift;
			if (t >= 4) {		/* if unisson */
				if (d1 != 0)
					d1 += 1.5;
				else	d2 += 1.5;
			}
			goto do_shift;
		}

		/* if the upper note is SEMIBREVE or higher, shift it */
		if (s1->dur >= SEMIBREVE
		    && s1->dur > s2->dur) {
			d1 = noteshift;

		/* else shift the 2nd voice */
		} else {
			d2 = noteshift;
			if (s1->dots > 0
			    && (d != 1 || (s1->pits[0] & 1)))
/*fixme: d always != 1 ?*/
				sd1 = 1;	/* and the dot of the 1st voice */
		}

		/* do the shift, and update the width */
	do_shift:

		/* shift the accidentals */
		for (i1 = 0; i1 <= s1->nhd; i1++) {
			int dp;
			float shft;

			if (s1->as.u.note.accs[i1] == 0)
				continue;
			for (i2 = 0; i2 <= s2->nhd; i2++) {
				if (s2->as.u.note.accs[i2] != 0) {
					dp = s1->pits[i1] - s2->pits[i2];
					if (dp > 5 || dp < -5)
						continue;
					if (dp == 0) {
						s2->as.u.note.accs[i2] = 0;
						continue;
					}
					shft = (dp <= -4 || dp >= 4) ? 4.5 : 7;
					if (dp > 0) {
						if (s1->as.u.note.accs[i1] & 0xf8)
							shft += 2;
						if (s2->shac[i2] < s1->shac[i1] + shft
						    && s2->shac[i2] > s1->shac[i1] - shft)
							s2->shac[i2] = s1->shac[i1] + shft;
					} else {
						if (s2->as.u.note.accs[i2] & 0xf8)
							shft += 2;
						if (s1->shac[i1] < s2->shac[i2] + shft
						    && s1->shac[i1] > s2->shac[i2] - shft)
							s1->shac[i1] = s2->shac[i2] + shft;
					}
				}
			}
		}

		/* handle the previous shift */
		m = s1->stem >= 0 ? 0 : s1->nhd;
		d1 -= s1->shhd[m];
		d2 += s1->shhd[m];
		m = s2->stem >= 0 ? 0 : s2->nhd;
		d1 += s2->shhd[m];
		d2 -= s2->shhd[m];

		if (d1 > 0) {			/* shift the 1st voice */
			if (s2->dots && sd2 == 0)	/* room for the dots */
				d1 += 8 + 3.5 * (s2->dots - 1);
			for (m = s1->nhd; m >= 0; m--)
				s1->shhd[m] += d1;
			s1->xmx += d1;
			if (sd2 != 0)
				s2->xmx = s1->xmx;
		}
		if (d2 > 0) {			/* shift the 2nd voice */
			if (s1->dots && sd1 == 0)	/* room for the dots */
				d2 += 8 + 3.5 * (s1->dots - 1);
			for (m = s2->nhd; m >= 0; m--) {
				s2->shhd[m] += d2;
				if (s2->as.u.note.accs[m] != 0
				    && s2->pits[m] < s1->pits[0] - 4)
					s2->shac[m] -= d2;
			}
			s2->xmx += d2;
			if (sd1 != 0)
				s1->xmx = s2->xmx;
		}
		s1->doty = dy1;
		s2->doty = dy2;
	}
}

/* -- set the stem lengths -- */
/* this routine is called only once per tune */
static void set_stems(void)
{
	struct SYMBOL *s, *s2, *g;
	float slen, scale;
	int ymn, ymx, nflags;

	for (s = tsfirst; s != 0; s = s->ts_next) {
		if (s->type != NOTE) {
			int ymin, ymax;

			if ((g = s->grace) == 0)
				continue;
			ymin = ymax = 12;
			for (; g != 0; g = g->next) {
				slen = GSTEM;
				if (g->nflags > 1)
					slen += 1.2 * (g->nflags - 1);
				ymn = 3 * (g->pits[0] - 18);
				ymx = 3 * (g->pits[g->nhd] - 18);
				if (s->stem >= 0) {
					g->y = ymn;
					g->ys = ymx + slen;
					ymx = (int) (g->ys + 0.5);
				} else {
					g->y = ymx;
					g->ys = ymn - slen;
					ymn = (int) (g->ys - 0.5);
				}
				ymx += 2;
				ymn -= 2;
				if (ymn < ymin)
					ymin = ymn;
				else if (ymx > ymax)
					ymax = ymx;
				g->ymx = ymx;
				g->ymn = ymn;
			}
			s->ymx = ymax;
			s->ymn = ymin;
			continue;
		}

		/* shift notes in chords (need stem direction to do this) */
		set_head_directions(s);

		/* if start or end of beam, adjust the number of flags
		 * with the other end */
		nflags = s->nflags;
		if ((s->sflags & S_WORD_ST) && !(s->as.flags & ABC_F_WORD_END)) {
			for (s2 = s->next; /*s2 != 0*/; s2 = s2->next) {
				if (s2->type == NOTE
				    && (s2->as.flags & ABC_F_WORD_END))
					break;
			}
/*			if (s2 != 0) */
			    if (s2->nflags > nflags)
				nflags = s2->nflags;
		} else if ((s->as.flags & ABC_F_WORD_END)
			   && !(s->sflags & S_WORD_ST)) {
			for (s2 = s->prev; /*s2 != 0*/; s2 = s2->prev) {
				if (s2->sflags & S_WORD_ST)
					break;
			}
/*			if (s2 != 0) */
			    if (s2->nflags > nflags)
				nflags = s2->nflags;
		}

		/* set height of stem end */
		slen = cfmt.stemheight;
		switch (nflags) {
		case 2: slen += 2; break;
		case 3:	slen += 5; break;
		case 4:	slen += 10; break;
		case 5:	slen += 16; break;
		}
		if ((scale = voice_tb[s->voice].scale) != 1)
			slen *= (scale + 1) * 0.5;
		ymn = 3 * (s->pits[0] - 18);
		if (s->nhd > 0) {
			slen -= 2;
			ymx = 3 * (s->pits[s->nhd] - 18);
		} else	ymx = ymn;
		if (s->as.flags & ABC_F_STEMLESS) {
			if (s->stem >= 0) {
				s->y = ymn;
				s->ys = (float) ymx;
			} else {
				s->ys = (float) ymn;
				s->y = ymx;
			}
			if (nflags == -4)	/* if longa */
				ymn -= 6;
			s->ymx = ymx + 4;
			s->ymn = ymn - 4;
		} else if (s->stem >= 0) {
			if (nflags >= 2)
				slen -= 1;
			if (s->pits[s->nhd] > 26
			    && (nflags <= 0
				|| !((s->sflags & S_WORD_ST)
				     && (s->as.flags & ABC_F_WORD_END)))) {
				slen -= 2;
				if (s->pits[s->nhd] > 28)
					slen -= 2;
			}
			s->y = ymn;
			if (s->as.u.note.ti1[0] != 0)
/*fixme
 *			    || s->as.u.note.ti2[0] != 0) */
				ymn -= 3;
			s->ymn = ymn - 4;
			s->ys = ymx + slen;
			if (s->ys < 12)
				s->ys = 12;
			s->ymx = (int) (s->ys + 2.5);
		} else {			/* stem down */
			if (s->pits[0] < 18
			    && (nflags <= 0
				|| !((s->sflags & S_WORD_ST)
				     && (s->as.flags & ABC_F_WORD_END)))) {
				slen -= 2;
				if (s->pits[0] < 16)
					slen -= 2;
			}
			s->ys = ymn - slen;
			if (s->ys > 12)
				s->ys = 12;
			s->ymn = (int) (s->ys - 2.5);
			s->y = ymx;
/*fixme:the tie may be lower*/
			if (s->as.u.note.ti1[s->nhd] != 0)
/*fixme
 *			    || s->as.u.note.ti2[s->nhd] != 0)*/
				ymx += 3;
			s->ymx = ymx + 4;
		}
	}
}

/* -- split up unsuitable bars at end of staff -- */
static void check_bar(struct SYMBOL *s)
{
	struct VOICE_S *p_voice;
	int bar_type, i;

	p_voice = &voice_tb[s->voice];

	/* search the last bar */
	while (s->type == CLEF || s->type == KEYSIG || s->type == TIMESIG) {
		if (s->type == TIMESIG)
			insert_meter |= 1;
		if ((s = s->prev) == 0)
			return;
	}
	if (s->type != BAR)
		return;

	if (s->as.u.bar.repeat_bar) {
		p_voice->bar_start = B_OBRA;
		p_voice->bar_text = s->as.text;
		p_voice->bar_repeat = 1;
		s->as.text = 0;
		s->as.u.bar.repeat_bar = 0;
	}
	bar_type = s->as.u.bar.type;
	if (bar_type == B_COL)			/* ':' */
		return;
	if ((bar_type & 0x07) != B_COL)		/* if not left repeat bar */
		return;
	if (!(s->sflags & S_RRBAR)) {		/* 'xx:' (not ':xx:') */
		p_voice->bar_start = bar_type;
		if (s->as.flags & ABC_F_INVIS)
			p_voice->bar_start |= 0x80;
		if (s->prev != 0 && s->prev->type == BAR) {
			s->prev->next = 0;
			if (s->ts_prev != 0)
				s->ts_prev->ts_next = s->ts_next;
			if (s->ts_next != 0)
				s->ts_next->ts_prev = s->ts_prev;
		} else	s->as.u.bar.type = B_BAR;
		return;
	}
	if (bar_type == B_DREP) {		/* '::' */
		s->as.u.bar.type = B_RREP;
		p_voice->bar_start = B_LREP;
		if (s->as.flags & ABC_F_INVIS)
			p_voice->bar_start |= 0x80;
		return;
	}
	for (i = 0; bar_type != 0; i++)
		bar_type >>= 4;
	bar_type = s->as.u.bar.type;
	s->as.u.bar.type = bar_type >> ((i / 2) * 4);
	i = ((i + 1) / 2 * 4);
/*fixme:bar_start is a byte!*/
	p_voice->bar_start = bar_type & ((1 << i) - 1);
	if (s->as.flags & ABC_F_INVIS)
		p_voice->bar_start |= 0x80;
}

/* -- define the start and end of a piece of tune -- */
/* tsnext becomes the beginning of the next line */
static void set_piece(void)
{
	struct SYSTEM *sy;
	struct SYMBOL *s, *s2;
	struct VOICE_S *p_voice;
	struct STAFF_S *p_staff;
	int staff;

	/* reset the staves */
	for (staff = 0; staff <= nstaff; staff++)
		staff_tb[staff].y = 0;		/* staff system not computed */

	/* search the next end of line,
	 * set the repeat measures, (remove some dble bars?) */
	sy = cursys;
	for (s = tsfirst; s != 0; s = s->ts_next) {
		if (s->sflags & S_NL)
			break;
		if (s->type == FMTCHG && s->u == REPEAT) {
			s = set_repeat(s, 1);
			continue;
		}
		if (s->type == STAVES)
			sy = sy->next;
#if 0
		if (s->type == BAR
		    && s->next != 0
		    && s->next->type == BAR
		    && !(s->next->sflags & S_NL)
		    && !s->next->as.u.bar.repeat_bar
		    && (s->as.text == 0
			|| s->next->as.text == 0)
		    && (s->as.u.bar.dc.n == 0
			|| s->next->as.u.bar.dc.n == 0)) {
			s2 = 0;
			if ((s->as.u.bar.type == B_SINGLE
			     || s->as.u.bar.type == B_DOUBLE)
			    && (s->next->as.u.bar.type & 0xf0)) {
				s2 = s->next;
				if (s2->as.u.bar.dc.n != 0)
					memcpy(&s->as.u.bar.dc,
						&s2->as.u.bar.dc,
						sizeof s->as.u.bar.dc);
				memcpy(&s->as.u.bar,
					&s2->as.u.bar,
					sizeof s->as.u.bar);
				if (s2->as.text != 0)
					s->as.text = s2->as.text;
			}
			if (s2 != 0)
				delsym(s2);
		}
#endif
	}

	/* set the global staff system */
	for (staff = 0; staff <= nstaff; staff++) {
		p_staff = &staff_tb[staff];
		p_staff->clef.stafflines = sy->staff[staff].clef.stafflines;
		p_staff->clef.staffscale = sy->staff[staff].clef.staffscale;
		p_staff->botbar = p_staff->clef.stafflines <= 3 ? 6 : 0;
		switch (p_staff->clef.stafflines) {
		case 0:
		case 1:
		case 3:
		case 4:	p_staff->topbar = 18; break;
		case 2:	p_staff->topbar = 12; break;
		case 5:	p_staff->topbar = 24; break;
		default:
			p_staff->topbar = 6 * (p_staff->clef.stafflines - 1);
			break;
		}
	}

	/* if last music line, nothing more to do */
	if ((tsnext = s) == 0)
		return;
	s->sflags &= ~S_NL;
	s = s->ts_prev;
	s->ts_next = 0;

	/* set end of voices */
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		int voice;

		voice = p_voice - voice_tb;
		for (s = tsnext->ts_prev; s != 0; s = s->ts_prev) {
			if (s->voice != voice)
				continue;

			/* set the word end / start */
			for (s2 = s; s2 != 0; s2 = s2->prev) {
				if (s2->type == BAR)
					break;
				if (s2->type == NOTE) {
					s2->as.flags |= ABC_F_WORD_END;
					break;
				}
			}
			for (s2 = s->next; s2 != 0; s2 = s2->next) {
				if (s2->type == NOTE) {
					s2->sflags |= S_WORD_ST;
					break;
				}
				if (s2->type == BAR)
					break;
			}
			s->next = 0;
			check_bar(s);
			break;
		}
		if (s == 0)
			p_voice->sym = 0;
	}
}

/* -- position the symbols along the staff -- */
static void set_sym_glue(float width)
{
	struct SYMBOL *s;
	float beta0, alfa, beta;
	int some_grace;
	float xmin, x, xmax, spafac;

	/* calculate the whole space of the symbols */
	some_grace = 0;
	s = tsfirst;
	xmin = x = xmax = 0;
	for (;;) {
		if (s->type == GRACE)
			some_grace = 1;
		if (s->shrink != 0) {
			float space;

			xmin += s->shrink;
			if ((space = s->space) < s->shrink)
				space = s->shrink;
			x += space;
			xmax += space * 1.8;
		}
		if (s->ts_next == 0)
			break;
		s = s->ts_next;
	}

	/* set max shrink and stretch */
	if (!cfmt.continueall)
		beta0 = BETA_X;
	else	beta0 = BETA_C;

	/* memorize the glue for the last music line */
	if (x - width >= 0) {
		alfa_last = (x - width) / (x - xmin);	/* shrink */
		beta_last = 0;
	} else {
		alfa_last = 0;
		beta = (width - x) / (xmax - x);	/* stretch */
		if (beta > beta0) {
			if (!cfmt.continueall) {
				error(0, s,
				      "Line underfull (%.0fpt of %.0fpt)",
					beta0 * xmax + (1 - beta0) * x,
					width);
			}
			if (!cfmt.stretchstaff) {
				width = x;
				beta_last = 0;
			}
		}
		if (!cfmt.stretchlast
		    && tsnext == 0		/* if last line of tune */
		    && beta >= beta_last) {

			/* shrink underfull last line same as previous */
			if (alfa != 0)
				width = (1 - alfa_last) * x + alfa_last * xmin;
			else	width = beta_last * xmax + (1 - beta_last) * x;
		}
	}

	spafac = width / x;			/* space expansion factor */

	/* define the x offsets for all symbols */
	x = xmax = 0;
	for (s = tsfirst; s != 0; ) {
		if (s->shrink != 0) {
			float new_space;

			new_space = s->shrink;
			if (s->space != 0) {
				if (new_space < s->space * spafac)
					new_space = s->space * spafac;
				xmax += s->space * 1.8;
			}
			x += new_space;
			xmax += new_space;
			s->x = x;
			s->xmax = xmax;
		}
		if (s->ts_next == 0)
			break;
		s = s->ts_next;
	}

	/* if the last symbol is not a bar, add some extra space */
	if (s->type != BAR
	    && s->type != FMTCHG) {
		xmin += s->wr + 3;
		if (tsnext != 0 && tsnext->space * 0.8 > s->wr + 4) {
			x += tsnext->space * 0.8 * spafac;
			xmax += tsnext->space * 0.8 * spafac * 1.8;
		} else {
/*fixme:should calculate the space according to the last symbol duration */
			x += (s->wr + 4) * spafac;
			xmax += (s->wr + 4) * spafac * 1.8;
		}
	}

	/* calculate the exact glue */
	alfa = beta = 0;
	if (x - width >= 0) {
		if (x == xmin)
			alfa = 1;
		else	alfa = (x - width) / (x - xmin);	/* shrink */
	} else {
		beta = (width - x) / (xmax - x);	/* stretch */
/*fixme:to see again*/
		if (beta > beta0) {
			if (!cfmt.stretchstaff)
				beta = 0;
		}
		if (!cfmt.stretchlast
		    && tsnext == 0		/* if last line of tune */
		    && beta >= beta_last) {

			/* shrink underfull last line same as previous */
			alfa = alfa_last;
			beta = beta_last;
		}
	}
	if (alfa != 0) {
#if 0
		if (alfa <= 1)
			error(0, s,
			      "Line too much shrunk (%.0fpt of %.0fpt)",
				xmin, width);
#endif
		realwidth = xmin * alfa + x * (1 - alfa);
	} else	realwidth = xmax * beta + x * (1 - beta);

	/* set the final x offsets */
	s = tsfirst;
	if (alfa != 0) {
		x = xmin = 0;
		for (;;) {
			if (s->shrink != 0) {
				xmin += s->shrink * alfa;
				x = xmin + s->x * (1 - alfa);
			}
			s->x = x;
			if (s->ts_next == 0)
				break;
			s = s->ts_next;
		}
	} else {
		x = 0;
		for (;;) {
			if (s->shrink != 0)
				x = s->xmax * beta + s->x * (1 - beta);
			s->x = x;
			if (s->ts_next == 0)
				break;
			s = s->ts_next;
		}
	}

	/* set the x offsets of the grace notes */
	if (some_grace) {
		for (s = tsfirst; s != 0; s = s->ts_next) {
			struct SYMBOL *g;

			if ((g = s->grace) == 0)
				continue;
			x = s->x - s->wl + (cfmt.gracespace >> 16) * 0.1;
			for ( ; g != 0; g = g->next)
				g->x += x;
		}
	}
}

/* -- initialize a new music line -- */
static void cut_symbols(void)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s;
	int voice;

	/* set the first symbol of each voice */
	tsnext->ts_prev = 0;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		p_voice->sym = 0;		/* may have no symbol */
		voice = p_voice - voice_tb;
		for (s = tsfirst; s != 0; s = s->ts_next) {
			if (s->voice == voice) {
				p_voice->sym = s;
				s->prev = 0;
				break;
			}
		}
	}

	init_music_line();	/* add the first symbols of the line */
	insert_meter = 0;
}

/* -- generate the music -- */
void output_music(void)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s;
	float lwidth, indent;

	/* set the staff system if any STAVES at start of the next line */
	for (s = tsfirst; s != 0; s = s->ts_next) {
		switch (s->type) {
		case PART:
		case CLEF:
		case KEYSIG:
		case TIMESIG:
		case TEMPO:
			continue;
		case STAVES:
			cursys = cursys->next;
			delsym(s);
			if (tsfirst == 0)
				return;
			break;
		}
		break;
	}
	check_buffer();	/* dump buffer if not enough space for a music line */
	set_global();			/* initialize the generator */
	if (first_voice->next != 0) {	/* if many voices */
		if (cfmt.combinevoices)
			combine_voices();
		set_multi();		/* set the stems direction in 'multi' */
	}
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next)
		set_beams(p_voice->sym);	/* decide on beams */
	set_stems();			/* set the stem lengths */
	if (first_voice->next != 0)	/* when multi-voices */
		set_overlap();		/* shift the notes on voice overlap */
	set_allsymwidth();		/* set the symbols width */

	lwidth = ((cfmt.landscape ? cfmt.pageheight : cfmt.pagewidth)
		- cfmt.leftmargin - cfmt.rightmargin)
			/ cfmt.scale;
	if (lwidth < 1 CM) {
		error(1, 0, "Bad page width %.1f", lwidth);
		lwidth = 20 CM;
	}
	indent = set_indent();
	cut_tune(lwidth, indent);
	alfa_last = 0.1;
	beta_last = 0;
	for (;;) {			/* loop per music line */
		float line_height;

		set_piece();
		set_sym_glue(lwidth - indent);
		if (indent != 0)
			PUT1("%.2f 0 T\n", indent); /* do indentation */
		PUT0("/dlsym{\n");
		outft = -1;
		draw_sym_near();
		PUT0("}def\n");
		outft = -1;
		line_height = draw_systems(indent);
		PUT0("dlsym\n");
		draw_all_symb();
		draw_all_deco();
		if (showerror > 1) {
			showerror = 1;
			for (s = tsfirst; s != 0; s = s->ts_next) {
				if (s->as.flags & ABC_F_ERROR) {
					putxy(s->x, staff_tb[s->staff].y + s->y);
					PUT0("showerror\n");
				}
			}
		}
		bskip(line_height);
		if (nwhistle != 0)
			draw_whistle();
		if (indent != 0)
			PUT1("%.2f 0 T\n", -indent);

		/* set the staff system if any STAVES at start of the next line */
		for (s = tsnext; s != 0; s = s->ts_next) {
			switch (s->type) {
			case PART:
			case CLEF:
			case KEYSIG:
			case TIMESIG:
			case TEMPO:
				continue;
			case STAVES:
				cursys = cursys->next;
				delsym(s);
				break;
			}
			break;
		}

		if ((tsfirst = tsnext) == 0)
			break;
		cut_symbols();
		buffer_eob();
		indent = set_indent();
	}
	outft = -1;
}

/* -- reset the generator -- */
void reset_gen(void)
{
	insert_meter = 3;
}
