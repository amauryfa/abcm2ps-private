/*
 * Music generator.
 *
 * This file is part of abcm2ps.
 *
 * Copyright (C) 1998-2007 Jean-François Moine
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
		dx *= 0.7;
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

/* -- delete a symbol -- */
static void delsym(struct SYMBOL *s)
{
	if (s->next != 0)
		s->next->prev = s->prev;
	if (s->prev != 0)
		s->prev->next = s->next;
	if (s->ts_next != 0) {
		if (s->shrink != 0) {
			s->ts_next->shrink = s->shrink;
			s->ts_next->space = s->space;
		}
		s->ts_next->ts_prev = s->ts_prev;
	}
	if (s->ts_prev != 0)
		s->ts_prev->ts_next = s->ts_next;
	if (tsnext == s)
		tsnext = s->ts_next;
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

	for (s = first_voice->sym; s != 0; s = s->ts_next) {
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
				if (s2->as.flags & ABC_F_WORD_END)
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
				if (s2->as.flags & ABC_F_WORD_END)
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

/* -- get the weight of a symbol -- */
static int get_weight(struct SYMBOL *s)
{
	int w;
static signed char w_tb[15] = {	/* !! index = symbol type !! */
	0,
	8,	/* 1- note */
	-1,	/* 2- rest = 8 - space = 2 */
	4,	/* 3- bar */
	3,	/* 4- clef */
	6,	/* 5- timesig */
	5,	/* 6- keysig */
	0,	/* 7- tempo */
	0,	/* 8- staves */
	8,	/* 9- mrest */
	0,	/* 10- part */
	7,	/* 11- grace */
	-2,	/* 12- fmtchg = 0 - stbrk = 1 */
	0,	/* 13- tuplet */
	0	/* 14- whistle */
};

	if ((w = w_tb[s->type]) >= 0)
		return w;
	if (w == -1)		/* rest */
		return s->dur != 0 ? 8 : 2;
	return s->u != STBRK ? 0 : 1;		/* fmtchg */
}

/* -- insert a clef change (treble or bass) before a symbol -- */
static void insert_clef(struct SYMBOL *s,
			int clef_type)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s2, *new_s;
	int staff;

	/* don't insert the clef between two bars */
	if (s->type == BAR && s->prev->type == BAR
/*	    && s->time == s->prev->time */
			)
		s = s->prev;

	/* search the main voice of the staff */
	staff = s->staff;
	p_voice = &voice_tb[s->voice];
	s2 = s;
	if (p_voice->second) {
		for (p_voice = first_voice; ; p_voice = p_voice->next)
			if (p_voice->staff == staff
			    /* && !p_voice->second */ )
				break;
		while (!(s2->sflags & S_SEQST))
			s2 = s2->ts_prev;
		for (; s2 != 0; s2 = s2->ts_next) {
			if (s2->voice == p_voice - voice_tb)
				break;
		}
		if (s2 == 0) {
			for (s2 = p_voice->sym; s2->next != 0; s2 = s2->next)
				;
		}
	}

	/* create the symbol */
	p_voice->last_symbol = s2->prev;
	new_s = sym_add(p_voice, CLEF);
	new_s->next = s2;
	s2->prev = new_s;

	new_s->as.u.clef.type = clef_type;
	new_s->as.u.clef.line = clef_type == TREBLE ? 2 : 4;
	new_s->as.u.clef.stafflines = -1;
	new_s->staff = staff;
	new_s->u = 1;		/* small clef */
	new_s->time = s->time;

	/* link in time */
	while (!(s->sflags & S_SEQST))
		s = s->ts_prev;
	if (s->time != new_s->time
	    || get_weight(s) != get_weight(new_s))
		new_s->sflags |= S_SEQST;
	new_s->ts_prev = s->ts_prev;
	new_s->ts_prev->ts_next = new_s;
	new_s->ts_next = s;
	s->ts_prev = new_s;
}

/* -- define the clef for a staff -- */
/* this function is called only once for the whole tune */
static void set_clef(int staff)
{
	struct SYMBOL *s, *last_chg;
	int clef_type, min, max, time;

	/* get the max and min pitches */
	min = max = 16;			/* 'C' */
	for (s = first_voice->sym; s != 0; s = s->ts_next) {
		if (s->staff != staff || s->type != NOTE)
			continue;
		if (s->pits[0] < min)
			min = s->pits[0];
		else if (s->pits[s->nhd] > max)
			max = s->pits[s->nhd];
	}

	staff_tb[staff].clef.type = TREBLE;
	staff_tb[staff].clef.line = 2;
	if (min >= 13)			/* all upper than 'G,' --> treble clef */
		return;
	if (max <= 19) {		/* all lower than 'F' --> bass clef */
		staff_tb[staff].clef.type = BASS;
		staff_tb[staff].clef.line = 4;
		return;
	}

	/* set clef changes */
	clef_type = TREBLE;
	last_chg = 0;
	for (s = first_voice->sym; s != 0; s = s->ts_next) {
		struct SYMBOL *s2, *s3, *s4;

		if (s->staff != staff || s->type != NOTE)
			continue;

		/* check if a clef change may occur */
		time = s->time;
		if (clef_type == TREBLE) {
			if (s->pits[0] > 12		/* F, */
			    || s->pits[s->nhd] > 20)	/* G */
				continue;
			s2 = s->ts_prev;
			if (s2->time == time
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
			if (s2->time == time
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
					staff_tb[staff].clef.line = 4;
				} else {
					clef_type = TREBLE;
					staff_tb[staff].clef.line = 2;
				}
				staff_tb[staff].clef.type = clef_type;
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
	}
}

/* -- sort the symbols by time -- */
/* this function is called only once for the whole tune */
static void def_tssym(void)
{
	struct SYMBOL *s, *t, *prev_sym;
	int time, bars, seq, fl, w;
	struct VOICE_S *p_voice;

	/* initialize, skipping the symbols set by init_music_line() */
	prev_sym = first_voice->sym;
	for (;;) {
		voice_tb[prev_sym->voice].s_anc = prev_sym->next;
		if (prev_sym->ts_next == 0)
			break;
		prev_sym = prev_sym->ts_next;
	}
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next)
		p_voice->selected = 0;
	bars = nbar- 1;			/* (for errors - KO if not -j ) */
	for (;;) {

		/* search the closest next time/sequence */
		seq = time = (unsigned) ~0 >> 1;	/* max int */
		for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
			if ((s = p_voice->s_anc) == 0
			    || s->time > time)
				continue;
			w = get_weight(s);
			if (s->time < time) {
				time = s->time;
				seq = w;
			} else if (seq > w)
				seq = w;
		}
		if (seq > 127)
			break;		/* echu (finished) */

		/* warn about incorrect number of notes / measures */
		fl = 0;
		for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
			if ((s = p_voice->s_anc) == 0
			    || s->time != time
			    || get_weight(s) != seq)
				continue;
			p_voice->selected = 1;
			switch (s->type) {
			case BAR:
				if (s->as.u.bar.type != B_OBRA
				    && s->as.u.bar.type != B_CBRA)
					fl |= 1;
				break;
			case MREST:
				fl |= 2;
				break;
			}
		}

		if (fl & 1) {		/* if any bar */
			int repair = 0;

			bars++;
			for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
				if ((s = p_voice->s_anc) == 0)
					continue;
				if (s->type == BAR && s->time == time)
					continue;
				if (s->prev == 0
				    || s->prev->time + s->prev->dur
						!= s->time /* normal time skip */
				    || time == 0)	/* vover + bar at start of tune */
					continue;
				for (t = s;
				     t->prev != 0 && t->prev->time == time;
				     t = t->prev)
					;
				while (t != 0 && t->time == time) {
					if (t->type == BAR)
						break;
					t = t->next;
				}
				if (t == 0 || t->time == time)
					continue;
				error(1, s,
				      "Bad bar at measure %d for voice %s",
				      bars, p_voice->name);
				while (s != 0 && s->type != BAR)
					s = s->next;
				if (s != 0 && s->time > time)
					repair = 1;
				break;
			}

			if (repair) {
				for (p_voice = first_voice;
				     p_voice;
				     p_voice = p_voice->next) {
					if ((t = p_voice->s_anc) == 0
					    || t->type != BAR)
						continue;
					time = s->time;
					for (; t != 0; t = t->next) {
						t->time = time;
						time += t->dur;
					}
				}
				bars--;
				continue;
			}
		}
		if ((fl & 2)		/* if any mrest alone in a staff */
		    && first_voice->next != 0) {	/* and many voices */
			fl = 0;
			for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
				if (!p_voice->selected)
					continue;
				if ((s = p_voice->s_anc) == 0)
					continue;
				if (s->type != MREST) {
					fl = 1;
					break;
				}
			}
			if (fl) {	/* if not only multi measure rests */
				for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
					int i, dt, time2;

					if ((s = p_voice->s_anc) == 0
					    || s->type != MREST)
						continue;
					p_voice->last_symbol = s->prev;
					i = s->as.u.bar.len;
					dt = s->dur / i;
					time2 = s->time;
					t = sym_add(p_voice, REST);
					t->as.type = ABC_T_REST;
					t->as.linenum = s->as.linenum;
					t->as.colnum = s->as.colnum;
					t->dur = t->as.u.note.lens[0] = dt;
					t->head = H_FULL;
					t->nflags = -2;
					t->time = time2;
					if (s->as.u.bar.dc.n > 0)
						memcpy(&t->as.u.note.dc,
							&s->as.u.bar.dc,
							sizeof t->as.u.note.dc);
					p_voice->s_anc = t;
					time2 += dt;
					while (--i > 0) {
						t = sym_add(p_voice, BAR);
						t->as.type = ABC_T_BAR;
						t->as.u.bar.type = B_SINGLE;
						t->as.linenum = s->as.linenum;
						t->as.colnum = s->as.colnum;
						t->time = time2;
						t = sym_add(p_voice, REST);
						t->as.type = ABC_T_REST;
						t->as.linenum = s->as.linenum;
						t->as.colnum = s->as.colnum;
						t->dur = t->as.u.note.lens[0] = dt;
						t->head = H_FULL;
						t->nflags = -2;
						t->time = time2;
						time2 += dt;
					}
					t = p_voice->last_symbol;
					if ((t->next = s->next) != 0)
						s->next->prev = t;
				}
			}
		}

		/* set the time linkage */
		w = get_weight(prev_sym);
		fl = w != 0;	/* don't start a new seq if undrawn symbols */
		for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
			if (!p_voice->selected)
				continue;
			p_voice->selected = 0;
			s = p_voice->s_anc;
			if (fl) {
				fl = 0;
				s->sflags |= S_SEQST;
			}
			s->ts_prev = prev_sym;
			prev_sym->ts_next = s;
			prev_sym = s;
			p_voice->s_anc = s->next;
		}
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
			if (s2->as.u.clef.type < 0 || s2->u)
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
		for (s2 = first_voice->sym; s2 != s; s2 = s2->ts_next) {
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
	if (s->type == BAR) {
		space *= 0.8;
		return space;
	}
	if (s->dur == 0)
		return space;

	/* reduce spacing within a beam */
	if ((s->sflags & S_WORD_ST) == 0)
		space *= fnnp;

	/* decrease/increase spacing if stems in opposite directions */
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
	s = first_voice->sym;
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
	int done;

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
		for (s = s->ts_next; s != 0; s = s->ts_next) {
			if (s->shrink != 0) {
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
			/* fall thru */
		case CLEF:
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
	for (s = first_voice->sym; ; s = s->ts_next) {
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
		if (voice_tb[s->voice].second
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
		if (s->as.u.clef.octave > 0)
			s->ymx += 12;
		if (s->y < 0)
			s->ymn += s->y;
		if (s->as.u.clef.octave < 0)
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

	for (s = first_voice->sym; s != 0; s = s->ts_next) {
		struct SYMBOL *g;
		int delta, np, m, pav;

		if (s->type == FMTCHG && s->u == REPEAT)
			s = set_repeat(s, 0);
		staff = s->staff;
		switch (s->type) {
		case CLEF:
			set_yval(s);
			if (voice_tb[s->voice].second
			    || s->as.u.clef.type < 0) {
				if (s->prev == 0)
					break;
				g = s->ts_prev;
				delsym(s);
				s = g;
				break;
			}
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
	struct SYMBOL *s, *t;
	int i, j, staff, rvoice, voice;
	struct {
		int nvoice;
		struct {
			int voice;
			short ymn;
			short ymx;
		} st[4];		/* (no more than 4 voices per staff) */
	} stb[MAXSTAFF];
	struct {
		short range;
		signed char st1, st2;
	} vtb[MAXVOICE];
	struct VOICE_S *p_voice;

	for (p_voice = first_voice, rvoice = 0;
	     p_voice != 0;
	     p_voice = p_voice->next, rvoice++)
		vtb[p_voice - voice_tb].range = rvoice;

	s = first_voice->sym;
	while (s != 0) {
		for (staff = nstaff; staff >= 0; staff--) {
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
			    || (t->as.flags & ABC_F_INVIS))
				continue;
			staff = t->staff;
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
			rvoice = vtb[voice].range;
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
				p_voice = &voice_tb[voice];
				if (p_voice->floating) {
					if (s->staff == p_voice->staff)
						s->multi = -1;
					else	s->multi = 1;
				}
				continue;
			}
			rvoice = vtb[voice].range;
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
					rvoice = vtb[t->voice].range;
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

/* -- set the staves and stems when multivoice -- */
/* this function is called only once per tune */
static void set_global(void)
{
	int staff;
	struct SYMBOL *s;
	struct VOICE_S *p_voice;

	/* adjust the pitches if old abc2ps behaviour of clef definition */
	if (cfmt.abc2pscompat) {
		int old_behaviour, done, max, min;

		old_behaviour = done = 0;
		for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
			if (!p_voice->forced_clef
			    || p_voice->clef.type == PERC)
				continue;

			/* search if any pitch is too high for the clef */
			max = 100;
			min = -100;
			for (s = p_voice->sym; s != 0; s = s->next) {
				switch (s->type) {
				case CLEF:
					if (s->as.u.clef.type < 0)
						continue;
					if (!s->as.u.clef.check_pitch) {
						max = 100;
						min = -100;
						continue;
					}
					switch (s->as.u.clef.type) {
					case TREBLE:
					case PERC:
						max = 100;
						min = -100;
						break;
					case ALTO:
						max = 25;	/* e */
						min = 14;	/* G, */
						break;
					case BASS:
						max = 21;	/* A */
						min = 10;	/* C, */
						break;
					}
					continue;
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
				int delta, i;
				struct SYMBOL *g;

				if (!p_voice->forced_clef
				    || p_voice->clef.type == PERC)
					continue;
				delta = 0;
				for (s = p_voice->sym; s != 0; s = s->next) {
					switch (s->type) {
					case CLEF:
						if (s->as.u.clef.type < 0)
							continue;
						if (!s->as.u.clef.check_pitch)
							delta = 0;
						else switch (s->as.u.clef.type) {
							default: delta = 0; break;
							case ALTO: delta = -7; break;
							case BASS: delta = -14; break;
						}
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

	/* set a pitch for all symbols and the start/end of words */
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
				if (s->prev->type != NOTE) {
					s->prev->pits[0] = (s->prev->pits[0]
							    + pitch) / 2;
				}
			} else	s->pits[0] = pitch;
		}
		if (lastnote != 0)
			lastnote->as.flags |= ABC_F_WORD_END;
	}

	/* sort the symbols by time */
	def_tssym();

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

	/* set the starting clefs and adjust the note pitches */
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		memcpy(&p_voice->sym->as.u.clef,
		       &staff_tb[p_voice->staff].clef,
		       sizeof p_voice->sym->as.u.clef);
		staff_tb[p_voice->staff].sep = p_voice->sep;
		staff_tb[p_voice->staff].maxsep = p_voice->maxsep;
	}
	set_pitch();
}

/* -- return the left indentation of the staves -- */
static float set_indent(void)
{
	int staff;
	float w, maxw;
	struct VOICE_S *p_voice;
	char *p, *q;

	maxw = 0;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
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
			if ((staff_tb[staff].flags[0]
					& (OPEN_BRACE | OPEN_BRACKET))
			    && w == 0)
				w = 10;
			else if (staff_tb[staff].flags[1]
					& (OPEN_BRACE | OPEN_BRACKET)) {
				w = 20;
				break;
			}
		}
		maxw += 4 * cwid(' ') * cfmt.font_tb[VOICEFONT].swfac + w;
	}
	if (insert_meter & 2) {			/* if first music line */
		maxw += cfmt.indent;
		insert_meter &= ~2;
	}
	return maxw;
}

/* -- set the y offset of the staves and return the whole height -- */
static float set_staff(void)
{
	struct SYMBOL *s;
	int staff, any_part, any_tempo;
	float y, staffsep, dy, maxsep, mbot, scale;
	struct {
		float mtop;
		int not_empty;
	} delta_tb[MAXSTAFF], *p_delta;

	/* compute the distance between the staves */
	memset(delta_tb, 0, sizeof delta_tb);
	if (cfmt.staffnonote) {
		for (s = first_voice->sym; s != 0; s = s->ts_next) {
			switch (s->type) {
			case NOTE:
			case REST:
			case BAR:
			case MREST:
			case GRACE:
				delta_tb[s->staff].not_empty = 1;
				break;
			}
		}
	} else {
		for (s = first_voice->sym; s != 0; s = s->ts_next) {
			switch (s->type) {
			case NOTE:
/*			case REST: */
				if (!(s->as.flags & ABC_F_INVIS))
					delta_tb[s->staff].not_empty = 1;
				break;
			}
		}
	}

	/* if a system brace has empty and non empty voice, keep all staves */
	for (staff = 0; staff <= nstaff; staff++) {
		int i, empty_fl;

		if (!(staff_tb[staff].flags[0] & OPEN_BRACE)
		     && !(staff_tb[staff].flags[1] & OPEN_BRACE))
			continue;
		empty_fl = 0;
		i = staff;
		while (staff <= nstaff) {
			if (delta_tb[staff].not_empty)
				empty_fl |= 1;
			else	empty_fl |= 2;
			if ((staff_tb[staff].flags[0] & CLOSE_BRACE)
			    || (staff_tb[staff].flags[1] & CLOSE_BRACE))
				break;
			staff++;
		}
		if (empty_fl == 3) {	/* if empty and not empty staves */
/*fixme: add measure bars on empty main voices*/
			while (i <= staff)
				delta_tb[i++].not_empty = 1;
		}
	}
	mbot = 0;
	{
		int i;
		float v;

		if (delta_tb[0].not_empty) {
			p_delta = &delta_tb[0];
			for (i = 0; i < YSTEP; i++) {
				v = staff_tb[0].top[i];
				if (p_delta->mtop < v)
					p_delta->mtop = v;
			}
		} else	staff_tb[0].empty = 1;
		for (staff = 1; staff <= nstaff; staff++) {
			if (!delta_tb[staff].not_empty) {
				staff_tb[staff].empty = 1;
				continue;
			}
			p_delta = &delta_tb[staff];
			for (i = 0; i < YSTEP; i++) {
				v = staff_tb[staff].top[i]
					- staff_tb[staff - 1].bot[i];
				if (p_delta->mtop < v)
					p_delta->mtop = v;
			}
		}
		for (staff = nstaff; staff >= 0; staff--) {
			if (!staff_tb[staff].empty) {
				p_delta = &delta_tb[staff];
				for (i = 0; i < YSTEP; i++) {
					v = staff_tb[staff].bot[i];
					if (mbot > v)
						mbot = v;
				}
				break;
			}
		}
	}

	/* handle the empty staves and their tablatures */
	{
		struct VOICE_S *p_voice;

		for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
			staff = p_voice->staff;
			if (!staff_tb[staff].empty)
				continue;
			if (p_voice->tabhead == 0) {
				staff_tb[staff].topbar = 0;
				continue;
			}
			delta_tb[staff].mtop += p_voice->tabha;
			if (staff < nstaff)
				delta_tb[staff + 1].mtop += p_voice->tabhu;
			else	mbot -= p_voice->tabhu;
			delta_tb[staff].not_empty = 1;
		}
	}

	/* scan the first voice to see if any part or tempo */
	any_part = any_tempo = 0;
	for (s = first_voice->sym->next; s != 0; s = s->next) {
		switch (s->type) {
		case PART:
			any_part = 1;
			break;
		case TEMPO:
			any_tempo = 1;
			break;
		default:
			continue;
		}
		if (any_part && any_tempo)
			break;
	}

	/* draw the parts and tempo indications if any */
	if (any_part || any_tempo) {
		dy = delta_tb[0].mtop * staff_tb[0].clef.staffscale;
		if (dy == 0)		/* first staff not displayed */
			dy = 24 + 14;
		dy = draw_partempo(dy, any_part, any_tempo);
	} else	dy = 0;

	/* set the staff offsets */
	staffsep = cfmt.staffsep * 0.5;
	maxsep = cfmt.maxstaffsep * 0.5;
	y = 0;
	for (staff = 0, p_delta = delta_tb;
	     staff <= nstaff;
	     staff++, p_delta++) {
		scale = staff_tb[staff].clef.staffscale;
		dy += p_delta->mtop * scale;
		if (!staff_tb[staff].empty) {
			staffsep += staff_tb[staff].topbar;
			if (dy < staffsep)
				dy = staffsep;
			maxsep += staff_tb[staff].topbar;
			if (dy > maxsep)
				dy = maxsep;
		}
		y += dy;
		staff_tb[staff].y = -y;
/*fixme: handle tablature?*/
		PUT2("/y%d{%.1f add}def\n", staff, -y);
		if (scale != 1) {
			PUT3("/scst%d{gsave 0 %.2f translate %.2f dup scale}def\n",
			     staff, -y, scale);
		}
		if (staff_tb[staff].sep != 0)
			staffsep = staff_tb[staff].sep;
		else	staffsep = cfmt.sysstaffsep;
		if (staff_tb[staff].maxsep != 0)
			maxsep = staff_tb[staff].maxsep;
		else	maxsep = cfmt.maxsysstaffsep;
		dy = 0;
	}
	if (mbot == 0) {
		for (staff = nstaff; staff >= 0; staff--) {
			if (delta_tb[staff].not_empty)
				break;
		}
		if (staff < 0)		/* no symbol in this system */
			return y;
	}
	dy = -(mbot * staff_tb[nstaff].clef.staffscale);
	staffsep = cfmt.staffsep * 0.5;
	if (dy < staffsep)
		dy = staffsep;
	maxsep = cfmt.maxstaffsep * 0.5;
	if (dy > maxsep)
		dy = maxsep;
	y += dy;
	if (y > cfmt.maxstaffsep)
		y = cfmt.maxstaffsep;

	/* set the height of the measure bars */
	dy = 0;
	for (staff = 0; staff <= nstaff; staff++) {
		if (staff_tb[staff].empty)
			staff_tb[staff].bar_height = 0;
		else {
			if (dy == 0)
				dy = staff_tb[staff].y + staff_tb[staff].topbar
					* staff_tb[staff].clef.staffscale;
			staff_tb[staff].bar_height = dy
				- staff_tb[staff].y - staff_tb[staff].botbar
					* staff_tb[staff].clef.staffscale;
		}
		if (staff_tb[staff].flags[0] & STOP_BAR)
			dy = 0;
		else	dy = staff_tb[staff].y + staff_tb[staff].botbar
				* staff_tb[staff].clef.staffscale;
	}

	/* return the whole height */
	return y;
}

/* -- decide on beams and on stem directions -- */
/* this routine is called only once per tune */
static void set_beams(struct SYMBOL *sym)
{
	struct SYMBOL *s, *t, *prev_note;
	int beam, laststem, lasty;

	beam = 0;
	laststem = -1;
	lasty = 0;
	prev_note = 0;
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
		prev_note = s;
	}
}

/* -- shift the notes when voices overlap -- */
/* this routine is called only once per tune */
static void set_overlap(void)
{
	struct SYMBOL *s, *s1, *s2;
	int d, i1, i2, m, sd1, sd2, t;
	float d1, d2, dy1, dy2, noteshift;

	for (s = first_voice->sym; s != 0; s = s->ts_next) {
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

	for (s = first_voice->sym; s != 0; s = s->ts_next) {
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
	struct SYMBOL *s, *s2;
	struct VOICE_S *p_voice;
	int i;

	/* reset the staves */
	for (i = nstaff; i >= 0; i--) {
		staff_tb[i].empty = 0;
		staff_tb[i].y = 0;
	}

	/* search the next end of line,
	 * set the repeat measures and remove some dble bars */
	for (s = first_voice->sym; s != 0; s = s->ts_next) {
		if (s->sflags & S_NL)
			break;
		if (s->type == FMTCHG && s->u == REPEAT)
			s = set_repeat(s, 1);
		if (s->next != 0
		    && s->next->type == BAR
		    && !(s->next->sflags & S_NL)
		    && s->next->next != 0
		    && s->next->next->type == BAR
		    && !(s->next->next->sflags & S_NL)
		    && !s->next->next->as.u.bar.repeat_bar
		    && (s->next->as.text == 0
			|| s->next->next->as.text == 0)
		    && (s->next->as.u.bar.dc.n == 0
			|| s->next->next->as.u.bar.dc.n == 0)) {
			s2 = 0;
			if ((s->next->as.u.bar.type == B_SINGLE
			     || s->next->as.u.bar.type == B_DOUBLE)
			    && (s->next->next->as.u.bar.type & 0xf0)) {
				s2 = s->next->next;
				if (s->next->as.u.bar.dc.n != 0)
					memcpy(&s2->as.u.bar.dc,
						&s->next->as.u.bar.dc,
						sizeof s2->as.u.bar.dc);
				memcpy(&s->next->as.u.bar,
					&s2->as.u.bar,
					sizeof s->next->as.u.bar);
				if (s2->as.text != 0)
					s->next->as.text = s2->as.text;
			}
			if (s2 != 0)
				delsym(s2);
		}
	}

	/* if last line, do nothing more */
	if (s == 0) {
		tsnext = 0;
		return;
	}
	s->sflags &= ~S_NL;

	tsnext = s;
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
				if (s2->type == NOTE) {
					s2->as.flags |= ABC_F_WORD_END;
					break;
				}
				if (s2->type == BAR)
					break;
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
	}
}

/* -- Set the characteristics of the glue between symbols, then
 *	position the symbols along the staff. If staff is overfull,
 *	only does symbols which fit in. -- */
static void set_sym_glue(float width)
{
	struct SYMBOL *s;
	float alfa0, beta0, alfa, beta;
	int some_grace;
	float xmin, x, xmax, spafac;

	/* calculate the whole space of the symbols */
	some_grace = 0;
	s = first_voice->sym;
	xmin = x = xmax = 0;
	for (;;) {
		if (s->type == GRACE)
			some_grace = 1;
		if (s->shrink != 0) {
			float space;

			xmin += s->shrink;
			if ((space = s->space) == 0)
				space = s->shrink;
			x += space;
			xmax += space * 1.8;
		}
		if (s->ts_next == 0)
			break;
		s = s->ts_next;
	}

	/* set max shrink and stretch */
	if (!cfmt.continueall) {
		alfa0 = ALFA_X;
		beta0 = BETA_X;
	} else {
		alfa0 = cfmt.maxshrink;
		beta0 = BETA_C;
	}

	/* memorize the glue for the last music line */
	alfa = beta = 0;
	if (x - width >= 0)
		alfa = (x - width) / (x - xmin);	/* shrink */
	else {
		beta = (width - x) / (xmax - x);	/* stretch */
		if (beta > beta0) {
			if (!cfmt.continueall) {
				error(0, s,
				      "Line underfull (%.0fpt of %.0fpt)",
					beta0 * xmax + (1 - beta0) * x,
					width);
			}
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
	if (x - width >= 0) {
		alfa_last = (x - width) / (x - xmin);
		beta_last = 0;
	} else {
		alfa_last = 0;
		beta_last = (width - x) / (xmax - x);
	}

	spafac = width / x;			/* space expansion factor */

	/* define the x offsets for all symbols */
	x = xmax = 0;
	s = first_voice->sym;
	for (; s != 0; ) {
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
	if (x - width >= 0)
		alfa = (x - width) / (x - xmin);	/* shrink */
	else {
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
	s = first_voice->sym;
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
		for (s = first_voice->sym; s != 0; s = s->ts_next) {
			struct SYMBOL *g;

			if ((g = s->grace) == 0)
				continue;
			x = s->x - s->wl + (cfmt.gracespace >> 16) * 0.1;
			for ( ; g != 0; g = g->next)
				g->x += x;
		}
	}
}

/* -- init a voice with clef, meter, key -- */
/* this routine is called starting a music piece,
 * it is called later for each new music line */
static void init_music_line(void)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s, *sym;
	struct STAFF_S *p_staff;
	int fl;

	fl = tsnext == 0 ? 0 : tsnext->time;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		p_voice->s_anc = p_voice->sym;
		p_voice->sym = 0;
		p_voice->time = fl;
	}
	sym = 0;

	/* add clef */
	fl = 1;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		p_staff = &staff_tb[p_voice->staff];
		if ((s = p_voice->s_anc) != 0 && s->type == CLEF
		    && !p_voice->second && p_voice->staff == s->staff) {
			int stafflines;
			float staffscale;

			if ((stafflines = s->as.u.clef.stafflines) < 0)
				stafflines = p_staff->clef.stafflines;
			if ((staffscale = s->as.u.clef.staffscale) == 0)
				staffscale = p_staff->clef.staffscale;
			if (s->as.u.clef.type >= 0)
				memcpy(&p_staff->clef,
					&s->as.u.clef,
					sizeof s->as.u.clef);
			p_staff->clef.stafflines = stafflines;
			p_staff->clef.staffscale = staffscale;
			p_voice->s_anc = s->next;
			delsym(s);
		}
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
		s = sym_add(p_voice, CLEF);
		memcpy(&p_voice->clef, &p_staff->clef,
		       sizeof p_voice->clef);
		memcpy(&s->as.u.clef, &p_voice->clef, sizeof s->as.u.clef);
		if (p_voice->second || s->as.u.clef.invis)
			s->as.flags |= ABC_F_INVIS;
		set_yval(s);
		if ((s->ts_prev = sym) != 0)		/* link in time */
			sym->ts_next = s;
		sym = s;
		if (fl) {
			fl = 0;
			s->sflags |= S_SEQST;
		}
	}

	/* add keysig */
	fl = 1;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		if (p_voice->second)
			continue;
		if ((s = p_voice->s_anc) != 0 && s->type == KEYSIG) {
			memcpy(&p_voice->key, &s->as.u.key,
				 sizeof s->as.u.key);
			p_voice->s_anc = s->next;
			delsym(s);
		}
		if (p_voice->key.sf != 0 || p_voice->key.nacc != 0) {
			s = sym_add(p_voice, KEYSIG);
			memcpy(&s->as.u.key, &p_voice->key, sizeof s->as.u.key);
			if (s->as.u.key.bagpipe && s->as.u.key.sf == 2)	/* K:Hp */
				s->u = 3;			/* --> G natural */
			set_yval(s);
			s->ts_prev = sym;
			sym->ts_next = s;
			sym = s;
			if (fl) {
				fl = 0;
				s->sflags |= S_SEQST;
			}
		}
	}

	/* add time signature if needed */
	if (insert_meter) {
		fl = 1;
		for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
			if (p_voice->second)
				continue;
			if (p_voice->meter.nmeter != 0) {	/* != M:none */
				s = sym_add(p_voice, TIMESIG);
				memcpy(&s->as.u.meter, &p_voice->meter,
				       sizeof s->as.u.meter);
				set_yval(s);
				s->ts_prev = sym;
				sym->ts_next = s;
				sym = s;
				if (fl) {
					fl = 0;
					s->sflags |= S_SEQST;
				}
			}
		}
	}

	/* add tempo if any (only one) */
	if ((s = info['Q' - 'A']) != 0) {
		info['Q' - 'A'] = 0;
		p_voice = first_voice;
		memset((&s->as) + 1, 0,
		       sizeof (struct SYMBOL) - sizeof (struct abcsym));
		p_voice->last_symbol->next = s;
		s->prev = p_voice->last_symbol;
		p_voice->last_symbol = s;
		s->voice = p_voice - voice_tb;
		s->staff = p_voice->staff;
		s->type = TEMPO;
		set_yval(s);
		s->ts_prev = sym;
		sym->ts_next = s;
		sym = s;
		s->sflags |= S_SEQST;
	}

	/* add bar if needed */
	fl = 1;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		int i;

		if (p_voice->second)
			continue;
		if (p_voice->bar_start == 0)
			continue;
		i = 4;
		if (p_voice->bar_text == 0	/* if repeat continuation */
		    && p_voice->bar_start == B_OBRA) {
			for (s = sym; s != 0; s = s->next) {	/* search the end of repeat */
				if (s->type == BAR) {
					if ((s->as.u.bar.type & 0xf0)	/* if complex bar */
					    || s->as.u.bar.type == B_CBRA
					    || s->as.u.bar.repeat_bar)
						break;
					if (--i < 0)
						break;
				}
			}
			if (s == 0 || sym == 0)
				i = -1;
			if (i >= 0 && sym->time == s->time)
				i = -1;		/* no note */
		}
		if (i >= 0) {
			s = sym_add(p_voice, BAR);
			s->as.u.bar.type = p_voice->bar_start & 0x7f;
			if (p_voice->bar_start & 0x80)
				s->as.flags |= ABC_F_INVIS;
			s->as.text = p_voice->bar_text;
			s->as.u.bar.repeat_bar = p_voice->bar_repeat;
			set_yval(s);
/*fixme: insert after tempo, staves and part?*/
			s->ts_prev = sym;
			sym->ts_next = s;
			sym = s;
			if (fl) {
				fl = 0;
				s->sflags |= S_SEQST;
			}
		}
		p_voice->bar_start = 0;
		p_voice->bar_repeat = 0;
		p_voice->bar_text = 0;
	}

	/* remember where to put the whistle */
/*fixme:should be before the first note*/
	fl = 1;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		if (p_voice->whistle) {
			sym_add(p_voice, WHISTLE);
			set_yval(s);
			s->ts_prev = sym;
			sym->ts_next = s;
			sym = s;
			if (fl) {
				fl = 0;
				s->sflags |= S_SEQST;
				if (tsnext != 0) {
					tsnext->sflags &= ~S_SEQST;
					tsnext->shrink = 0;
				}
			}
		}
	}

	/* link the new symbols to the other symbols in each voice */
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		s = p_voice->last_symbol;
		if ((s->next = p_voice->s_anc) != 0)
			p_voice->s_anc->prev = s;
	}

	/* if initialization of a new music line, set the time linkage
	 * and compute the spacing */
	if ((s = tsnext) != 0) {
		sym->ts_next = s;
		s->ts_prev = sym;
		tsnext = first_voice->sym;
		for (s = s->ts_next ; s != 0; s = s->ts_next)
			if (s->shrink != 0)
				break;
		if (s != 0)
			s->ts_prev->ts_next = 0;
		set_allsymwidth();
		if (s != 0)
			s->ts_prev->ts_next = s;
	}
}

/* -- initialize a new music line -- */
static void cut_symbols(void)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s, *s1;

	clrarena(3);

	/* set the first symbol of each voice */
	if ((s1 = tsnext) == 0)
		return;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		int voice;

		p_voice->sym = 0;		/* may have no symbol */
		voice = p_voice - voice_tb;
		for (s = s1; s != 0; s = s->ts_next) {
			if (s->voice == voice) {
				p_voice->sym = s;
				s->prev = 0;
				break;
			}
		}
	}

	/* add the first symbols of the line */
	init_music_line();
	insert_meter = 0;
}

/* -- output for parsed symbol list -- */
void output_music(void)
{
	struct VOICE_S *p_voice;
	int voice;
	float lwidth, indent;

	for (p_voice = first_voice; p_voice; p_voice = p_voice->next)
		if (p_voice->sym != 0)
			break;
	if (p_voice == 0)
		return;		/* no symbol at all */

	lvlarena(2);

	voice_dup();	/* duplicate the voices appearing in many staves */

	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		p_voice->cstaff = p_voice->staff;
		if (!p_voice->second)
			memcpy(&staff_tb[ p_voice->staff].clef,
				&p_voice->clef,
				sizeof p_voice->clef);
	}
	init_music_line();
	insert_meter &= ~1;

	alfa_last = 0.1;
	beta_last = 0;

	check_buffer();	/* dump buffer if not enough space for a music line */

	set_global();			/* set global characteristics */
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
	clrarena(3);
	lvlarena(3);
	for (;;) {		/* loop over pieces of line for output */
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
		line_height = set_staff();
		draw_vname(indent);
		PUT0("dlsym\n");
		draw_all_symb();
		draw_all_deco();
		if (showerror > 1) {
			struct SYMBOL *s;

			showerror = 1;
			for (s = first_voice->sym; s != 0; s = s->ts_next) {
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
		if (tsnext == 0)
			break;
		cut_symbols();
		buffer_eob();
		indent = set_indent();
	}
	lvlarena(2);

	/* reset the parser */
	for (voice = MAXVOICE; --voice >= 0; ) {
		voice_tb[voice].sym = 0;
		voice_tb[voice].time = 0;
		voice_tb[voice].have_ly = 0;
	}
	outft = -1;
}

/* -- reset the generator -- */
void reset_gen(void)
{
	insert_meter = 3;
}
