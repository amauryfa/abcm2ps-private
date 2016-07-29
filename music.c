/*
 * Music generator.
 *
 * This file is part of abcm2ps.
 *
 * Copyright (C) 1998-2011 Jean-François Moine
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

#include <string.h>
#include <ctype.h>

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
static int smallest_duration;

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
	if (s->next == 0) {
		if (s->extra != 0) {
			s->type = FMTCHG;
			s->u = -1;
			return;
		}
	} else {
		s->next->prev = s->prev;
		if (s->extra != 0) {
			struct SYMBOL *g;

			if ((g = s->next->extra) == 0)
				s->next->extra = s->extra;
			else {
				for (; g->next != 0; g = g->next)
					;
				g->next = s->extra;
			}
		}
	}
	if (s->prev != 0)
		s->prev->next = s->next;
	if (s->ts_next != 0) {
		if ((s->sflags & S_SEQST)
		    && !(s->ts_next->sflags & S_SEQST)) {
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
	if (s->as.type != s2->as.type)
		return 0;
	if (s->as.type == ABC_T_NOTE
	    && ((s2->sflags ^ s->sflags) & (S_BEAM_ST | S_BEAM_END)))
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
	int nhd, nhd2, type;

	nhd = s->nhd;
	s2 = s->ts_next;
	nhd2 = s2->nhd;
	s2->extra = 0;
	if (s->as.type != s2->as.type) {	/* if note and rest */
		if (s2->as.type == ABC_T_REST)
			goto delsym2;
		s->as.type = ABC_T_NOTE;	/* copy the note into the rest */
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

	/* force the tie directions */
	type = s->as.u.note.ti1[0];
	if ((type & 0x03) == SL_AUTO)
		s->as.u.note.ti1[0] = SL_BELOW | (type & ~SL_DOTTED);
	type = s->as.u.note.ti1[nhd];
	if ((type & 0x03) == SL_AUTO)
		s->as.u.note.ti1[nhd] = SL_ABOVE | (type & ~SL_DOTTED);
delsym2:
/*fixme: KO if gchord/annot in both symbols*/
	if (s2->as.text != 0 && s->as.text == 0) {
		s->as.text = s2->as.text;
		s->gcf = s2->gcf;
		s->anf = s2->anf;
	}
	delsym(s2);			/* remove the next symbol */
}

/* -- try to combine voices */
static void combine_voices(void)
{
	struct SYMBOL *s, *s2, *g;
	int i, r;

	for (s = tsfirst; s->ts_next != 0; s = s->ts_next) {
		if ((g = s->extra) != 0 && (s->sflags & S_IN_TUPLET)) {
			r = 0;
			for ( ; g != 0; g = g->next) {
				if (g->type == TUPLET
				    && g->as.u.tuplet.r_plet > r)
					r = g->as.u.tuplet.r_plet;
			}
			i = r;
			for (s2 = s; s2 != 0; s2 = s2->next) {
				if (s2->ts_next == 0)
					break;
				if (s2->type != NOTEREST)
					continue;
				if (!may_combine(s2))
					break;
				if (--i <= 0)
					break;
			}
			if (i > 0) {
				while (--r > 0)
					s = s->next;
				continue;
			}
			r = s->as.u.tuplet.r_plet;
			for (s2 = s; s2 != 0; s2 = s2->next) {
				if (s2->type != NOTEREST)
					continue;
				do_combine(s2);
				if (--r <= 0)
					break;
			}
			continue;
		}
#if 1
		if (s->as.type != ABC_T_NOTE) {
			if (s->as.type != ABC_T_REST)
				continue;
			if (may_combine(s)) {
				do_combine(s);
				if (may_combine(s))	/* when 3 voices */
					do_combine(s);
			}
			continue;
		}
		if (!(s->sflags & S_BEAM_ST))
			continue;
#else
//		if (s->as.type != ABC_T_NOTE || !(s->sflags & S_BEAM_ST))
//		if (s->type != NOTEREST || !(s->sflags & S_BEAM_ST))
		if (s->type != NOTEREST)
			continue;
#endif
		if (s->sflags & S_BEAM_END) {
			if (may_combine(s)) {
				do_combine(s);
				if (may_combine(s))	/* when 3 voices */
					do_combine(s);
			}
			continue;
		}
		s2 = s;
		for (;;) {
			if (!may_combine(s2)) {
				s2 = 0;
				break;
			}
			if (s2->as.type == ABC_T_REST
			    || (s2->sflags & S_BEAM_END))
				break;
			do {
				s2 = s2->next;
			} while (s2->type != NOTEREST);
		}
		if (s2 == 0)
			continue;
		s2 = s;
		for (;;) {
			do_combine(s2);
			if (may_combine(s2))	/* when 3 voices */
				do_combine(s2);
			if (s2->as.type == ABC_T_REST
			    || (s2->sflags & S_BEAM_END))
				break;
			do {
				s2 = s2->next;
			} while (s2->type != NOTEREST);
		}
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
	if ((p_voice->last_sym = s->prev) == 0)
		p_voice->sym = 0;
	p_voice->time = s->time;
	new_s = sym_add(p_voice, CLEF);
	new_s->next = s;
	s->prev = new_s;

	new_s->as.u.clef.type = clef_type;
	new_s->as.u.clef.line = clef_type == TREBLE ? 2 : 4;
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

/* -- define the clef for a staff with no explicit clef -- */
/* this function is called only once for the whole tune */
static void set_clef(int staff)
{
	struct SYSTEM *sy;
	struct SYMBOL *s, *last_chg;
	int clef_type, min, max, time;

	/* get the max and min pitches */
	min = max = 16;			/* 'C' */
	for (s = tsfirst; s != 0; s = s->ts_next) {
		if (s->staff != staff || s->as.type != ABC_T_NOTE)
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
	sy->staff[staff].clef.type = clef_type;
	sy->staff[staff].clef.line = clef_type == TREBLE ? 2 : 4;
	last_chg = 0;
	for (s = tsfirst; s != 0; s = s->ts_next) {
		struct SYMBOL *s2, *s3, *s4;

		if (s->staff != staff || s->as.type != ABC_T_NOTE) {
			if (s->type == STAVES) {
				sy = sy->next;	/* keep the starting clef */
				sy->staff[staff].clef.type = clef_type;
				sy->staff[staff].clef.line = clef_type == TREBLE ? 2 : 4;
				last_chg = s;
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
			    && s2->as.type == ABC_T_NOTE
			    && s2->pits[0] >= 19)	/* F */
				continue;
			s2 = s->ts_next;
			if (s2 != 0
			    && s2->staff == staff
			    && s2->time == time
			    && s2->as.type == ABC_T_NOTE
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
			    && s2->as.type == ABC_T_NOTE
			    && s2->pits[0] <= 13)	/* G, */
				continue;
			s2 = s->ts_next;
			if (s2 != 0
			    && s2->staff == staff
			    && s2->time == time
			    && s2->as.type == ABC_T_NOTE
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
#if 0
		time = last_chg == 0 ? -1 : last_chg->time;
#endif
		for (s2 = s->ts_prev; s2 != last_chg; s2 = s2->ts_prev) {
#if 0
			if (s2->time <= time)
				break;
#endif
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
			if (s2->as.type != ABC_T_NOTE)
#else
			if (s2->type != NOTEREST)	/* neither note nor rest */
#endif
				continue;

			/* exit loop if a clef change cannot occur */
			if (s2->as.type == ABC_T_NOTE) {
				if (clef_type == TREBLE) {
					if (s2->pits[0] >= 19)		/* F */
						break;
				} else {
					if (s2->pits[s2->nhd] <= 13)	/* G, */
						break;
				}
			}

#if 1 /*fixme:test*/
			/* have a 2nd choice on beam start */
#if 1
#if 1
#if 1 /* double clef pb - clef change on 2nd voice */
			if ((s2->sflags & S_BEAM_ST)
			 && !voice_tb[s2->voice].second)
#else
			if (s2->sflags & S_BEAM_ST)
#endif
				s3 = s2;
#else
			if ((s3->sflags & S_BEAM_ST) == 0)
				s3 = s2;
#endif
#else
			if ((s2->sflags & S_BEAM_ST)
			    || (s3->sflags & S_BEAM_ST) == 0)
				s3 = s2;
#endif
#else
			/* have a 2nd choice if word starts on the main voice */
			if (!voice_tb[s2->voice].second
			    && voice_tb[s2->voice].staff == staff) {
				if ((s2->sflags & S_BEAM_ST)
				    || s3 == 0
				    || (s3->sflags & S_BEAM_ST) == 0)
					s3 = s2;
			}
#endif
		}
		s2 = last_chg;
		last_chg = s;

		/* if first change, see if any note before */
		if (s2 == 0 || s2->type == STAVES) {
#if 1 /*fixme:test*/
			s4 = s3;
#else
			if ((s4 = s3) == 0)
				s4 = s;
#endif
			for (s4 = s4->ts_prev; s4 != s2; s4 = s4->ts_prev) {
				if (s4->staff != staff)
					continue;
				if (s4->as.type == ABC_T_NOTE)
					break;
			}

			/* if no note, change the clef of the staff */
			if (s4 == s2) {
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

			if (s->as.type != ABC_T_NOTE) {
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
				if (s1->as.type == ABC_T_NOTE)
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
				if (s1->as.type == ABC_T_NOTE)
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
	for (g = s->extra; ; g = g->next) {
		if (g->type == NOTEREST)
			break;
	}
	g->sflags |= S_BEAM_ST;
	for ( ; ; g = g->next) {
		if (g->type != NOTEREST) {
			if (g->next == 0)
				break;
			continue;
		}
		set_head_directions(g);
		for (m = g->nhd; m >= 0; m--) {
			if (g->as.u.note.accs[m]) {
				xx += 5;
				if (g->as.u.note.accs[m] & 0xf8)
					xx += 2;
				break;
			}
		}
		g->x = xx;

		if (g->nflags <= 0)
			g->sflags |= S_BEAM_ST | S_BEAM_END;
		next = g->next;
		if (next == 0) {
			g->sflags |= S_BEAM_END;
			break;
		}
		if (next->nflags <= 0 || (next->as.flags & ABC_F_SPACE))
			g->sflags |= S_BEAM_END;
		if (g->sflags & S_BEAM_END) {
			next->sflags |= S_BEAM_ST;
			xx += gspinside / 4;
		}
		if (g->nflags <= 0)
			xx += gspinside / 4;
		if (g->y > next->y + 8)
			xx -= 1.5;
		xx += gspinside;
	}

	xx += gspleft + gspright;
	if ((next = s->next) != 0
	    && next->as.type == ABC_T_NOTE) {	/* if before a note */
		if (g->y >= (float) (3 * (next->pits[next->nhd] - 18)))
			xx -= 1;		/* above, a bit closer */
		else if ((g->sflags & S_BEAM_ST)
			 && g->y < (float) (3 * (next->pits[0] - 18) - 7))
			xx += 2;	/* below with flag, a bit further */
	}

	/* return the whole width */
	return xx;
}

/* -- compute the width needed by a guitar chord -- */
static float gchord_width(struct SYMBOL *s,
			  float wlnote,
			  float wlw)
{
	struct SYMBOL *s2;
	float lspc, rspc, w;
	char *p, *q, sep, antype;

	str_font(s->gcf);
	lspc = rspc = cwid(' ') * cfmt.font_tb[s->gcf].swfac;
	p = s->as.text;
	antype = '\0';
	sep = '\n';
	for (;;) {
		if (*p != '\0' && strchr("^_<>@", *p) != 0)
			antype = *p++;
		else	antype = '\0';
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
	s2 = s->prev;
	if (s2 != 0 && s2->as.text != 0)
		AT_LEAST(wlw, lspc);
/*fixme: pb when ">" only*/
	for (s2 = s->next; s2 != 0; s2 = s2->next) {
		switch (s2->type) {
		default:
			continue;
		case NOTEREST:
		case SPACE:
		case BAR:
			if (s2->as.text != 0)
				AT_LEAST(s->wr, rspc);
			break;
		}
		break;
	}
	return wlw;
}

/* -- set the width needed by the lyrics -- */
static float ly_width(struct SYMBOL *s, float wlw)
{
	struct SYMBOL *k;
	struct lyrics *ly = s->ly;
	struct lyl *lyl;
	struct tblt_s *tblt;
	float align, xx, w;
	int i;

	/* check if the lyrics contain tablature definition */
	for (i = 0; i < 2; i++) {
		if ((tblt = voice_tb[s->voice].tblts[i]) == 0)
			continue;
		if (tblt->pitch == 0) {		/* yes, no width */
			for (i = 0; i < MAXLY; i++) {
				if ((lyl = ly->lyl[i]) == 0)
					continue;
				lyl->s = 0;
			}
			return wlw;
		}
	}

	align = 0;
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
//		    || p[1] == '(' || p[1] == ')') {
		    || *p == '(' || *p == ')') {
			float sz;

//			if (p[1] == '(')
			if (*p == '(')
//				sz = cwid((unsigned char) p[1]);
				sz = cwid((unsigned char) *p);
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
		} else if (*p == LY_HYPH || *p == LY_UNDER)
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
			case NOTEREST:
				if (k->ly == 0
				    || k->ly->lyl[i] == 0)
					xx -= 9;
				else if (k->ly->lyl[i]->t[0] == LY_HYPH
					 || k->ly->lyl[i]->t[0] == LY_UNDER)
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
			default:
				xx -= 5;
				break;
			}
			break;
		}
		if (xx > s->wr)
			s->wr = xx;
		}
	if (align > 0) {
		for (i = 0; i < MAXLY; i++) {
			if ((lyl = ly->lyl[i]) == 0)
				continue;
			if (isdigit((unsigned char) lyl->t[0]))
				lyl->s = align;
		}
	}
	return wlw;
}

/* -- set the width of a symbol -- */
/* This routine sets the minimal left and right widths wl,wr
 * so that successive symbols are still separated when
 * no extra glue is put between them */
static void set_width(struct SYMBOL *s)
{
	struct SYMBOL *s2;
	int i, m;
	float xx, w, wlnote, wlw;

	switch (s->type) {
	case NOTEREST:

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
		if (s2 != 0) {
			switch (s2->type) {
			case BAR:
			case CLEF:
			case KEYSIG:
			case TIMESIG:
				wlnote += 3;
				break;
			}
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
		if (s2 != 0) {
			switch (s2->type) {
			case BAR:
			case CLEF:
			case KEYSIG:
			case TIMESIG:
				wlnote -= 3;
				break;
			}
		}

		/* room for the decorations */
		if (s->as.u.note.dc.n > 0)
			wlnote += deco_width(s);

		/* space for flag if stem goes up on standalone note */
		if ((s->sflags & (S_BEAM_ST | S_BEAM_END)) == (S_BEAM_ST | S_BEAM_END)
		    && s->stem > 0 && s->nflags > 0)
			AT_LEAST(s->wr, s->xmx + 12);

		/* leave room for dots and set their offset */
		if (s->dots > 0) {

			/* standalone with up-stem and flags */
			if (s->nflags > 0 && s->stem > 0
			    && s->xmx == 0 && s->doty == 0
			    && (s->sflags & (S_BEAM_ST | S_BEAM_END))
					== (S_BEAM_ST | S_BEAM_END)
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

		/* if a tremolo on 2 notes, have space for the small beam(s) */
		if ((s->sflags & (S_TREM2 | S_BEAM_END)) == (S_TREM2 | S_BEAM_END))
			AT_LEAST(wlnote, 20);

		wlw = wlnote;

		if (s2 != 0) {
			switch (s2->type) {
			case NOTEREST:	/* extra space when up stem - down stem */
				if (s2->as.type == ABC_T_REST)
					break;
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
				    || s2->u)
					break;
				wlw += 8;
				break;
			case KEYSIG:
/*			case TIMESIG:	*/
				wlw += 4;
				break;
			}
		}

		/* leave room for guitar chord */
		/* !! this sequence is tied to draw_gchord() !! */
		if (s->as.text != 0)
			wlw = gchord_width(s, wlnote, wlw);

		/* leave room for vocals under note */
		/* related to draw_lyrics() */
		if (s->ly != 0)
			wlw = ly_width(s, wlw);
#if 0
/*new fixme*/
		/* reduce right space when not followed by a note */
		for (k = s->next; k != 0; k = k->next) {
			switch (k->type) {
			default:
				s->pr *= 0.8;
				break;
			case NOTEREST:
				break;
			}
			break;
		}

		/* squeeze notes a bit if big jump in pitch */
		if (s->as.type == ABC_T_NOTE
		    && s2->as.type == ABC_T_NOTE) {
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
		if (s2 != 0 && s2->type == GRACE)
			s->wl = wlnote - 4.5;
		else	s->wl = wlw;
		break;
	case SPACE:
		if (s->as.u.note.lens[1] < 0)
			xx = 10;
		else	xx = (float) s->as.u.note.lens[1] * 0.5;
		s->wr = xx;
		if (s->as.text != 0)
			xx = gchord_width(s, xx, xx);
		if (s->as.u.note.dc.n > 0)
			xx += deco_width(s);
		s->wl = xx;
		break;
	case BAR:
		if (s->sflags & S_NOREPBRA)
			break;
		if (!(s->as.flags & ABC_F_INVIS)) {
			int bar_type;

			w = 5;
			bar_type = s->as.u.bar.type;
			switch (bar_type) {
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
			s->wl = w;
			if (s->next != 0
			    && s->next->type != TIMESIG)
				s->wr = 8;
			else	s->wr = 5;
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
		if (s->as.u.bar.repeat_bar)
			xx += 8;
		else if (s->prev->as.text != 0) {
			float spc;

			spc = xx * GCHPRE;
			if (spc > 8)
				spc = 8;
			AT_LEAST(s->wl, spc);
			xx -= spc;
		}
		for (s2 = s->next; s2 != 0; s2 = s2->next) {
			switch (s2->type) {
			case GRACE:
				continue;
			case NOTEREST:
				if (s2->as.text != 0)
					AT_LEAST(s->wr, xx);
				break;
			default:
				break;
			}
			break;
		}
		break;
	case CLEF:
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
		s->wr = (float) (5.5 * n1 + esp);
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
	case STBRK:
		if ((s->wl = s->xmx) == 0)
			break;		/* no space */
		if (s->next == 0 || s->next->type != CLEF)
			s->wr = 8;
		else {
			s->wr = 2;
			s->next->u = 0;	/* big clef */
		}
		break;
#if 0
	case TEMPO:
	case PART:
	case TUPLET:
#endif
	case FMTCHG:
	case STAVES:		/* no space */
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
	if (smallest_duration >= MINIM) {
		if (smallest_duration >= SEMIBREVE)
			len /= 4;
		else
			len /= 2;
	}
	if (len == 0) {
		switch (s->type) {
		case MREST:
			return s->wl + 16;
/*fixme:do same thing at start of line*/
		case NOTEREST:
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
	if ((s->sflags & S_BEAM_ST) == 0)
		space *= fnnp;

	/* decrease/increase spacing if stems in opposite directions */
/*fixme:to be done later, after x computed in sym_glue*/
	if (s->as.type == ABC_T_NOTE && s->nflags >= 0) {
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
static void set_allsymwidth(struct SYMBOL *last_s)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s, *s2, *s3;
	struct tblt_s *tblt;
	int i;
	float new_val, shrink, space;

	/* loop on the symbol sequences */
	s = tsfirst;
	for (;;) {
		s2 = s;
		shrink = space = 0;
		do {
			int ymx1, ymn1, ymx2, ymn2;
			float wl;

			/* set the minimum space before and after the symbol */
			set_width(s2);

			/* calculate the minimum space before the symbol */
			ymx1 = s2->ymx;
			ymn1 = s2->ymn;
			wl = s2->wl;
			if (s2->ts_prev == 0)
				new_val = wl;
			else
				new_val = 0;
			for (s3 = s->ts_prev; s3 != 0; s3 = s3->ts_prev) {
				if (s3->staff == s2->staff
				 && (!(s3->as.flags & ABC_F_INVIS)
					|| s3->voice == s2->voice)
				 && new_val <= s3->wr + wl) {
					if (s3->type == NOTEREST
					 && s2->type == NOTEREST) {
						new_val = s3->wr + wl;
					} else {
						switch (s3->type) {
						default:
							ymx2 = s3->ymx;
							ymn2 = s3->ymn;
							if (ymn1 > ymx2
							 || ymx1 < ymn2)
								break;
							/* fall thru */
						case SPACE:
						case BAR:
						case CLEF:
						case TIMESIG:
						case KEYSIG:
							new_val = s3->wr + wl;
							break;
						}
					}
				}
				if ((s3->sflags & S_SEQST) && new_val != 0)
					break;
				wl -= s3->shrink;
				if (wl < 0)
					break;
			}
			if (shrink < new_val)
				shrink = new_val;
			new_val = set_space(s2);
			if (space < new_val)
				space = new_val;
			if ((s2 = s2->ts_next) == last_s)
				break;
		} while (!(s2->sflags & S_SEQST));

		/* set the spaces at start of sequence */
		if (shrink == 0 && space == 0) {
			s->sflags &= ~S_SEQST;	/* no space (clef) */
			s->time = s->ts_prev->time;
		} else {
			s->shrink = shrink;
			s->space = space;
		}
		if ((s = s2) == last_s)
			break;
	}

	/* have room for the tablature header */
	space = 0;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		for (i = 0; i < 2; i++) {
			if ((tblt = p_voice->tblts[i]) == 0)
				continue;
			if (tblt->wh > space)
				space = tblt->wh;
		}
	}
	if (space == 0)
		return;
	shrink = 0;
	for (s = tsfirst; s != last_s; s = s->ts_next) {
		if (s->shrink != 0)
			shrink += s->shrink;
		if (s->as.type == ABC_T_NOTE)
			break;
	}
	if (s != last_s && shrink < space) {
		while (!(s->sflags & S_SEQST))
			s = s->ts_prev;
		s->shrink += space - shrink;
	}
}

/* -- set the repeat sequences / measures -- */
/* if !xset, check and change the sequence repeat,
 *	and just check the measure repeat */
/* if xset, check and change the measure repeat */
static void set_repeat(struct SYMBOL *g,
			struct SYMBOL *s,
			int xset)
{
	struct SYMBOL *s2, *s3;
	int i, j, n, dur, staff, voice;

	staff = s->staff;
	voice = s->voice;

	/* treat the sequence repeat */
	if ((n = g->doty) < 0) {		/* number of notes / measures */
		n = -n;
		i = n;				/* number of notes to repeat */
		for (s2 = s->ts_prev; s2 != 0; s2 = s2->ts_prev) {
			if (s2->staff != staff)
				continue;
			if (s2->voice != voice
			    && !(s2->as.flags & ABC_F_INVIS)) {
				error(0, s2, "Other voice in sequence to repeat");
				goto delrep;
			}
			if (s2->dur == 0) {
				if (s2->type == BAR) {
					error(0, s2, "Bar in sequence to repeat");
					goto delrep;
				}
				continue;
			}
			if (--i <= 0)
				break;
		}
		if (s2 == 0) {
			error(0, s, "Not enough symbols to repeat");
			goto delrep;
		}

		i = g->nohdix * n;		/* repeat number */
		for (s2 = s; s2 != 0; s2 = s2->ts_next) {
			if (s2->staff != staff)
				continue;
			if (s2->voice != voice
			    && !(s2->as.flags & ABC_F_INVIS)) {
				error(0, s2, "Other voice in repeat sequence");
				goto delrep;
			}
			if (s2->dur == 0) {
				if (s2->type == BAR) {
					error(0, s2, "Bar in repeat sequence");
					goto delrep;
				}
				continue;
			}
			if (--i <= 0)
				break;
		}
		if (s2 == 0
		    || s2->next == 0) {	/* should have a measure bar */
			error(0, s, "Not enough symbols after repeat sequence");
			goto delrep;
		}
		s3 = s;
		for (j = g->nohdix; --j >= 0; ) {
			i = n;			/* number of notes/rests */
			if (s3->dur != 0)
				i--;
			s2 = s3->next;
			while (i > 0) {
				if (s2->dur != 0)
					i--;
				s2->extra = 0;
				delsym(s2);
				s2 = s2->next;
			}
			s3->type = NOTEREST;
			s3->as.type = ABC_T_REST;
			s3->dur = s3->as.u.note.lens[0]
				= s2->time - s3->time;
			s3->sflags |= S_REPEAT | S_BEAM_ST;
			s3->doty = -1;
			set_width(s3);
			if (s3->sflags & S_SEQST)
				s3->space = set_space(s3);
			s3->head = H_SQUARE;
			s3 = s2;
		}
		goto delrep;			/* done */
	}

	/* first check of the measure repeat */
	if (!xset) {
		i = n;				/* number of measures to repeat*/
		for (s2 = s->ts_prev; s2 != 0; s2 = s2->ts_prev) {
			if (s2->staff != staff)
				continue;
			if (s2->voice != voice) {
				if (!(s2->as.flags & ABC_F_INVIS)) {
					error(0, s2, "Other voice in measure to repeat");
					goto delrep;
				}
			} else if (s2->type == BAR) {
				if (--i <= 0)
					break;
			}
		}
		if (s2 == 0) {
			error(0, s, "Not enough measures to repeat");
			goto delrep;
		}

		i = g->nohdix * n;		/* repeat number */
		for (s2 = s; s2 != 0; s2 = s2->ts_next) {
			if (s2->staff != staff)
				continue;
			if (s2->voice != voice) {
				if (!(s2->as.flags & ABC_F_INVIS)) {
					error(0, s2, "Other voice in repeat measure");
					goto delrep;
				}
			} else if (s2->type == BAR) {
				if (--i <= 0)
					break;
			}
		}
		if (s2 == 0) {
			error(0, s, "Not enough symbols after repeat measure");
			goto delrep;
		}
		return;				/* OK, treat it later */
	}

	/* second check and replace */
	i = g->nohdix * n;			/* check if NL later in the line*/
	for (s2 = s; s2 != 0; s2 = s2->next) {
//		if (s2->sflags & S_NL)
//			goto delrep;
		if (s2->type == BAR) {
			if (--i <= 0)
				break;
		}
	}
	dur = (s2->time - s->time) / g->nohdix;	/* repeat duration */
	s2 = s->prev->prev;			/* check if NL before */
	if (n == 2) {				/* if repeat 2 measures */
		for (; s2 != 0; s2 = s2->prev) {
			if (s2->type == BAR) {
				s2 = s2->prev;
				break;
			}
//			if (s2->sflags & S_NL)
//				goto delrep;
		}
	}
	for (; s2 != 0; s2 = s2->prev) {
		if (s2->type == BAR || s2->type == CLEF)
			break;
//		if (s2->sflags & S_NL)
//			goto delrep;
	}
	if (s2 == 0 || s->time - s2->time != dur)
		goto delrep;	/* the previous measure is not in the music line */

	dur /= n;
	if (n == 2) {			/* repeat 2 measures (one time) */
		s3 = s;
		s2 = s->next;
		for (;;) {
			if (s2->type == BAR)
				break;
			s2->extra = 0;
			delsym(s2);
			s2 = s2->next;
		}
		s3->type = NOTEREST;
		s3->as.type = ABC_T_REST;
		s3->dur = s3->as.u.note.lens[0] = dur;
		s3->as.flags = ABC_F_INVIS;
/*fixme: should set many parameters for set_width*/
//		set_width(s3);
		if (s3->sflags & S_SEQST)
			s3->space = set_space(s3);
		s2->as.u.bar.len = 2;
		if (s2->sflags & S_SEQST)
			s2->space = set_space(s2);
		s3 = s2->next;
		s2 = s3->next;
		for (;;) {
			if (s2->type == BAR || s2->type == CLEF)
				break;
			s2->extra = 0;
			delsym(s2);
			s2 = s2->next;
		}
		s3->type = NOTEREST;
		s3->as.type = ABC_T_REST;
		s3->dur = s3->as.u.note.lens[0] = dur;
		s3->as.flags = ABC_F_INVIS;
		set_width(s3);
		if (s3->sflags & S_SEQST)
			s3->space = set_space(s3);
		if (s2->sflags & S_SEQST)
			s2->space = set_space(s2);
		return;
	}

	/* repeat 1 measure */
	s3 = s;
	for (j = g->nohdix; --j >= 0; ) {
		s2 = s3->next;
		for (;;) {
			if (s2->type == BAR || s2->type == CLEF)
				break;
			s2->extra = 0;
			delsym(s2);
			s2 = s2->next;
		}
		s3->type = NOTEREST;
		s3->as.type = ABC_T_REST;
		s3->dur = s3->as.u.note.lens[0] = dur;
		s3->sflags &= S_NL | S_SEQST;
		s3->sflags |= S_REPEAT | S_BEAM_ST;
/*fixme: should set many parameters for set_width*/
//		set_width(s3);
		if (s3->sflags & S_SEQST)
			s3->space = set_space(s3);
		if (s2->sflags & S_SEQST)
			s2->space = set_space(s2);
		if (g->nohdix == 1) {
			s3->doty = 1;
			break;
		}
		s3->doty = g->nohdix - j + 1;	/* number to print above the repeat rest */
		s3 = s2->next;
	}
	return;

delrep:					/* remove the %%repeat */
	g->type = FMTCHG;
	g->u = -1;
}

/* -- define the beginning of a new music line -- */
static struct SYMBOL *set_nl(struct SYMBOL *s)
{
	struct SYMBOL *s2;
	int time, done;

	/* if normal symbol, cut here */
	switch (s->type) {
	case CLEF:
	case KEYSIG:
	case TIMESIG:
	case BAR:
		break;
	case GRACE:
		if (cfmt.continueall && s->next != 0
		 && s->next->as.type != ABC_T_NOTE)
			break;
		/* fall thru */
	default:
		time = s->time + s->dur;
		for (s = s->ts_next; ; s = s->ts_next) {
			if (s == 0)
				return s;
			if ((s->sflags & S_SEQST)
			 && s->time >= time)
				break;
		}
		if (s->type == BAR)
			break;

		/* don't cut beamed notes */
		for (s2 = s->ts_next; s2 != 0; s2 = s2->ts_next) {
			if (s2->sflags & S_SEQST) {
				s2 = s2->ts_prev;
				break;
			}
		}
		if (s2 == 0)
			return s;
		done = 1;
		for ( ; s2 != s; s2 = s2->ts_prev) {
			if (s2->as.type == ABC_T_NOTE
			 && !(s2->sflags & S_BEAM_ST))
				done = 0;
			if (s2->sflags & S_SEQST) {
				if (done) {
					s = s2;
					break;
				}
				done = 1;
			}
		}
		s->sflags |= S_NL;
		return s;
	}

	/* go back to handle the staff breaks at end of line */
	for (; s != 0; s = s->ts_prev) {
		if (!(s->sflags & S_SEQST))
			continue;
		switch (s->type) {
		case CLEF:
		case KEYSIG:
		case TIMESIG:
			continue;
		}
		break;
	}
	done = 0;
	for (; s != 0; s = s->ts_next) {
		if (!(s->sflags & S_SEQST))
			continue;
		if (done < 0)
			break;
		switch (s->type) {
		case BAR:
			if (done)
				break;
			done = 1;
			continue;
		case STBRK:
			if (s->doty == 0) {	/* if not forced */
				delsym(s);	/* remove */
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
		if (!(s->sflags & S_SEQST))
			continue;
		shrink = s->shrink;
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
				last = set_nl(last);
			return last;
		}
		xline = wwidth / nlines;
		x = indent;
		s2 = 0;
		for ( ; s != last; s = s->ts_next) {
			if (!(s->sflags & S_SEQST))
				continue;
			shrink = s->shrink;
			space = s->space;
			if (space < shrink)
				x += shrink;
			else
				x += shrink * cfmt.maxshrink
					+ space * (1 - cfmt.maxshrink);
			if (s->type == BAR) {
				s2 = s;
				x2 = x;
			}
			if (x > xline) {
				if (s->next != 0 && s->next->type == BAR
				 && (s->next->sflags & S_SEQST)) {
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
			if (!(s2->sflags & S_SEQST))
				continue;
			shrink = s2->shrink;
			if ((space = s2->space) < shrink)
				wwidth -= shrink;
			else
				wwidth -= shrink * cfmt.maxshrink
					+ space * (1 - cfmt.maxshrink);
		}

		/* don't count part of the width of the next symbol */
		indent = s->shrink * -0.8;
		first = s;
	}
	return s;
}

/* -- set the bar numbers -- */
static void set_bar_num(void)
{
	struct SYMBOL *s;
	int bar_time, wmeasure;
	int bar_num;

	wmeasure = voice_tb[cursys->top_voice].meter.wmeasure;
	if (wmeasure == 0)				/* if M:none */
		wmeasure = 1;

	/* don't count a bar at start of line */
	for (s = tsfirst; ; s = s->ts_next) {
		if (s == 0)
			return;
		switch (s->type) {
		case TIMESIG:
		case CLEF:
		case KEYSIG:
		case FMTCHG:
		case STBRK:
			continue;
		case BAR:
			if (s->u != 0) {
				nbar = s->u;		/* (%%setbarnb) */
				break;
			}
			if (s->as.u.bar.repeat_bar
			    && s->as.text != 0
			    && cfmt.contbarnb == 0) {
				if (s->as.text[0] == '1')
					nbar_rep = nbar;
				else {
					nbar = nbar_rep; /* restart bar numbering */
					s->u = nbar;
				}
			}
			break;
		}
		break;
	}

	/* set the measure number on the top bars */
	bar_time = s->time + wmeasure;	/* for incomplete measure at start of tune */
	bar_num = nbar;
	for ( ; s != 0; s = s->ts_next) {
		switch (s->type) {
		case TIMESIG:
/*fixme: KO if M:none*/
			wmeasure = s->as.u.meter.wmeasure;
/*fixme: KO if bar after the time sig*/
			bar_time = s->time + wmeasure;
			break;
		case MREST:
			bar_num += s->as.u.bar.len - 1;
//			bar_time += wmeasure * (s->as.u.bar.len - 1);
			while (s->ts_next != 0
			    && s->ts_next->type != BAR)
				s = s->ts_next;
			break;
		case BAR:
			if (s->u != 0) {
				bar_num = s->u;		/* (%%setbarnb) */
				s->u = 0;
				if (s->time < bar_time)
					break;
			} else {
				if (s->time < bar_time)	/* incomplete measure */
					break;
				bar_num++;
			}
			if (s->as.u.bar.repeat_bar
				 && s->as.text != 0
				 && cfmt.contbarnb == 0) {
				if (s->as.text[0] == '1')
					nbar_rep = bar_num;
				else		/* restart bar numbering */
					bar_num = nbar_rep;
			}
			s->u = bar_num;
			bar_time = s->time + wmeasure;
			break;
		}
	}
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
		wmeasure = voice_tb[cursys->top_voice].meter.wmeasure;
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
		if (!(s->sflags & S_SEQST))
			continue;
		xmin += s->shrink;
		if (xmin > lwidth) {
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
		if (!(s->sflags & S_EOLN))
			continue;
		s2 = set_nl(s);
		s->sflags &= ~S_EOLN;
		s = s2;
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
		    || (s->as.flags & ABC_F_INVIS)) {
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
/* this function is called only once per tune */
static void set_pitch(struct SYMBOL *last_s)
{
	struct SYSTEM *sy;
	struct SYMBOL *s;
	int staff, delta, dur;
	char staff_clef[MAXSTAFF];
	static int delta_tb[4] = {
		0 - 2 * 2,
		6 - 3 * 2,
		12 - 4 * 2,
		0 - 2 * 2
	};

	sy = cursys;
	for (staff = 0; staff <= sy->nstaff; staff++) {
		delta = delta_tb[sy->staff[staff].clef.type];
		staff_clef[staff] = delta + sy->staff[staff].clef.line * 2;
	}
	dur = BASE_LEN;
	for (s = tsfirst; s != last_s; s = s->ts_next) {
		struct SYMBOL *g;
		int np, m, pav;

		if ((g = s->extra) != 0) {
			for ( ; g != 0; g = g->next) {
				if (g->type == FMTCHG && g->u == REPEAT) {
					set_repeat(g, s, 0);
					break;
				}
			}
		}
		staff = s->staff;
		switch (s->type) {
		case CLEF:
			if (s->sflags & S_SECOND) {
/*fixme:%%staves:can this happen?*/
				if (s->prev == 0)
					break;
				delsym(s);
				break;
			}
			set_yval(s);
			delta = delta_tb[s->as.u.clef.type];
			staff_clef[staff] = delta + s->as.u.clef.line * 2;
			break;
		case STAVES:
			sy = sy->next;
			for (staff = 0; staff <= sy->nstaff; staff++) {
				delta = delta_tb[sy->staff[staff].clef.type];
				staff_clef[staff] = delta + sy->staff[staff].clef.line * 2;
			}
			break;
		case GRACE:
			for (g = s->extra; g != 0; g = g->next) {
				if (g->type != NOTEREST)
					continue;
				delta = staff_clef[g->staff];
				if (delta != 0) {
					for (m = g->nhd; m >= 0; m--)
						g->pits[m] += delta;
				}
				g->ymn = 3 * (g->pits[0] - 18) - 2;
				g->ymx = 3 * (g->pits[g->nhd] - 18) + 2;
			}
			set_yval(s);
			break;
		case KEYSIG:
			s->pits[0] = staff_clef[staff];
			set_yval(s);
			break;
		default:
			set_yval(s);
			break;
		case MREST:
			s->ymx = 24 + 15;
			s->ymn = -2;
			break;
		case NOTEREST:
			if (s->as.type != ABC_T_NOTE) {
				s->y = 12;		/* rest */
				s->ymx = 12 + 8;
				s->ymn = 12 - 8;
				break;
			}
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
			if (s->dur < dur)
				dur = s->dur;
			break;
		}
	}
	smallest_duration = dur;
}

/* -- set the stem direction when multi-voices -- */
/* this function is called only once per tune */
static void set_stem_dir(void)
{
	struct SYSTEM *sy;
	struct SYMBOL *s, *t, *u;
	int i, staff, nst, rvoice, voice;
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

		/* get the max/min offsets in the delta time */
/*fixme: the stem height is not calculated yet*/
		for (u = s;
		     u != 0 && u->type != BAR && u->type != STAVES;
		     u = u->ts_next) {
			if (u->type != NOTEREST
			    || (u->as.flags & ABC_F_INVIS))
				continue;
			staff = u->staff;
#if 1
/*fixme:test*/
if (staff > nst) {
	bug("set_multi(): bad staff number\n", 1);
}
#endif
			voice = u->voice;
			if (vtb[voice].st1 < 0)
				vtb[voice].st1 = staff;
			else if (vtb[voice].st1 != staff) {
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

			if (u->as.type != ABC_T_NOTE)
				continue;
			if (u->ymx > stb[staff].st[i].ymx)
				stb[staff].st[i].ymx = u->ymx;
			if (u->ymn < stb[staff].st[i].ymn)
				stb[staff].st[i].ymn = u->ymn;
			if (u->sflags & S_XSTEM) {
				if (u->ts_prev->staff != staff - 1
				    || u->ts_prev->as.type != ABC_T_NOTE) {
					error(1, s, "Bad +xstem+");
					u->sflags &= ~S_XSTEM;
/*fixme:nflags KO*/
				} else {
					u->ts_prev->multi = 1;
					u->multi = 1;
					u->as.flags |= ABC_F_STEMLESS;
				}
			}
		}

		for ( ; s != u; s = s->ts_next) {
			if (s->type != NOTEREST		/* if not note or rest */
			 && s->type != GRACE)
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
						    && (s->sflags & (S_BEAM_ST | S_BEAM_END))
								== (S_BEAM_ST | S_BEAM_END)
						    && ((t = s->ts_next) == 0
							|| t->staff != s->staff
							|| t->time != s->time))
							s->multi = -1;
					}
				}
			}
		}

		while (s != 0) {
			switch (s->type) {
			case STAVES:
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
				/*fall thru*/
			case BAR:
				s = s->ts_next;
				continue;
			}
			break;
		}
	}
}

/* -- adjust the vertical offset of the rests -- */
/* this function is called only once per tune */
static void set_rest_offset(void)
{
	struct SYSTEM *sy;
	struct SYMBOL *s, *t, *u;
	int i, j, staff, nst, rvoice, voice;
	int next_time, delta_time;
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
	delta_time = BASE_LEN / 4;	/* crotchet */
	if (voice_tb[cursys->top_voice].meter.wmeasure > BASE_LEN)
		delta_time *= 2;	/* measure longer than 4/4 */
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

		/* get the max/min offsets in the measure */
/*fixme: the stem height is not calculated yet*/
		next_time = s->time + delta_time;
		for (u = s;
		     u != 0 && u->time < next_time
				&& u->type != BAR && u->type != STAVES;
		     u = u->ts_next) {
			if (u->type != NOTEREST
			    || (u->as.flags & ABC_F_INVIS))
				continue;
			staff = u->staff;
#if 1
/*fixme:test*/
if (staff > nst) {
	bug("set_multi(): bad staff number\n", 1);
}
#endif
			voice = u->voice;
			if (vtb[voice].st1 < 0)
				vtb[voice].st1 = staff;
			else if (vtb[voice].st1 != staff) {
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

			if (u->as.type != ABC_T_NOTE) {

				/* avoid clash of rest on a whole measure */
				if (u->next != 0 && u->next->type == BAR
				 && u->next->time  > next_time)
					next_time = u->next->time;
				continue;
			}

			if (u->ymx > stb[staff].st[i].ymx)
				stb[staff].st[i].ymx = u->ymx;
			if (u->ymn < stb[staff].st[i].ymn)
				stb[staff].st[i].ymn = u->ymn;

			/* if beamed notes, continue to the next note */
			if (!(u->sflags & S_BEAM_END)) {
				if (u->time + u->dur + u->next->dur > next_time)
					next_time = u->time + u->dur + u->next->dur;
			}
		}

		for ( ; s != u; s = s->ts_next) {
			int us, ls, not_alone, y;

			if (s->as.type != ABC_T_REST)
				continue;
			staff = s->staff;
			voice = s->voice;
			if (stb[staff].nvoice <= 0)	/* voice alone on the staff */
				continue;
			rvoice = sy->voice[voice].range;
			for (i = stb[staff].nvoice; i >= 0; i--) {
				if (stb[staff].st[i].voice == rvoice)
					break;
			}
			if (i < 0)
				continue;		/* voice ignored */
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
					if (t->as.type != ABC_T_REST
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

		while (s != 0) {
			switch (s->type) {
			case STAVES:
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
				/*fall thru*/
			case BAR:
				s = s->ts_next;
				continue;
			}
			break;
		}
	}
}

/* -- create a starting symbol -- */
static struct SYMBOL *sym_new(int type,
				struct VOICE_S *p_voice,
				struct SYMBOL *last_ts)
{
	struct SYMBOL *s;

	s = (struct SYMBOL *) getarena(sizeof *s);
	memset(s, 0, sizeof *s);
	s->type = type;
	s->voice = p_voice - voice_tb;
	s->staff = p_voice->staff;
	s->time = last_ts->time;

	if ((s->next = p_voice->last_sym->next) != 0)
		s->next->prev = s;
	p_voice->last_sym->next = s;
	s->prev = p_voice->last_sym;
	p_voice->last_sym = s;

	s->ts_next = last_ts;
	s->ts_prev = last_ts->ts_prev;
	s->ts_prev->ts_next = s;
	if (s->ts_prev->type != type)
		s->sflags |= S_SEQST;
	last_ts->ts_prev = s;
	if (last_ts->type == type && s->voice != last_ts->voice) {
		last_ts->sflags &= ~S_SEQST;
		last_ts->shrink = 0;
	}
	return s;
}

/* -- init the symbols at start of a music line -- */
/* this routine is called starting a tune generation,
 * and it is called later for each new music line */
static void init_music_line(void)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s, *last_s;
	int voice, staff;

	/* initialize the voices */
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		voice = p_voice - voice_tb;
		if (cursys->voice[voice].range < 0)
			continue;
		p_voice->second = cursys->voice[voice].second;
		p_voice->staff = staff = cursys->voice[voice].staff;
		s = p_voice->sym;
		if (s == 0)
			continue;
		if (s->type == CLEF) {		/* move the clefs and keysig's */
			if (!p_voice->second) {
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
	}

	/* add a clef at start of each voice */
	last_s = tsfirst;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		voice = p_voice - voice_tb;
		if (cursys->voice[voice].range < 0)
			continue;
		if (last_s->voice == voice && last_s->type == CLEF) {
			last_s->u = 0;		/* normal clef */
#if 0
			if (cursys->staff[p_voice->staff].clef.invis)
				s->as.flags |= ABC_F_INVIS;
#endif
			p_voice->last_sym = last_s;
			last_s = last_s->ts_next;
		} else {
			s = (struct SYMBOL *) getarena(sizeof *s);
			memset(s, 0, sizeof *s);
			s->type = CLEF;
			s->voice = voice;
			s->staff = p_voice->staff;
			s->time = last_s->time;
			if ((s->next = p_voice->sym) != 0)
				s->next->prev = s;
			p_voice->sym = s;
			p_voice->last_sym = s;
			s->ts_next = last_s;
			if ((s->ts_prev = last_s->ts_prev) == 0) {
				tsfirst = s;
				s->sflags |= S_SEQST;
			} else	s->ts_prev->ts_next = s;
			last_s->ts_prev = s;
			if (last_s->type == CLEF)
				last_s->sflags &= ~S_SEQST;
			memcpy(&s->as.u.clef,
				&cursys->staff[p_voice->staff].clef,
				sizeof s->as.u.clef);
			if (cursys->voice[voice].second)
				s->sflags |= S_SECOND;
			if (cursys->staff[p_voice->staff].clef.invis)
				s->as.flags |= ABC_F_INVIS;
			set_yval(s);
		}
	}

	/* add keysig */
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		voice = p_voice - voice_tb;
		if (cursys->voice[voice].range < 0
		    || cursys->voice[voice].second)
			continue;
		if (last_s->voice == voice && last_s->type == KEYSIG) {
			p_voice->last_sym = last_s;
			last_s = last_s->ts_next;
			continue;
		}
		if (p_voice->key.sf != 0 || p_voice->key.nacc != 0) {
			s = sym_new(KEYSIG, p_voice, last_s);
			memcpy(&s->as.u.key, &p_voice->key, sizeof s->as.u.key);
			if (s->as.u.key.bagpipe && s->as.u.key.sf == 2)
				s->u = 3;	/* K:Hp --> G natural */
			set_yval(s);
		}
	}

	/* add time signature if needed */
	if (insert_meter) {
		for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
			voice = p_voice - voice_tb;
			if (cursys->voice[voice].range < 0
			    || cursys->voice[voice].second
			    || p_voice->meter.nmeter == 0)	/* M:none */
				continue;
			if (last_s->voice == voice && last_s->type == TIMESIG) {
				p_voice->last_sym = last_s;
				last_s = last_s->ts_next;
				continue;
			}
			s = sym_new(TIMESIG, p_voice, last_s);
			memcpy(&s->as.u.meter, &p_voice->meter,
			       sizeof s->as.u.meter);
			set_yval(s);
		}
	}

	/* add bar if needed */
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		int i;

		voice = p_voice - voice_tb;
		if (cursys->voice[voice].range < 0
		    || cursys->voice[voice].second
		    || p_voice->bar_start == 0)
			continue;
		i = 4;
		if (p_voice->bar_text == 0	/* if repeat continuation */
		    && p_voice->bar_start == B_OBRA) {
			for (s = p_voice->last_sym;
			     s != 0;
			     s = s->next) {	/* search the end of repeat */
				if (s->type != BAR)
					continue;
				if ((s->as.u.bar.type & 0xf0)	/* if complex bar */
				    || s->as.u.bar.type == B_CBRA
				    || s->as.u.bar.repeat_bar)
					break;
				if (--i < 0)
					break;
			}
			if (s == 0)
				i = -1;
			if (i >= 0 && p_voice->last_sym->time == s->time)
				i = -1;		/* no note */
		}
		if (i >= 0) {
			s = sym_new(BAR, p_voice, last_s);
			s->as.u.bar.type = p_voice->bar_start & 0x3fff;
			if (p_voice->bar_start & 0x8000)
				s->as.flags |= ABC_F_INVIS;
			if (p_voice->bar_start & 0x4000)
				s->sflags |= S_NOREPBRA;
			s->as.text = p_voice->bar_text;
			s->as.u.bar.repeat_bar = p_voice->bar_repeat;
			set_yval(s);
		}
		p_voice->bar_start = 0;
		p_voice->bar_repeat = 0;
		p_voice->bar_text = 0;
	}

	/* add tempo if any (only one) */
	if ((s = info['Q' - 'A']) != 0) {
		if (cfmt.fields[0] & (1 << ('Q' - 'A'))) {
			p_voice = &voice_tb[cursys->top_voice];
			s->type = TEMPO;
			s->voice = p_voice - voice_tb;
			s->staff = p_voice->staff;
			s->time = last_s->time;
			s->next = last_s->extra;
			last_s->extra = s;
		}
		info['Q' - 'A'] = 0;
	}

	/* if initialization of a new music line, compute the spacing,
	 * including the first (old) sequence */
	if ((s = tsnext) != 0) {	/* (if called from cut_symbols()) */
		if ((s = last_s) != 0) {
			for ( ; s != 0; s = s->ts_next)
				if (s->sflags & S_SEQST)
					break;
			for (s = s->ts_next; s != 0; s = s->ts_next)
				if (s->sflags & S_SEQST)
					break;
		}
		set_pitch(last_s);
		set_allsymwidth(s);
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
//	sy = cursys;
	if (cfmt.abc2pscompat) {
		int i;
		for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
			int delta;
			struct SYMBOL *g;

//			i = p_voice - voice_tb;
//			i = sy->voice[i].clef.type;
			i = p_voice->clef.type;
			if (!p_voice->forced_clef
			    || i == PERC)
				continue;
			delta = delpit[i];
			for (s = p_voice->sym; s != 0; s = s->next) {
				switch (s->type) {
				case CLEF:
					i = s->as.u.clef.type;
					if (!s->as.u.clef.check_pitch)
						i = 0;
					delta = delpit[i];
					break;
				case NOTEREST:
					if (delta == 0)
						break;
					if (s->as.type == ABC_T_REST)
						break;
					for (i = s->nhd; i >= 0; i--)
						s->pits[i] += delta;
					break;
				case GRACE:
					if (delta == 0)
						break;
					for (g = s->extra; g != 0; g = g->next) {
						if (g->type != NOTEREST)
							continue;
						for (i = g->nhd; i >= 0; i--)
							g->pits[i] += delta;
					}
					break;
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
			if (s->as.type == ABC_T_NOTE) {
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
				if (s->as.flags & ABC_F_SPACE)
					start_flag = 1;
				break;
			case MREST:
				start_flag = 1;
				break;
			case BAR:
				if (!(s->sflags & S_BEAM_ON))
					start_flag = 1;
				if (s->next == 0
				    && s->prev->as.type == ABC_T_NOTE
				    && s->prev->dur >= BREVE)
					s->prev->head = H_SQUARE;
				break;
			case NOTEREST:
				if (s->sflags & S_TREM2)
					break;
				if (s->as.flags & ABC_F_SPACE)
					start_flag = 1;
				if (start_flag
				    || s->nflags <= 0) {
					if (lastnote != 0) {
						lastnote->sflags |= S_BEAM_END;
						lastnote = 0;
					}
					if (s->nflags <= 0) {
						s->sflags |= (S_BEAM_ST | S_BEAM_END);
						start_flag = 1;
					} else if (s->as.type == ABC_T_NOTE) {
						s->sflags |= S_BEAM_ST;
						start_flag = 0;
					}
				}
				if (s->as.type == ABC_T_NOTE)
					lastnote = s;
				break;
			}
			if (s->as.type == ABC_T_NOTE) {
				pitch = s->pits[0];
				if (s->prev != 0
				    && s->prev->as.type != ABC_T_NOTE) {
					s->prev->pits[0] = (s->prev->pits[0]
							    + pitch) / 2;
				}
			} else	s->pits[0] = pitch;
		}
		if (lastnote != 0)
			lastnote->sflags |= S_BEAM_END;
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
	init_music_line();
	insert_meter &= ~1;		/* keep the 'first line' flag */
	set_pitch(0);			/* adjust the note pitches */
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
		if (s->as.type != ABC_T_NOTE) {
			struct SYMBOL *g;

			if (s->type != GRACE)
				continue;
			g = s->extra;
			if (s->stem == 0
			    && (s->stem = s->multi) == 0)
				s->stem = 1;
			for (; g != 0; g = g->next) {
				g->stem = s->stem;
				g->multi = s->multi;
			}
			continue;
		}

		if (s->stem == 0		/* if not explicitly set */
		    && (s->stem = s->multi) == 0) { /* and alone on the staff */

			/* notes in a beam have the same stem direction */
			if (beam)
				s->stem = laststem;
			else if ((s->sflags & (S_BEAM_ST | S_BEAM_END))
					== S_BEAM_ST) { /* start of beam */
				int avg, n;

				avg = s->yav;
				n = 12;
				for (t = s->next; t != 0; t = t->next) {
					if (t->as.type == ABC_T_NOTE) {
						if (t->multi != 0) {
							avg = n - t->multi;
							break;
						}
						avg += t->yav;
						n += 12;
					}
					if (t->sflags & S_BEAM_END)
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
		} else {			/* stem set by set_stem_dir */
			if ((s->sflags & (S_BEAM_ST | S_BEAM_END))
					== S_BEAM_ST) /* start of beam */
				beam = 1;
		}
		if (s->sflags & S_BEAM_END)
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
		if (s->as.type != ABC_T_NOTE
		    || (s->as.flags & ABC_F_INVIS))
			continue;

		/* treat the stem on two staves with different directions */
		if ((s->sflags & S_XSTEM)
		    && s->ts_prev->stem < 0) {
			s2 = s->ts_prev;
			for (m = 0; m <= s2->nhd; m++) {
				s2->shhd[m] += STEM_XOFF * 2;
				s2->shac[m] -= STEM_XOFF * 2;
			}
			s2->xmx += STEM_XOFF * 2;
		}

		/* search the next note at the same time on the same staff */
		s2 = s;
		for (;;) {
			if ((s2 = s2->ts_next) == 0)
				break;
			if (s2->time != s->time) {
				s2 = 0;
				break;
			}
			if (s2->as.type == ABC_T_NOTE
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
		else
			noteshift = 7.8;

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
			    && (s1->sflags & (S_BEAM_ST | S_BEAM_END))
					== (S_BEAM_ST | S_BEAM_END)) { /* if a flag */
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
				if (cfmt.shiftunisson
				    || s1->dots != s2->dots)
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
				    && (s2->sflags & (S_BEAM_ST | S_BEAM_END))
						== (S_BEAM_ST | S_BEAM_END) /* if flag */
				    && s1->pits[0] < s2->pits[0]
				    && 3 * (s1->pits[s1->nhd] - 18) > s2->ymn)
					d1 = noteshift;
				else
					d1 = noteshift * 0.3;	// (was 0.6)
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
				dp = s1->pits[i1] - s2->pits[i2];
				if (dp > 5 || dp < -5)
					continue;
				if (s2->as.u.note.accs[i2] == 0) {
					if (s2->shhd[i2] < 0
					    && dp == 3) {
						s1->shac[i1] = 9 + 7;
					}
					continue;
				}
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
		if (s->as.type != ABC_T_NOTE) {
			int ymin, ymax;

			if (s->type != GRACE)
				continue;
			ymin = ymax = 12;
			for (g = s->extra; g != 0; g = g->next) {
				if (g->type != NOTEREST)
					continue;
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
		if ((s->sflags & (S_BEAM_ST | S_BEAM_END)) == S_BEAM_ST) {
			for (s2 = s->next; /*s2 != 0*/; s2 = s2->next) {
				if (s2->as.type == ABC_T_NOTE
				    && (s2->sflags & S_BEAM_END))
					break;
			}
/*			if (s2 != 0) */
			    if (s2->nflags > nflags)
				nflags = s2->nflags;
		} else if ((s->sflags & (S_BEAM_ST | S_BEAM_END)) == S_BEAM_END) {
			for (s2 = s->prev; /*s2 != 0*/; s2 = s2->prev) {
				if (s2->sflags & S_BEAM_ST)
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
		if (s->u != 0)
			slen += 2 * s->u;		/* tremolo */
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
				|| (s->sflags & (S_BEAM_ST | S_BEAM_END))
					!= (S_BEAM_ST | S_BEAM_END))) {
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
				|| (s->sflags & (S_BEAM_ST | S_BEAM_END))
					!= (S_BEAM_ST | S_BEAM_END))) {
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
		if (s->as.flags & ABC_F_INVIS)
			p_voice->bar_start |= 0x8000;
		if (s->sflags & S_NOREPBRA)
			p_voice->bar_start |= 0x4000;
	}
	bar_type = s->as.u.bar.type;
	if (bar_type == B_COL)			/* ':' */
		return;
	if ((bar_type & 0x07) != B_COL)		/* if not left repeat bar */
		return;
	if (!(s->sflags & S_RRBAR)) {		/* 'xx:' (not ':xx:') */
		p_voice->bar_start = bar_type & 0x3fff;
		if (s->as.flags & ABC_F_INVIS)
			p_voice->bar_start |= 0x8000;
		if (s->sflags & S_NOREPBRA)
			p_voice->bar_start |= 0x4000;
		if (s->prev != 0 && s->prev->type == BAR)
			delsym(s);
		else	s->as.u.bar.type = B_BAR;
		return;
	}
	if (bar_type == B_DREP) {		/* '::' */
		s->as.u.bar.type = B_RREP;
		p_voice->bar_start = B_LREP;
		if (s->as.flags & ABC_F_INVIS)
			p_voice->bar_start |= 0x8000;
		if (s->sflags & S_NOREPBRA)
			p_voice->bar_start |= 0x4000;
		return;
	}
	for (i = 0; bar_type != 0; i++)
		bar_type >>= 4;
	bar_type = s->as.u.bar.type;
	s->as.u.bar.type = bar_type >> ((i / 2) * 4);
	i = ((i + 1) / 2 * 4);
	bar_type &= 0x3fff;
	p_voice->bar_start = bar_type & ((1 << i) - 1);
	if (s->as.flags & ABC_F_INVIS)
		p_voice->bar_start |= 0x8000;
	if (s->sflags & S_NOREPBRA)
		p_voice->bar_start |= 0x4000;
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
	sy = cursys;
	for (staff = 0; staff <= nstaff; staff++) {
		p_staff = &staff_tb[staff];
		p_staff->y = 0;		/* staff system not computed */
		p_staff->clef.stafflines = sy->staff[staff].clef.stafflines;
		if (sy->staff[staff].clef.staffscale != 0)
			p_staff->clef.staffscale = sy->staff[staff].clef.staffscale;
	}

	/* search the next end of line,
	 * set the repeat measures, (remove some dble bars?) */
	for (s = tsfirst; s != 0; s = s->ts_next) {
		if (s->sflags & S_NL)
			break;
		if ((s2 = s->extra) != 0) {
			for ( ; s2 != 0; s2 = s2->next) {
				if (s2->type == FMTCHG && s2->u == REPEAT) {
					set_repeat(s2, s, 1);
					break;
				}
			}
		}
		if (s->type == STAVES) {
			sy = sy->next;
			for (staff = 0; staff <= sy->nstaff; staff++) {
				p_staff = &staff_tb[staff];
//--fixme: what is this comparison ?
				if (sy->staff[staff].clef.stafflines > p_staff->clef.stafflines)
					p_staff->clef.stafflines = sy->staff[staff].clef.stafflines;
				if (sy->staff[staff].clef.staffscale != 0)
					p_staff->clef.staffscale = sy->staff[staff].clef.staffscale;
			}
		}
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

	for (staff = 0; staff <= nstaff; staff++) {
		p_staff = &staff_tb[staff];
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
				if (s2->as.type == ABC_T_NOTE) {
					s2->sflags |= S_BEAM_END;
					break;
				}
			}
			for (s2 = s->next; s2 != 0; s2 = s2->next) {
				if (s2->as.type == ABC_T_NOTE) {
					s2->sflags |= S_BEAM_ST;
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
		if (s->sflags & S_SEQST) {
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
#if 0
	if (s->type == FMTCHG
	    && s->u == PSSEQ) {		/* if PS sequence at end of line */
		s->sflags &= ~S_SEQST;
		s->shrink = 0;
	}
#endif

	/* set max shrink and stretch */
	if (!cfmt.continueall)
		beta0 = BETA_X;
	else	beta0 = BETA_C;

	/* memorize the glue for the last music line */
	if (tsnext != 0) {
		if (x - width >= 0) {
			alfa_last = (x - width) / (x - xmin);	/* shrink */
			beta_last = 0;
		} else {
			alfa_last = 0;
			beta_last = (width - x) / (xmax - x);	/* stretch */
			if (beta_last > beta0) {
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
		}
	} else {			/* if last music line */
		if (x < width) {
			beta = (width - x) / (xmax - x);	/* stretch */
			if (!cfmt.stretchlast
			    && beta >= beta_last) {
				beta = beta_last * xmax + (1 - beta_last) * x;

				/* shrink underfull last line same as previous */
				if (beta < width * 0.75)
					width = beta;
			}
		}
	}

	spafac = width / x;			/* space expansion factor */

	/* define the x offsets for all symbols */
	x = xmax = 0;
	for (s = tsfirst; s != 0; ) {
		if (s->sflags & S_SEQST) {
			float new_space;

			new_space = s->shrink;
			if (s->space != 0) {
				if (new_space < s->space * spafac)
					new_space = s->space * spafac;
				xmax += s->space * spafac * 1.8;
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
		float min;

		min = s->wr;
		while (!(s->sflags & S_SEQST)) {
			s = s->ts_prev;
			if (s->wr > min)
				min = s->wr;
		}
		xmin += min + 3;
		if (tsnext != 0 && tsnext->space * 0.8 > s->wr + 4) {
			x += tsnext->space * 0.8 * spafac;
			xmax += tsnext->space * 0.8 * spafac * 1.8;
/*#if 0*/
		} else {
/*fixme:should calculate the space according to the last symbol duration */
			x += (min + 4) * spafac;
			xmax += (min + 4) * spafac * 1.8;
/*#endif*/
		}
	}

	/* calculate the exact glue */
	alfa = beta = 0;
	if (x >= width) {
		if (x == xmin)
			alfa = 1;
		else	alfa = (x - width) / (x - xmin);	/* shrink */
#if 1
		if (alfa > 1) {
			error(0, s,
			      "Line too much shrunk (%.0fpt of %.0fpt)",
				xmin, width);
			alfa = 1;
		}
#endif
		realwidth = xmin * alfa + x * (1 - alfa);
	} else {
		if (xmax > x)
			beta = (width - x) / (xmax - x);	/* stretch */
		else	beta = 1;		/* (no note) */
		if (beta > beta0) {
			if (!cfmt.stretchstaff)
				beta = 0;
		}
		realwidth = xmax * beta + x * (1 - beta);
	}

	/* set the final x offsets */
	s = tsfirst;
	if (alfa != 0) {
		x = xmin = 0;
		for (;;) {
			if (s->sflags & S_SEQST) {
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
			if (s->sflags & S_SEQST)
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

			if (s->type != GRACE)
				continue;
			x = s->x - s->wl + (cfmt.gracespace >> 16) * 0.1;
			for (g = s->extra; g != 0; g = g->next)
				if (g->type == NOTEREST)
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
	tsfirst->ts_prev = 0;
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
/* -- initialize the start of generation / new music line -- */
static void gen_init(void)
{
	struct SYMBOL *s;

	for (s = tsfirst; s != 0; s = s->ts_next) {
		if (s->extra != 0)
			output_ps(s, ABC_S_HEAD);
		switch (s->type) {
		case CLEF:
		case KEYSIG:
		case TIMESIG:
			continue;
		case FMTCHG:
			if (s->extra != 0)
				output_ps(s, 127);
			delsym(s);
			if (tsfirst == 0)
				return;
			continue;
		case STAVES:
#if 0
/*fixme: does this work in all cases? */
		    {
			int staff;

			for (staff = 0; staff < MAXSTAFF; staff++) {
				cursys->next->staff[staff].clef.type =
					cursys->staff[staff].clef.type;
				cursys->next->staff[staff].clef.line =
					cursys->staff[staff].clef.line;
				cursys->next->staff[staff].clef.octave =
					cursys->staff[staff].clef.octave;
				cursys->next->staff[staff].clef.invis =
					cursys->staff[staff].clef.invis;
			}
		    }
#endif
			cursys = cursys->next;
			delsym(s);
			if (tsfirst == 0)
				return;
			break;
		}
		return;
	}
	tsfirst = 0;			/* no note */
}

/* -- update the clefs at start of line -- */
static void update_clefs(void)
{
	struct SYMBOL *s;
	int staff;

	s = tsfirst;
	while (s->type == CLEF)
		s = s->ts_next;
	for ( ; s != 0; s = s->ts_next) {
		if (s->type != CLEF)
			continue;
		staff = s->staff;
		cursys->staff[staff].clef.type = s->as.u.clef.type;
		cursys->staff[staff].clef.line = s->as.u.clef.line;
		cursys->staff[staff].clef.octave = s->as.u.clef.octave;
		cursys->staff[staff].clef.invis = s->as.u.clef.invis;
	}
}

/* -- show the errors -- */
static void error_show(void)
{
	struct SYMBOL *s;

	for (s = tsfirst; s != 0; s = s->ts_next) {
		if (s->as.flags & ABC_F_ERROR) {
			putxy(s->x, staff_tb[s->staff].y + s->y);
			PUT0("showerror\n");
		}
	}
}

/* -- buffer information until the staves are defined -- */
static float delayed_output(float indent)
{
	float line_height;
	char *outbuf_sav, *mbf_sav, tmpbuf[BUFFSZ];

	outbuf_sav = outbuf;
	mbf_sav = mbf;
	mbf = outbuf = tmpbuf;
	*outbuf = '\0';
	outft = -1;
	draw_sym_near();
	outbuf = outbuf_sav;
	mbf = mbf_sav;
	outft = -1;
	line_height = draw_systems(indent);
	a2b("%s", tmpbuf);
	return line_height;
}

/* -- generate the music -- */
void output_music(void)
{
	struct VOICE_S *p_voice;
	float lwidth, indent;

	/* set the staff system if any STAVES at start of the next line */
	gen_init();
	if (tsfirst == 0)
		return;
	check_buffer();	/* dump buffer if not enough space for a music line */
	set_global();			/* initialize the generator */
	if (first_voice->next != 0) {	/* if many voices */
		if (cfmt.combinevoices)
			combine_voices();
		set_stem_dir();		/* set the stems direction in 'multi' */
		set_rest_offset();	/* set the vertical offset of rests */
	}
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next)
		set_beams(p_voice->sym);	/* decide on beams */
	set_stems();			/* set the stem lengths */
	if (first_voice->next != 0)	/* when multi-voices */
		set_overlap();		/* shift the notes on voice overlap */
	set_allsymwidth(0);		/* set the symbols width */

	lwidth = ((cfmt.landscape ? cfmt.pageheight : cfmt.pagewidth)
		- cfmt.leftmargin - cfmt.rightmargin)
			/ cfmt.scale;
	if (lwidth < 1 CM) {
		error(1, 0, "Bad page width %.1f", lwidth);
		lwidth = 20 CM;
	}
	indent = set_indent();
	set_bar_num();
	cut_tune(lwidth, indent);
	alfa_last = 0.1;
	beta_last = 0;
	for (;;) {			/* loop per music line */
		float line_height;

		set_piece();
		set_sym_glue(lwidth - indent);
		if (indent != 0)
			PUT1("%.2f 0 T\n", indent); /* do indentation */
		line_height = delayed_output(indent);
		draw_all_symb();
		draw_all_deco();
		if (showerror > 1) {
			showerror = 1;
			error_show();
		}
		bskip(line_height);
		if (indent != 0)
			PUT1("%.2f 0 T\n", -indent);
		update_clefs();
		tsfirst = tsnext;
		gen_init();
		if (tsfirst == 0)
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
