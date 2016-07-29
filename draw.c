/*
 * Drawing functions.
 *
 * This file is part of abcm2ps.
 *
 * Copyright (C) 1998-2008 Jean-Fran�ois Moine
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
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#include "abcparse.h"
#include "abc2ps.h"

struct BEAM {			/* packages info on one beam */
	struct SYMBOL *s1, *s2;
	float a, b;
	short nflags;
};

static char *acc_tb[] = { "", "sh", "nt", "ft", "dsh", "dft" };

/* scaling stuff */
static float cur_scale = 1;	/* voice or staff scale */
static float cur_trans = 0;	/* != 0 when scaled staff */

static void draw_note(float x,
		      struct SYMBOL *s,
		      int fl);
static void set_sscale(int staff);
static void set_tie_room(void);

/* -- up/down shift needed to get k*6 -- */
static float rnd6(float y)
{
	int iy;

	iy = ((int) (y + 2.999) + 12) / 6 * 6 - 12;
	return iy - y;
}

/* -- compute the best vertical offset for the beams -- */
static float b_pos(int grace,
		   int stem,
		   int flags,
		   float b)
{
	float d1, d2, shift, depth;
	float top, bot;

	shift = !grace ? BEAM_SHIFT : 3;
	depth = !grace ? BEAM_DEPTH : 1.7;
	if (stem > 0) {
		bot = b - (flags - 1) * shift - depth;
		if (bot > 26)
			return 0;
		top = b;
	} else {
		top = b + (flags - 1) * shift + depth;
		if (top < -2)
			return 0;
		bot = b;
	}

	d1 = rnd6(top - BEAM_OFFSET);
	d2 = rnd6(bot + BEAM_OFFSET);
	if (d1 * d1 > d2 * d2)
		return d2;
	return d1;
}

/* -- calculate a beam -- */
/* (the staves may be defined or not) */
static int calculate_beam(struct BEAM *bm,
			  struct SYMBOL *s1)
{
	struct SYMBOL *s, *s2;
	int notes, nflags, staff, voice, two_staves, two_dir;
	float x, y, ys, a, b, max_stem_err;
	float sx, sy, sxx, sxy, syy, a0, stem_xoff, scale;
	static float min_tb[2][6] = {
		{STEM_MIN, STEM_MIN,
			STEM_MIN2, STEM_MIN3, STEM_MIN4, STEM_MIN4},
		{STEM_CH_MIN, STEM_CH_MIN,
			STEM_CH_MIN2, STEM_CH_MIN3, STEM_CH_MIN4, STEM_CH_MIN4}
	};

	/* must have one printed note head */
	if (s1->as.flags & s1->next->as.flags & ABC_F_INVIS)
		return 0;

	/* find first and last note in beam */
	notes = nflags = 0;	/* set x positions, count notes and flags */
	two_staves = two_dir = 0;
	staff = s1->staff;
	voice = s1->voice;
	stem_xoff = (s1->as.flags & ABC_F_GRACE) ? GSTEM_XOFF : STEM_XOFF;
	for (s2 = s1; ; s2 = s2->next) {
		if (s2->as.type != ABC_T_NOTE)
			continue;
		if (s2->nflags > nflags)
			nflags = s2->nflags;
		notes++;
		if (s2->staff != staff)
			two_staves = 1;
		if (s2->stem != s1->stem)
			two_dir = 1;
		if (s2->sflags & S_BEAM_END)
			break;
	}
	bm->s2 = s2;			/* (don't display the flags) */
	if (staff_tb[staff].y == 0) {	/* staves not defined */
		if (two_staves)
			return 0;
	} else {			/* staves defined */
		if (!two_staves && !(s1->as.flags & ABC_F_GRACE)) {
			bm->s1 = s1;	/* beam already calculated */
			bm->a = (s1->ys- s2->ys) / (s1->xs - s2->xs);
			bm->b = s1->ys - s1->xs * bm->a
				+ staff_tb[staff].y;
			bm->nflags = nflags;
			return 1;
		}
	}

	sx = sy = sxx = sxy = syy = 0;	/* linear fit through stem ends */
	for (s = s1; ; s = s->next) {
		if (s->as.type != ABC_T_NOTE)
			continue;
		if ((scale = voice_tb[s->voice].scale) == 1)
			scale = staff_tb[s->staff].clef.staffscale;
		if (s->stem >= 0)
			x = stem_xoff + s->shhd[0];
		else	x = -stem_xoff + s->shhd[s->nhd];
		x *= scale;
		x += s->x;
		s->xs = x;
		y = s->ys + staff_tb[s->staff].y;
		sx += x; sy += y;
		sxx += x * x; sxy += x * y; syy += y * y;
		if (s == s2)
			break;
	}

	/* beam fct: y=ax+b */
	a = (sxy * notes - sx * sy) / (sxx * notes - sx * sx);
	b = (sy - a * sx) / notes;

	/* the next few lines modify the slope of the beam */
	if (!(s1->as.flags & ABC_F_GRACE)) {
		if (notes >= 3) {
			float hh;

			hh = syy - a * sxy - b * sy;	/* flatten if notes not in line */
			if (hh > 0
			    && hh / (notes - 2) > .5)
				a *= BEAM_FLATFAC;
		}
		if (a >= 0)
			a = BEAM_SLOPE * a / (BEAM_SLOPE + a);	/* max steepness for beam */
		else	a = BEAM_SLOPE * a / (BEAM_SLOPE - a);
	} else {
		if (a > BEAM_SLOPE)
			a = BEAM_SLOPE;
		else if (a < -BEAM_SLOPE)
			a = -BEAM_SLOPE;
	}

	/* to decide if to draw flat etc. use normalized slope a0 */
	a0 = a * (s2->xs - s1->xs) / (20 * (notes - 1));

	if (a0 * a0 < BEAM_THRESH * BEAM_THRESH)
		a = 0;			/* flat below threshhold */

	b = (sy - a * sx) / notes;	/* recalculate b for new slope */

/*  if (nflags>1) b=b+2*stem;*/	/* leave a bit more room if several beams */

	/* have flat beams when asked */
	if (cfmt.flatbeams
	    && voice_tb[voice].key.bagpipe) {
		if (!(s1->as.flags & ABC_F_GRACE))
			b = -11 + staff_tb[staff].y;
		else	b = 35 + staff_tb[staff].y;
		a = 0;
	}

/*fixme: have a look again*/
	/* have room for the symbols in the staff */
	max_stem_err = 0;		/* check stem lengths */
	s = s1;
	if (two_dir) {				/* 2 directions */
/*fixme: more to do*/
		if (!(s1->as.flags & ABC_F_GRACE))
			ys = BEAM_SHIFT;
		else	ys = 3;
		ys *= (nflags - 1);
		ys += BEAM_DEPTH;
		ys *= .5;
		if (s1->stem != s2->stem && s1->nflags < s2->nflags)
			ys *= s2->stem;
		else	ys *= s1->stem;
		b += ys;
	} else if (!(s1->as.flags & ABC_F_GRACE)) {	/* normal notes */
		float stem_err, beam_h;

		beam_h = BEAM_DEPTH + BEAM_SHIFT * (nflags - 1);
		while (s->ts_prev->as.type == ABC_T_NOTE
		       && s->ts_prev->time == s->time)
			s = s->ts_prev;
		for (; s != 0 && s->time <= s2->time; s = s->ts_next) {
			if (s->as.type != ABC_T_NOTE
			    || (s->as.flags & ABC_F_INVIS)
			    || (s->staff != staff
				&& s->voice != voice)) {
				continue;
			}
			ys = a * s->x + b - staff_tb[s->staff].y;
			if (s->voice == voice) {
				if (s->nhd == 0)
					stem_err = min_tb[0][(unsigned) s->nflags];
				else	stem_err = min_tb[1][(unsigned) s->nflags];
				if (s->stem > 0) {
					if (s->pits[s->nhd] > 26) {
						stem_err -= 2;
						if (s->pits[s->nhd] > 28)
							stem_err -= 2;
						if (stem_err < 5)
							stem_err = 5;
					}
					stem_err -= ys - (float) (3 * (s->pits[s->nhd] - 18));
				} else {
					if (s->pits[0] < 18) {
						stem_err -= 2;
						if (s->pits[0] < 16)
							stem_err -= 2;
						if (stem_err < 5)
							stem_err = 5;
					}
					stem_err -= (float) (3 * (s->pits[0] - 18)) - ys;
				}
				stem_err += BEAM_DEPTH + BEAM_SHIFT * (s->nflags - 1);
			} else {
/*fixme: KO when two_staves*/
				if (s1->stem > 0) {
					if (s->stem > 0) {
/*fixme: KO when the voice numbers are inverted*/
						if (s->ymn > ys + 4
						    || s->ymx < ys - beam_h - 2)
							continue;
						if (s->voice > voice)
							stem_err = s->ymx - ys;
						else	stem_err = s->ymn + 8 - ys;
					} else	stem_err = s->ymx - ys;
				} else {
					if (s->stem < 0) {
						if (s->ymx < ys - 4
						    || s->ymn > ys - beam_h - 2)
							continue;
						if (s->voice < voice)
							stem_err = ys - s->ymn;
						else	stem_err = ys - s->ymx + 8;
					} else	stem_err = ys - s->ymn;
				}
				stem_err += 2 + beam_h;
			}
			if (stem_err > max_stem_err)
				max_stem_err = stem_err;
		}
	} else {				/* grace notes */
		for ( ; ; s = s->next) {
			float stem_err;

			ys = a * s->xs + b - staff_tb[s->staff].y;
			stem_err = GSTEM - 2;
			if (s->stem > 0)
				stem_err -= ys - (float) (3 * (s->pits[s->nhd] - 18));
			else	stem_err += ys - (float) (3 * (s->pits[0] - 18));
			stem_err += 3 * (s->nflags - 1);
			if (stem_err > max_stem_err)
				max_stem_err = stem_err;
			if (s == s2)
				break;
		}
	}

	if (max_stem_err > 0)		/* shift beam if stems too short */
		b += s1->stem * max_stem_err;

	/* have room for the gracenotes, bars and clefs */
/*fixme: test*/
    if (!two_staves && !two_dir)
	for (s = s1->next; ; s = s->next) {
		struct SYMBOL *g;

		switch (s->type) {
		case BAR:
			if (!(s->as.flags & ABC_F_INVIS))
				break;
			/*fall thru*/
		case CLEF:
			y = a * s->x + b;
			if (s1->stem > 0) {
				y = s->ymx - y
					+ BEAM_DEPTH + BEAM_SHIFT * (nflags - 1)
					+ 2;
				if (y > 0)
					b += y;
			} else {
				y = s->ymn - y
					- BEAM_DEPTH - BEAM_SHIFT * (nflags - 1)
					- 7;
				if (y < 0)
					b += y;
			}
			break;
		case GRACE:
			g = s->extra;
			for ( ; g != 0; g = g->next) {
				y = a * g->x + b;
				if (s1->stem > 0) {
					y = g->ys - y
						+ BEAM_DEPTH + BEAM_SHIFT * (nflags - 1)
						+ 2;
					if (y > 0)
						b += y;
				} else {
					y = g->y - y
						- BEAM_DEPTH - BEAM_SHIFT * (nflags - 1)
						- 7;
					if (y < 0)
						b += y;
				}
			}
			break;
		}
		if (s == s2)
			break;
	}

	if (a == 0)		/* shift flat beams onto staff lines */
		b += b_pos(s1->as.flags & ABC_F_GRACE, s1->stem, nflags, b - staff_tb[staff].y);

	/* adjust final stems and rests under beam */
	for (s = s1; ; s = s->next) {
		switch (s->as.type) {
		case ABC_T_NOTE:
			s->ys = a * s->xs + b - staff_tb[s->staff].y;
			if (s->stem > 0)
				s->ymx = s->ys + 2.5;
			else	s->ymn = s->ys - 2.5;
			break;
		case ABC_T_REST:
			y = a * s->x + b - staff_tb[s->staff].y;
			if (s1->stem > 0) {
				y -= BEAM_DEPTH + BEAM_SHIFT * (nflags - 1);
				y -= s->head != H_FULL ? 4 : 9;
				if (s1->multi == 0 && y > 12)
					y = 12;
				if (s->y <= y)
					break;
			} else {
				y += BEAM_DEPTH + BEAM_SHIFT * (nflags - 1);
				y += s->head != H_FULL ? 4 : 11;
				if (s1->multi == 0 && y < 12)
					y = 12;
				if (s->y >= y)
					break;
			}
			if (s->head != H_FULL) {
				int iy;

				iy = ((int) (y + 3) + 12) / 6 * 6 - 12;
				y = iy;
			}
			s->y = y;
			break;
		}
		if (s == s2)
			break;
	}

	/* save beam parameters */
	if (staff_tb[staff].y == 0)	/* if staves not defined */
		return 0;
	bm->s1 = s1;
	bm->a = a;
	bm->b = b;
	bm->nflags = nflags;
	return 1;
}

/* -- draw a single beam -- */
/* (the staves are defined) */
static void draw_beam(float x1,
		      float x2,
		      float dy,
		      float h,
		      struct BEAM *bm,
		      int n)			/* beam number (1..n) */
{
	struct SYMBOL *s;
	float y1, dy2;

	s = bm->s1;
	if (n > s->nflags - s->u
	    && (s->sflags & S_TREM2) && s->head != H_EMPTY) {
		if (s->head >= H_OVAL) {
			x1 = s->x + 6;
			x2 = bm->s2->x - 6;
		} else {
			x1 += 5;
			x2 -= 6;
		}
	}

	y1 = bm->a * x1 + bm->b - dy;
	x2 -= x1;
	dy2 = bm->a * x2;

	putf(h);
	putx(x2);
	putf(dy2);
	putxy(x1, y1);
	PUT0("bm\n");
}

/* -- draw the beams for one word -- */
/* (the staves are defined) */
static void draw_beams(struct BEAM *bm)
{
	struct SYMBOL *s, *s1, *s2;
	int i, beam_dir;
	float shift, bshift, bstub, bh;

	s1 = bm->s1;
/*fixme: KO if many staves with different scales*/
	set_scale(s1->voice);
	s2 = bm->s2;
	if (!(s1->as.flags & ABC_F_GRACE)) {
		bshift = BEAM_SHIFT;
		bstub = BEAM_STUB;
		shift = .34;		/* (half width of the stem) */
		bh = BEAM_DEPTH;
	} else {
		bshift = 3;
		bstub = 3.2;
		shift = .29;
		bh = 1.6;
	}

/*fixme: quick hack for stubs at end of beam and different stem directions*/
	beam_dir = s1->stem;
	if (s1->stem != s2->stem
	    && s1->nflags < s2->nflags)
		beam_dir = s2->stem;
	if (beam_dir < 0)
		bh = -bh;
	if (cur_trans == 0 && cur_scale != 1) {
		bm->a /= cur_scale;
		bm->b = s1->ys - s1->xs * bm->a
			+ staff_tb[s1->staff].y;
		bshift *= cur_scale;
	}

	/* make first beam over whole word and adjust the stem lengths */
	draw_beam(s1->xs - shift, s2->xs + shift, 0., bh, bm, 1);
	for (s = s1; ; s = s->next) {
		if (s->as.type == ABC_T_NOTE
		    && s->stem != beam_dir) {
			s->ys = bm->a * s->xs + bm->b
				- staff_tb[s->staff].y
				+ bshift * (s->nflags - 1) * s->stem
				- bh;
		}
		if (s == s2)
			break;
	}

	/* other beams with two or more flags */
	shift = 0;
	for (i = 2; i <= bm->nflags; i++) {
		shift += bshift;
		for (s = s1; ; s = s->next) {
			struct SYMBOL *k1, *k2;
			float x1;

			if (s->as.type != ABC_T_NOTE
			    || s->nflags < i) {
				if (s == s2)
					break;
				continue;
			}
			if ((s->sflags & S_TREM1)
			    && i > s->nflags - s->u) {
				if (s->head >= H_OVAL)
					x1 = s->x;
				else
					x1 = s->xs;
				draw_beam(x1 - 5, x1 + 5,
					  (shift + 2.5) * beam_dir,
					  bh, bm, i);
				if (s == s2)
					break;
				continue;
			}
			k1 = s;
			for (;;) {
				if (s == s2)
					break;
				if ((s->next->type == NOTEREST
				     && s->next->nflags < i)
				    || (s->next->sflags & S_BEAM_BR1)
				    || ((s->next->sflags & S_BEAM_BR2)
					 && i > 2))
					break;
				s = s->next;
			}
			k2 = s;
			while (k2->as.type != ABC_T_NOTE)
				k2 = k2->prev;
			x1 = k1->xs;
			if (k1 == k2) {
				if (k1 == s1
				    || (k1->sflags & S_BEAM_BR1)
				    || ((k1->sflags & S_BEAM_BR2)
					&& i > 2)) {
					x1 += bstub;
				} else if (k1 == s2)
					x1 -= bstub;
				else {
					struct SYMBOL *k;

					k = k1->next;
					while (k->as.type != ABC_T_NOTE)
						k = k->next;
					if ((k->sflags & S_BEAM_BR1)
					    || ((k->sflags & S_BEAM_BR2)
						&& i > 2))
						x1 -= bstub;
					else {
						k1 = k1->prev;
						while (k1->as.type != ABC_T_NOTE)
							k1 = k1->prev;
						if (k1->nflags < k->nflags
						    || (k1->nflags == k->nflags
							&& k1->dots < k->dots))
							x1 += bstub;
						else	x1 -= bstub;
					}
				}
			}
			draw_beam(x1, k2->xs,
#if 1
				  shift * beam_dir,
#else
				  shift * k1->stem,	/*fixme: more complicated */
#endif
				  bh, bm, i);
			if (s == s2)
				break;
		}
	}
}

/* -- draw a system brace or bracket -- */
static void draw_sysbra(float x, int staff, int flag)
{
	int i, end;
	float yt, yb;

	while (cursys->staff[staff].empty
	       || staff_tb[staff].clef.stafflines == 0) {
		if (cursys->staff[staff].flags & flag)
			return;
		staff++;
	}
	i = end = staff;
	for (;;) {
		if (!cursys->staff[i].empty
		    && staff_tb[i].clef.stafflines != 0)
			end = i;
		if (cursys->staff[i].flags & flag)
			break;
		i++;
	}
	yt = staff_tb[staff].y + staff_tb[staff].topbar
				* staff_tb[staff].clef.staffscale;
	yb = staff_tb[end].y + staff_tb[end].botbar
				* staff_tb[end].clef.staffscale;
	PUT4("%.1f %.1f %.1f %s\n",
	     yt - yb, x, yt,
	     (flag & (CLOSE_BRACE | CLOSE_BRACE2)) ? "brace" : "bracket");
}

/* -- draw the left side of the staves -- */
static void draw_lstaff(float x)
{
	int i, j, l, nst;
	float yb;

	if (cfmt.alignbars)
		return;
	nst = cursys->nstaff;
	l = 0;
	for (i = 0; i < nst; i++) {
		if (cursys->staff[i].flags & (OPEN_BRACE | OPEN_BRACKET))
			l++;
		if (!cursys->staff[i].empty
		    && staff_tb[i].clef.stafflines != 0)
			break;
		if (cursys->staff[i].flags & (CLOSE_BRACE | CLOSE_BRACKET))
			l--;
	}
	for (j = nst; j > i; j--) {
		if (!cursys->staff[j].empty
		    && staff_tb[j].clef.stafflines != 0)
			break;
	}
	if (i == j && l == 0)
		return;
	set_scale(-1);
	yb = staff_tb[j].y + staff_tb[j].botbar
				* staff_tb[j].clef.staffscale;
	PUT3("%.1f %.1f %.1f bar\n",
	     staff_tb[i].y
		+ staff_tb[i].topbar * staff_tb[i].clef.staffscale
		- yb,
	     x, yb);
	for (i = 0; i <= nst; i++) {
		if (cursys->staff[i].flags & OPEN_BRACE)
			draw_sysbra(x, i, CLOSE_BRACE);
		if (cursys->staff[i].flags & OPEN_BRACKET)
			draw_sysbra(x, i, CLOSE_BRACKET);
		if (cursys->staff[i].flags & OPEN_BRACE2)
			draw_sysbra(x - 6, i, CLOSE_BRACE2);
		if (cursys->staff[i].flags & OPEN_BRACKET2)
			draw_sysbra(x - 6, i, CLOSE_BRACKET2);
	}
}

/* -- draw a staff -- */
static void draw_staff(int staff,
			float x1, float x2)
{
	int nlines;
	float y;

	/* draw the staff */
	set_sscale(staff);
	y = staff_tb[staff].y;
	nlines = staff_tb[staff].clef.stafflines;
	switch (nlines) {
	case 0:
		return;
	case 1:
		y += 12;
		break;
	case 2:
	case 3:
		y += 6;
		break;
	}
	putx(x2 - x1);
	PUT1("%d ", nlines);
	putxy(x1, y);
	PUT0("staff\n");
}

/* -- draw the time signature -- */
static void draw_timesig(float x,
			 struct SYMBOL *s)
{
	int i, staff, l;
	char *f, meter[64];
	float dx;

	if (s->as.u.meter.nmeter == 0)
		return;
	staff = s->staff;
	x -= s->wl;
	for (i = 0; i < s->as.u.meter.nmeter; i++) {
		l = strlen(s->as.u.meter.meter[i].top);
		if (l > sizeof s->as.u.meter.meter[i].top)
			l = sizeof s->as.u.meter.meter[i].top;
		if (s->as.u.meter.meter[i].bot[0] != '\0') {
			int l2;

			sprintf(meter, "(%.8s)(%.2s)",
				s->as.u.meter.meter[i].top,
				s->as.u.meter.meter[i].bot);
			f = "tsig";
			l2 = strlen(s->as.u.meter.meter[i].bot);
			if (l2 > sizeof s->as.u.meter.meter[i].bot)
				l2 = sizeof s->as.u.meter.meter[i].bot;
			if (l2 > l)
				l = l2;
		} else switch (s->as.u.meter.meter[i].top[0]) {
			case 'C':
				if (s->as.u.meter.meter[i].top[1] != '|')
					f = "csig";
				else {
					f = "ctsig";
					l--;
				}
				meter[0] = '\0';
				break;
			case 'c':
				if (s->as.u.meter.meter[i].top[1] != '.')
					f = "imsig";
				else {
					f = "iMsig";
					l--;
				}
				meter[0] = '\0';
				break;
			case 'o':
				if (s->as.u.meter.meter[i].top[1] != '.')
					f = "pmsig";
				else {
					f = "pMsig";
					l--;
				}
				meter[0] = '\0';
				break;
			case '(':
			case ')':
				sprintf(meter, "(\\%s)",
					s->as.u.meter.meter[i].top);
				f = "stsig";
				break;
			default:
				sprintf(meter, "(%.8s)",
					s->as.u.meter.meter[i].top);
				f = "stsig";
				break;
		}
		if (meter[0] != '\0')
			PUT1("%s ", meter);
		dx = (float) (13 * l);
		putxy(x + dx * .5, staff_tb[staff].y);
		PUT1("%s\n", f);
		x += dx;
	}
}

/* -- draw a key signature -- */
static void draw_keysig(struct VOICE_S *p_voice,
			float x,
			struct SYMBOL *s)
{
	int old_sf = s->u;
	int staff = p_voice->staff;
	float staffb = staff_tb[staff].y;
	int i, clef_ix, shift;
	signed char *p_seq;

	static char clef_tb[4] = {
		7 - 2, 3 - 3, 6 - 4, 7 - 2};
	static char sharp_cl[7] = {24, 9, 15, 21, 6, 12, 18};
	static char flat_cl[7] = {12, 18, 24, 9, 15, 21, 6};
	static signed char sharp1[6] = {-9, 12, -9, -9, 12, -9};
	static signed char sharp2[6] = {12, -9, 12, -9, 12, -9};
	static signed char flat1[6] = {9, -12, 9, -12, 9, -12};
	static signed char flat2[6] = {-12, 9, -12, 9, -12, 9};

	clef_ix = clef_tb[(unsigned) cursys->staff[staff].clef.type]
		+ cursys->staff[staff].clef.line;
	if (clef_ix >= 7)
		clef_ix -= 7;

	/* normal accidentals */
	if (s->as.u.key.nacc == 0) {

		/* if flats to sharps, or sharps to flats, put neutrals */
		if (s->as.u.key.sf == 0
		    || old_sf * s->as.u.key.sf < 0) {

			/* old sharps */
			shift = sharp_cl[clef_ix];
			p_seq = shift > 9 ? sharp1 : sharp2;
			for (i = 0; i < old_sf; i++) {
				putxy(x, staffb + shift);
				PUT0("nt0 ");
				shift += *p_seq++;
				x += 5;
			}

			/* old flats */
			shift = flat_cl[clef_ix];
			p_seq = shift < 18 ? flat1 : flat2;
			for (i = 0; i > old_sf; i--) {
				putxy(x, staffb + shift);
				PUT0("nt0 ");
				shift += *p_seq++;
				x += 5;
			}
			if (s->as.u.key.sf != 0)
				x += 3;		/* extra space */

		/* if less sharps or flats, put neutrals */
		/* sharps */
		} else if (s->as.u.key.sf > 0) {
			if (s->as.u.key.sf < old_sf) {
				shift = sharp_cl[clef_ix];
				p_seq = shift > 9 ? sharp1 : sharp2;
				for (i = 0; i < s->as.u.key.sf; i++)
					shift += *p_seq++;
				for (; i < old_sf; i++) {
					putxy(x, staffb + shift);
					PUT0("nt0 ");
					shift += *p_seq++;
					x += 5;
				}
				x += 3;		/* extra space */
			}
		/* flats */
		} else /*if (s->as.u.key.sf < 0)*/ {
			if (s->as.u.key.sf > old_sf) {
				shift = flat_cl[clef_ix];
				p_seq = shift < 18 ? flat1 : flat2;
				for (i = 0; i > s->as.u.key.sf; i--)
					shift += *p_seq++;
				for (; i > old_sf; i--) {
					putxy(x, staffb + shift);
					PUT0("nt0 ");
					shift += *p_seq++;
					x += 5;
				}
				x += 3;		/* extra space */
			}
		}

		/* new sharps */
		shift = sharp_cl[clef_ix];
		p_seq = shift > 9 ? sharp1 : sharp2;
		for (i = 0; i < s->as.u.key.sf; i++) {
			putxy(x, staffb + shift);
			PUT0("sh0 ");
			shift += *p_seq++;
			x += 5;
		}

		/* new flats */
		shift = flat_cl[clef_ix];
		p_seq = shift < 18 ? flat1 : flat2;
		for (i = 0; i > s->as.u.key.sf; i--) {
			putxy(x, staffb + shift);
			PUT0("ft0 ");
			shift += *p_seq++;
			x += 5;
		}
	} else {
		int last_acc, last_shift;

		/* explicit accidentals */
		last_acc = s->as.u.key.accs[0];
		last_shift = 100;
		for (i = 0; i < s->as.u.key.nacc; i++) {
			if (s->as.u.key.accs[i] != last_acc) {
				last_acc = s->as.u.key.accs[i];
				x += 3;
			}
			shift = clef_ix * 6
				+ 3 * (s->as.u.key.pits[i] - 18);
			while (shift < -3)
				shift += 21;
			while (shift > 24 + 3)
				shift -= 21;
			if (shift == last_shift + 21
			    || shift == last_shift - 21)
				x -= 5;		/* same note */
			last_shift = shift;
			putxy(x, staffb + shift);
			PUT2("%s%d ",
			     acc_tb[last_acc & 0x07], micro_tb[last_acc >> 3]);
			x += 5;
		}
	}
	if (old_sf != 0 || s->as.u.key.sf != 0 || s->as.u.key.nacc >= 0)
		PUT0("\n");
}

/* -- convert the standard measure bars -- */
static int bar_cnv(int bar_type)
{
	switch (bar_type) {
	case B_OBRA:
	case B_CBRA:
	case (B_OBRA << 4) + B_CBRA:
		return 0;			/* invisible */
	case B_COL:
		return B_BAR;			/* dotted */
	case (B_CBRA << 4) + B_BAR:
		return B_BAR;
	case (B_BAR << 4) + B_COL:
		bar_type |= (B_OBRA << 8);
		break;
	case (B_BAR << 8) + (B_COL << 4) + B_COL:
		bar_type |= (B_OBRA << 12);
		break;
	case (B_BAR << 12) + (B_COL << 8) + (B_COL << 4) + B_COL:
		bar_type |= (B_OBRA << 16);
		break;
	case (B_COL << 4) + B_BAR:
	case (B_COL << 8) + (B_COL << 4) + B_BAR:
	case (B_COL << 12) + (B_COL << 8) + (B_COL << 4) + B_BAR:
		bar_type <<= 4;
		bar_type |= B_CBRA;
		break;
	case (B_COL << 4) + B_COL:
		bar_type = (B_COL << 12) + (B_CBRA << 8) + (B_OBRA << 4) + B_COL;
		break;
	}
	return bar_type;
}

/* -- draw a measure bar -- */
static void draw_bar(struct SYMBOL *s)
{
	int staff, bar_type, dotted;
	float x, yb, h;
	char *psf;

	staff = s->staff;
	yb = staff_tb[staff].y;
	h = s->ys;
	x = s->x;

	/* if measure repeat, draw the '%' like glyphs */
	if (s->as.u.bar.len != 0) {
		struct SYMBOL *s2;

		set_scale(s->voice);
		if (s->as.u.bar.len == 1) {
			for (s2 = s->prev; s2->as.type != ABC_T_REST; s2 = s2->prev)
				;
			putxy(s2->x, yb);
			PUT0("mrep\n");
		} else {
			putxy(x, yb);
			PUT0("mrep2\n");
			if (s->voice == first_voice - voice_tb) {
/*fixme				set_font(s->gcf); */
				set_font(cfmt.anf);
				putxy(x, yb + staff_tb[staff].topbar + 4);
				PUT1("M(%d)showc\n", s->as.u.bar.len);
			}
		}
	}
	dotted = s->as.u.bar.dotted || s->as.u.bar.type == B_COL;
	bar_type = bar_cnv(s->as.u.bar.type);
	if (bar_type == 0)
		return;				/* invisible */
	yb += staff_tb[staff].botbar * staff_tb[staff].clef.staffscale;
	for (;;) {
		psf = "bar";
		switch (bar_type & 0x07) {
		case B_BAR:
			if (dotted)
				psf = "dotbar";
			break;
		case B_OBRA:
		case B_CBRA:
			psf = "thbar";
			x -= 3;
			break;
		case B_COL:
			x -= 2;
			break;
		}
		switch (bar_type & 0x07) {
		default:
			set_scale(-1);
			PUT4("%.1f %.1f %.1f %s ", h, x, yb, psf);
			break;
		case B_COL:
			set_sscale(staff);
			putxy(x + 1, staff_tb[staff].y);
			PUT0("rdots ");
			break;
		}
		bar_type >>= 4;
		if (bar_type == 0)
			break;
		x -= 3;
	}
	PUT0("\n");
}

/* -- draw a rest -- */
/* (the staves are defined) */
static void draw_rest(struct SYMBOL *s)
{
	int i, y;
	float x, dotx, staffb;

static char *rest_tb[NFLAGS_SZ] = {
	"r128", "r64", "r32", "r16", "r8",
	"r4",
	"r2", "r1", "r0", "r00"
};

	/* if rest alone in the measure, center */
	x = s->x + s->shhd[0] * cur_scale;
	if (s->dur == voice_tb[s->voice].meter.wmeasure) {
		struct SYMBOL *prev;

		if (s->next != 0)
			x = s->next->x;
		else	x = realwidth;
		prev = s->prev;
		if (prev->type != BAR && !(s->sflags & S_SECOND)) {
			for (prev = prev->ts_next; ; prev = prev->ts_next) {
				switch (prev->type) {
				case CLEF:
				case KEYSIG:
				case TIMESIG:
				case FMTCHG:
					continue;
				default:
					break;
				}
				prev = prev->ts_prev;
				break;
			}
		}
		x = (x + prev->x) * .5;

		/* center the associated decorations */
		if (s->as.u.note.dc.n > 0)
			deco_update(s, x - s->x);
		s->x = x;
	}
	if ((s->as.flags & ABC_F_INVIS)
	    && !(s->sflags & S_OTHER_HEAD))
		return;

	staffb = staff_tb[s->staff].y;		/* bottom of staff */

	if (s->sflags & S_REPEAT) {
		putxy(x, staffb);
		if (s->doty < 0)
			PUT0("srep\n");
		else {
			PUT0("mrep\n");
			if (s->doty > 2
			    && s->voice == first_voice - voice_tb) {
/*fixme				set_font(s->gcf); */
				set_font(cfmt.anf);
				putxy(x, staffb + 24 + 4);
				PUT1("M(%d)showc\n", s->doty);
			}
		}
		return;
	}

	y = s->y;

	if (s->sflags & S_OTHER_HEAD) {
		draw_all_deco_head(s, x, y + staffb);
		return;
	}

	i = C_XFLAGS - s->nflags;		/* rest_tb index */
	if (i == 7 && y == 12
	    && staff_tb[s->staff].clef.stafflines <= 2)
		y -= 6;				/* semibreve a bit lower */

	putxy(x, y + staffb);				/* rest */
	PUT1("%s ", rest_tb[i]);

	/* output ledger line(s) when greater than minim */
	if (i >= 6) {
		int yb, yt;

		switch (staff_tb[s->staff].clef.stafflines) {
		case 0:
			yb = 12;
			yt = 12;
			break;
		case 1:
			yb = 6;
			yt = 18;
			break;
		case 2:
			yb = 0;
			yt = 18;
			break;
		case 3:
			yb = 0;
			yt = 24;
			break;
		default:
			yb = -6;
			yt = staff_tb[s->staff].clef.stafflines * 6;
			break;
		}
		switch (i) {
		case 6:					/* minim */
			if (y <= yb || y >= yt) {
				putxy(x, y + staffb);
				PUT0("hl ");
			}
			break;
		case 7:					/* semibreve */
			if (y < yb || y >= yt - 6) {
				putxy(x, y + 6 + staffb);
				PUT0("hl ");
			}
			break;
		default:
			if (y < yb || y >= yt - 6) {
				putxy(x,y + 6 + staffb);
				PUT0("hl ");
			}
			if (i == 9)			/* longa */
				y -= 6;
			if (y <= yb || y >= yt) {
				putxy(x, y + staffb);
				PUT0("hl ");
			}
			break;
		}
	}

	dotx = 8;
	for (i = 0; i < s->dots; i++) {
		PUT1("%.1f 3 dt ", dotx);
		dotx += 3.5;
	}
	PUT0("\n");
}

/* -- draw grace notes -- */
/* (the staves are defined) */
static void draw_gracenotes(struct SYMBOL *s)
{
	int yy;
	float x0, y0, x1, y1, x2, y2, x3, y3, bet1, bet2, dy1, dy2;
	float staffb;
	struct SYMBOL *g, *last;
	struct BEAM bm;

	/* draw the notes */
	bm.s2 = 0;				/* (draw flags) */
	for (g = s->extra; ; g = g->next) {
		if (g->type != NOTEREST)
			continue;
		if (s->extra->next != 0) {	/* if many notes */
			if ((g->sflags & S_BEAM_ST)
			    && !(g->sflags & S_BEAM_END)) {
				if (calculate_beam(&bm, g))
					draw_beams(&bm);
			}
		}
		draw_note(g->x, g, bm.s2 == 0);
		if (g == bm.s2)
			bm.s2 = 0;		/* (draw flags again) */

		if (g->as.flags & ABC_F_SAPPO) {	/* (on 1st note only) */
			if (g->next == 0) {	/* if one note */
				x1 = 9;
				y1 = g->stem > 0 ? 5 : -5;
			} else {		/* many notes */
				x1 = (g->next->x - g->x) * .5 + 4;
				y1 = (g->ys + g->next->ys) * .5 - g->y;
				if (g->stem > 0)
					y1 -= 1;
				else	y1 += 1;
			}
			putxy(x1, y1);
			PUT1("g%ca\n", g->stem > 0 ? 'u' : 'd');
		}
		if (g->next == 0)
			break;
	}

	/* slur */
	if (voice_tb[s->voice].key.bagpipe	/* no slur when bagpipe */
	    || !cfmt.graceslurs
	    || s->as.u.note.slur_st		/* explicit slur */
	    || s->next == 0
	    || s->next->as.type != ABC_T_NOTE)
		return;
	last = g;
	yy = 127;
	for (g = s->extra; g != 0; g = g->next) {
		if (g->type != NOTEREST)
			continue;
		if (g->y < yy) {
			yy = g->y;
			last = g;
		}
	}
	x0 = last->x;
	y0 = (last->stem >= 0 ? (float) last->y : last->ys) - 5;
	if (s->extra != last) {
		x0 -= 4;
		y0 += 1;
	}
	s = s->next;
	x3 = s->x - 1;
	if (s->stem < 0)
		x3 -= 4;
	y3 = 3 * (s->pits[0] - 18) - 5;
	dy1 = (x3 - x0) * .4;
	if (dy1 > 3)
		dy1 = 3;
	dy2 = dy1;
	bet1 = .2;
	bet2 = .8;
	if (y0 > y3 + 7) {
		x0 = last->x - 1;
		y0 += .5;
		y3 += 6.5;
		x3 = s->x - 5.5;
		dy2 = (y0 - y3) * .2;
		dy1 = (y0 - y3) * .8;
		bet1 = 0;
	} else if (y3 > y0 + 4) {
		y3 = y0 + 4;
		x0 = last->x + 2;
		y0 = (last->stem >= 0 ? last->y : last->ys) - 4;
	}

	x1 = bet1 * x3 + (1 - bet1) * x0;
	y1 = bet1 * y3 + (1 - bet1) * y0 - dy1;
	x2 = bet2 * x3 + (1 - bet2) * x0;
	y2 = bet2 * y3 + (1 - bet2) * y0 - dy2;

	staffb = staff_tb[s->staff].y;		/* bottom of staff */
	putxy(x1, y1 + staffb);
	putxy(x2, y2 + staffb);
	putxy(x3, y3 + staffb);
	putxy(x0, y0 + staffb);
	PUT0("gsl\n");
}

/* -- set the y offset of the dots -- */
static void setdoty(struct SYMBOL *s,
		    signed char *y_tb)
{
	int m, m1, y, doty;

	/* set the normal offsets */
	doty = s->doty;
	for (m = 0; m <= s->nhd; m++) {
		y = 3 * (s->pits[m] - 18);	/* note height on staff */
		if ((y % 6) == 0) {
			if (doty != 0)
				y -= 3;
			else	y += 3;
		}
		y_tb[m] = y;
	}

	/* dispatch and recenter the dots in the staff spaces */
	for (m = 0; m < s->nhd; m++) {
		if (y_tb[m + 1] > y_tb[m])
			continue;
		m1 = m;
		while (m1 > 0) {
			if (y_tb[m1] > y_tb[m1 - 1] + 6)
				break;
			m1--;
		}
		if (3 * (s->pits[m1] - 18) - y_tb[m1]
				< y_tb[m + 1] - 3 * (s->pits[m + 1] - 18)) {
			while (m1 <= m)
				y_tb[m1++] -= 6;
		} else	y_tb[m + 1] = y_tb[m] + 6;
	}
}

/* -- draw m-th head with accidentals and dots -- */
/* (the staves are defined) */
static void draw_basic_note(float x,
			    struct SYMBOL *s,
			    int m,
			    signed char *y_tb)
{
	int i, y, no_head, head, dots, nflags;
	float staffb, shhd;
	char *p;
	char perc_hd[8];

	staffb = staff_tb[s->staff].y;		/* bottom of staff */
	y = 3 * (s->pits[m] - 18);		/* note height on staff */
	shhd = s->shhd[m] * cur_scale;

	/* draw the note decorations */
	no_head = (s->sflags & S_OTHER_HEAD);
	if (no_head)
		draw_all_deco_head(s, x + shhd, y + staffb);
	if (s->as.u.note.decs[m] != 0) {
		int n;

		i = s->as.u.note.decs[m] >> 3;		/* index */
		n = i + (s->as.u.note.decs[m] & 0x07);	/* # deco */
		for ( ; i < n; i++)
			no_head |= draw_deco_head(s->as.u.note.dc.t[i],
						  x + shhd,
						  y + staffb,
						  s->stem);
	}
	if (s->as.flags & ABC_F_INVIS)
		return;

	/* special case when no head */
	if (s->nohdix >= 0) {
		if ((s->stem > 0 && m <= s->nohdix)
		    || (s->stem < 0 && m >= s->nohdix)) {
			PUT0("/x ");			/* set x y */
			putx(x + shhd);
			PUT0("def/y ");
			puty(y + staffb);
			PUT0("def");
			return;
		}
	}

	identify_note(s, s->as.u.note.lens[m],
		      &head, &dots, &nflags);

	/* output a ledger line if horizontal shift */
	if (s->shhd[m]) {
		int yy;

		yy = 0;
		if (y >= 30) {
			yy = y;
			if (yy % 6)
				yy -= 3;
		} else if (y <= -6) {
			yy = y;
			if (yy % 6)
				yy += 3;
		}
		if (yy) {
			putxy(x + shhd, yy + staffb);
			PUT0("hl ");
		}
	}

	/* draw the head */
	putxy(x + shhd, y + staffb);
	if (no_head)
		p = "/y exch def/x exch def";
	else if (s->as.flags & ABC_F_GRACE)
		p = "ghd";
	else if (cursys->staff[s->staff].clef.type == PERC
		 && (i = s->as.u.note.accs[m]) != 0) {
		i &= 0x07;
		sprintf(perc_hd, "p%shd", acc_tb[i]);
		p = perc_hd;
	} else {
		switch (head) {
		case H_OVAL:
			if (s->as.u.note.lens[m] < BREVE) {
				p = "HD";
				break;
			}
			if (s->head != H_SQUARE) {
				p = "HDD";
				break;
			}
			/* fall thru */
		case H_SQUARE:
			if (s->as.u.note.lens[m] < BREVE * 2)
				p = "breve";
			else	p = "longa";
			break;
		case H_EMPTY:
			p = "Hd"; break;
		default:
			p = "hd"; break;
		}
	}
	PUT0(p);

	/* draw the dots */
/*fixme: to see for grace notes*/
	if (dots) {
		float dotx;
		int doty;

		dotx = (int) (8. + s->xmx);
		doty = y_tb[m] - y;
		while (--dots >= 0) {
			PUT2(" %.1f %d dt", dotx - shhd, doty);
			dotx += 3.5;
		}
	}

	/* draw the accidental */
	if ((i = s->as.u.note.accs[m]) != 0
	    && cursys->staff[s->staff].clef.type != PERC) {
		x -= s->shac[m] * cur_scale;
		PUT0(" ");
		putx(x);
		PUT2((s->as.flags & ABC_F_GRACE) ? "gsc %s%d grestore"
					: "y %s%d",
		     acc_tb[i & 0x07], micro_tb[i >> 3]);
	}
}

/* -- draw a note or a chord -- */
/* (the staves are defined) */
static void draw_note(float x,
		      struct SYMBOL *s,
		      int fl)
{
	int i, m, ma, y;
	float staffb, slen;
	char c, *hltype;
	signed char y_tb[MAXHD];

	if (s->dots)
		setdoty(s, y_tb);
	if (s->head >= H_OVAL)
		x += 1;
	staffb = staff_tb[s->staff].y;

	/* output the ledger lines */
	if (!(s->as.flags & ABC_F_INVIS)) {
		if (s->as.flags & ABC_F_GRACE)
			hltype = "ghl";
		else {
			switch (s->head) {
			default:
				hltype = "hl";
				break;
			case H_OVAL:
				hltype = "hl1";
				break;
			case H_SQUARE:
				hltype = "hl2";
				break;
			}
		}
		y = 3 * (s->pits[0] - 18);	/* lower ledger lines */
		i = -6;
		switch (staff_tb[s->staff].clef.stafflines) {
		case 0:
		case 1: i = 6; break;
		case 2:
		case 3: i = 0; break;
		}
		for ( ; i >= y; i -= 6) {
			putxy(x, i + staffb);
			PUT1("%s ", hltype);
		}
		y = 3 * (s->pits[s->nhd] - 18);	/* upper ledger lines */
		switch (staff_tb[s->staff].clef.stafflines) {
		case 0:
		case 1:
		case 2: i = 18; break;
		case 3: i = 24; break;
		default: i = staff_tb[s->staff].clef.stafflines * 6; break;
		}
		for ( ; i <= y; i += 6) {
			putxy(x, i + staffb);
			PUT1("%s ", hltype);
		}
	}

	/* draw the master note, first or last one */
	if (cfmt.setdefl)
		set_defl(s->stem >= 0 ? DEF_STEMUP : 0);
	ma = s->stem >= 0 ? 0 : s->nhd;

	draw_basic_note(x, s, ma, y_tb);

	/* add stem and flags */
	if (!(s->as.flags & (ABC_F_INVIS | ABC_F_STEMLESS))) {
		char c2;

		c = s->stem >= 0 ? 'u' : 'd';
		slen = (s->ys - s->y) / voice_tb[s->voice].scale;
		if (!fl || s->nflags - s->u <= 0) {	/* stem only */
			c2 = (s->as.flags & ABC_F_GRACE) ? 'g' : 's';
			if (s->nflags > 0) {	/* (fix for PS low resolution) */
				if (s->stem >= 0)
					slen -= 1;
				else	slen += 1;
			}
			PUT3(" %.1f %c%c", slen, c2, c);
		} else {				/* stem and flags */
			if (cfmt.straightflags)
				c = 's';		/* straight flag */
			c2 = (s->as.flags & ABC_F_GRACE) ? 'g' : 'f';
			PUT4(" %d %.1f s%c%c", s->nflags - s->u, slen, c2, c);
		}
	} else if (s->sflags & S_XSTEM) {	/* cross-staff stem */
		struct SYMBOL *s2;

		s2 = s->ts_prev;
		if (s2->stem > 0)
			slen = s2->y - s->y;
		else
			slen = s2->ys - s->y;
		slen += staff_tb[s2->staff].y - staffb;
/*fixme:KO when different scales*/
		slen /= voice_tb[s->voice].scale;
		PUT1(" %.1f su", slen);
	}

	/* draw the tremolo bars */
	if (!(s->as.flags & ABC_F_INVIS)
	    && fl
	    && (s->sflags & S_TREM1)) {
		float x1;

		x1 = x;
		if (s->stem > 0)
			slen = 3 * (s->pits[s->nhd] - 18);
		else
			slen = 3 * (s->pits[0] - 18);
		if (s->head >= H_OVAL) {
			if (s->stem > 0)
				slen = slen + 5 + 5.4 * s->u;
			else
				slen = slen - 5 - 5.4;
		} else {
			x1 += ((s->as.flags & ABC_F_GRACE)
					? GSTEM_XOFF : STEM_XOFF)
							* s->stem;
			if (s->stem > 0)
				slen = slen + 6 + 5.4 * s->u;
			else
				slen = slen - 6 - 5.4;
		}
		slen /= voice_tb[s->voice].scale;
		PUT1(" %d ", s->u);
		putxy(x1, staffb + slen);
		PUT0("trem");
	}

	/* draw the other note heads */
	for (m = 0; m <= s->nhd; m++) {
		if (m == ma)
			continue;
		PUT0(" ");
		draw_basic_note(x, s, m, y_tb);
	}
	PUT0("\n");
}

/* -- find where to terminate/start a slur -- */
static struct SYMBOL *next_scut(struct SYMBOL *s)
{
	struct SYMBOL *prev;

	prev = s;
	for (s = s->next; s != 0; s = s->next) {
		if (s->type == BAR
		    && ((s->sflags & S_RRBAR)
			|| s->as.u.bar.type == B_THIN_THICK
			|| s->as.u.bar.type == B_THICK_THIN
			|| (s->as.u.bar.repeat_bar
			    && s->as.text != 0
			    && s->as.text[0] != '1')))
			return s;
		prev = s;
	}
	/*fixme: KO when no note for this voice at end of staff */
	return prev;
}

static struct SYMBOL *prev_scut(struct SYMBOL *s)
{
	struct SYMBOL *sym;
	int voice;
	float x;

	voice = s->voice;
	for (s = s->prev ; s != 0; s = s->prev) {
		if (s->type == BAR
		    && ((s->sflags & S_RRBAR)
			|| s->as.u.bar.type == B_THIN_THICK
			|| s->as.u.bar.type == B_THICK_THIN
			|| (s->as.u.bar.repeat_bar
			    && s->as.text != 0
			    && s->as.text[0] != '1')))
			return s;
	}

	/* return sym before first note/rest/bar */
	sym = voice_tb[voice].sym;
	for (s = sym->next; s != 0; s = s->next) {
		switch (s->as.type) {
		case ABC_T_NOTE:
		case ABC_T_REST:
		case ABC_T_BAR:
			x = s->x;
			do {
				s = s->prev;
			} while (s->x == x);
			return s;
		}
	}
	return sym;
}

/* -- decide whether a slur goes up or down -- */
static int slur_direction(struct SYMBOL *k1,
			  struct SYMBOL *k2)
{
	struct SYMBOL *s;
	int some_upstem, low;

	some_upstem = low = 0;
	for (s = k1; ; s = s->next) {
		if (s->as.type == ABC_T_NOTE) {
			if (!(s->as.flags & ABC_F_STEMLESS)) {
				if (s->stem < 0)
					return 1;
				some_upstem = 1;
			}
			if (s->pits[0] < 22)	/* if under middle staff */
				low = 1;
		}
		if (s == k2)
			break;
	}
	if (!some_upstem && !low)
		return 1;
	return -1;
}

/* -- output a slur / tie -- */
static void slur_out(float x1,
		     float y1,
		     float x2,
		     float y2,
		     int s,
		     float height,
		     int dotted,
		     int staff)	/* if < 0, the staves are defined */
{
	float alfa, beta, mx, my, xx1, yy1, xx2, yy2, dx, dy, dz;

	alfa = .3;
	beta = .45;

	/* for wide flat slurs, make shape more square */
	dy = y2 - y1;
	if (dy < 0)
		dy = -dy;
	dx = x2 - x1;
	if (dx > 40. && dy / dx < .7) {
		alfa = .3 + .002 * (dx - 40.);
		if (alfa > .7)
			alfa = .7;
	}

	/* alfa, beta, and height determine Bezier control points pp1,pp2
	 *
	 *           X====alfa===|===alfa=====X
	 *	    /		 |	       \
	 *	  pp1		 |	        pp2
	 *	  /	       height		 \
	 *	beta		 |		 beta
	 *      /		 |		   \
	 *    p1		 m		     p2
	 *
	 */

	mx = .5 * (x1 + x2);
	my = .5 * (y1 + y2);

	xx1 = mx + alfa * (x1 - mx);
	yy1 = my + alfa * (y1 - my) + height;
	xx1 = x1 + beta * (xx1 - x1);
	yy1 = y1 + beta * (yy1 - y1);

	xx2 = mx + alfa * (x2 - mx);
	yy2 = my + alfa * (y2 - my) + height;
	xx2 = x2 + beta * (xx2 - x2);
	yy2 = y2 + beta * (yy2 - y2);

	dx = .03 * (x2 - x1);
	if (dx > 10.)
		dx = 10.;
	dy = s;
	dz = .2 + .001 * (x2 - x1);
	if (dz > .6)
		dz = .6;
	dz *= s;

	if (staff < 0) {
		putxy(xx2 - dx, yy2 + dy);
		putxy(xx1 + dx, yy1 + dy);
		putxy(x1, y1 + dz);
		PUT1("0 %.1f ", dz);
		putxy(xx1, yy1);
		putxy(xx2, yy2);
		putxy(x2, y2);
		putxy(x1, y1);
	} else {
		putxy(xx2 - dx, yy2 + dy);
		PUT1("y%d ", staff);
		putxy(xx1 + dx, yy1 + dy);
		PUT1("y%d ", staff);
		putxy(x1, y1 + dz);
		PUT2("y%d 0 %.1f ", staff, dz);
		putxy(xx1, yy1);
		PUT1("y%d ", staff);
		putxy(xx2, yy2);
		PUT1("y%d ", staff);
		putxy(x2, y2);
		PUT1("y%d ", staff);
		putxy(x1, y1);
		PUT1("y%d ", staff);
	}
	PUT0(dotted ? "dSL\n" : "SL\n");
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

/* -- draw a phrasing slur between two symbols -- */
/* (the staves are not yet defined) */
/* (not a pretty routine, this) */
static int draw_slur(struct SYMBOL *k1,
		     struct SYMBOL *k2,
		     int m1,
		     int m2,
		     int slur_type,
		     int dotted)
{
	struct SYMBOL *k;
	float x1, y1, x2, y2, height, addy;
	float a, y, z, h, dx, dy;
	int s, nn, upstaff, two_staves;

/*fixme: if two staves, may have upper or lower slur*/
	switch (slur_type) {
	case SL_ABOVE: s = 1; break;
	case SL_BELOW: s = -1; break;
	default:
		if ((s = slur_multi(k1, k2)) == 0)
			s = slur_direction(k1, k2);
		break;
	}

	nn = 1;
	upstaff = k1->staff;
	two_staves = 0;
	if (k1 != k2)
	    for (k = k1->next; k != 0; k = k->next) {
		if (k->type == NOTEREST) {
			nn++;
			if (k->staff != upstaff) {
				two_staves = 1;
				if (k->staff < upstaff)
					upstaff = k->staff;
			}
		}
		if (k == k2)
			break;
	}
/*fixme: KO when two staves*/
if (two_staves) error(0, k1, "*** multi-staves slurs not treated");

	/* fix endpoints */
	x1 = k1->x + k1->xmx;		/* take the max right side */
	if (k1 != k2)
		x2 = k2->x;
	else {			/* (the slur starts on last note of the line) */
		for (k = k2->ts_next; k != 0; k = k->ts_next)
			if (k->type == STAVES)
				break;
		if (k == 0)
			x2 = realwidth;
		else	x2 = k->x;
	}
	y1 = (float) (s > 0 ? k1->ymx + 2 : k1->ymn - 2);
	y2 = (float) (s > 0 ? k2->ymx + 2 : k2->ymn - 2);

	if (k1->as.type == ABC_T_NOTE) {
		if (s > 0) {
			if (k1->stem > 0) {
				x1 += 5;
				if ((k1->sflags & S_BEAM_END)
				    && k1->nflags >= -1	/* if with a stem */
				    && (!(k1->sflags & S_IN_TUPLET)
					|| k1->ys > y1 - 3)) {
					if (k1->nflags > 0) {
						x1 += 2;
						y1 = k1->ys - 3;
					} else	y1 = k1->ys - 6;
				}
			}
		} else {
			if (k1->stem < 0) {
				x1 -= 1;
				if ((k1->sflags & S_BEAM_END)
				    && k1->nflags >= -1
				    && (!(k1->sflags & S_IN_TUPLET)
					|| k1->ys < y1 + 3)) {
					if (k1->nflags > 0) {
						x1 += 2;
						y1 = k1->ys + 3;
					} else	y1 = k1->ys + 6;
				}
			}
		}
	}

	if (k2->as.type == ABC_T_NOTE) {
		if (s > 0) {
			if (k2->stem > 0) {
				x2 += 1;
				if ((k2->sflags & S_BEAM_ST)
				    && k2->nflags >= -1
				    && (!(k2->sflags & S_IN_TUPLET)
					|| k2->ys > y2 - 3))
					y2 = k2->ys - 6;
			}
		} else {
			if (k2->stem < 0) {
				x2 -= 5;
				if ((k2->sflags & S_BEAM_ST)
				    && k2->nflags >= -1
				    && (!(k2->sflags & S_IN_TUPLET)
					|| k2->ys < y2 + 3))
					y2 = k2->ys + 6;
			}
		}
	}

	if (k1->as.type != ABC_T_NOTE) {
		y1 = y2 + 1.2 * s;
		x1 = k1->x + k1->wr * .5;
		if (x1 > x2 - 12)
			x1 = x2 - 12;
	}

	if (k2->as.type != ABC_T_NOTE) {
		y2 = y1 + 1.2 * s;
		if (k1 != k2)
			x2 = k2->x - k2->wl * .3;
	}

	if (nn >= 3) {
		if (k1->next->x < x1 + 48) {
			if (s > 0) {
				y = k1->next->ymx - 2;
				if (y1 < y)
					y1 = y;
			} else {
				y = k1->next->ymn + 2;
				if (y1 > y)
					y1 = y;
			}
		}
		if (k2->prev->x > x2 - 48) {
			if (s > 0) {
				y = k2->prev->ymx - 2;
				if (y2 < y)
					y2 = y;
			} else {
				y = k2->prev->ymn + 2;
				if (y2 > y)
					y2 = y;
			}
		}
	}

#if 0
	/* shift endpoints */
	addx = .04 * (x2 - x1);
	if (addx > 3.0)
		addx = 3.0;
	addy = .01 * (x2 - x1);
	if (addy > 3.0)
		addy = 3.0;
	x1 += addx;
	x2 -= addx;

/*fixme: to simplify*/
	if (k1->staff == upstaff)
		y1 += s * addy;
	else	y1 = -6;
	if (k2->staff == upstaff)
		y2 += s * addy;
	else	y2 = -6;
#endif

	a = (y2 - y1) / (x2 - x1);		/* slur steepness */
	if (a > SLUR_SLOPE || a < -SLUR_SLOPE) {
		if (a > SLUR_SLOPE)
			a = SLUR_SLOPE;
		else	a = -SLUR_SLOPE;
		if (a * s > 0)
			y1 = y2 - a * (x2 - x1);
		else	y2 = y1 + a * (x2 - x1);
	}

	/* for big vertical jump, shift endpoints */
	y = y2 - y1;
	if (y > 8)
		y = 8;
	else if (y < -8)
		y = -8;
	z = y;
	if (z < 0)
		z = -z;
	dx = .5 * z;
	dy = .3 * y;
	if (y * s > 0) {
		x2 -= dx;
		y2 -= dy;
	} else {
		x1 += dx;
		y1 += dy;
	}

	/* special case for grace notes */
	if (k1->as.flags & ABC_F_GRACE)
		x1 = k1->x - GSTEM_XOFF * .5;
	if (k2->as.flags & ABC_F_GRACE)
		x2 = k2->x + GSTEM_XOFF * 1.5;

	h = 0;
	a = (y2 - y1) / (x2 - x1);
	if (k1 != k2) {
	    addy = y1 - a * x1;
	    for (k = k1->next; k != k2 ; k = k->next) {
		if (k->staff != upstaff)
			continue;
		switch (k->type) {
		case NOTEREST:
			if (s > 0) {
				y = 3 * (k->pits[k->nhd] - 18) + 6;
				if (y < k->ys + 2)
					y = k->ys + 2;
				y -= a * k->x + addy;
				if (y > h)
					h = y;
			} else {
				y = 3 * (k->pits[0] - 18) - 6;
				if (y > k->ys - 2)
					y = k->ys - 2;
				y -= a * k->x + addy;
				if (y < h)
					h = y;
			}
			break;
		case GRACE: {
			struct SYMBOL *g;

			for (g = k->extra; g != 0; g = g->next) {
				y = g->y - a * k->x - addy;
				if (s > 0) {
					y += GSTEM + 2;
					if (y > h)
						h = y;
				} else {
					y -= 2;
					if (y < h)
						h = y;
				}
			}
			break;
		    }
		}
	    }
	    y1 += .45 * h;
	    y2 += .45 * h;
	    h *= .65;
	}

	if (nn > 3)
		height = (.08 * (x2 - x1) + 12) * s;
	else	height = (.03 * (x2 - x1) + 8) * s;
	if (s > 0) {
		if (height < 3 * h)
			height = 3 * h;
		if (height > 40)
			height = 40;
	} else {
		if (height > 3 * h)
			height = 3 * h;
		if (height < -40)
			height = -40;
	}

	y = y2 - y1;
	if (y < 0)
		y = -y;
	if (s > 0) {
		if (height < .8 * y)
			height = .8 * y;
	} else {
		if (height > -.8 * y)
			height = -.8 * y;
	}
	height *= cfmt.slurheight;

/*fixme: ugly!*/
	if (m1 >= 0)
		y1 = (float) (3 * (k1->pits[m1] - 18) + 5 * s);
	if (m2 >= 0)
		y2 = (float) (3 * (k2->pits[m2] - 18) + 5 * s);

	slur_out(x1, y1, x2, y2, s, height, dotted, upstaff);

	/* have room for other symbols */
	dx = x2 - x1;
	a = (y2 - y1) / dx;
/*fixme: it seems to work with .4, but why?*/
	addy = y1 - a * x1 + .4 * height;
	for (k = k1; ; k = k->next) {
		if (k == k2)
			break;
		if (k->staff == upstaff) {
			y = a * k->x + addy;
			if (k->ymx < y)
				k->ymx = y;
			else if (k->ymn > y)
				k->ymn = y;
			if (k->next == k2)
				dx = x2;
			else	dx = k->next->x;
			if (k != k1)
				x1 = k->x;
			dx -= x1;
			y_set(k, s > 0, x1, dx, y);
		}
	}
	return s > 0 ? SL_ABOVE : SL_BELOW;
}

/* -- draw the slurs between 2 symbols --*/
static void draw_slurs(struct SYMBOL *first,
		       struct SYMBOL *last)
{
	struct SYMBOL *s, *s1, *k, *gr1, *gr2;
	int i, m1, m2, again, gr1_out, slur_type, cont;

	for (;;) {
		again = 0;
		gr1 = gr2 = 0;
		s = first;
		for (;;) {
			if (s == 0 || s == last) {
				if (gr1 == 0
				    || (s = gr1->next) == 0
				    || s == last)
					break;
				gr1 = 0;
			}
			if (s->type == GRACE) {
				gr1 = s;
				s = s->extra;
				continue;
			}
			if ((s->type != NOTEREST && s->type != SPACE)
			    || (s->as.u.note.slur_st == 0
				&& !(s->sflags & S_SL1))) {
				s = s->next;
				continue;
			}
			k = 0;			/* find matching slur end */
			s1 = s->next;
			gr1_out = 0;
			for (;;) {
				if (s1 == 0) {
					if (gr2 != 0) {
						s1 = gr2->next;
						gr2 = 0;
						continue;
					}
					if (gr1 == 0 || gr1_out)
						break;
					s1 = gr1->next;
					gr1_out = 1;
					continue;
				}
				if (s1->type == GRACE) {
					gr2 = s1;
					s1 = s1->extra;
					continue;
				}
				if (s1->type == BAR
				    && ((s1->sflags & S_RRBAR)
					|| s1->as.u.bar.type == B_THIN_THICK
					|| s1->as.u.bar.type == B_THICK_THIN
					|| (s1->as.u.bar.repeat_bar
					    && s1->as.text != 0
					    && s1->as.text[0] != '1'))) {
					k = s1;
					break;
				}
				if (s1->type != NOTEREST && s1->type != SPACE) {
					s1 = s1->next;
					continue;
				}
				if (s1->as.u.note.slur_end
				    || (s1->sflags & S_SL2)) {
					k = s1;
					break;
				}
				if (s1->as.u.note.slur_st
				    || (s1->sflags & S_SL1)) {
					again++;
					break;
				}
				if (s1 == last)
					break;
				s1 = s1->next;
			}
			if (s1 == 0)
				k = next_scut(s);
			else if (k == 0) {
				s = s1;
				if (s == last)
					break;
				continue;
			}

			/* if slur in grace note sequence, change the linkages */
			if (gr1 != 0) {
				for (s1 = s; s1->next != 0; s1 = s1->next)
					;
				s1->next = gr1->next;
				gr1->next->prev = s1;
				gr1->as.u.note.slur_st = SL_AUTO;
			}
			if (gr2 != 0) {
				gr2->prev->next = gr2->extra;
				gr2->extra->prev = gr2->prev;
				gr2->as.u.note.slur_st = SL_AUTO;
			}
			if (s->as.u.note.slur_st) {
				slur_type = s->as.u.note.slur_st & 0x03;
				s->as.u.note.slur_st >>= 2;
				m1 = -1;
			} else {
				for (m1 = 0; m1 <= s->nhd; m1++)
					if (s->as.u.note.sl1[m1])
						break;
				slur_type = s->as.u.note.sl1[m1] & 0x03;
				s->as.u.note.sl1[m1] >>= 2;
				if (s->as.u.note.sl1[m1] == 0) {
					for (i = m1 + 1; i <= s->nhd; i++)
						if (s->as.u.note.sl1[i])
							break;
					if (i > s->nhd)
						s->sflags &= ~S_SL1;
				}
			}
			m2 = -1;
			cont = 0;
			if ((k->type == NOTEREST || k->type == SPACE)
			    && (k->as.u.note.slur_end
				    || (k->sflags & S_SL2))) {
				if (k->as.u.note.slur_end)
					k->as.u.note.slur_end--;
				else {
					for (m2 = 0; m2 <= k->nhd; m2++)
						if (k->as.u.note.sl2[m2])
							break;
					k->as.u.note.sl2[m2]--;
					if (k->as.u.note.sl2[m2] == 0) {
						for (i = m2 + 1; i <= k->nhd; i++)
							if (k->as.u.note.sl2[i])
								break;
						if (i > k->nhd)
							k->sflags &= ~S_SL2;
					}
				}
			} else {
				if (k->type != BAR
				    || (!(k->sflags & S_RRBAR)
					&& k->as.u.bar.type != B_THIN_THICK
					&& k->as.u.bar.type != B_THICK_THIN
					&& (!k->as.u.bar.repeat_bar
					    || k->as.text == 0
					    || k->as.text[0] == '1')))
					cont = 1;
			}
			slur_type = draw_slur(s, k, m1, m2,
					      slur_type, s->as.flags & ABC_F_DOTTED_SLUR);
			if (cont) {
/*fixme: the slur types are inverted*/
				voice_tb[k->voice].slur_st <<= 2;
				voice_tb[k->voice].slur_st += slur_type;
			}

			/* if slur in grace note sequence, restore the linkages */
			if (gr1 != 0) {
				gr1->next->prev->next = 0;
				gr1->next->prev = gr1;
			}
			if (gr2 != 0) {
				gr2->prev->next = gr2;
				gr2->extra->prev = 0;
			}

			if (s->as.u.note.slur_st
			    || (s->sflags & S_SL1))
				continue;
			if (s == last)
				break;
			s = s->next;
		}
		if (again == 0)
			break;
	}
}

/* -- draw a tuplet -- */
/* (the staves are not yet defined) */
/* See 'tuplets' in format.txt about the value of 'u' */
static struct SYMBOL *draw_tuplet(struct SYMBOL *t,	/* tuplet in extra */
				  struct SYMBOL *s)	/* main note */
{
	struct SYMBOL *s1, *s2, *sy, *next, *g;
	int r, upstaff, nb_only, some_slur;
	float x1, x2, y1, y2, xm, ym, a, s0, yy, yx, dy;

	next = s;
	if ((t->u & 0x0f00) == 0x100)		/* if 'when' == never */
		return next;

	/* treat the nested tuplets starting on this symbol */
	for (g = t->next; g != 0; g = g->next) {
		if (g->type == TUPLET) {
			sy = draw_tuplet(g, s);
			if (sy->time > next->time)
				next = sy;
		}
	}

	/* search the first and last notes/rests of the tuplet */
	r = t->as.u.tuplet.r_plet;
	s1 = 0;
	some_slur = 0;
	upstaff = s->staff;
	for (s2 = s; s2 != 0; s2 = s2->next) {
		if (s2 != s) {
			for (g = s2->extra; g != 0; g = g->next) {
				if (g->type == TUPLET) {
					sy = draw_tuplet(g, s2);
					if (sy->time > next->time)
						next = sy;
				}
			}
		}
		if (s2->type != NOTEREST) {
			if (s2->type == GRACE) {
				for (g = s2->extra; g != 0; g = g->next) {
					if (g->type != NOTEREST)
						continue;
					if (g->as.u.note.slur_st
					    || (g->sflags & S_SL1))
						some_slur = 1;
				}
			}
			continue;
		}
		if (s2->as.u.note.slur_st
		    || (s2->sflags & S_SL1))
			some_slur = 1;
		if (s2->staff < upstaff)
			upstaff = s2->staff;
		if (s1 == 0)
			s1 = s2;
		if (--r <= 0)
			break;
	}
	if (s2 == 0)
		return next;			/* no solution... */
	if (s2->time > next->time)
		next = s2;

	/* draw the slurs when inside the tuplet */
	if (some_slur)
		draw_slurs(s1, s2);
	if ((t->u & 0x0f0) == 0x10) {	/* 'what' == slur */
		nb_only = 1;
		draw_slur(s1, s2, -1, -1, 
			  s1->stem > 0 ? SL_ABOVE : SL_BELOW, 0);
	} else {

		/* search if a bracket is needed */
		if ((t->u & 0x0f00) == 0x200	/* if 'when' == always */
		    || s1->as.type != ABC_T_NOTE || s2->as.type != ABC_T_NOTE)
			nb_only = 0;
		else {
			nb_only = 1;
			for (sy = s1; ; sy = sy->next) {
				if (sy->type != NOTEREST) {
					if (sy->type == GRACE
					    || sy->type == SPACE)
						continue;
					nb_only = 0;
					break;
				}
				if (sy == s2)
					break;
				if (sy->sflags & S_BEAM_END) {
					nb_only = 0;
					break;
				}
			}
			if (nb_only
			    && !(s1->sflags & (S_BEAM_ST | S_BEAM_BR1 | S_BEAM_BR2))) {
				for (sy = s1->prev; sy != 0; sy = sy->prev) {
					if (sy->type == NOTEREST) {
						if (sy->nflags >= s1->nflags)
							nb_only = 0;
						break;
					}
				}
			}
			if (nb_only && !(s2->sflags & S_BEAM_END)) {
				for (sy = s2->next; sy != 0; sy = sy->next) {
					if (sy->type == NOTEREST) {
						if (!(sy->sflags & (S_BEAM_BR1 | S_BEAM_BR2))
						    && sy->nflags >= s2->nflags)
							nb_only = 0;
						break;
					}
				}
			}
		}
	}

	/* if number only, draw it */
	if (nb_only) {
		float a, b;

		if ((t->u & 0x0f) == 1)		/* if 'value' == none */
			return next;
		xm = (s2->x + s1->x) * .5;
		a = (s2->ys - s1->ys) / (s2->x - s1->x);
		b = s1->ys - a * s1->x;
#if 1
#if 0
		for (sy = s1; ; sy = sy->next) {
			if (sy->x >= xm)
				break;
		}
#endif
		yy = a * xm + b;
		if (s1->stem > 0) {
			ym = y_get(s1, 1, xm - 3, 6, 0);
			if (ym > yy)
				b += ym - yy;
			b += 4;
		} else {
			ym = y_get(s1, 0, xm - 3, 6, 0);
			if (ym < yy)
				b += ym - yy;
			b -= 12;
		}
#else
		if (s1->stem > 0) {
			for (sy = s1; ; sy = sy->next) {
				yy = a * sy->x + b;
				ym = sy->ymx;
				if (ym > yy)
					b += ym - yy;
				if (sy == s2)
					break;
			}
			b += 4;
		} else {
			for (sy = s1; ; sy = sy->next) {
				yy = a * sy->x + b;
				ym = sy->ymn;
				if (ym < yy)
					b += ym - yy;
				if (sy == s2)
					break;
			}
			b -= 12;
		}
#endif
		if (s1->stem * s2->stem > 0) {
			if (s1->stem > 0)
				xm += GSTEM_XOFF;
			else	xm -= GSTEM_XOFF;
		}
		ym = a * xm + b;
		if ((t->u & 0x0f) == 0)		/* if 'value' == number */
			PUT1("(%d)", t->as.u.tuplet.p_plet);
		else	PUT2("(%d:%d)", t->as.u.tuplet.p_plet,
			     t->as.u.tuplet.q_plet);
		putxy(xm, ym);
		PUT1("y%d bnum\n", s1->staff);

		for (sy = s1; ; sy = sy->next) {
			if (sy->x >= xm)
				break;
		}
		if (s1->stem > 0) {
			ym += 8;
			if (sy->ymx < ym)
				sy->ymx = (short) ym;
			y_set(sy, 1, xm - 3, 6, ym);
		} else {
			if (sy->ymn > ym)
				sy->ymn = (short) ym;
			y_set(sy, 0, xm - 3, 6, ym);
		}
		return next;
	}

	if ((t->u & 0x0f0) != 0)	/* if 'what' != square */
		fprintf(stderr, "'what' value of %%%%tuplets not yet coded\n");

/*fixme: two staves not treated*/
/*fixme: to optimize*/
    if (s1->multi >= 0) {

	/* sole or upper voice: the bracket is above the staff */
	x1 = s1->x - 4;
	x2 = s2->x + 4;
	r = s2->stem >= 0 ? 0 : s2->nhd;
	if (s2->shhd[r] > 0)
		x2 += s2->shhd[r];
	y1 = 24;
	if (s1->staff == upstaff) {
		sy = s1;
		if (sy->as.type != ABC_T_NOTE) {
			for (sy = sy->next; sy != s2; sy = sy->next)
				if (sy->as.type == ABC_T_NOTE)
					break;
		}
		ym = y_get(sy, 1, sy->x, 0, 0);
		if (ym > y1)
			y1 = ym;
		if (s1->stem > 0)
			x1 += 3;
	}
	y2 = 24;
	if (s2->staff == upstaff) {
		sy = s2;
		if (sy->as.type != ABC_T_NOTE) {
			for (sy = sy->prev; sy != s1; sy = sy->prev)
				if (sy->as.type == ABC_T_NOTE)
					break;
		}
		ym = y_get(sy, 1, sy->x, 0, 0);
		if (ym > y2)
			y2 = ym;
		if (s2->stem > 0)
			x2 += 3;
	}
	if (s2->as.type == ABC_T_NOTE && s2->prev->as.type == ABC_T_NOTE
	    && s2->dur > s2->prev->dur)
		x2 += 5;

	xm = .5 * (x1 + x2);
	ym = .5 * (y1 + y2);

	a = (y2 - y1) / (x2 - x1);
	s0 = 3 * (s2->pits[s2->nhd] - s1->pits[s1->nhd]) / (x2 - x1);
	if (s0 > 0) {
		if (a < 0)
			a = 0;
		else if (a > s0)
			a = s0;
	} else {
		if (a > 0)
			a = 0;
		else if (a < s0)
			a = s0;
	}
	if (a * a < .1 * .1)
		a = 0;

	/* shift up bracket if needed */
	dy = 0;
	for (sy = s1; ; sy = sy->next) {
		if (sy->dur == 0	/* not a note or a rest */
		    || sy->staff != upstaff) {
			if (sy == s2)
				break;
			continue;
		}
		yy = ym + (sy->x - xm) * a;
		yx = y_get(sy, 1, sy->x, 0, 0);
		if (yx - yy > dy)
			dy = yx - yy;
		if (sy == s2)
			break;
	}

	ym += dy + 4;
	y1 = ym + a * (x1 - xm);
	y2 = ym + a * (x2 - xm);
	putxy(x2 - x1, y2 - y1);
	putxy(x1, y1 + 4);
	PUT1("y%d tubr", upstaff);

	/* shift the slurs / decorations */
	ym += 8;
	for (sy = s1; ; sy = sy->next) {
		if (sy->staff == upstaff) {
			yy = ym + (sy->x - xm) * a;
			if (sy->ymx < yy)
				sy->ymx = yy;
			if (sy == s2)
				break;
			y_set(sy, 1, sy->x, sy->next->x - sy->x, yy);
		} else if (sy == s2)
			break;
	}

    } else {	/* lower voice of the staff: the bracket is below the staff */
/*fixme: think to all that again..*/
	x1 = s1->x - 8;
	x2 = s2->x + 2;
	if (s2->shhd[s2->nhd] > 0)
		x2 += s2->shhd[s2->nhd];
	if (s2->as.type == ABC_T_NOTE && s2->prev->as.type == ABC_T_NOTE
	    && s2->dur > s2->prev->dur)
		x2 += 5;
	if (s1->staff == upstaff) {
		sy = s1;
		if (sy->as.type != ABC_T_NOTE) {
			for (sy = sy->next; sy != s2; sy = sy->next)
				if (sy->as.type == ABC_T_NOTE)
					break;
		}
		y1 = y_get(sy, 0, sy->x, 0, 0);
	} else	y1 = 0;
	if (s2->staff == upstaff) {
		sy = s2;
		if (sy->as.type != ABC_T_NOTE) {
			for (sy = sy->prev; sy != s1; sy = sy->prev)
				if (sy->as.type == ABC_T_NOTE)
					break;
		}
		y2 = y_get(sy, 0, sy->x, 0, 0);
	} else	y2 = 0;

	xm = .5 * (x1 + x2);
	ym = .5 * (y1 + y2);

	a = (y2 - y1) / (x2 - x1);
	s0 = 3 * (s2->pits[0] - s1->pits[0]) / (x2 - x1);
	if (s0 > 0) {
		if (a < 0)
			a = 0;
		else if (a > s0)
			a = s0;
	} else {
		if (a > 0)
			a = 0;
		else if (a < s0)
			a = s0;
	}
	if (a * a < .1 * .1)
		a = 0;

	/* shift down bracket if needed */
	dy = 0;
	for (sy = s1; ; sy = sy->next) {
		if (sy->dur == 0	/* not a note nor a rest */
		    || sy->staff != upstaff) {
			if (sy == s2)
				break;
			continue;
		}
		yy = ym + (sy->x - xm) * a;
		yx = y_get(sy, 0, sy->x, 0, 0);
		if (yx - yy < dy)
			dy = yx - yy;
		if (sy == s2)
			break;
	}

	ym += dy - 12;
	y1 = ym + a * (x1 - xm);
	y2 = ym + a * (x2 - xm);
	putxy(x2 - x1, y2 - y1);
	putxy(x1, y1 + 4);
	PUT1("y%d tubrl",upstaff);

	/* shift the slurs / decorations */
	ym -= 8;
	for (sy = s1; ; sy = sy->next) {
		if (sy->staff == upstaff) {
			if (sy == s2)
				break;
			yy = ym + (sy->x - xm) * a;
			if (sy->ymn > yy)
				sy->ymn = (short) yy;
			y_set(sy, 0, sy->x, sy->next->x - sy->x, yy);
		}
		if (sy == s2)
			break;
	}
    } /* lower voice */

	if ((t->u & 0x0f) == 1) {	/* if 'value' == none */
		PUT0("\n");
		return next;
	}
	yy = .5 * (y1 + y2);
	if ((t->u & 0x0f) == 0)		/* if 'value' == number */
		PUT1("(%d)", t->as.u.tuplet.p_plet);
	else	PUT2("(%d:%d)", t->as.u.tuplet.p_plet,
		     t->as.u.tuplet.q_plet);
	putxy(xm, yy);
	PUT1("y%d bnumb\n", upstaff);
	return next;
}

/* -- draw the ties between two notes/chords -- */
static void draw_note_ties(struct SYMBOL *k1,
			   struct SYMBOL *k2,
			   int ntie,
			   int *mhead1,
			   int *mhead2,
			   int dotted,
			   int job)
{
	int i, s, m1, m2, p1, p2, y1, y2;
	float x1, x2, h;

	for (i = 0; i < ntie; i++) {
		m1 = mhead1[i];
		p1 = k1->pits[m1];
		m2 = mhead2[i];
		p2 = k2->pits[m2];
		if (k1->as.u.note.ti1[m1] == SL_ABOVE)
			s = 1;
		else	s = -1;
		x1 = k1->x;
		h = k1->shhd[m1];		/* head shift */
		if (s > 0) {
			if (m1 < k1->nhd && k1->pits[m1] + 1 == k1->pits[m1 + 1])
				if (k1->shhd[m1 + 1] > h)
					h = k1->shhd[m1 + 1];
		} else {
			if (m1 > 0 && k1->pits[m1] == k1->pits[m1 - 1] + 1)
				if (k1->shhd[m1 - 1] > h)
					h = k1->shhd[m1 - 1];
		}
		if (h > 0)
			x1 += h;
		x2 = k2->x;
		h = k2->shhd[m2];
		if (s > 0) {
			if (m2 < k2->nhd && k2->pits[m2] + 1 == k2->pits[m2 + 1])
				if (k2->shhd[m2 + 1] < h)
					h = k2->shhd[m2 + 1];
		} else {
			if (m2 > 0 && k2->pits[m2] == k2->pits[m2 - 1] + 1)
				if (k2->shhd[m2 - 1] < h)
					h = k2->shhd[m2 - 1];
		}
		if (h < 0)
			x2 += h;
		if (job == 2) {		/* half tie from last note in line */
			if (k1 != k2)
				x2 -= k2->wl;
			else {
				struct SYMBOL *k;

				for (k = k2->ts_next; k != 0; k = k->ts_next)
					if (k->type == STAVES)
						break;
				if (k == 0)
					x2 = realwidth;
				else	x2 = k->x;
			}
			if (x2 < x1 + 16)
				x2 = x1 + 16;
		} else if (job == 1) {	/* half tie to first note in line */
			x1 = k1->x;
			if (x1 > x2 - 20)
				x1 = x2 - 20;
		}
		if (x2 - x1 > 20) {
			x1 += 2;
			x2 -= 2;
		}
		y1 = 3 * (p1 - 18) + 2 * s;
		y2 = 3 * (p2 - 18) + 2 * s;
		if (job != 1) {
			if (k1->nhd != 0)
				x1 += 4.5;
			else	y1 += ((p1 % 2) ? 3 : 2) * s;
			if (s > 0) {
				if (k1->nflags > -2 && k1->stem > 0
				    && k1->nhd == 0)
					x1 += 4.5;
				if (!(p1 % 2) && k1->dots > 0)
					y1 = 3 * (p1 - 18) + 6;
			}
		}
		if (job != 2) {
			if (k2->nhd != 0)
				x2 -= 4.5;
			else	y2 += ((p2 % 2) ? 3 : 2) * s;
			if (s < 0) {
				if (k2->nflags > -2 && k2->stem < 0
				    && k2->nhd == 0)
					x2 -= 4.5;
			}
			if (job != 0)
				y1 = y2;
		} else {
			y2 = y1;
			if (k1 == k2)		/* if continuation on next line */
				k1->as.u.note.ti1[m1] =
					s > 0 ? SL_ABOVE : SL_BELOW;
		}

		/* tie between 2 staves */
/*fixme: should also do that when clef change?*/
/*fixme:dotted not treated*/
		if (k1->staff != k2->staff) {
			s = k1->staff - k2->staff;
			y1 = 3 * (p1 - 18) + 3 * s;
			y2 = 3 * (p2 - 18) - 3 * s;
			x1 += 4;
			x2 -= 4;
			putxy(x1, staff_tb[k1->staff].y + y1);
			PUT0("M ");
			putxy(x2, staff_tb[k2->staff].y + y2);
			PUT0("lineto stroke\n");
			continue;
		}

		h = (.04 * (x2 - x1) + 8) * s;
		slur_out(x1, staff_tb[k1->staff].y + y1,
			 x2, staff_tb[k1->staff].y + y2,
			 s, h, dotted, -1);
	}
}

/* -- draw ties between neighboring notes/chords -- */
static void draw_ties(struct SYMBOL *k1,
		      struct SYMBOL *k2,
		      int job)		/* 0: normal
					 * 1: at start of line
					 * 2: at end of line */
{
	int i, m1, nh1, pit, ntie, ntie3, time;
	int mhead1[MAXHD], mhead2[MAXHD], mhead3[MAXHD];

	ntie = ntie3 = 0;
	nh1 = k1->nhd;
	time = k1->time + k1->dur;

	/* 2-note case: set up list of ties to draw */
	if (job != 2 && k2->time == time) {
		for (i = 0; i <= nh1; i++) {
			if (k1->as.u.note.ti1[i] != 0) {
				pit = k1->as.u.note.pits[i];
				for (m1 = k2->nhd; m1 >= 0; m1--) {
					if (k2->as.u.note.pits[m1] == pit) {
						mhead1[ntie] = i;
						mhead2[ntie++] = m1;
						break;
					}
				}
				if (m1 < 0)
					mhead3[ntie3++] = i;
			}
		}
		draw_note_ties(k1, k2,
				ntie, mhead1, mhead2,
				k1->as.flags & ABC_F_DOTTED_TIE, job);
		if (ntie3 == 0)
			return;			/* no bad tie */
	} else {

		/* half ties from last note in line */
		for (i = 0; i <= nh1; i++) {
			if (k1->as.u.note.ti1[i])
				mhead3[ntie3++] = i;
		}
		if (job == 2) {
			draw_note_ties(k1, k2 ? k2 : k1,
					ntie3, mhead3, mhead3,
					k1->as.flags & ABC_F_DOTTED_TIE, job);
			return;
		}
	}

	/* try an other voice */
	k2 = k1->ts_next;
	while (k2 != 0 && k2->time < time)
		k2 = k2->ts_next;
	while (k2 != 0 && k2->time == time) {
		if (k2->as.type != ABC_T_NOTE
		    || k2->staff != k1->staff) {
			k2 = k2->ts_next;
			continue;
		}
		ntie = 0;
		for (i = ntie3; --i >= 0; ) {
			pit = k1->as.u.note.pits[mhead3[i]];
			for (m1 = k2->nhd; m1 >= 0; m1--) {
				if (k2->as.u.note.pits[m1] == pit) {
					mhead1[ntie] = mhead3[i];
					mhead2[ntie++] = m1;
					ntie3--;
					mhead3[i] = mhead3[ntie3];
					break;
				}
			}
		}
		if (ntie > 0) {
			draw_note_ties(k1, k2,
					ntie, mhead1, mhead2,
					k1->as.flags & ABC_F_DOTTED_TIE,
					job == 1 ? 1 : 0);
			if (ntie3 == 0)
				return;
		}
		k2 = k2->ts_next;
	}
	if (ntie3 != 0)
		error(1, k1, "Bad tie");
}

/* -- draw all ties between neighboring notes -- */
static void draw_all_ties(struct VOICE_S *p_voice)
{
	struct SYMBOL *s1, *s2, *rtie;
	struct SYMBOL tie;

	for (s1 = p_voice->sym->next; s1 != 0; s1 = s1->next)
		if (s1->type != KEYSIG && s1->type != TIMESIG)
			break;
	rtie = p_voice->rtie;			/* tie from 1st repeat bar */
	for (s2 = s1; s2 != 0; s2 = s2->next) {
		if (s2->as.type == ABC_T_NOTE)
			break;
		if (s2->type != BAR
		    || !s2->as.u.bar.repeat_bar
		    || s2->as.text == 0)
			continue;
		if (s2->as.text[0] == '1')	/* 1st repeat bar */
			rtie = p_voice->tie;
		else	p_voice->tie = rtie;
	}
	if (s2 == 0)
		return;
	if (p_voice->tie != 0) {		/* tie from previous line */
		p_voice->tie->x = s1->x + s1->wr;
		s1 = p_voice->tie;
		p_voice->tie = 0;
		s1->staff = s2->staff;
		s1->ts_next = tsfirst->next;	/* (for tie to other voice) */
		s1->time = s2->time - s1->dur;	/* (if after repeat sequence) */
		draw_ties(s1, s2, 1);		/* tie to 1st note */
	}

	for (;;) {
		for (s1 = s2; s1 != 0; s1 = s1->next) {
			if (s1->sflags & S_TI1)
				break;
			if (rtie == 0)
				continue;
			if (s1->type != BAR
			    || !s1->as.u.bar.repeat_bar
			    || s1->as.text == 0)
				continue;
			if (s1->as.text[0] == '1') {	/* 1st repeat bar */
				rtie = 0;
				continue;
			}
			for (s2 = s1->next; s2 != 0; s2 = s2->next)
				if (s2->as.type == ABC_T_NOTE)
					break;
			if (s2 == 0) {
				s1 = 0;
				break;
			}
			memcpy(&tie, rtie, sizeof tie);
			tie.x = s1->x + s1->wr;
			tie.next = s2;
			tie.staff = s2->staff;
			tie.time = s2->time - tie.dur;
			draw_ties(&tie, s2, 1);
		}
		if (s1 == 0)
			break;
		for (s2 = s1->next; s2 != 0; s2 = s2->next) {
			if (s2->as.type == ABC_T_NOTE)
				break;
			if (s2->type == BAR) {
				if ((s2->sflags & S_RRBAR)
				    || s2->as.u.bar.type == B_THIN_THICK
				    || s2->as.u.bar.type == B_THICK_THIN)
					break;
				if (!s2->as.u.bar.repeat_bar
				    || s2->as.text == 0)
					continue;
				if (s2->as.text[0] != '1')
					break;
				rtie = s1;		/* 1st repeat bar */
			}
		}
		if (s2 == 0) {
			draw_ties(s1, s1, 2);
			p_voice->tie = s1;
			break;
		}
		draw_ties(s1, s2, s2->as.type == ABC_T_NOTE ? 0 : 2);
	}
	p_voice->rtie = rtie;
}

/* -- draw all phrasing slurs for one staff -- */
/* (the staves are not yet defined) */
static void draw_all_slurs(struct VOICE_S *p_voice)
{
	struct SYMBOL *s, *k;
	int i, m2, slur_type;
	unsigned char slur_st;

	if ((s = p_voice->sym->next) == 0)
		return;
	slur_st = p_voice->slur_st;
	p_voice->slur_st = 0;

	/* draw the slurs inside the music line */
	draw_slurs(s, 0);

	/* do unbalanced slurs still left over */
	for ( ; s != 0; s = s->next) {
		if (s->type != NOTEREST)
			continue;
		while (s->as.u.note.slur_end
		       || (s->sflags & S_SL2)) {
			if (s->as.u.note.slur_end) {
				s->as.u.note.slur_end--;
				m2 = -1;
			} else {
				for (m2 = 0; m2 <= s->nhd; m2++)
					if (s->as.u.note.sl2[m2])
						break;
				s->as.u.note.sl2[m2]--;
				if (s->as.u.note.sl2[m2] == 0) {
					for (i = m2 + 1; i <= s->nhd; i++)
						if (s->as.u.note.sl2[i])
							break;
					if (i > s->nhd)
						s->sflags &= ~S_SL2;
				}
			}
			slur_type = slur_st & 0x03;
			k = prev_scut(s);
			draw_slur(k, s, -1, m2, slur_type, 0);
			if (k->type != BAR
			    || (!(k->sflags & S_RRBAR)
				&& k->as.u.bar.type != B_THIN_THICK
				&& k->as.u.bar.type != B_THICK_THIN
				&& (!k->as.u.bar.repeat_bar
				    || k->as.text == 0
				    || k->as.text[0] == '1')))
				slur_st >>= 2;
		}
	}
	s = p_voice->sym->next;
	while (slur_st != 0) {
		slur_type = slur_st & 0x03;
		slur_st >>= 2;
		k = next_scut(s);
		draw_slur(s, k, -1, -1, slur_type, 0);
		if (k->type != BAR
		    || (!(k->sflags & S_RRBAR)
			&& k->as.u.bar.type != B_THIN_THICK
			&& k->as.u.bar.type != B_THICK_THIN
			&& (!k->as.u.bar.repeat_bar
			    || k->as.text == 0
			    || k->as.text[0] == '1'))) {
/*fixme: the slur types are inverted*/
			p_voice->slur_st <<= 2;
			p_voice->slur_st += slur_type;
		}
	}
}

/* -- work out accidentals to be applied to each note -- */
static void setmap(int sf,	/* number of sharps/flats in key sig (-7 to +7) */
		   unsigned char *map)	/* for 7 notes only */
{
	int j;

	for (j = 7; --j >= 0; )
		map[j] = A_NULL;
	switch (sf) {
	case 7: map[6] = A_SH;
	case 6: map[2] = A_SH;
	case 5: map[5] = A_SH;
	case 4: map[1] = A_SH;
	case 3: map[4] = A_SH;
	case 2: map[0] = A_SH;
	case 1: map[3] = A_SH;
		break;
	case -7: map[3] = A_FT;
	case -6: map[0] = A_FT;
	case -5: map[4] = A_FT;
	case -4: map[1] = A_FT;
	case -3: map[5] = A_FT;
	case -2: map[2] = A_FT;
	case -1: map[6] = A_FT;
		break;
	}
}

/* -- draw the tablature with w: -- */
static void draw_tblt_w(struct VOICE_S *p_voice,
			int nly,
			float y,
			struct tblt_s *tblt)
{
	struct SYMBOL *s;
	struct lyrics *ly;
	struct lyl *lyl;
	char *p;
	int j, l;

	PUT2("/y{%.1f y%d}def ", y, p_voice->staff);
	set_font(VOCALFONT);
	PUT3("%.1f 0 y %d %s\n", realwidth, nly, tblt->head);
	for (j = 0; j < nly ; j++) {
		for (s = p_voice->sym->next; s != 0; s = s->next) {
			if ((ly = s->ly) == 0
			    || (lyl = ly->lyl[j]) == 0) {
				if (s->type == BAR) {
					if (tblt->bar == 0)
						continue;
					p = &tex_buf[16];
					*p-- = '\0';
					l = bar_cnv(s->as.u.bar.type);
					while (l != 0) {
						*p-- = "?|[]:???"[l & 0x07];
						l >>= 4;
					}
					p++;
					PUT4("(%s)%.1f y %d %s ",
						p, s->x, j,
						tblt->bar);
				}
				continue;
			}
			PUT4("(%s)%.1f y %d %s ",
				lyl->t, s->x, j, tblt->note);
		}
		PUT0("\n");
	}
}

/* -- draw the tablature with automatic pitch -- */
static void draw_tblt_p(struct VOICE_S *p_voice,
			float y,
			struct tblt_s *tblt)
{
	struct SYMBOL *s;
	int j, pitch, octave, sf, tied;
	unsigned char workmap[70];	/* sharps/flats - base: lowest 'C' */
	unsigned char basemap[7];
	static int scale[7] = {0, 2, 4, 5, 7, 9, 11};	/* index = natural note */
	static int acc_pitch[6] = {0, 1, 0, -1, 2, -2};	/* index = enum accidentals */

	sf = p_voice->key.sf;
	setmap(sf, basemap);
	for (j = 0; j < 10; j++)
		memcpy(&workmap[7 * j], basemap, 7);
	PUT4("gsave 0 %.1f y%d T(%.2s)%s\n",
		y, p_voice->staff,
		tblt->instr, tblt->head);
	tied = 0;
	for (s = p_voice->sym; s != 0; s = s->next) {
		switch (s->type) {
		case NOTEREST:
			if (s->as.type == ABC_T_REST)
				continue;
			if (tied) {
				tied = s->as.u.note.ti1[0];
				continue;
			}
			break;
		case KEYSIG:
			sf = s->as.u.key.sf;
			setmap(sf, basemap);
			for (j = 0; j < 10; j++)
				memcpy(&workmap[7 * j], basemap, 7);
			continue;
		case BAR:
			if (s->as.flags & ABC_F_INVIS)
				continue;
			for (j = 0; j < 10; j++)
				memcpy(&workmap[7 * j], basemap, 7);
			continue;
		default:
			continue;
		}
		pitch = s->as.u.note.pits[0] + 19;
		if (s->as.u.note.accs[0] != 0) {
			workmap[pitch] = s->as.u.note.accs[0] == A_NT
				? A_NULL
				: (s->as.u.note.accs[0] % 0x07);
		}
		pitch = scale[pitch % 7]
			+ acc_pitch[workmap[pitch]]
			+ 12 * (pitch / 7)
			- tblt->pitch;
		octave = 0;
		while (pitch < 0) {
			pitch += 12;
			octave--;
		}
		while (pitch >= 36) {
			pitch -= 12;
			octave++;
		}
		PUT4("%d %d %.2f %s\n", octave, pitch, s->x, tblt->note);
		tied = s->as.u.note.ti1[0];
	}
	PUT0("grestore\n");
}

/* -- draw the lyrics under (or above) notes -- */
/* !! this routine is tied to set_width() !! */
static float draw_lyrics(struct VOICE_S *p_voice,
			 int nly,
			 float y,
			 int incr)	/* 1: below, -1: above */
{
	int hyflag, l, j, lflag;
	char *p;
	float lastx, w, lskip, desc;
	struct SYMBOL *s;
	struct FONTSPEC *f;
	struct lyrics *ly;
	struct lyl *lyl;

	/* check if the lyrics contain tablatures */
	if (p_voice->tblts[0] != 0) {
		if (p_voice->tblts[0]->pitch == 0)
			return y;		/* yes */
		if (p_voice->tblts[1] != 0
		    && p_voice->tblts[1]->pitch == 0)
			return y;		/* yes */
	}

	outft = -1;				/* force font output */
	lskip = 0;				/* (compiler warning) */
	f = 0;					/* (force new font) */
	if (incr > 0) {				/* under the staff */
		j = 0;
/*fixme: may not be the current font*/
		y -= cfmt.font_tb[VOCALFONT].size;
		if (y > -cfmt.vocalspace)
			y = -cfmt.vocalspace;
	} else {
		j = nly - 1;
		nly = -1;
		if (y < 24 + cfmt.vocalspace - cfmt.font_tb[VOCALFONT].size)
			y = 24 + cfmt.vocalspace - cfmt.font_tb[VOCALFONT].size;
	}
/*fixme: may not be the current font*/
	desc = cfmt.font_tb[VOCALFONT].size * .25;	/* descent */
	for (; j != nly ; j += incr) {
		float x0, shift;

		PUT2("/y{%.1f y%d}def ", y + desc, p_voice->staff);
		hyflag = lflag = 0;
		if (p_voice->hy_st & (1 << j)) {
			hyflag = 1;
			p_voice->hy_st &= ~(1 << j);
		}
		for (s = p_voice->sym; /*s != 0*/; s = s->next)
			if (s->type != KEYSIG && s->type != TIMESIG)
				break;
		if (s->prev != 0)
			lastx = s->prev->x;
		else
			lastx = 0;
		x0 = 0;
		if (f != 0)
			lskip = f->size * 1.1;
		for ( ; s != 0; s = s->next) {
			if ((ly = s->ly) == 0
			    || (lyl = ly->lyl[j]) == 0) {
				switch (s->type) {
				case NOTEREST:
					if (s->as.type == ABC_T_NOTE)
						break;
					/* fall thru */
				case MREST:
					if (lflag) {
						PUT2("%.1f %.1f y wln ",
						     x0 - lastx, lastx + 3);
						lflag = 0;
						lastx = s->x + s->wr;
					}
				}
				continue;
			}
			if (lyl->f != f) {		/* font change */
				f = lyl->f;
				set_font(f - cfmt.font_tb);
				if (lskip < f->size * 1.1)
					lskip = f->size * 1.1;
			}
			p = lyl->t;
			w = lyl->w;
			shift = lyl->s;
			if (hyflag) {
				if (*p == '\x03')		/* '_' */
					*p = '\x02';
				else if (*p != '\x02') {	/* not '-' */
					PUT2("%.1f %.1f y hyph ",
					     s->x - shift - lastx, lastx);
					hyflag = 0;
					lastx = s->x + s->wr;
				}
			}
			if (lflag
			    && *p != '\x03') {		/* not '_' */
				PUT2("%.1f %.1f y wln ",
				     x0 - lastx + 3, lastx + 3);
				lflag = 0;
				lastx = s->x + s->wr;
			}
			if (*p == '\x02'		/* '-' */
			    || *p == '\x03') {		/* '_' */
				if (x0 == 0 && lastx > s->x - 18)
					lastx = s->x - 18;
				if (*p == '\x02')
					hyflag = 1;
				else	lflag = 1;
				x0 = s->x - shift;
				continue;
			}
			x0 = s->x - shift;
			l = strlen(p) - 1;
			if (p[l] == '\x02') {		/* '-' at end */
				p[l] = '\0';
				hyflag = 1;
			}
			PUT2("%.1f y M(%s)lyshow ", x0, p);
			lastx = x0 + w;
		}
		if (hyflag) {
			x0 = realwidth - 10;
			if (x0 < lastx + 10)
				x0 = lastx + 10;
			PUT2("%.1f %.1f y hyph ",
			     x0 - lastx, lastx);
			if (cfmt.hyphencont)
				p_voice->hy_st |= (1 << j);
		}

		/* see if any underscore in the next line */
		for (s = tsnext; s != 0; s = s->ts_next)
			if (s->voice == p_voice - voice_tb)
				break;
		for ( ; s != 0; s = s->next) {
			if (s->as.type == ABC_T_NOTE) {
				if (s->ly != 0 && s->ly->lyl[j] != 0
				    && s->ly->lyl[j]->t[0] == '\x03') {
					lflag = 1;
					x0 = realwidth - 15;
					if (x0 < lastx + 12)
						x0 = lastx + 12;
				}
				break;
			}
		}
		if (lflag)
			PUT2("%.1f %.1f y wln",
			     x0 - lastx + 3, lastx + 3);
		PUT0("\n");
		if (incr > 0)
			y -= lskip;
		else	y += lskip;
	}
	if (incr > 0)
		y += lskip;
	return y;
}

/* -- draw all the lyrics and the tablatures -- */
/* (the staves are not yet defined) */
static void draw_all_lyrics(void)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s;
	int staff, voice, nly, i;
	struct {
		short a, b;
		float top, bot;
	} lyst_tb[MAXSTAFF];
	char nly_tb[MAXVOICE];
	char above_tb[MAXVOICE];
	char rv_tb[MAXVOICE];
	float top, bot, y;

	/* check if any lyric */
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		if (p_voice->have_ly
		    || p_voice->tblts[0] != 0)
			break;
	}
	if (p_voice == 0)
		return;

	/* compute the number of lyrics per voice - staff
	 * and their y offset on the staff */
	memset(above_tb, 0, sizeof above_tb);
	memset(nly_tb, 0, sizeof nly_tb);
	memset(lyst_tb, 0, sizeof lyst_tb);
	staff = -1;
	top = bot = 0;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		if (p_voice->sym == 0)
			continue;
		voice = p_voice - voice_tb;
		if (p_voice->staff != staff) {
			top = 0;
			bot = 0;
			staff = p_voice->staff;
		}
		nly = 0;
		if (p_voice->have_ly) {
			for (s = p_voice->sym; s != 0; s = s->next) {
				struct lyrics *ly;
				float x, w;

				if ((ly = s->ly) == 0)
					continue;
/*fixme:should get the real width*/
				x = s->x;
				if (ly->lyl[0] != 0) {
					x -= ly->lyl[0]->s;
					w = ly->lyl[0]->w;
				} else	w = 10;
				y = y_get(s, 1, x, w, 0);
				if (top < y)
					top = y;
				y = y_get(s, 0, x, w, 0);
				if (bot > y)
					bot = y;
				for (i = MAXLY; --i >= 0; )
					if (ly->lyl[i] != 0)
						break;
				i++;
				if (i > nly)
					nly = i;
			}
		} else {
			y = y_get(p_voice->sym, 1, 0, realwidth, 0);
			if (top < y)
				top = y;
			y = y_get(p_voice->sym, 0, 0, realwidth, 0);
			if (bot > y)
				bot = y;
		}
		lyst_tb[staff].top = top;
		lyst_tb[staff].bot = bot;
		if (nly == 0)
			continue;
		nly_tb[voice] = nly;
		if (p_voice->ly_pos != 0)
			above_tb[voice] = p_voice->ly_pos > 0;
		else if (cfmt.vocalabove
/*fixme:%%staves:KO - find an other way*/
			 || (p_voice->next != 0
			     && p_voice->next->staff == staff
			     && p_voice->next->have_ly))
			above_tb[voice] = 1;
		if (above_tb[voice])
			lyst_tb[staff].a = 1;
		else	lyst_tb[staff].b = 1;
	}

	/* draw the lyrics under the staves */
	i = 0;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		struct tblt_s *tblt;

		if (p_voice->sym == 0)
			continue;
		if (!p_voice->have_ly
		    && p_voice->tblts[0] == 0)
			continue;
		voice = p_voice - voice_tb;
		if (above_tb[voice]) {
			rv_tb[i++] = voice;
			continue;
		}
		staff = p_voice->staff;
		if (nly_tb[voice] > 0)
			lyst_tb[staff].bot = draw_lyrics(p_voice, nly_tb[voice],
							 lyst_tb[staff].bot, 1);
		for (nly = 0; nly < 2; nly++) {
			if ((tblt = p_voice->tblts[nly]) == 0)
				continue;
			if (tblt->hu > 0) {
				lyst_tb[staff].bot -= tblt->hu;
				lyst_tb[staff].b = 1;
			}
			if (tblt->pitch == 0)
				draw_tblt_w(p_voice, nly_tb[voice],
					lyst_tb[staff].bot, tblt);
			else	draw_tblt_p(p_voice, lyst_tb[staff].bot,
					tblt);
			if (tblt->ha != 0) {
				lyst_tb[staff].top += tblt->ha;
				lyst_tb[staff].a = 1;
			}
		}
	}

	/* draw the lyrics above the staff */
	while (--i >= 0) {
		voice = rv_tb[i];
		p_voice = &voice_tb[voice];
		staff = p_voice->staff;
		lyst_tb[staff].top = draw_lyrics(p_voice, nly_tb[voice],
						 lyst_tb[staff].top, -1);
	}

	/* set the max y offsets of all symbols */
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		if (p_voice->sym == 0)
			continue;
		staff = p_voice->staff;
		if (lyst_tb[staff].a) {
			top = lyst_tb[staff].top + 2;
			for (s = p_voice->sym->next; s != 0; s = s->next) {
/*fixme: may have lyrics crossing a next symbol*/
				if (s->ly != 0) {
/*fixme:should set the real width*/
					y_set(s, 1, s->x - 2, 10, top);
				}
			}
		}
		if (lyst_tb[staff].b) {
			bot = lyst_tb[staff].bot - 2;
			if (nly_tb[p_voice - voice_tb] > 0) {
				for (s = p_voice->sym->next; s != 0; s = s->next) {
					if (s->ly != 0) {
/*fixme:should set the real width*/
						y_set(s, 0, s->x - 2, 10, bot);
					}
				}
			} else {
				y_set(p_voice->sym, 0, 0, realwidth, bot);
			}
		}
	}
}

/* -- draw the symbols near the notes -- */
/* (the staves are not yet defined) */
/* order:
 * - beams
 * - decorations near the notes
 * - measure bar numbers
 * - n-plets
 * - decorations tied to the notes
 * - slurs
 * - guitar chords
 * - then remaining decorations
 */
void draw_sym_near(void)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s;

	/* calculate the beams but don't draw them (the staves are undefined) */
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		struct BEAM bm;

		for (s = p_voice->sym; s != 0; s = s->next) {
			if (s->as.type == ABC_T_NOTE
			    && (s->sflags & S_BEAM_ST)
			    && !(s->sflags & S_BEAM_END))
				calculate_beam(&bm, s);
		}
	}

	/* initialize the y offsets */
	{
		int i, staff;

		for (staff = 0; staff <= nstaff; staff++) {
			for (i = 0; i < YSTEP; i++) {
				staff_tb[staff].top[i] = 0;
				staff_tb[staff].bot[i] = 24;
			}
		}
	}

	set_tie_room();
	draw_deco_near();

	/* set the min/max vertical offsets */
	for (s = tsfirst; s != 0; s = s->ts_next) {
		int y;
		struct SYMBOL *g;

		if (s->prev == 0)
			continue;		/* skip the clefs */
		if (s->as.flags & ABC_F_INVIS)
			continue;
		if (s->type == GRACE) {
			g = s->extra;
			for ( ; g != 0; g = g->next) {
				y_set(s, 1, g->x - g->wl, g->wl + g->wr, g->ymx);
				y_set(s, 0, g->x - g->wl, g->wl + g->wr, g->ymn);
			}
			continue;
		}
		if (s->type != MREST) {
			y_set(s, 1, s->x - s->wl, s->wl + s->wr, s->ymx);
			y_set(s, 0, s->x - s->wl, s->wl + s->wr, s->ymn);
		} else {
			y_set(s, 1, s->x - 16, 32, s->ymx);
		}
		if (s->as.type != ABC_T_NOTE)
			continue;

		/* have room for the accidentals */
		if (s->as.u.note.accs[s->nhd]) {
			y = s->y + 8;
			if (s->ymx < y)
				s->ymx = y;
			y_set(s, 1, s->x, 0., y);
		}
		if (s->as.u.note.accs[0]) {
			y = s->y;
			if ((s->as.u.note.accs[0] & 0x07) == A_SH
			    || s->as.u.note.accs[0] == A_NT)
				y -= 7;
			else	y -= 5;
			if (s->ymn > y)
				s->ymn = y;
			y_set(s, 0, s->x, 0., y);
		}
	}

	if (cfmt.measurenb >= 0)
		draw_measnb();

	draw_deco_note();

	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		if ((s = p_voice->sym) == 0)
			continue;
		for (s = s->next; s != 0; s = s->next) {
			struct SYMBOL *g;

			if ((s->sflags & S_IN_TUPLET)
			    && (g = s->extra) != 0) {
				for ( ; g != 0; g = g->next) {
					if (g->type == TUPLET) {
						s = draw_tuplet(g, s);
						break;
					}
				}
			}
		}
		draw_all_slurs(p_voice);
	}

	/* set the top and bottom for all symbols to be out of the staves */
	{
		int top, bot, i, staff;

		for (staff = 0; staff <= nstaff; staff++) {
			top = staff_tb[staff].topbar + 2;
			bot = staff_tb[staff].botbar - 2;
/*fixme:should handle stafflines changes*/
			for (i = 0; i < YSTEP; i++) {
				if (top > staff_tb[staff].top[i])
					staff_tb[staff].top[i] = (float) top;
				if (bot < staff_tb[staff].bot[i])
					staff_tb[staff].bot[i] = (float) bot;
			}
		}
	}
	draw_all_lyrics();
	outft = -1;				/* force font output */
	draw_deco_staff();
}

/* -- draw the name/subname of the voices -- */
static void draw_vname(float indent)
{
	struct VOICE_S *p_voice;
	int n, staff;
	struct {
		int nl;
		char *v[8];
	} staff_d[MAXSTAFF], *staff_p;
	char *p, *q;
	float y;

	for (staff = cursys->nstaff; staff >= 0; staff--) {
		if (!cursys->staff[staff].empty)
			break;
	}
	if (staff < 0)
		return;

	memset(staff_d, 0, sizeof staff_d);
	n = 0;
	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		staff = p_voice->staff;
		if (cursys->staff[staff].empty)
			continue;
		if (p_voice->new_name) {
			p_voice->new_name = 0;
			p = p_voice->nm;
		} else	p = p_voice->snm;
		if (p == 0)
			continue;
		if (cursys->staff[staff].flags & CLOSE_BRACE2) {
			while (!(cursys->staff[staff].flags & OPEN_BRACE2))
				staff--;
		} else if (cursys->staff[staff].flags & CLOSE_BRACE) {
			while (!(cursys->staff[staff].flags & OPEN_BRACE))
				staff--;
		}
		staff_p = &staff_d[staff];
		for (;;) {
			staff_p->v[staff_p->nl++] = p;
			if ((p = strstr(p, "\\n")) == 0
			    || staff_p->nl >= MAXSTAFF)
				break;
			p += 2;
		}
		n++;
	}
	if (n == 0)
		return;
	str_font(VOICEFONT);
	indent = -indent * .5;			/* center */
	for (staff = nstaff; staff >= 0; staff--) {
		staff_p = &staff_d[staff];
		if (staff_p->nl == 0)
			continue;
		y = staff_tb[staff].y
			+ staff_tb[staff].topbar * .5
				* staff_tb[staff].clef.staffscale
			+ 9 * (staff_p->nl - 1)
			- cfmt.font_tb[VOICEFONT].size * .3;
		n = staff;
		if (cursys->staff[staff].flags & OPEN_BRACE2) {
			while (!(cursys->staff[n].flags & CLOSE_BRACE2))
				n++;
		} else if (cursys->staff[staff].flags & OPEN_BRACE) {
			while (!(cursys->staff[n].flags & CLOSE_BRACE))
				n++;
		}
		if (n != staff)
			y -= (staff_tb[staff].y - staff_tb[n].y) * .5;
		for (n = 0; n < staff_p->nl; n++) {
			p = staff_p->v[n];
			if ((q = strstr(p, "\\n")) != 0)
				*q = '\0';
			PUT2("%.1f %.1f M ", indent, y);
			put_str(p, A_CENTER);
			y -= 18.;
			if (q != 0)
				*q = '\\';
		}
	}
}

/* -- set the y offset of the staves and return the whole height -- */
static float set_staff(void)
{
	struct SYSTEM *sy;
	struct SYMBOL *s, *g;
	int staff, any_part, any_tempo;
	float y, staffsep, dy, maxsep, mbot, scale;
	struct {
		float mtop;
		int not_empty;
	} delta_tb[MAXSTAFF], *p_delta;

	/* search the empty staves in each parts */
	sy = cursys;
	for (staff = 0; staff <= nstaff; staff++) {
		sy->staff[staff].empty = 1;
		staff_tb[staff].empty = 0;
	}
	memset(delta_tb, 0, sizeof delta_tb);
	if (cfmt.staffnonote) {
		for (s = tsfirst; s != 0; s = s->ts_next) {
			switch (s->type) {
			case STAVES:
				for (staff = 0; staff <= nstaff; staff++)
					if (!sy->staff[staff].empty)
						delta_tb[staff].not_empty = 1;
				sy = sy->next;
				for (staff = 0; staff <= nstaff; staff++)
					sy->staff[staff].empty = 1;
				break;
			case NOTEREST:
			case SPACE:
			case BAR:
			case MREST:
			case GRACE:
				sy->staff[s->staff].empty = 0;
				break;
			}
		}
	} else {
		for (s = tsfirst; s != 0; s = s->ts_next) {
			switch (s->type) {
			case STAVES:
				for (staff = 0; staff <= nstaff; staff++)
					if (!sy->staff[staff].empty)
						delta_tb[staff].not_empty = 1;
				sy = sy->next;
				for (staff = 0; staff <= nstaff; staff++)
					sy->staff[staff].empty = 1;
				break;
			case NOTEREST:
				if (s->as.type == ABC_T_REST)
					break;
				if (!(s->as.flags & ABC_F_INVIS))
					sy->staff[s->staff].empty = 0;
				break;
			}
		}
	}
	/* 'sy' is the last staff system - here are the staff distances */
	for (staff = 0; staff <= nstaff; staff++)
		if (!sy->staff[staff].empty)
			delta_tb[staff].not_empty = 1;

	/* if a system brace has empty and non empty staves, keep all staves */
	for (staff = 0; staff <= nstaff; staff++) {
		int i, empty_fl;

		if (!(cursys->staff[staff].flags & (OPEN_BRACE | OPEN_BRACE2)))
			continue;
		empty_fl = 0;
		i = staff;
		while (staff <= nstaff) {
			if (delta_tb[staff].not_empty)
				empty_fl |= 1;
			else	empty_fl |= 2;
			if (cursys->staff[staff].flags & (CLOSE_BRACE | CLOSE_BRACE2))
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

	/* handle the empty staves and their tablatures
	 * and output the scale of the voices */
	{
		struct VOICE_S *p_voice;
		int i;
		float ha, hu;

		for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
			if (p_voice->scale != 1)
				PUT2("/scvo%d{gsave %.2f dup scale}def\n",
				     p_voice - voice_tb, p_voice->scale);
			staff = p_voice->staff;
			if (!staff_tb[staff].empty)
				continue;
			ha = hu = 0;
			for (i = 0; i < 2; i++) {
				if (p_voice->tblts[i] != 0
				    && p_voice->tblts[i]->pitch == 0) {
					ha += p_voice->tblts[i]->ha;
					hu += p_voice->tblts[i]->hu;
				}
			}
			if (ha == 0 && hu == 0) {
				staff_tb[staff].topbar = 0;
				continue;
			}
			delta_tb[staff].mtop += ha;
			if (staff < nstaff)
				delta_tb[staff + 1].mtop += hu;
			else	mbot -= hu;
			delta_tb[staff].not_empty = 1;
		}
	}

	/* scan the first voice to see if any part or tempo */
	any_part = any_tempo = 0;
	for (s = first_voice->sym; s != 0; s = s->next) {
		if ((g = s->extra) == 0)
			continue;
		for ( ; g != 0; g = g->next) {
			switch (g->type) {
			case PART:
				any_part = 1;
				break;
			case TEMPO:
				any_tempo = 1;
				break;
			}
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
		if (sy->staff[staff].sep != 0)
			staffsep = sy->staff[staff].sep;
		else	staffsep = cfmt.sysstaffsep;
		if (sy->staff[staff].maxsep != 0)
			maxsep = sy->staff[staff].maxsep;
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

	/* return the whole height */
	return y;
}

/* -- set the height of the measure bars -- */
static void bar_set(float *bar_height)
{
	int staff;
	float dy;

	dy = 0;
	for (staff = 0; staff <= nstaff; staff++) {
		if (cursys->staff[staff].empty) {
			bar_height[staff] = 0;
			if (dy == 0)
				continue;
		} else {
			if (dy == 0)
				dy = staff_tb[staff].y + staff_tb[staff].topbar
					* staff_tb[staff].clef.staffscale;
			bar_height[staff] = dy
				- staff_tb[staff].y - staff_tb[staff].botbar
					* staff_tb[staff].clef.staffscale;
		}
		if (cursys->staff[staff].flags & STOP_BAR)
			dy = 0;
		else	dy = staff_tb[staff].y + staff_tb[staff].botbar
				* staff_tb[staff].clef.staffscale;
	}
}

/* -- draw the staff systems and the measure bars -- */
float draw_systems(float indent)
{
	struct SYMBOL *s;
	int staff, staff_st;
	float xstaff[MAXSTAFF], bar_height[MAXSTAFF];
	float x, x2;
	float line_height;

	line_height = set_staff();
	draw_vname(indent);

	/* draw the staff, skipping the staff breaks */
	for (staff = 0; staff <= nstaff; staff++)
		xstaff[staff] = cursys->staff[staff].empty ? -1 : 0;
	bar_set(bar_height);
	staff_st = 1;
	for (s = tsfirst; s != 0; s = s->ts_next) {
		staff = s->staff;
		switch (s->type) {
		case STAVES:
			if (staff_st) {
				if (cursys->nstaff > 0) {
				    for (staff = 0; staff <= nstaff; staff++) {
					if ((x = xstaff[staff]) >= 0) {
						draw_lstaff(x);
						break;
					}
				    }
				    staff_st = 0;
				}
			}
			cursys = cursys->next;
			for (staff = 0; staff <= nstaff; staff++) {
				if (cursys->staff[staff].empty) {
					if ((x = xstaff[staff]) >= 0) {
#if 1
						if (s->ts_prev->type == BAR)
							x2 = s->ts_prev->x;
						else	x2 = s->x;
#else
						x2 = s->ts_prev->x;
						if (s->ts_prev->type != BAR)
							x2 += s->ts_prev->wr;
#endif
						draw_staff(staff, x, x2);
						xstaff[staff] = -1;
					}
				} else if (xstaff[staff] < 0) {
#if 1
					if (s->ts_next->type != BAR)
						xstaff[staff] = s->x;
					else	xstaff[staff] = s->ts_next->x;
#else
					xstaff[staff] = s->ts_next->x
						- s->ts_next->wl; /* (clef) */
#endif
				}
			}
			bar_set(bar_height);
			break;
		case BAR:
			if (s->as.flags & ABC_F_INVIS)
				break;
			if ((s->sflags & S_SECOND)
			    || cursys->staff[staff].empty)
				s->as.flags |= ABC_F_INVIS;
			else	s->ys = bar_height[staff];
			break;
		case STBRK:
			if (s->prev == 0 || (x = xstaff[staff]) < 0)
				continue;
			x2 = s->prev->x;
			if (x2 <= x)
				continue;
			if (s->prev->type != BAR)
				x2 += s->prev->wr;
			draw_staff(staff, x, x2);
			if (staff == 0) {
				if (staff_st && cursys->nstaff > 0)
					draw_lstaff(x);
				staff_st = s->xmx > .5 CM;
			}
			if (s->xmx != 0) {
				xstaff[staff] = s->x;
				if (s->ts_next != 0 && s->ts_next->type == STAVES)
						s->ts_next->x = s->x;
			} else	xstaff[staff] = x2;
			break;
		default:
			if (cursys->staff[staff].empty)
				s->as.flags |= ABC_F_INVIS;
			break;
		}
	}
	for (staff = 0; staff <= nstaff; staff++) {
		if ((x = xstaff[staff]) < 0
		    || x >= realwidth - 8)
			continue;
		draw_staff(staff, x, realwidth);
		if (staff_st) {
			draw_lstaff(x);
			staff_st = 0;
		}
	}
	set_scale(-1);
	return line_height;
}

/* -- output PostScript sequences -- */
void output_ps(struct SYMBOL *s, int state)
{
	struct SYMBOL *g, *g2;

	g = s->extra;
	g2 = 0;
	for (;;) {
		if (g->type == FMTCHG
		    && g->u == PSSEQ
		    && g->as.state <= state) {
			PUT1("%s\n", g->as.text);
			if (g2 == 0)
				s->extra = g->next;
			else	g2->next = g->next;
		} else	g2 = g;
		if ((g = g->next) == 0)
			break;
	}
}

/* -- draw remaining symbols when the staves are defined -- */
static void draw_symbols(struct VOICE_S *p_voice)
{
	struct BEAM bm;
	struct SYMBOL *s;
	float x, y;
	int staff;

	/* output the PostScript code at start of line */
	for (s = p_voice->sym; s != 0; s = s->next) {
		if (s->extra != 0)
			output_ps(s, 127);
		switch (s->type) {
		case CLEF:
		case KEYSIG:
		case TIMESIG:
		case BAR:
			continue;	/* skip the symbols added by init_music_line() */
		}
		break;
	}

	bm.s2 = 0;
	for (s = p_voice->sym; s != 0; s = s->next) {
		if (s->extra != 0)
			output_ps(s, 127);
		if ((s->as.flags & ABC_F_INVIS)
		    && s->type != NOTEREST)
			continue;
		x = s->x;
		switch (s->type) {
		case NOTEREST:
			set_scale(s->voice);
			if (s->as.type == ABC_T_NOTE) {
				if ((s->sflags & S_BEAM_ST)
				    && !(s->sflags & S_BEAM_END)) {
					if (calculate_beam(&bm, s))
						draw_beams(&bm);
				}
				draw_note(x, s, bm.s2 == 0);
				if (s == bm.s2)
					bm.s2 = 0;
				break;
			}
			draw_rest(s);
			break;
		case BAR:
			draw_bar(s);
			break;
		case CLEF:
			staff = s->staff;
			if (s->sflags & S_SECOND)
/*			    || p_voice->staff != staff)	*/
				break;		/* only one clef per staff */
			cursys->staff[staff].clef.type = s->as.u.clef.type;
			cursys->staff[staff].clef.line = s->as.u.clef.line;
			cursys->staff[staff].clef.octave = s->as.u.clef.octave;
			cursys->staff[staff].clef.invis = s->as.u.clef.invis;
			if ((s->as.flags & ABC_F_INVIS)
			    || staff_tb[staff].empty)
				break;
			set_sscale(staff);
			y = staff_tb[staff].y;
			putxy(x, y + s->y);
			if (s->as.u.clef.name != 0)
				PUT1("%s\n", s->as.u.clef.name);
			else
				PUT2("%c%cclef\n",
				     s->u ? 's' : ' ',
				     "tcbp"[(unsigned) s->as.u.clef.type]);
			if (s->as.u.clef.octave == 0)
				break;
/*fixme:break the compatibility and avoid strange numbers*/
			if (s->as.u.clef.octave > 0)
				y += s->ymx - 36 - 12;
			else	y += s->ymn + 19 + 5;
			putxy(x, y);
			PUT1("oct%c\n",
			     s->as.u.clef.octave > 0 ? 'u' : 'l');
			break;
		case TIMESIG:
			memcpy(&p_voice->meter, &s->as.u.meter,
			       sizeof p_voice->meter);
			if ((s->sflags & S_SECOND)
			    || staff_tb[s->staff].empty)
				break;
			if (cfmt.alignbars && s->staff != 0)
				break;
			set_sscale(s->staff);
			draw_timesig(x, s);
			break;
		case KEYSIG:
			memcpy(&p_voice->key, &s->as.u.key,
			       sizeof p_voice->key);
			if ((s->sflags & S_SECOND)
			    || staff_tb[s->staff].empty)
				break;
			set_sscale(s->staff);
			draw_keysig(p_voice, x, s);
			break;
		case MREST:
			set_scale(s->voice);
			PUT1("(%d)\n", s->as.u.bar.len);
			putxy(x, staff_tb[s->staff].y);
			PUT0("mrest\n");
			break;
		case GRACE:
			set_scale(s->voice);
			draw_gracenotes(s);
			break;
		case SPACE:
		case STAVES:
		case STBRK:
		case FMTCHG:
			break;			/* nothing */
		default:
			bug("Symbol not drawn", 1);
		}
	}
	set_scale(p_voice - voice_tb);
	draw_all_ties(p_voice);
}

/* -- draw all symbols -- */
void draw_all_symb(void)
{
	struct VOICE_S *p_voice;

	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
#if 1 /*fixme:test*/
		if (p_voice->sym == 0)
#else
		if (staff_tb[p_voice->staff].empty || p_voice->sym == 0)
#endif
			continue;
		draw_symbols(p_voice);
	}
}

/* -- output a floating value, and x and y according to the current scale -- */
void putf(float v)
{
	PUT1("%.1f ", v);
}

void putx(float x)
{
	putf(x / cur_scale);
}

void puty(float y)
{
	putf(cur_trans == 0 ?
		y / cur_scale :		/* scaled voice */
		y - cur_trans);		/* scaled staff */
}

void putxy(float x, float y)
{
	if (cur_trans == 0)
		PUT2("%.1f %.1f ",
		     x / cur_scale, y / cur_scale);	/* scaled voice */
	else	PUT2("%.1f %.1f ",
		     x / cur_scale, y - cur_trans);	/* scaled staff */
}

/* -- set the staff or voice scale -- */
void set_scale(int voice)
{
	struct VOICE_S *p_voice;
	int staff;
	float scale, trans;

	staff = -1;
	if (voice >= 0) {
		p_voice = &voice_tb[voice];
		scale = p_voice->scale;
		if (scale == 1) {
			staff = p_voice->staff;
			scale = staff_tb[staff].clef.staffscale;
		}
/*fixme: KO when scale of staff != 1*/
	} else	scale = 1;
	if (staff >= 0 && scale != 1)
		trans = staff_tb[staff].y;
	else	trans = 0;
	if (scale == cur_scale && trans == cur_trans)
		return;
	if (cur_scale != 1)
		PUT0("grestore ");
	cur_scale = scale;
	cur_trans = trans;
	if (scale != 1) {
		if (staff < 0)
			PUT1("scvo%d ", voice);
		else	PUT1("scst%d ", staff);
	}
}

/* -- set the staff scale (only) -- */
static void set_sscale(int staff)
{
	float scale, trans;

	if (staff >= 0)
		scale = staff_tb[staff].clef.staffscale;
	else	scale = 1;
	if (staff >= 0 && scale != 1)
		trans = staff_tb[staff].y;
	else	trans = 0;
	if (scale == cur_scale && trans == cur_trans)
		return;
	if (cur_scale != 1)
		PUT0("grestore ");
	cur_scale = scale;
	cur_trans = trans;
	if (scale != 1)
		PUT1("scst%d ", staff);
}

/* -- set the tie directions for one voice -- */
static void set_tie_dir(struct SYMBOL *sym)
{
	struct SYMBOL *s;
	int i, ntie, dir, sec, pit;

	for (s = sym; s != 0; s = s->next) {
		if (!(s->sflags & S_TI1))
			continue;

		/* if other voice, set the ties in opposite direction */
		if (s->multi != 0) {
/*			struct SYMBOL *s2;

			s2 = s->ts_next;
			if (s2->time == s->time && s2->staff == s->staff) { */
				dir = s->multi > 0 ? SL_ABOVE : SL_BELOW;
				for (i = 0; i <= s->nhd; i++) {
					if (s->as.u.note.ti1[i] == SL_AUTO)
						s->as.u.note.ti1[i] = dir;
				}
				continue;
/*			} */
		}

		/* if one note, set the direction according to the stem */
		sec = ntie = 0;
		pit = 128;
		for (i = 0; i <= s->nhd; i++) {
			if (s->as.u.note.ti1[i]) {
				ntie++;
				if (pit < 128
				    && s->as.u.note.pits[i] <= pit + 1)
					sec++;
				pit = s->as.u.note.pits[i];
			}
		}
		if (ntie <= 1) {
			dir = s->stem < 0 ? SL_ABOVE : SL_BELOW;
			for (i = 0; i <= s->nhd; i++) {
				if (s->as.u.note.ti1[i]) {
					if (s->as.u.note.ti1[i] == SL_AUTO)
						s->as.u.note.ti1[i] = dir;
					break;
				}
			}
			continue;
		}
		if (sec == 0) {
			if (ntie & 1) {
/* in chords with an odd number of notes, the outer noteheads are paired off
 * center notes are tied according to their position in relation to the
 * center line */
				ntie = ntie / 2 + 1;
				dir = SL_BELOW;
				for (i = 0; i <= s->nhd; i++) {
					if (s->as.u.note.ti1[i]) {
						if (--ntie == 0) {	/* central tie */
							if (s->as.u.note.pits[i] >= 22)
								dir = SL_ABOVE;
						}
						if (s->as.u.note.ti1[i] == SL_AUTO)
							s->as.u.note.ti1[i] = dir;
						if (ntie == 0)
							dir = SL_ABOVE;
					}
				}
				continue;
			} else {
/* even number of notes, ties divided in opposite directions */
				ntie /= 2;
				dir = SL_BELOW;
				for (i = 0; i <= s->nhd; i++) {
					if (s->as.u.note.ti1[i]) {
						if (s->as.u.note.ti1[i] == SL_AUTO)
							s->as.u.note.ti1[i] = dir;
						if (--ntie == 0)
							dir = SL_ABOVE;
					}
				}
				continue;
			}
		}
/*fixme: treat more than one second */
/*		if (nsec == 1) {	*/
/* When a chord contains the interval of a second, tie those two notes in
 * opposition; then fill in the remaining notes of the chord accordingly */
			pit = 128;
			for (i = 0; i <= s->nhd; i++) {
				if (s->as.u.note.ti1[i]) {
					if (pit < 128
					    && s->as.u.note.pits[i] <= pit + 1) {
						ntie = i;
						break;
					}
					pit = s->as.u.note.pits[i];
				}
			}
			dir = SL_BELOW;
			for (i = 0; i <= s->nhd; i++) {
				if (s->as.u.note.ti1[i]) {
					if (ntie == i)
						dir = SL_ABOVE;
					if (s->as.u.note.ti1[i] == SL_AUTO)
						s->as.u.note.ti1[i] = dir;
				}
			}
/*fixme..
			continue;
		}
..*/
/* if a chord contains more than one pair of seconds, the pair farthest
 * from the center line receives the ties drawn in opposition */
	}
}

/* -- have room for the ties out of the staves -- */
static void set_tie_room(void)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s, *s2;

	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		if ((s = p_voice->sym) == 0
		    || (s = s->next) == 0)
			continue;
		set_tie_dir(s);
		for ( ; s != 0; s = s->next) {
			float dx, y, dy;

			if (!(s->sflags & S_TI1))
				continue;
			if (s->pits[0] < 20 && s->as.u.note.ti1[0] == SL_BELOW)
				;
			else if (s->pits[s->nhd] > 24
				 && s->as.u.note.ti1[s->nhd] == SL_ABOVE)
				;
			else	continue;
			s2 = s->next;
			while (s2 != 0 && s2->as.type != ABC_T_NOTE)
				s2 = s2->next;
			if (s2 != 0) {
				if (s2->staff != s->staff)
					continue;
				dx = s2->x - s->x - 10;
			} else	dx = realwidth - s->x - 10;
			if (dx < 100)
				dy = 9;
			else if (dx < 300)
				dy = 12;
			else	dy = 16;
			if (s->pits[s->nhd] > 24) {
				y = 3 * (s->pits[s->nhd] - 18) + dy;
				if (s->ymx < y)
					s->ymx = y;
				if (s2 != 0 && s2->ymx < y)
					s2->ymx = y;
				y_set(s, 1, s->x + 5, dx, y);
			}
			if (s->pits[0] < 20) {
				y = 3 * (s->pits[0] - 18) - dy;
				if (s->ymn > y)
					s->ymn = y;
				if (s2 != 0 && s2->ymn > y)
					s2->ymn = y;
				y_set(s, 0, s->x + 5, dx, y);
			}
		}
	}
}
