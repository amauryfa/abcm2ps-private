/*
 * Parsing functions.
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
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#include "abcparse.h"
#include "abc2ps.h" 

struct STAFF_S staff_tb[MAXSTAFF];	/* staff table */
int nstaff;				/* (0..MAXSTAFF-1) */

struct VOICE_S voice_tb[MAXVOICE];	/* voice table */
static struct VOICE_S *curvoice;	/* current voice while parsing */
struct VOICE_S *first_voice;		/* first voice */

struct FORMAT dfmt;		/* current global format */
unsigned short *micro_tb;	/* ptr to the microtone table of the tune */

static int lyric_nb;			/* current number of lyric lines */
static struct SYMBOL *lyric_start;	/* 1st note of the line for w: */
static struct SYMBOL *lyric_cont;	/* current symbol when w: continuation */

static struct SYMBOL *grace_head, *grace_tail;
static struct SYMBOL *voice_over;	/* voice overlay */
static int over_bar;			/* voice overlay in a measure */
static int staves_found;

static int bar_number;			/* (for %%setbarnb) */

float multicol_start;			/* (for multicol) */
static float multicol_max;
static float lmarg, rmarg;

static void get_clef(struct SYMBOL *s);
static void get_key(struct SYMBOL *s);
static void get_meter(struct SYMBOL *s);
static void get_voice(struct SYMBOL *s);
static void get_note(struct SYMBOL *s);
static struct abcsym *process_pscomment(struct abcsym *as);
static void set_tuplet(struct SYMBOL *s);
static void sym_link(struct SYMBOL *s, int type);

/* -- duplicate the symbols of the voices appearing in many staves -- */
void voice_dup(void)
{
	struct VOICE_S *p_voice, *p_voice1;
	struct SYMBOL *s, *s2, *g, *g2;

	for (p_voice = first_voice; p_voice; p_voice = p_voice->next) {
		int voice;

		if (p_voice->clone < 0)
			continue;
		voice = p_voice - voice_tb;
		p_voice1 = &voice_tb[(unsigned) p_voice->clone];
		p_voice->name = p_voice1->name;
		for (s = p_voice1->sym;
		     s != 0;
		     s = s->next) {
			s2 = (struct SYMBOL *) getarena(sizeof *s2);
			memcpy(s2, s, sizeof *s2);
			if (p_voice->sym != 0) {
				p_voice->last_symbol->next = s2;
				s2->prev = p_voice->last_symbol;
			} else	p_voice->sym = s2;
			p_voice->last_symbol = s2;
			s2->voice = voice;
			s2->staff = p_voice->staff;
			s2->ly = 0;
			if ((g = s2->grace) != 0) {
				g2 = (struct SYMBOL *) getarena(sizeof *g2);
				memcpy(g2, g, sizeof *g2);
				s2->grace = g2;
				s2 = g2;
				s2->voice = voice;
				s2->staff = p_voice->staff;
				for (g = g->next; g != 0; g = g->next) {
					g2 = (struct SYMBOL *) getarena(sizeof *g2);
					memcpy(g2, g, sizeof *g2);
					s2->next = g2;
					g2->prev = s2;
					s2 = g2;
					s2->voice = voice;
					s2->staff = p_voice->staff;
				}
			}
		}
	}
}

/* -- change the accidentals and "\\n" in the guitar chords -- */
static void gchord_adjust(struct SYMBOL *s)
{
	char *p;
	int freegchord, l;

	s->gcf = cfmt.gcf;
	s->anf = cfmt.anf;
	freegchord = cfmt.freegchord;
	p = s->as.text;
	if (*p != '\0' && strchr("^_<>@", *p) != 0)
		freegchord = 1;		/* annotation */
/*fixme: treat 'dim' as 'o', 'halfdim' as '/o', and 'maj' as a triangle*/
	while (*p != '\0') {
		switch (*p) {
		case '#':
			if (!freegchord)
				*p = '\201';
			break;
		case 'b':
			if (!freegchord)
				*p = '\202';
			break;
		case '=':
			if (!freegchord)
				*p = '\203';
			break;
		case '\\':
			p++;
			switch (*p) {
			case '\0':
				return;
			case 'n':
				p[-1] = ';';
				goto move;
			case '#':
				p[-1] = '\201';
				goto move;
			case 'b':
				p[-1] = '\202';
				goto move;
			case '=':
				p[-1] = '\203';
			move:
				l = strlen(p);
				memmove(p, p + 1, l);
				p--;
				break;
			}
			break;
		}
		if (*p == ';' && p[-1] != '\\')
			if (p[1] != '\0' && strchr("^_<>@", p[1]) != 0)
				*p = '\n';
		if (*p == '\n') {
			if (p[1] != '\0' && strchr("^_<>@", p[1]) != 0)
				freegchord = 1;
			else	freegchord = cfmt.freegchord;
		}
		p++;
	}
}

/* -- parse a lyric (vocal) definition -- */
static char *get_lyric(char *p)
{
	struct SYMBOL *s;
	char word[128], *q;
	int ln;
	struct FONTSPEC *f;

	f = &cfmt.font_tb[cfmt.vof];
	str_font(cfmt.vof);			/* (for tex_str) */

	if ((s = lyric_cont) == 0) {
		if (lyric_nb >= MAXLY)
			return "Too many lyric lines";
		ln = lyric_nb++;
		s = lyric_start;
	} else	{
		lyric_cont = 0;
		ln = lyric_nb - 1;
	}

	/* scan the lyric line */
	while (*p != '\0') {
		while (isspace((unsigned char) *p))
			p++;
		if (*p == '\0')
			break;
		switch (*p) {
		case '|':
			while (s != 0
			       && (s->type != BAR
				   || s->as.u.bar.type == B_OBRA
				   || s->as.u.bar.type == B_CBRA))
				s = s->next;
			if (s == 0)
				return "Not enough bar lines for lyric line";
			s = s->next;
			p++;
			continue;
		case '-':
			word[0] = '\x02';
			word[1] = '\0';
			p++;
			break;
		case '_':
			word[0] = '\x03';
			word[1] = '\0';
			p++;
			break;
		case '*':
			word[0] = *p++;
			word[1] = '\0';
			break;
		case '\\':
			if (p[1] == '\0') {
				lyric_cont = s;
				return 0;
			}
			/* fall thru */
		default:
			q = word;
			for (;;) {
				unsigned char c;

				c = *p;
				switch (c) {
				case '\0':
				case ' ':
				case '\t':
				case '_':
				case '*':
				case '|':
					break;
				case '~':
					c = ' ';
					goto addch;
				case '-':
					c = '\x02';
					goto addch;
				case '\\':
					if (p[1] == '\0')
						break;
					switch (p[1]) {
					case '~':
					case '_':
					case '*':
					case '|':
					case '-':
					case ' ':
						c = *++p;
						break;
					}
					/* fall thru */
				default:
				addch:
					if (q < &word[sizeof word - 1])
						*q++ = c;
					p++;
					if (c == '\x02')
						break;
					continue;
				}
				break;
			}
			*q = '\0';
			break;
		}

		/* store word in next note */
		while (s != 0 && s->type != NOTE)
			s = s->next;
		if (s == 0)
			return "Too many words in lyric line";
		if (word[0] != '*') {
			struct lyl *lyl;
			float w;

			if (s->ly == 0) {
				s->ly = (struct lyrics *) getarena(sizeof (struct lyrics));
				memset(s->ly, 0, sizeof (struct lyrics));
			}
			w = tex_str(word);

			/* handle the font change at start of text */
			q = tex_buf;
			if (*q == '$' && isdigit((unsigned char) q[1])
			    && (unsigned) (q[1] - '0') < FONT_UMAX) {
				int ft;

				ft = q[1] - '0';
				if (ft == 0)
					ft = cfmt.vof;
				f = &cfmt.font_tb[ft];
				str_font(ft);
				q += 2;
			}
			lyl = (struct lyl *) getarena(sizeof *s->ly->lyl[0]
						    + strlen(q));
			s->ly->lyl[ln] = lyl;
			lyl->f = f;
			lyl->w = w;
			strcpy(lyl->t, q);

		}
		s = s->next;
	}
	while (s != 0 && s->type != NOTE)
		s = s->next;
	if (s != 0)
		return "Not enough words for lyric line";
	return 0;
}

/* -- get a voice overlay -- */
static void get_over(struct SYMBOL *s)
{
	struct VOICE_S *p_voice, *p_voice2;
	int ctime;

	/* treat the end of overlay */
	p_voice = curvoice;
	if (over_bar) {
		struct SYMBOL *s2;

		s2 = sym_add(p_voice, BAR);
		s2->as.type = ABC_T_BAR;
		s2->as.linenum = s->as.linenum;
		s2->as.colnum = s->as.colnum;
	}
	if (s->as.type == ABC_T_BAR
	    || s->as.u.v_over.type == V_OVER_E)  {
		over_bar = 0;
		if (voice_over == 0) {
			error(1, s,
			      "Erroneous end of voice overlap");
			return;
		}
		voice_over = 0;
		if (s->as.type != ABC_T_BAR)
			p_voice = &voice_tb[s->as.u.v_over.voice];
		else for (p_voice = p_voice->prev; ; p_voice = p_voice->prev)
			if (p_voice->name[0] != '&')
				break;
		curvoice = p_voice;
		return;
	}

	/* treat the overlay start */
	if (s->as.u.v_over.type == V_OVER_S) {
		voice_over = s;
		return;
	}

	/* create the extra voice if not done yet */
	p_voice2 = &voice_tb[s->as.u.v_over.voice];
	if (p_voice2->name == 0) {
		p_voice2->name = "&";
		p_voice2->second = 1;
		p_voice2->cstaff = p_voice2->staff = p_voice->staff;
		p_voice2->scale = p_voice->scale;
		if ((p_voice2->next = p_voice->next) != 0)
			p_voice2->next->prev = p_voice2;
		p_voice->next = p_voice2;
		p_voice2->prev = p_voice;
	}

	if (voice_over == 0) {
		voice_over = s;
		over_bar = 1;
		ctime = p_voice2->time;
		for (s = p_voice->last_symbol; /*s != 0*/; s = s->prev)
			if (s->type == BAR || s->time <= ctime)
				break;
	} else {
		struct SYMBOL *tmp;

		tmp = s;
		s = (struct SYMBOL *) voice_over->as.next;
/*fixme: what if this symbol is not in the voice?*/
		if (s->voice != curvoice - voice_tb) {
			error(1, s, "Voice overlay not closed");
			voice_over = 0;
			return;
		}
		voice_over = tmp;
	}
	ctime = s->time;
	p_voice2->time = ctime;
	curvoice = p_voice2;
}

/* -- get staves definition (%%staves) -- */
static void get_staves(struct SYMBOL *s)
{
	int i, staff, flags0, dup_voice, v;
	struct staff_s *p_staff;
	struct VOICE_S *p_voice, *p_voice2;

	/* clear, then link the voices */
	for (i = 0, p_voice = voice_tb; i < MAXVOICE; i++, p_voice++) {
		p_voice->clone = -1;
		p_voice->next = 0;
		p_voice->prev = 0;
		p_voice->second = 0;
		p_voice->floating = 0;
		if (p_voice->name != 0 && p_voice->name[0] == '&')
			p_voice->name = 0;
	}

	p_voice2 = 0;
	dup_voice = MAXVOICE;
	for (i = 0, p_staff = s->as.u.staves;
	     i < MAXVOICE && p_staff->voice>= 0;
	     i++, p_staff++) {
		p_voice = &voice_tb[p_staff->voice];

		/* if voice already inserted, duplicate it */
		if (p_voice == p_voice2 || p_voice->next || p_voice->prev) {
			struct VOICE_S *p_voice3;

			dup_voice--;
			p_voice3 = &voice_tb[dup_voice];
			memcpy(p_voice3, p_voice, sizeof *p_voice3);
			p_voice3->clone = p_staff->voice;
			p_voice3->next = 0;
			p_voice3->second = 0;
			p_voice3->floating = 0;
			p_voice = p_voice3;
			p_staff->voice = dup_voice;
		}

		/* link the voices */
		if ((p_voice->prev = p_voice2) == 0)
			first_voice = p_voice;
		else	p_voice2->next = p_voice;
		p_voice2 = p_voice;
	}

	/* define the staves */
	memset(staff_tb, 0, sizeof staff_tb);
	staff = -1;
	for (i = 0, p_staff = s->as.u.staves;
	     i < MAXVOICE && p_staff->voice >= 0;
	     i++, p_staff++) {
		flags0 = p_staff->flags[0];
#if MAXSTAFF < MAXVOICE
		if (staff >= MAXSTAFF - 1) {
			error(1, s, "Too many staves - aborting");
			exit(2);
		}
#endif
		staff++;

		p_voice = &voice_tb[p_staff->voice];
		p_voice->staff = p_voice->cstaff = staff;

		if ((flags0 & (OPEN_PARENTH | CLOSE_PARENTH))
				== (OPEN_PARENTH | CLOSE_PARENTH))
			flags0 &= ~(OPEN_PARENTH | CLOSE_PARENTH);
		if (flags0 == 0 && p_staff->flags[1] == 0)
			continue;
		staff_tb[staff].flags[0] = flags0;
		staff_tb[staff].flags[1] = p_staff->flags[1];
		if ((flags0 & OPEN_BRACE)
		    || (p_staff->flags[1] & OPEN_BRACE)) {
			for (v = i + 1; v < MAXVOICE; v++)
				if ((s->as.u.staves[v].flags[0] & CLOSE_BRACE)
				    || (s->as.u.staves[v].flags[1] & CLOSE_BRACE))
					break;
			switch (v - i) {
			case 2:				/* {a b c} */
				if (flags0 & OPEN_PARENTH
				    || (p_staff[1].flags[0] & OPEN_PARENTH))
					break;
				i++;
				p_staff++;
				p_voice = &voice_tb[p_staff->voice];
				p_voice->second = 1;
				p_voice->floating = 1;
				p_voice->staff = p_voice->cstaff = staff;
				break;
			case 3:				/* {a b c d} */
				if (flags0 & OPEN_PARENTH
				    || (p_staff[1].flags[0] & OPEN_PARENTH)
				    || (p_staff[2].flags[0] & OPEN_PARENTH))
					break;
/*fixme:to remove*/
				/* -> {(a b) (c d)} */
				flags0 |= OPEN_PARENTH;
				p_staff[1].flags[0] |= CLOSE_PARENTH;
				p_staff[2].flags[0] |= OPEN_PARENTH;
				p_staff[3].flags[0] |= CLOSE_PARENTH;
				break;
			}
		}
		if (flags0 & OPEN_PARENTH) {
			while (i < MAXVOICE) {
				i++;
				p_staff++;
				p_voice = &voice_tb[p_staff->voice];
				p_voice->second = 1;
				p_voice->staff = p_voice->cstaff = staff;
				if (p_staff->flags[0] & CLOSE_PARENTH)
					break;
			}
			staff_tb[staff].flags[0] |= p_staff->flags[0];
			staff_tb[staff].flags[1] |= p_staff->flags[1];
		}
	}
	if (staff < 0)
		staff = 0;
	nstaff = staff;
	if (cfmt.alignbars)
		cfmt.alignbars = nstaff + 1;
}

/* -- re-initialize all potential voices -- */
static void voice_init(void)
{
	struct VOICE_S *p_voice;
	int	i;

	for (i = 0, p_voice = voice_tb;
	     i < MAXVOICE;
	     i++, p_voice++) {
		p_voice->sym = p_voice->last_symbol = 0;
		p_voice->clone = -1;
		p_voice->bar_start = 0;
		p_voice->time = 0;
		p_voice->slur_st = 0;
		p_voice->hy_st = 0;
		p_voice->tie = 0;
		p_voice->rtie = 0;
	}
}

/* -- output the music and lyrics after tune -- */
static void out_musly(int eob)
{
	output_music();
	if (info['W' - 'A'] != 0) {
		put_words(info['W' - 'A']);
		info['W' - 'A'] = 0;
	}
	if (eob)
		buffer_eob();
}

/* -- identify info line, store in proper place	-- */
static char *state_txt[4] = {
	"global", "header", "tune", "embedded"
};
static void get_info(struct SYMBOL *s,
		     int info_type,
		     struct abctune *t)
{
	INFO *inf;
	char *p;

	/* change global or local */
	inf = s->as.state == ABC_S_GLOBAL ? &default_info : &info;
	lvlarena(s->as.state != ABC_S_GLOBAL);

	switch (info_type) {
	case 'd':
		break;
	case 'I':
		process_pscomment(&s->as);	/* same as pseudo-comment */
		break;
	case 'K':
		get_key(s);
		if (s->as.state != ABC_S_HEAD)
			break;
		tunenum++;
		PUT2("%% --- %s (%s) ---\n"
			"%% --- font ",
			&info['X' - 'A']->as.text[2],
			&info['T' - 'A']->as.text[2]);
		outft = -1;
		set_font(TITLEFONT);		/* for index */
		outft = -1;
		PUT0("\n");
		if (!epsf)
			bskip(cfmt.topspace);
		write_heading(t);
		reset_gen();
		if (!cfmt.printtempo)
			info['Q' - 'A'] = 0;
		nbar = nbar_rep = cfmt.measurefirst;	/* measure numbering */
		curvoice = first_voice;		/* switch to the 1st voice */
		break;
	case 'L':
		break;
	case 'M':
		get_meter(s);
		break;
	case 'P':
		if (!cfmt.printparts)
			break;
		switch (s->as.state) {
		case ABC_S_GLOBAL:
		case ABC_S_HEAD:
			(*inf)['P' - 'A'] = s;
			break;
		case ABC_S_TUNE: {
			struct VOICE_S *p_voice;

			p_voice = curvoice;
			curvoice = first_voice;
			sym_link(s, PART);
			if (p_voice->time != curvoice->time) {
				error(1, s, "Misplaced P:");
				if (p_voice->time > curvoice->time)
					s->time = curvoice->time = p_voice->time;
				else	p_voice->time = curvoice->time;
			}
			curvoice = p_voice;
			break;
		    }
		default:
			sym_link(s, PART);
			break;
		}
		break;
	case 'Q':
		if (curvoice != first_voice)	/* tempo only for first voice */
			break;
		switch (s->as.state) {
		case ABC_S_GLOBAL:
		case ABC_S_HEAD:
			(*inf)['Q' - 'A'] = s;
			break;
		default:
			if (cfmt.printtempo)
				sym_link(s, TEMPO);
			break;
		}
		break;
	case 's':
		break;
	case 'T':
		switch (s->as.state) {
		case ABC_S_GLOBAL:	/* T: without X: */
			goto newtune;
		case ABC_S_HEAD:
			goto addinfo;
		default:
			out_musly(1);
			write_title(s);
			bskip(cfmt.musicspace + 0.2 CM);
			voice_init();
			reset_gen();		/* (display the time signature) */
			curvoice = first_voice;
			break;
		}
		break;
	case 'U': {
		unsigned char *deco;

		deco = s->as.state == ABC_S_GLOBAL ? deco_glob : deco_tune;
		deco[s->as.u.user.symbol] = deco_intern(s->as.u.user.value);
		break;
	    }
	case 'u':
		break;
	case 'V':
		get_voice(s);
		break;
	case 'w':
		if (cfmt.musiconly
		    || s->as.state != ABC_S_TUNE)
			break;
		if (lyric_start == 0)
			break;
		p = &s->as.text[2];
		while (isspace((unsigned char) *p))
		       p++;
		if ((p = get_lyric(p)) != 0)
			error(1, s, "%s", p);
		curvoice->have_ly = 1;
		break;
	case 'W':
		if (s->as.state == ABC_S_GLOBAL)
			break;
		goto addinfo;
	case 'X':
newtune:
		if (!epsf)
			write_buffer();	/* flush stuff left from %% lines */
		dfmt = cfmt;		/* save the format at start of tune */
		memcpy(&info, &default_info, sizeof info);
		info['X' - 'A'] = s;
		if (info_type == 'T')
			info['T' - 'A'] = s;
		memcpy(&deco_tune, &deco_glob, sizeof deco_tune);
		voice_init();		/* initialize all the voices */
		break;
	default:
addinfo:
		if (info_type >= 'A' && info_type <= 'Z') {
			struct SYMBOL *prev;

			prev = (*inf)[info_type - 'A'];
			if (prev == 0
			    || (prev->as.state == ABC_S_GLOBAL
				&& s->as.state != ABC_S_GLOBAL)) {
				(*inf)[info_type - 'A'] = s;
				break;
			}
			while (prev->next != 0)
				prev = prev->next;
			prev->next = s;
			s->prev = prev;
			break;
		}
		error(1, s, "%s info '%c:' not treated",
			state_txt[(int) s->as.state], info_type);
		break;
	}
}

/* -- set head type, dots, flags for note -- */
void identify_note(struct SYMBOL *s,
		   int dur,
		   int *p_head,
		   int *p_dots,
		   int *p_flags)
{
	int head, dots, flags;

	if (dur % 12 != 0)
		error(1, s, "Invalid note duration");
	dur /= 12;			/* see BASE_LEN for values */
	if (dur == 0)
		error(1, s, "Note too short");
	for (flags = 5; dur != 0; dur >>= 1, flags--) {
		if (dur & 1)
			break;
	}
	dur >>= 1;
	switch (dur) {
	case 0: dots = 0; break;
	case 1: dots = 1; break;
	case 3: dots = 2; break;
	case 7: dots = 3; break;
	default:
		error(1, s, "Note too much dotted");
		dots = 3;
		break;
	}
	flags -= dots;
	if (flags >= 0)
		head = H_FULL;
	else switch (flags) {
	default:
		error(1, s, "Note too long");
		flags = -4;
		/* fall thru */
	case -4:
		head = H_SQUARE;
		break;
	case -3:
		head = cfmt.squarebreve ? H_SQUARE : H_OVAL;
		break;
	case -2:
		head = H_OVAL;
		break;
	case -1:
		head = H_EMPTY;
		break;
	}
	*p_head = head;
	*p_flags = flags;
	*p_dots = dots;
}

/* -- measure bar -- */
static void get_bar(struct SYMBOL *s)
{
	int bar_type, t;
	struct SYMBOL *s2;

	bar_type = s->as.u.bar.type;

	/* remove the repeat indication if not wanted */
	if (curvoice->norepbra && s->as.u.bar.repeat_bar)
		return;

	/* remove the invisible repeat bars when no shift needed */
	if (bar_type == B_OBRA
	    && (curvoice == first_voice
		|| curvoice->second
		|| (staff_tb[curvoice->staff - 1].flags[0] & STOP_BAR))) {
		s2 = curvoice->last_symbol;
		if (s2 != 0 && s2->type == BAR
		    && s2->as.text == 0) {
			s2->as.text = s->as.text;
			s2->as.u.bar.repeat_bar = s->as.u.bar.repeat_bar;
			return;
		}
	}

	/* merge back-to-back repeat bars */
	if (bar_type == B_LREP && s->as.text == 0) {
		s2 = curvoice->last_symbol;
		t = curvoice->time;
		while (s2 != 0 && s2->time == t) {
			if (s2->type == BAR
			    && s2->as.u.bar.type == B_RREP) {
				s2->as.u.bar.type = B_DREP;
				return;
			}
			s2 = s2->prev;
		}
	}

	sym_link(s, BAR);

	if ((bar_type & 0xf0) != 0) {
		do {
			bar_type >>= 4;
		} while ((bar_type & 0xf0) != 0);
		if (bar_type == B_COL)
			s->sflags |= S_RRBAR;
	}

	if (bar_number != 0
	    && curvoice == first_voice) {
		s->u = bar_number;
		bar_number = 0;
	}

	/* the bar must be before a key signature */
/*fixme: and also before a time signature*/
	if ((s2 = s->prev) != 0
	    && s2->type == KEYSIG
	    && (s2->prev == 0 || s2->prev->type != BAR)) {
		curvoice->last_symbol = s2;
		s2->next = 0;
		s2->prev->next = s;
		s->prev = s2->prev;
		s->next = s2;
		s2->prev = s;
	}
	if (s->as.u.bar.dc.n > 0)
		deco_cnv(&s->as.u.bar.dc, s);	/* convert the decorations */
	if (s->as.text != 0 && !s->as.u.bar.repeat_bar)
		gchord_adjust(s);		/* adjust the guitar chords */
}

/* -- do a tune -- */
void do_tune(struct abctune *t,
	     int header_only)
{
	struct abcsym *as;
	struct SYMBOL *s, *s2;
	int i;

	/* initialize */
	memset(voice_tb, 0, sizeof voice_tb);
	voice_tb[0].name = "1";	/* implicit voice */
	voice_over = 0;
	nstaff = 0;
	memset(staff_tb, 0, sizeof staff_tb);
	staves_found = 0;
	for (i = MAXVOICE; --i >= 0; ) {
		voice_tb[i].clef.line = 2;	/* treble clef on 2nd line */
		voice_tb[i].clef.stafflines = 5;
		voice_tb[i].clef.staffscale = 1;
		voice_tb[i].meter.nmeter = 1;
		voice_tb[i].meter.wmeasure = BASE_LEN;
		voice_tb[i].meter.meter[0].top[0] = '4';
		voice_tb[i].meter.meter[0].bot[0] = '4';
		voice_tb[i].wmeasure = BASE_LEN;
		voice_tb[i].scale = 1;
	}
	voice_init();		/* initialize all the voices */
	for (i = 0; i < nwhistle; i++)
		voice_tb[whistle_tb[i].voice].whistle = 1;
	micro_tb = t->micro_tb;	/* microtone values */

	curvoice = first_voice = voice_tb;
	if (cfmt.oneperpage) {
		use_buffer = 0;
		close_page();
	} else	use_buffer = !cfmt.splittune;

	/* set the note duration - this is needed for tuplets */
	if (!header_only) {
		for (as = t->first_sym; as != 0; as = as->next) {
			if (as->type != ABC_T_NOTE
			    && as->type != ABC_T_REST)
				continue;
			s = (struct SYMBOL *) as;
			s->dur = s->as.u.note.lens[0];
		}
	}

	/* scan the tune */
	grace_head = 0;
	for (as = t->first_sym; as != 0; as = as->next) {
		if (header_only && as->state != ABC_S_GLOBAL)
			break;
		s = (struct SYMBOL *) as;
		switch (as->type) {
		case ABC_T_INFO: {
			int info_type;

			switch (as->text[0]) {
			case 'X':
			case 'T':
			case 'W':
				if (header_only)
					continue;
			}
			info_type = as->text[0];
			for (;;) {
				get_info(s, info_type, t);
				if (as->next == 0
				    || as->next->type != ABC_T_INFO2)
					break;
				as = as->next;
				s = (struct SYMBOL *) as;
			}
			break;
		    }
		case ABC_T_PSCOM:
			as = process_pscomment(as);
			break;
		case ABC_T_NOTE:
		case ABC_T_REST:
			get_note(s);
			break;
		case ABC_T_BAR:
			if (over_bar)
				get_over(s);
			get_bar(s);
			break;
		case ABC_T_CLEF:
			get_clef(s);
			break;
		case ABC_T_EOLN:
			if (cfmt.continueall || cfmt.barsperstaff)
				continue;
			if (curvoice->last_symbol != 0)
				curvoice->last_symbol->sflags |= S_EOLN;
			if (!cfmt.alignbars)
				continue;
			i = (curvoice - voice_tb) + 1;
			if (i < cfmt.alignbars) {
				curvoice = &voice_tb[i];
				continue;
			}
			out_musly(1);
			curvoice = &voice_tb[0];
			continue;
		case ABC_T_MREST: {
			int dur;

			dur = curvoice->wmeasure * as->u.bar.len;
			if (curvoice->second) {
				curvoice->time += dur;
				break;
			}
			sym_link(s, MREST);
			s->dur = dur;
			if (s->as.text != 0)		/* adjust the */
				gchord_adjust(s);	/* guitar chords */
			if (s->as.u.bar.dc.n > 0)
				deco_cnv(&s->as.u.bar.dc, s);
			break;
		    }
		case ABC_T_MREP: {
			int n;

			if (as->next == 0 || as->next->type != ABC_T_BAR) {
				error(1, s,
				      "Measure repeat not followed by a bar");
				break;
			}
			n = as->u.bar.len;
			if (curvoice->second) {
				curvoice->time += curvoice->wmeasure * n;
				break;
			}
			s2 = sym_add(curvoice, REST);
			s2->as.type = ABC_T_REST;
			s2->as.linenum = as->linenum;
			s2->as.colnum = as->colnum;
			s2->as.flags |= ABC_F_INVIS;
			s2->dur = curvoice->wmeasure;
			curvoice->time += curvoice->wmeasure;
			if (n == 1) {
				as->next->u.bar.len = n; /* <n> in the next bar */
				break;
			}
			while (--n > 0) {
				s2 = sym_add(curvoice, BAR);
				s2->as.linenum = as->linenum;
				s2->as.colnum = as->colnum;
				s2->as.u.bar.type = B_SINGLE;
				if (n == as->u.bar.len - 1)
					s2->as.u.bar.len = as->u.bar.len;
				s2 = sym_add(curvoice, REST);
				s2->as.type = ABC_T_REST;
				s2->as.linenum = as->linenum;
				s2->as.colnum = as->colnum;
				s2->as.flags |= ABC_F_INVIS;
				s2->dur = curvoice->wmeasure;
				curvoice->time += curvoice->wmeasure;
			}
			break;
		    }
		case ABC_T_V_OVER:
			get_over(s);
			continue;
		case ABC_T_TUPLET:
			set_tuplet(s);
			break;
		default:
			continue;
		}
		if (s->type == 0)
			continue;
		if (s->dur != 0 && grace_head == 0)
			curvoice->time += s->dur;
		else if (s->as.flags & ABC_F_GR_END)
			grace_head = 0;
	}

	out_musly(0);
	if (!header_only) {
		if (cfmt.writehistory)
			put_history();
	}
	buffer_eob();
	if (epsf) {
		if (nbuf > 0)
			write_eps();
	} else	write_buffer();

	if (info['X' - 'A'] != 0) {
		cfmt = dfmt;	/* restore the format at start of tune */
		info['X' - 'A'] = 0;
	}
}

/* -- get a clef definition (in K: or V:) -- */
static void get_clef(struct SYMBOL *s)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s2;
	int i, stafflines;
	float staffscale;

	p_voice = curvoice;
	if (s->as.prev->type == ABC_T_INFO) {
		switch (s->as.prev->text[0]) {
		case 'K':
			if (s->as.prev->state == ABC_S_HEAD) {
				if (s->as.u.clef.type >= 0) {
					for (i = MAXVOICE, p_voice = voice_tb;
					     --i >= 0;
					     p_voice++) {
						stafflines = p_voice->clef.stafflines;
						staffscale = p_voice->clef.staffscale;
						memcpy(&p_voice->clef, &s->as.u.clef,
						       sizeof p_voice->clef);
						p_voice->clef.stafflines = stafflines;
						p_voice->clef.staffscale = staffscale;
						p_voice->forced_clef = 1;
					}
				}
				if ((stafflines = s->as.u.clef.stafflines) >= 0) {
					for (i = MAXVOICE, p_voice = voice_tb;
					     --i >= 0;
					     p_voice++)
						p_voice->clef.stafflines = stafflines;
				}
				if ((staffscale = s->as.u.clef.staffscale) != 0) {
					for (i = MAXVOICE, p_voice = voice_tb;
					     --i >= 0;
					     p_voice++)
						p_voice->clef.staffscale = staffscale;
				}
				return;
			}
			break;
		case 'V':	/* clef relative to a voice definition */
			p_voice = &voice_tb[(int) s->as.prev->u.voice.voice];
			break;
		}
	}

	if (p_voice->sym == 0) {
		i = p_voice->staff;
		if ((stafflines = s->as.u.clef.stafflines) < 0)
			stafflines = p_voice->clef.stafflines;
		if ((staffscale = s->as.u.clef.staffscale) == 0)
			staffscale = p_voice->clef.staffscale;
		if (s->as.u.clef.type >= 0) {
			memcpy(&p_voice->clef,
			       &s->as.u.clef, 		/* initial clef */
			       sizeof p_voice->clef);
		}
		p_voice->clef.stafflines = stafflines;
		p_voice->clef.staffscale = staffscale;
	} else {
		sym_link(s, CLEF);
		s->u = 1;	/* small clef */

		/* the clef change must be before a key signature */
		s2 = s->prev;
		if (s2->type == KEYSIG) {
			s2->next = 0;
			p_voice->last_symbol = s2;
			if ((s->prev = s2->prev) != 0)
				s->prev->next = s;
			s->next = s2;
			s2->prev = s;
		}

		/* the clef change must be before a bar */
		s2 = s->prev;
		if (s2 != 0 && s2->type == BAR) {
			if ((s2->next = s->next) != 0)
				s->next->prev = s2;
			else	p_voice->last_symbol = s2;
			if ((s->prev = s2->prev) != 0)
				s->prev->next = s;
			s->next = s2;
			s2->prev = s;
		}
	}
	if (s->as.u.clef.type >= 0)
		p_voice->forced_clef = 1;		/* don't change */
}

/* -- get a key signature definition (K:) -- */
static void get_key(struct SYMBOL *s)
{
	struct VOICE_S *p_voice;
	struct SYMBOL *s2;
	int i;

	if (s->as.u.key.empty == 1)
		return;			/* clef only */
	switch (s->as.state) {
	case ABC_S_HEAD:
		for (i = MAXVOICE, p_voice = voice_tb;
		     --i >= 0;
		     p_voice++) {
			memcpy(&p_voice->key, &s->as.u.key, sizeof p_voice->key);
			p_voice->sfp = s->as.u.key.sf;
			if (p_voice->key.bagpipe
			    && p_voice->stem == 0)
				p_voice->stem = -1;
		}
		break;
	case ABC_S_TUNE:
	case ABC_S_EMBED:
		if (curvoice->sym == 0) {

			/* if first symbol of the first voice, apply to all voices */
			if (curvoice == first_voice
			    && s->as.state == ABC_S_TUNE) {	/* (not embedded) */
				for (i = MAXVOICE, p_voice = voice_tb;
				     --i >= 0;
				     p_voice++) {
					memcpy(&p_voice->key, &s->as.u.key,
					       sizeof p_voice->key);
					p_voice->sfp = s->as.u.key.sf;
					if (p_voice->key.bagpipe
					    && p_voice->stem == 0)
						p_voice->stem = -1;
				}
			} else {
				memcpy(&curvoice->key, &s->as.u.key,
				       sizeof curvoice->key);
				curvoice->sfp = s->as.u.key.sf;
				if (curvoice->key.bagpipe
				    && curvoice->stem == 0)
					curvoice->stem = -1;
			}
			break;
		}
		if (curvoice->sfp == s->as.u.key.sf	/* if same key */
		    && s->as.next->type != ABC_T_CLEF)	/* but not explicit clef */
			break;				/* ignore */
		sym_link(s, KEYSIG);
		s->u = curvoice->sfp;		/* old key signature */
		curvoice->sfp = s->as.u.key.sf;

		/* the key signature must be just after a bar */
		if ((s2 = s->prev) != 0 && s2->prev != 0) {
			switch (s2->type) {
			case TIMESIG:
			case TEMPO:
			case PART:
/*			case FMTCHG: */
				curvoice->last_symbol = s2;
				s2->next = 0;
				s2->prev->next = s;
				s->prev = s2->prev;
				s->next = s2;
				s2->prev = s;
				break;
			}
		}
		break;
	}
}

/* -- set meter from M: -- */
static void get_meter(struct SYMBOL *s)
{
	struct VOICE_S *p_voice;
	int i;

	switch (s->as.state) {
	case ABC_S_GLOBAL:
		/*fixme: keep the values and apply to all tunes?? */
		break;
	case ABC_S_HEAD: {
		for (i = MAXVOICE, p_voice = voice_tb;
		     --i >= 0;
		     p_voice++) {
			memcpy(&p_voice->meter, &s->as.u.meter,
			       sizeof p_voice->meter);
			p_voice->wmeasure = s->as.u.meter.wmeasure;
		}
		break;
	    }
	case ABC_S_TUNE:
	case ABC_S_EMBED:
		curvoice->wmeasure = s->as.u.meter.wmeasure;
		if (curvoice->sym == 0) {

			/* if first symbol of the first voice, apply to all voices */
			if (curvoice == first_voice
			    && s->as.state == ABC_S_TUNE) {	/* (not embedded) */
				for (i = MAXVOICE, p_voice = voice_tb;
				     --i >= 0;
				     p_voice++) {
					memcpy(&p_voice->meter, &s->as.u.meter,
					       sizeof p_voice->meter);
					p_voice->wmeasure = s->as.u.meter.wmeasure;
				}
			} else	memcpy(&curvoice->meter, &s->as.u.meter,
				       sizeof curvoice->meter);
			reset_gen();	/* (display the time signature) */
			break;
		}
		if (s->as.u.meter.nmeter == 0)
			break;		/* M:none */
		sym_link(s, TIMESIG);
		break;
	}
}

/* -- treat a 'V:' -- */
static void get_voice(struct SYMBOL *s)
{
	int voice;
	struct VOICE_S *p_voice;

	voice = s->as.u.voice.voice;
	p_voice = &voice_tb[voice];
	if (p_voice->prev == 0 && p_voice != first_voice) {	/* new voice */
		struct VOICE_S *p_voice2;

		if (cfmt.alignbars) {
			error(1, s, "V: does not work with %%%%alignbars");
		}
		if (!staves_found) {
			if (!s->as.u.voice.merge) {
#if MAXSTAFF < MAXVOICE
				if (nstaff >= MAXSTAFF - 1) {
					error(1, s, "Too many staves");
					return;
				}
#endif
				nstaff++;
			} else	p_voice->second = 1;
			p_voice->staff = p_voice->cstaff = nstaff;
			for (p_voice2 = first_voice;
			     p_voice2->next != 0;
			     p_voice2 = p_voice2->next)
				;
			p_voice2->next = p_voice;
			p_voice->prev = p_voice2;
		} else	p_voice->staff = p_voice->cstaff = nstaff + 1;
	}

	/* if in tune, switch to this voice */
	switch (s->as.state) {
	case ABC_S_TUNE:
	case ABC_S_EMBED:
		curvoice = p_voice;
		break;
	}

	/* if something has changed, update */
	if (s->as.u.voice.name != 0)
		p_voice->name = s->as.u.voice.name;
	if (s->as.u.voice.fname != 0) {
		p_voice->nm = s->as.u.voice.fname;
		p_voice->new_name = 1;
	}
	if (s->as.u.voice.nname != 0)
		p_voice->snm = s->as.u.voice.nname;
	if (s->as.u.voice.stem != 0)
		p_voice->stem = s->as.u.voice.stem != 2
			? s->as.u.voice.stem : 0;
	if (s->as.u.voice.gstem != 0)
		p_voice->gstem = s->as.u.voice.gstem != 2
			? s->as.u.voice.gstem : 0;
	if (s->as.u.voice.dyn != 0)
		p_voice->dyn = s->as.u.voice.dyn != 2
			? s->as.u.voice.dyn : 0;
	if (s->as.u.voice.lyrics != 0)
		p_voice->ly_pos = s->as.u.voice.lyrics != 2
			? s->as.u.voice.lyrics : 0;
	if (s->as.u.voice.scale != 0)
		p_voice->scale = s->as.u.voice.scale;
}

/* -- note or rest -- */
static void get_note(struct SYMBOL *s)
{
	int i, m, type;

	s->nhd = m = s->as.u.note.nhd;
	type = s->as.type == ABC_T_NOTE ? NOTE : REST;
	if (!(s->as.flags & ABC_F_GRACE)) {	/* normal note/rest */
		if (grace_head != 0)
			grace_head = 0;
		sym_link(s, type);
		s->stem = curvoice->stem;
	} else {			/* in a grace note sequence */
		int div;

		if (grace_head == 0) {
			struct SYMBOL *s2;

			s2 = sym_add(curvoice, GRACE);
			s2->as.linenum = s->as.linenum;
			s2->as.colnum = s->as.colnum;
			grace_head = s2;
			grace_head->grace = grace_tail = s;
			s2->stem = curvoice->gstem;
		} else {
			grace_tail->next = s;
			s->prev = grace_tail;
			grace_tail = s;
		}
		s->voice = curvoice - voice_tb;
		s->staff = curvoice->cstaff;

		/* adjust the grace note duration */
		if (!curvoice->key.bagpipe) {
			div = 4;
			if (s->prev == 0) {
				if (s->as.next == 0
				    || s->as.next->type != ABC_T_NOTE
				    || !(s->as.next->flags & ABC_F_GRACE))
					div = 2;
			}
		} else	div = 8;
		for (i = 0; i <= m; i++)
			s->as.u.note.lens[i] /= div;
		s->dur = s->as.u.note.lens[0];
		s->type = type;
	}
	s->nohdix = -1;

	/* convert the decorations */
	if (s->as.u.note.dc.n > 0)
		deco_cnv(&s->as.u.note.dc, s);

	/* change the figure of whole measure rests */
	if (s->type == REST) {
		if (s->dur == curvoice->wmeasure) {
			if (s->dur < BASE_LEN * 2)
				s->as.u.note.lens[0] = BASE_LEN;
			else if (s->dur < BASE_LEN * 4)
				s->as.u.note.lens[0] = BASE_LEN * 2;
			else	s->as.u.note.lens[0] = BASE_LEN * 4;
		}
	}

	/* sort by pitch the notes of the chord (lowest first) */
	else for (;;) {
		int nx = 0;

		for (i = 1; i <= m; i++) {
			if (s->as.u.note.pits[i] < s->as.u.note.pits[i-1]) {
				int k;
#define xch(f) \
	k = s->as.u.note.f[i]; \
	s->as.u.note.f[i] = s->as.u.note.f[i-1]; \
	s->as.u.note.f[i-1] = k
				xch(pits);
				xch(lens);
				xch(accs);
				xch(sl1);
				xch(sl2);
				xch(ti1);
				xch(decs);
#undef xch
				nx++;
			}
		}
		if (nx == 0)
			break;
	}

	memcpy(s->pits, s->as.u.note.pits, sizeof s->pits);

	/* get the max head type, number of dots and number of flags */
	{
		int head, dots, nflags, l;

		if ((l = s->as.u.note.lens[0]) != 0) {
			identify_note(s, l, &head, &dots, &nflags);
			s->head = head;
			s->dots = dots;
			if (!(s->sflags & S_TREM)) {
				s->nflags = nflags;
				for (i = 1; i <= m; i++) {
					if (s->as.u.note.lens[i] == l)
						continue;
					identify_note(s, s->as.u.note.lens[i],
						      &head, &dots, &nflags);
					if (head > s->head)
						s->head = head;
					if (dots > s->dots)
						s->dots = dots;
					if (nflags > s->nflags)
						s->nflags = nflags;
				}
				if (s->sflags & S_XSTEM)
					s->nflags = 0;	/* word start+end */
			} else {	/* 2nd tremolo note */
				s->prev->head = head;
				if (head >= H_OVAL) {
					s->as.flags |= ABC_F_STEMLESS;
					s->prev->as.flags |= ABC_F_STEMLESS;
				}
			}
		}
	}
	if (s->nflags <= -2)
		s->as.flags |= ABC_F_STEMLESS;

	for (i = 0; i <= m; i++) {
		if (s->as.u.note.sl1[i] != 0)
			s->sflags |= S_SL1;
		if (s->as.u.note.sl2[i] != 0)
			s->sflags |= S_SL2;
		if (s->as.u.note.ti1[i] != 0)
			s->sflags |= S_TI1;
	}

	if (s->as.flags & ABC_F_LYRIC_START) {
		lyric_start = s;
		lyric_cont = 0;
		lyric_nb = 0;
	}

	/* adjust the guitar chords */
	if (s->as.text != 0)
		gchord_adjust(s);
}

/* -- treat a postscript definition -- */
static void ps_def(struct SYMBOL *s,
		   char *p)
{
	if (s->as.state == ABC_S_TUNE
	    || s->as.state == ABC_S_EMBED) {
		sym_link(s, FMTCHG);
		s->u = PSSEQ;
		s->as.text = p;
		return;
	}
	if (file_initialized) {
		PUT1("%s\n", p);
		return;
	}
	interpret_fmt_line("postscript", p, 0);
}

/* -- process a pseudo-comment (%% or I:) -- */
static struct abcsym *process_pscomment(struct abcsym *as)
{
	char w[32], *p;
	float h1;
	struct SYMBOL *s = (struct SYMBOL *) as;

	p = as->text + 2;		/* skip '%%' */
	while (isspace((unsigned char) *p))
		p++;
	if (strncasecmp(p, "fmt ", 4) == 0) {
		p += 4;			/* skip 'fmt' */
		while (isspace((unsigned char) *p))
			p++;
	}
	p = get_str(w, p, sizeof w);
	switch (w[0]) {
	case 'b':
		if (strcmp(w, "beginps") == 0) {
			for (;;) {
				if (as->next == 0)
					return as;
				as = as->next;
				p = as->text;
				if (*p == '%' && p[1] == '%') {
					p += 2;
					while (isspace((unsigned char) *p))
						p++;
					if (strncasecmp(p, "fmt ", 4) == 0) {
						p += 4;
						while (isspace((unsigned char) *p))
							p++;
					}
					if (strncmp(p, "endps", 5) == 0)
						return as;
				}
				if (*p == '%')
					continue;	/* skip comment lines */
				s = (struct SYMBOL *) as;
				ps_def(s, p);
			}
			/* not reached */
		}
		if (strcmp(w, "begintext") == 0) {
			int job;

			if (epsf && as->state != ABC_S_HEAD)
				return as;
			if ((job = cfmt.textoption) == T_SKIP)
				return as;
			if (*p != '\0') {
				job = get_textopt(p);
				if (job < 0) {
					error(1, s,
					      "Bad argument for begintext: %s", p);
					job = T_LEFT;
				}
			}
			out_musly(1);
			for (;;) {
				if (as->next == 0)
					return as;
				as = as->next;
				p = as->text;
				if (*p == '%' && p[1] == '%') {
					p += 2;
					while (isspace((unsigned char) *p))
						p++;
					if (strncasecmp(p, "fmt ", 4) == 0) {
						p += 4;
						while (isspace((unsigned char) *p))
							p++;
					}
					if (strncmp(p, "endtext", 7) == 0) {
						if (job != T_SKIP)
							write_text_block(job, as->state);
						return as;
					}
				}
				if (job != T_SKIP)
					add_to_text_block(p, job);
			}
			/* not reached */
		}
		break;
	case 'E':
		if (strcmp(w, "EPS") == 0) {
			float x1, y1, x2, y2;
			FILE *fp;
			char fn[BSIZE], line[BSIZE];

			out_musly(1);
			if (cfmt.textoption == T_SKIP)
				return as;
			get_str(line, p, BSIZE);
			if ((fp = open_file(line, "eps", fn)) == 0) {
				error(1, s, "No such file: %s", line);
				return as;
			}

			/* get the bounding box */
			while (fgets(line, sizeof line, fp)) {
				if (strncmp(line, "%%BoundingBox:", 14) == 0) {
					if (sscanf(&line[14], "%f %f %f %f",
						   &x1, &y1, &x2, &y2) == 4)
						break;
				}
			}
			fclose(fp);
			if (strncmp(line, "%%BoundingBox:", 14) != 0) {
				error(1, s,
				      "No bounding box in '%s'", fn);
				return as;
			}
			if (cfmt.textoption == T_CENTER
			    || cfmt.textoption == T_RIGHT) {
				float lw;

				lw = ((cfmt.landscape ? cfmt.pageheight : cfmt.pagewidth)
					- cfmt.leftmargin - cfmt.rightmargin) / cfmt.scale;
				if (cfmt.textoption == T_CENTER)
					x1 += (lw - (x2 - x1)) * 0.5;
				else	x1 += lw - (x2 - x1);
			}
			PUT0("\001");	/* include file (must be the first after eob) */
			bskip(y2 - y1);
			PUT3("%.2f %.2f%%%s\n", x1, y1, fn);
			buffer_eob();
			return as;
		}
		break;
	case 'm':
		if (strcmp(w, "maxsysstaffsep") == 0) {
			if (as->state != ABC_S_TUNE
			    && as->state != ABC_S_EMBED)
				break;
			curvoice->maxsep = scan_u(p);
			return as;
		}
		if (strcmp(w, "measrep") == 0)
			goto irepeat;
		if (strcmp(w, "multicol") == 0) {
			float bposy;

			out_musly(1);
			if (strncmp(p, "start", 5) == 0) {
				if (!in_page)
					PUT0("%%\n");	/* initialize the output */
				bposy = get_bposy();
				multicol_max = multicol_start = bposy;
				lmarg = cfmt.leftmargin;
				rmarg = cfmt.rightmargin;
			} else if (strncmp(p, "new", 3) == 0) {
				if (multicol_start == 0)
					error(1, s,
					      "%%%%multicol new without start");
				else {
					bposy = get_bposy();
					if (bposy < multicol_start)
						abskip(bposy - multicol_start);
					if (bposy < multicol_max)
						multicol_max = bposy;
					cfmt.leftmargin = lmarg;
					cfmt.rightmargin = rmarg;
				}
			} else if (strncmp(p, "end", 3) == 0) {
				if (multicol_start == 0)
					error(1, s,
					      "%%%%multicol end without start");
				else {
					bposy = get_bposy();
					if (bposy > multicol_max)
						abskip(bposy - multicol_max);
					cfmt.leftmargin = lmarg;
					cfmt.rightmargin = rmarg;
					multicol_start = 0;
					PUT0("%%\n");	/* force write_buffer */
					buffer_eob();
				}
			} else {
				error(1, s,
				      "Unknown keyword '%s' in %%%%multicol", p);
			}
			return as;
		}
		break;
	case 'n':
		if (strcmp(w, "newpage") == 0) {
			if (epsf)
				return as;
			out_musly(0);
			write_buffer();
			use_buffer = 0;
			if (isdigit((unsigned char) *p))
				pagenum = atoi(p);
			close_page();
			return as;
		}
		break;
	case 'p':
		if (strcmp(w, "postscript") == 0) {
			ps_def(s, p);
			return as;
		}
		break;
	case 'r':
		if (strcmp(w, "repbra") == 0) {
			if (as->state != ABC_S_TUNE
			    && as->state != ABC_S_EMBED)
				return as;
			curvoice->norepbra = !atoi(p);
			return as;
		}
		if (strcmp(w, "repeat") == 0) {
			int n, k;

irepeat:
			if (curvoice->last_symbol == 0) {
				error(1, s,
				      "%%%%repeat cannot start a tune");
				return as;
			}
			if (*p == '\0') {
				n = 1;
				k = 1;
			} else {
				n = atoi(p);
				if (n < 1
				    || (curvoice->last_symbol->type == BAR
					&& n > 2)) {
					error(1, s,
					      "Incorrect 1st value in %%%%repeat");
					return as;
				}
				while (*p != '\0' && !isspace((unsigned char) *p))
					p++;
				while (isspace((unsigned char) *p))
					p++;
				if (*p == '\0')
					k = 1;
				else {
					k = atoi(p);
					if (k < 1
					    || (curvoice->last_symbol->type == BAR
						&& n == 2
						&& k > 1)) {
						error(1, s,
						      "Incorrect 2nd value in %%%%repeat");
						return as;
					}
				}
			}
			sym_link(s, FMTCHG);
			s->u = REPEAT;
			if (curvoice->last_symbol->prev->type == BAR)
				s->doty = n;
			else	s->doty = -n;
			s->nohdix = k;
			as->text = 0;
			return as;
		}
	case 's':
		if (strcmp(w, "setbarnb") == 0) {
			if (as->state == ABC_S_TUNE
			    || as->state == ABC_S_EMBED) {
				bar_number = atoi(p);
				return as;
			}
			strcpy(w, "measurefirst");
			break;
		}
		if (strcmp(w, "sep") == 0) {
			float h2, len, lwidth;

			out_musly(0);
			lwidth = (cfmt.landscape ? cfmt.pageheight : cfmt.pagewidth)
				- cfmt.leftmargin - cfmt.rightmargin;
			h1 = h2 = len = 0;
			if (*p != '\0') {
				h1 = scan_u(p);
				while (*p != '\0' && !isspace((unsigned char) *p))
					p++;
				while (isspace((unsigned char) *p))
					p++;
			}
			if (*p != '\0') {
				h2 = scan_u(p);
				while (*p != '\0' && !isspace((unsigned char) *p))
					p++;
				while (isspace((unsigned char) *p))
					p++;
			}
			if (*p != '\0')
				len = scan_u(p);
			if (h1 < 1)
				h1 = 0.5 CM;
			if (h2 < 1)
				h2 = h1;
			if (len < 1)
				len = 3.0 CM;
			bskip(h1);
			PUT2("%.1f %.1f sep0\n",
			     len / cfmt.scale,
			     (lwidth - len) * 0.5 / cfmt.scale);
			bskip(h2);
			buffer_eob();
			return as;
		}
		if (strcmp(w, "staff") == 0) {
			int staff;

			if (as->state != ABC_S_TUNE
			    && as->state != ABC_S_EMBED)
				return as;
			if (*p == '+')
				staff = curvoice->cstaff + atoi(p + 1);
			else if (*p == '-')
				staff = curvoice->cstaff - atoi(p + 1);
			else	staff = atoi(p) - 1;
			if ((unsigned) staff > nstaff) {
				error(1, s, "Bad staff in %%%%staff");
				return as;
			}
			curvoice->floating = 0;
			curvoice->cstaff = staff;
			return as;
		}
		if (strcmp(w, "staffbreak") == 0) {
			if (as->state != ABC_S_TUNE
			    && as->state != ABC_S_EMBED)
				return as;
			sym_link(s, FMTCHG);
			s->u = STBRK;
			if (isdigit(*p)) {
				s->xmx = scan_u(p);
				if (p[strlen(p) - 1] == 'f')
					s->doty = 1;
			} else {
				s->xmx = 0.5 CM;
				if (*p == 'f')
					s->doty = 1;
			}
			return as;
		}
		if (strcmp(w, "staves") == 0
		    || strcmp(w, "score") == 0) {
			if (as->state == ABC_S_GLOBAL)
				return as;
			if (as->state == ABC_S_TUNE
			    || as->state == ABC_S_EMBED) {
				output_music();
				buffer_eob();
				voice_init();
			}
			get_staves(s);
			curvoice = first_voice;
			staves_found = 1;
			return as;
		}
		if (strcmp(w, "sysstaffsep") == 0) {
			if (as->state != ABC_S_TUNE
			    && as->state != ABC_S_EMBED)
				break;
			curvoice->sep = scan_u(p);
			return as;
		}
		break;
	case 'c':
	case 't':
		if (strcmp(w, "text") == 0 || strcmp(w, "center") == 0) {
			int job;

			if (epsf && as->state == ABC_S_GLOBAL)
				return as;
			if ((job = cfmt.textoption) == T_SKIP)
				return as;
			if (w[0] == 'c')
				job = T_CENTER;
			out_musly(1);
			add_to_text_block(p, job);
			write_text_block(job, as->state);
			return as;
		}
		if (strcmp(w, "tablature") == 0) {
			float h;

			if (as->state != ABC_S_TUNE
			    && as->state != ABC_S_EMBED)
				return as;
/* %%tablature [<h above>] <h under> <head funct> <note funct> <bar funct> */
			h = scan_u(p);
			while (*p != '\0' && !isspace((unsigned char) *p))
				p++;
			while (isspace((unsigned char) *p))
				p++;
			if (isdigit(*p)) {
				curvoice->tabha = h;
				curvoice->tabhu = scan_u(p);
				while (*p != '\0' && !isspace((unsigned char) *p))
					p++;
				while (isspace((unsigned char) *p))
					p++;
			} else	curvoice->tabhu = h;
			if (*p != '\0') {
				curvoice->tabhead = p;
				while (*p != '\0' && !isspace((unsigned char) *p))
					p++;
			}
			if (*p != '\0') {
				*p = '\0';
				p++;
				while (isspace((unsigned char) *p))
					p++;
				curvoice->tabnote = p;
				while (*p != '\0' && !isspace((unsigned char) *p))
					p++;
			}
			if (*p != '\0') {
				*p = '\0';
				p++;
				while (isspace((unsigned char) *p))
					p++;
				curvoice->tabbar = p;
			} else {
				error(1, s, "Wrong values in %%%%tablature");
				curvoice->tabhead = 0;
			}
		}
		break;
	case 'v':
		if (strcmp(w, "vskip") == 0) {
			out_musly(0);
			h1 = scan_u(p);
			bskip(h1);
			buffer_eob();
			return as;
		}
		break;
	}
	if (as->state == ABC_S_TUNE
	    || as->state == ABC_S_EMBED) {
		if (strcmp(w, "leftmargin") == 0
		    || strcmp(w, "rightmargin") == 0
		    || strcmp(w, "scale") == 0) {
			out_musly(1);
		}
	}
	interpret_fmt_line(w, p, 0);
	if (cfmt.alignbars && strcmp(w, "alignbars") == 0) {
		struct VOICE_S *p_voice;
		int i;

		output_music();
		if ((unsigned) cfmt.alignbars > MAXSTAFF) {
			error(1, s, "Too big value in %%%%alignbars");
			cfmt.alignbars = MAXSTAFF;
		}
		if (staves_found)
			cfmt.alignbars = nstaff + 1;
		first_voice = curvoice = p_voice = &voice_tb[0];
		for (i = 0; i < cfmt.alignbars; i++) {
			staff_tb[i].flags[0] |= STOP_BAR;
			p_voice->staff = p_voice->cstaff = i;
			if (i > 0)
				p_voice->prev = p_voice - 1;
			else	p_voice->prev = 0;
			if (i >= cfmt.alignbars - 1) {
				p_voice->next = 0;
				break;
			}
			p_voice->next = p_voice + 1;
			p_voice++;
		}
		nstaff = i;
	}
	return as;
}

/* -- set the duration of notes/rests in a tuplet -- */
static void set_tuplet(struct SYMBOL *t)
{
	struct abcsym *as, *first;
	struct SYMBOL *s;
	int l, r, lplet, grace;

	r = t->as.u.tuplet.r_plet;
	grace = t->as.flags & ABC_F_GRACE;
	l = 0;
	first = 0;
	for (as = t->as.next; as != 0; as = as->next) {
/*fixme: KO if voice change..*/
		if (as->type != ABC_T_NOTE && as->type != ABC_T_REST)
			continue;
		if (as->u.note.lens[0] == 0)	/* space ('y') */
			continue;
		if (grace) {
			if (!(as->flags & ABC_F_GRACE))
				continue;
		} else {
			if (as->flags & ABC_F_GRACE)
				continue;
		}
		if (first == 0)
			first = as;
		s = (struct SYMBOL *) as;
		l += s->dur;
		if (--r <= 0)
			break;
	}
	if (as == 0) {
		error(1, t,
		      "End of tune found inside a tuplet");
		return;
	}
	lplet = (l * t->as.u.tuplet.q_plet) / t->as.u.tuplet.p_plet;
	r = t->as.u.tuplet.r_plet;
	for (as = first; as != 0; as = as->next) {
		int olddur;

/*fixme: KO if voice change..*/
		if (as->type != ABC_T_NOTE && as->type != ABC_T_REST)
			continue;
		if (as->u.note.lens[0] == 0)
			continue;
		if (grace) {
			if (!(as->flags & ABC_F_GRACE))
				continue;
		} else {
			if (as->flags & ABC_F_GRACE)
				continue;
		}
		s = (struct SYMBOL *) as;
		if (as != first)
			s->sflags |= S_IN_TUPLET;
		olddur = s->dur;
		s->dur = (olddur * lplet) / l;
		if (--r <= 0) {
			break;
		}
		l -= olddur;
		lplet -= s->dur;
	}
/*fixme: KO if in a grace sequence*/
	if (grace)
		error(1, t,
		      "Tuplets in grace note sequence not yet treated");
	else {
		sym_link(t, TUPLET);
		t->u = cfmt.tuplets;
	}
}

/* -- add a new symbol at end of list -- */
struct SYMBOL *sym_add(struct VOICE_S *p_voice, int type)
{
	struct SYMBOL *s;
	struct VOICE_S *p_voice2;

	s = (struct SYMBOL *) getarena(sizeof *s);
	memset(s, 0, sizeof *s);
	p_voice2 = curvoice;
	curvoice = p_voice;
	sym_link(s, type);
	curvoice = p_voice2;
	return s;
}

/* -- link a ABC symbol into a voice -- */
static void sym_link(struct SYMBOL *s, int type)
{
	struct VOICE_S *p_voice = curvoice;

/*	memset((&s->as) + 1, 0, sizeof (struct SYMBOL) - sizeof (struct abcsym)); */
	if (p_voice->sym != 0) {
		p_voice->last_symbol->next = s;
		s->prev = p_voice->last_symbol;
	} else	p_voice->sym = s;
	p_voice->last_symbol = s;

	s->type = type;
	s->voice = p_voice - voice_tb;
	s->staff = p_voice->cstaff;
	s->time = p_voice->time;
}
