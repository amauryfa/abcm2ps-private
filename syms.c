/*
 * Postscript definitions.
 *
 * This file is part of abcm2ps.
 *
 * Copyright (C) 1998-2006 Jean-Fran�ois Moine
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
#include <time.h>
#include <string.h>

#include "abcparse.h"
#include "abc2ps.h"

static char ps_head[] =
	"/xymove{/x 2 index def/y 1 index def M}!\n"

	/* str showc - center at current pt */
	"/showc{dup stringwidth pop .5 mul neg 0 RM show}!\n"

	/* str showr - show right-aligned */
	"/showr{dup stringwidth pop neg 0 RM show}!\n"

	/* str showb - show in box */
	"/showb{	dup currentpoint 3 -1 roll show\n"
	"	.6 SLW\n"
	"	exch 2 sub exch 3 sub 3 -1 roll\n"
	"	stringwidth pop 4 add fh 4 add rectstroke}!\n"

#if 0
	"/showcb{ % usage: str showcb - show centered in box\n"
	"	dup stringwidth pop dup .5 mul neg 0 RM currentpoint 4 -1 roll show\n"
	"	.6 SLW\n"
	"	exch 2 sub exch 3 sub 3 -1 roll\n"
	"	4 add fh 4 add rectstroke}!\n"
#endif

	/* x y tclef - treble clef */
	"/tclef{	M\n"
	"	-1.9 3.7 RM\n"
	"	-3.3 1.9 -3.1 6.8 2.4 8.6 RC\n"
	"	7 0 9.8 -8 4.1 -11.7 RC\n"
	"	-5.2 -2.4 -12.5 0 -13.3 6.2 RC\n"
	"	-0.7 6.4 4.15 10.5 10 15.3 RC\n"
	"	4 4 3.6 6.1 2.8 9.6 RC\n"
	"	-2.3 -1.5 -4.7 -4.8 -4.5 -8.5 RC\n"
	"	0.8 -12.2 3.4 -17.3 3.5 -26.3 RC\n"
	"	0.3 -4.4 -1.2 -6.2 -3.8 -6.2 RC\n"
	"	-3.7 -0.1 -5.8 4.3 -2.8 6.1 RC\n"
	"	3.9 1.9 6.1 -4.6 1.4 -4.8 RC\n"
	"	0.7 -1.2 4.6 -0.8 4.2 4.2 RC\n"
	"	-0.2 10.3 -3 15.7 -3.5 28.3 RC\n"
	"	0 4.1 0.6 7.4 5 10.6 RC\n"
	"	2.3 -3.2 2.9 -10 1 -12.7 RC\n"
	"	-2.4 -4.3 -11.5 -10.3 -11.8 -15 RC\n"
	"	0.4 -7 6.9 -8.5 11.7 -6.1 RC\n"
	"	3.9 3 1.3 8.8 -3.7 8.1 RC\n"
	"	-4 -0.2 -4.8 -3.1 -2.7 -5.7 RC\n"
	"	fill}!\n"

	"/stclef{exch 0.85 div exch 0.85 div gsave 0.85 dup scale tclef grestore}!\n"

	/* x y octu - upper '8' */
	"/octu{/Times-Roman 12 selectfont M -1.5 36 RM(8)show}!\n"
	/* x y octl - lower '8' */
	"/octl{/Times-Roman 12 selectfont M -3.5 -19 RM(8)show}!\n"

	/* x y bclef - bass clef */
	"/bclef{	M\n"
	"	-8.8 3.5 RM\n"
	"	6.3 1.9 10.2 5.6 10.5 10.8 RC\n"
	"	0.3 4.9 -0.5 8.1 -2.6 8.8 RC\n"
	"	-2.5 1.2 -5.8 -0.7 -5.9 -4.1 RC\n"
	"	1.8 3.1 6.1 -0.6 3.1 -3 RC\n"
	"	-3 -1.4 -5.7 2.3 -1.9 7 RC\n"
	"	2.6 2.3 11.4 0.6 10.1 -8 RC\n"
	"	-0.1 -4.6 -5 -10.2 -13.3 -11.5 RC\n"
	"	15.5 17 RM\n"
	"	0 1.5 2 1.5 2 0 RC\n"
	"	0 -1.5 -2 -1.5 -2 0 RC\n"
	"	0 -5.5 RM\n"
	"	0 1.5 2 1.5 2 0 RC\n"
	"	0 -1.5 -2 -1.5 -2 0 RC\n"
	"	fill}!\n"

	"/sbclef{exch 0.85 div exch 0.85 div gsave 0.85 dup scale 0 3 T bclef grestore}!\n"

	"/cchalf{0 12 M\n"
	"	2.6 5 RL\n"
	"	2.3 -5.8 5.2 -2.4 4.7 1.6 RC\n"
	"	0.4 3.9 -3 6.7 -5.1 4 RC\n"
	"	4.1 0.5 0.9 -5.3 -0.9 -1.4 RC\n"
	"	-0.5 3.4 6.5 4.3 7.8 -0.8 RC\n"
	"	1.9 -5.6 -4.1 -9.8 -6 -5.4 RC\n"
	"	-1.6 -3 RL\n"
	"	fill}!\n"

	/* x y cclef */
	"/cclef{	gsave T\n"
	"	cchalf 0 24 T 1 -1 scale cchalf\n"
	"	-5.5 0 3 24 rectfill\n"
	"	-0.5 0 M 0 24 RL 0.7 SLW stroke grestore}!\n"

	"/scclef{exch .85 div exch .85 div gsave .85 dup scale\n"
	"	2 add cclef grestore}!\n"

	/* x y pclef */
	"/pclef{	exch 2.7 sub exch 2 add 5.4 20\n"
	"	1.4 SLW rectstroke}!\n"
	"/spclef{pclef}!\n"

	/* t dx dy x y bm - beam, depth t */
	"/bm{	M 3 copy RL neg 0 exch RL\n"
	"	neg exch neg exch RL 0 exch RL fill}!\n"

	/* str x y bnum - tuplet number / ratio */
	"/bnum{M/Times-Italic 12 selectfont showc}!\n"
	/* same with clearing below the number */
	"/bnumb{	currentgray /Times-Italic 12 selectfont\n"
	"	3 index stringwidth pop 4 add\n"
	"	dup .5 mul neg 4 index add 3 index 3 -1 roll 8\n"
	"	1.0 setgray rectfill setgray M showc}!\n"

	/* dx dy x y tubr - tuplet bracket */
	"/tubr{3 sub M 0 3 RL RL 0 -3 RL dlw stroke}!\n"
	"/tubrl{3 add M 0 -3 RL RL 0 3 RL dlw stroke}!\n"

	/* x y r00 - longa rest */
	"/r00{	xymove\n"
	"	-1 -6 RM currentpoint 3 12 rectfill}!\n"

	/* x y r0 - breve rest */
	"/r0{	xymove\n"
	"	-1 0 RM currentpoint 3 6 rectfill}!\n"

	/* x y r1 - rest */
	"/r1{	xymove\n"
	"	-3 3 RM currentpoint 7 3 rectfill}!\n"

	/* x y r2 - half rest */
	"/r2{	xymove\n"
	"	-3 0 RM currentpoint 7 3 rectfill}!\n"

	/* x y r4 - quarter rest */
	"/r4{	xymove\n"
	"	-1 8.5 RM\n"
	"	3.6 -5.1 RL\n"
	"	-2.1 -5.2 RL\n"
	"	2.2 -4.3 RL\n"
	"	-2.6 2.3 -5.1 0 -2.4 -2.6 RC\n"
	"	-4.8 3 -1.5 6.9 1.4 4.1 RC\n"
	"	-3.1 4.5 RL\n"
	"	1.9 5.1 RL\n"
	"	-1.5 3.5 RL\n"
	"	fill}!\n"

	/* 1/8 .. 1/64 rest element */
	"/r8e{	-1.5 -1.5 -2.4 -2 -3.6 -2 RC\n"
	"	2.4 2.8 -2.8 4 -2.8 1.2 RC\n"
	"	0 -2.7 4.3 -2.4 5.9 -0.6 RC\n"
	"	fill}!\n"

	/* x y r8 - eighth rest */
	"/r8{	xymove\n"
	"	.5 SLW 3.3 4 RM\n"
	"	-3.4 -9.6 RL stroke\n"
	"	x y M 3.4 4 RM r8e}!\n"

	/* x y r16 - 16th rest */
	"/r16{	xymove\n"
	"	.5 SLW 3.3 4 RM\n"
	"	-4 -15.6 RL stroke\n"
	"	x y M 3.4 4 RM r8e\n"
	"	x y M 1.9 -2 RM r8e}!\n"

	/* x y r32 - 32th rest */
	"/r32{	xymove\n"
	"	.5 SLW 4.8 10 RM\n"
	"	-5.5 -21.6 RL stroke\n"
	"	x y M 4.9 10 RM r8e\n"
	"	x y M 3.4 4 RM r8e\n"
	"	x y M 1.9 -2 RM r8e}!\n"

	/* x y r64 - 64th rest */
	"/r64{	xymove\n"
	"	.5 SLW 4.8 10 RM\n"
	"	-7 -27.6 RL stroke\n"
	"	x y M 4.9 10 RM r8e\n"
	"	x y M 3.4 4 RM r8e\n"
	"	x y M 1.9 -2 RM r8e\n"
	"	x y M 0.4 -8 RM r8e}!\n"

	/* x y r128 - 128th rest */
	"/r128{	xymove\n"
	"	.5 SLW 5.8 16 RM\n"
	"	-8.5 -33.6 RL stroke\n"
	"	x y M 5.9 16 RM r8e\n"
	"	x y M 4.4 10 RM r8e\n"
	"	x y M 2.9 4 RM r8e\n"
	"	x y M 1.4 -2 RM r8e\n"
	"	x y M -0.1 -8 RM r8e}!\n"

	/* dx dy dt - dot relative to head */
	"/dt{x y M RM currentpoint 1.2 0 360 arc fill}!\n"

	/* x y hld - fermata */
	"/hld{	1.5 add 2 copy 1.5 add M currentpoint 1.3 0 360 arc\n"
	"	M -7.5 0 RM\n"
	"	0 11.5 15 11.5 15 0 RC\n"
	"	-0.25 0 RL\n"
	"	-1.25 9 -13.25 9 -14.5 0 RC\n"
	"	fill}!\n"

	/* x y dnb - down bow */
	"/dnb{	dlw M -3.2 2 RM\n"
	"	0 7.2 RL\n"
	"	6.4 0 RM\n"
	"	0 -7.2 RL\n"
	"	currentpoint stroke M\n"
	"	-6.4 4.8 RM\n"
	"	0 2.4 RL\n"
	"	6.4 0 RL\n"
	"	0 -2.4 RL\n"
	"	fill}!\n"

	/* x y upb - up bow */
	"/upb{	dlw M -2.6 9.4 RM\n"
	"	2.6 -8.8 RL\n"
	"	2.6 8.8 RL\n"
	"	stroke}!\n"

	/* x y grm - gracing mark */
	"/grm{	M -5 2.5 RM\n"
	"	5 8.5 5.5 -4.5 10 2 RC\n"
	"	-5 -8.5 -5.5 4.5 -10 -2 RC fill}!\n"

	/* x y stc - staccato mark */
	"/stc{M currentpoint 1.2 0 360 arc fill}!\n"

	/* x y emb - emphasis bar */
	"/emb{	1.2 SLW 1 setlinecap M\n"
	"	-2.5 0 RM 5 0 RL stroke 0 setlinecap}!\n"

	/* x y cpu - roll sign above head */
	"/cpu{	M -6 0 RM\n"
	"	0.4 7.3 11.3 7.3 11.7 0 RC\n"
	"	-1.3 6 -10.4 6 -11.7 0 RC fill}!\n"

	/* x y sld - slide */
	"/sld{	M -7.2 -4.8 RM\n"
	"	1.8 -0.7 4.5 0.2 7.2 4.8 RC\n"
	"	-2.1 -5 -5.4 -6.8 -7.6 -6 RC fill}!\n"

	/* x y trl - trill sign */
	"/trl{	/Times-BoldItalic 16 selectfont\n"
	"	M -4 2 RM(tr)show}!\n"

	/* x y umrd - upper mordent */
	"/umrd{	4 add M\n"
	"	2.2 2.2 RL 2.1 -2.9 RL 0.7 0.7 RL\n"
	"	-2.2 -2.2 RL -2.1 2.9 RL -0.7 -0.7 RL\n"
	"	-2.2 -2.2 RL -2.1 2.9 RL -0.7 -0.7 RL\n"
	"	2.2 2.2 RL 2.1 -2.9 RL 0.7 0.7 RL fill}!\n"

	/* x y lmrd - lower mordent */
	"/lmrd{	2 copy umrd 8 add M\n"
	"	.6 SLW 0 -8 RL stroke}!\n"

	/* str x y fng - finger (0-5) */
	"/fng{/Bookman-Demi 8 selectfont M -3 1 RM show}!\n"

	/* str x y dacs - D.C. / D.S. */
	"/dacs{/Times-Roman 16 selectfont 3 add M showc}!\n"

	/* x y brth - breath */
	"/brth{/Times-BoldItalic 30 selectfont 6 add M(,)show}!\n"

	/* str x y pf - p, f, pp, .. */
	"/pf{/Times-BoldItalic 16 selectfont 5 add M show}!\n"

	/* str x y sfz */
	"/sfz{	M -7 5 RM pop\n"
	"	/Times-Italic 14 selectfont(s)show\n"
	"	/Times-BoldItalic 16 selectfont(f)show\n"
	"	/Times-Italic 14 selectfont(z)show}!\n"

	/* x y coda - coda */
	"/coda{	1 SLW 2 add 2 copy M 0 20 RL\n"
	"	2 copy M -10 10 RM 20 0 RL stroke\n"
	"	10 add 6 0 360 arc 1.7 SLW stroke}!\n"

	/* x y sgno - segno */
	"/sgno{	M 0 3 RM currentpoint currentpoint currentpoint\n"
	"	1.5 -1.7 6.4 0.3 3 3.7 RC\n"
	"	-10.4 7.8 -8 10.6 -6.5 11.9 RC\n"
	"	4 1.9 5.9 -1.7 4.2 -2.6 RC\n"
	"	-1.3 -0.7 -2.9 1.3 -0.7 2 RC\n"
	"	-1.5 1.7 -6.4 -0.3 -3 -3.7 RC\n"
	"	10.4 -7.8 8 -10.6 6.5 -11.9 RC\n"
	"	-4 -1.9 -5.9 1.7 -4.2 2.6 RC\n"
	"	1.3 0.7 2.9 -1.3 0.7 -2 RC\n"
	"	fill\n"
	"	M 0.8 SLW -6 1.2 RM 12.6 12.6 RL stroke\n"
	"	7 add exch -6 add exch 1.2 0 360 arc fill\n"
	"	8 add exch 6 add exch 1.2 0 360 arc fill}!\n"

	/* w x y cresc - crescendo */
	"/cresc{	1 SLW M dup 5 RM\n"
	"	defl 1 and 0 eq\n"
	"	{dup neg 4 RL 4 RL}\n"
	"	{dup neg 2.2 RL 0 3.6 RM 2.2 RL}\n"
	"	ifelse stroke}!\n"

	/* w x y dim - diminuendo */
	"/dim{	1 SLW 5 add M\n"
	"	defl 2 and 0 eq\n"
	"	{dup 4 RL neg 4 RL}\n"
	"	{dup 2.2 RL 0 3.6 RM neg 2.2 RL}\n"
	"	ifelse stroke}!\n"

	/* x y dplus - plus */
	"/dplus{	1.2 SLW 0.5 add M 0 6 RL\n"
	"	-3 -3 RM 6 0 RL stroke}!\n"

	/* x y accent - accent */
	"/accent{1.2 SLW M -4 2 RM\n"
	"	8 2 RL -8 2 RL stroke}!\n"

	/* x y turn - turn */
	"/turn{	M 5.2 8 RM\n"
	"	1.4 -0.5 0.9 -4.8 -2.2 -2.8 RC\n"
	"	-4.8 3.5 RL\n"
	"	-3 2 -5.8 -1.8 -3.6 -4.4 RC\n"
	"	1 -1.1 2 -0.8 2.1 0.1 RC\n"
	"	0.1 0.9 -0.7 1.2 -1.9 0.6 RC\n"
	"	-1.4 0.5 -0.9 4.8 2.2 2.8 RC\n"
	"	4.8 -3.5 RL\n"
	"	3 -2 5.8 1.8 3.6 4.4 RC\n"
	"	-1 1.1 -2 0.8 -2.1 -0.1 RC\n"
	"	-0.1 -0.9 0.7 -1.2 1.9 -0.6 RC\n"
	"	fill}!\n"

	/* x y trnx - turn with line through it */
	"/turnx{	2 copy turn M\n"
	"	.6 SLW 0 1.5 RM 0 9 RL stroke}!\n"

	/* x y lphr - longphrase */
	"/lphr{1.2 SLW M 0 -18 RL stroke}!\n"

	/* x y mphr - mediumphrase */
	"/mphr{1.2 SLW M 0 -12 RL stroke}!\n"

	/* x y sphr - shortphrase */
	"/sphr{1.2 SLW M 0 -6 RL stroke}!\n"

	/* w x y ltr - long trill */
	"/ltr{	gsave 4 add T\n"
	"	0 6 3 -1 roll{\n"
	/*		% first loop draws left half of squiggle; second draws right\n*/
	"		0 1 1{\n"
	"			0 0.4 M\n"
	"			2 1.9 3.4 2.3 3.9 0 curveto\n"
	"			2.1 0 lineto\n"
	"			1.9 0.8 1.4 0.7 0 -0.4 curveto\n"
	"			fill\n"
	"			pop 180 rotate -6 0 T\n"
	"		}for\n"
	/*		% shift axes right one squiggle*/
	"		pop 6 0 T\n"
	"	}for\n"
	"	grestore}!\n"

	/* h x ylow arp - arpeggio */
	"/arp{gsave 90 rotate exch neg ltr grestore}!\n"

	/* x y wedge - wedge */
	"/wedge{1 add M -1.5 5 RL 3 0 RL -1.5 -5 RL fill}!\n"

	/* x y opend - 'open' sign */
	"/opend{dlw M currentpoint 3 add 2.5 -90 270 arc stroke}!\n"

	/* x y snap - 'snap' sign */
	"/snap{	dlw 2 copy M -3 6 RM\n"
	"	0 5 6 5 6 0 RC\n"
	"	0 -5 -6 -5 -6 0 RC\n"
	"	5 add M 0 -6 RL stroke}!\n"

	/* x y thumb - 'thumb' sign */
	"/thumb{	dlw 2 copy M -2.5 7 RM\n"
	"	0 6 5 6 5 0 RC\n"
	"	0 -6 -5 -6 -5 0 RC\n"
	"	2 add M 0 -4 RL stroke}!\n"

	/* y hl - helper line at height y */
	"/hl{	.8 SLW x -6 add exch M\n"
	"	12 0 RL stroke}!\n"
	/* y hl1 - longer helper line */
	"/hl1{	.8 SLW x -7 add exch M\n"
	"	14 0 RL stroke}!\n"
	/* y hl2 - more longer helper line */
	"/hl2{	.8 SLW x -8 add exch M\n"
	"	16 0 RL stroke}!\n"

	/* -- accidentals -- */
	/* x y sh0 - sharp sign */
	"/sh0{	gsave T .9 SLW\n"
	"	-1.2 -8.4 M 0 15.4 RL\n"
	"	1.4 -7.2 M 0 15.4 RL stroke\n"
	"	-2.6 -3 M 5.4 1.6 RL 0 -2.2 RL -5.4 -1.6 RL 0 2.2 RL fill\n"
	"	-2.6 3.4 M 5.4 1.6 RL 0 -2.2 RL -5.4 -1.6 RL 0 2.2 RL fill\n"
	"	grestore}!\n"
	/* x y ft0 - flat sign */
	"/ft0{	gsave T .8 SLW\n"
	"	-1.8 2.5 M\n"
	"	6.4 3.3 6.5 -3.6 0 -6.6 RC\n"
	"	4.6 3.9 4.5 7.6 0 5.7 RC\n"
	"	currentpoint fill M\n"
	"	0 7.1 RM 0 -12.6 RL stroke\n"
	"	grestore}!\n"
	/* x y nt0 - natural sign */
	"/nt0{	gsave T .5 SLW\n"
	"	-2 -4.3 M 0 12.2 RL\n"
	"	1.3 -7.8 M 0 12.2 RL stroke\n"
	"	2.1 SLW\n"
	"	-2 -2.9 M 3.3 0.6 RL\n"
	"	-2 2.4 M 3.3 0.6 RL stroke\n"
	"	grestore}!\n"
	/* x y dsh0 - double sharp */
	"/dsh0{	2 copy M .7 SLW\n"
	"	-2 -2 RM 4 4 RL\n"
	"	-4 0 RM 4 -4 RL stroke\n"
	"	.5 SLW 2 copy M 1.3 -1.3 RM\n"
	"	2 -0.2 RL 0.2 -2 RL -2 0.2 RL -0.2 2 RL fill\n"
	"	2 copy M 1.3 1.3 RM\n"
	"	2 0.2 RL 0.2 2 RL -2 -0.2 RL -0.2 -2 RL fill\n"
	"	2 copy M -1.3 1.3 RM\n"
	"	-2 0.2 RL -0.2 2 RL 2 -0.2 RL 0.2 -2 RL fill\n"
	"	M -1.3 -1.3 RM\n"
	"	-2 -0.2 RL -0.2 -2 RL 2 0.2 RL 0.2 2 RL fill}!\n"
	/* x y ftx - narrow flat sign */
	"/ftx{	-1.4 2.7 RM\n"
	"	5.7 3.1 5.7 -3.6 0 -6.7 RC\n"
	"	3.9 4 4 7.6 0 5.8 RC\n"
	"	currentpoint fill M\n"
	"	dlw 0 7.1 RM 0 -12.4 RL stroke}!\n"
	/* x y dft0 - double flat sign */
	"/dft0{2 copy M -2.5 0 RM ftx M 1.5 0 RM ftx}!\n"
	/* ancillary function for grace note accidentals */
	"/gsc{gsave y T .7 dup scale 0 0}!\n"

	/* some microtone accidentals */
	/* 1/4 ton sharp */
	"/sh1{	gsave T .9 SLW\n"
	"	0 -7.8 M 0 15.4 RL stroke\n"
	"	-1.8 -2.7 M 3.6 1.1 RL 0 -2.2 RL -3.6 -1.1 RL 0 2.2 RL fill\n"
	"	-1.8 3.7 M 3.6 1.1 RL 0 -2.2 RL -3.6 -1.1 RL 0 2.2 RL fill\n"
	"	grestore}!\n"
	/* 3/4 ton sharp */
	"/sh513{	gsave T .8 SLW\n"
	"	-2.5 -8.7 M 0 15.4 RL\n"
	"	0 -7.8 M 0 15.4 RL\n"
	"	2.5 -6.9 M 0 15.4 RL stroke\n"
	"	-3.7 -3.1 M 7.4 2.2 RL 0 -2.2 RL -7.4 -2.2 RL 0 2.2 RL fill\n"
	"	-3.7 3.2 M 7.4 2.2 RL 0 -2.2 RL -7.4 -2.2 RL 0 2.2 RL fill\n"
	"	grestore}!\n"
	/* 1/4 ton flat */
	"/ft1{gsave -1 1 scale exch neg exch ft0 grestore}!\n"
	/* 3/4 ton flat */
	"/ft513{2 copy gsave -1 1 scale exch neg 3 add exch M ftx grestore\n"
	"	M 1.5 0 RM ftx}!\n"

#if 1
	/* accidentals in strings */
	"/accfont{\n"
	"	/CharStrings CharStrings dup length 3 add dict copy def\n"
	"	FontMatrix 0 get 1 eq{\n"
	"	 CharStrings /sharpchar{pop\n"
	"		.650 0 0 -.050 .650 .750 setcachedevice\n"
	"		.050 dup scale 5.8 7 sh0}bind put\n"
	"	 CharStrings /flatchar{pop\n"
	"		.600 0 0 0 .600 .750 setcachedevice\n"
	"		.050 dup scale 5.8 6 ft0}bind put\n"
	"	 CharStrings /natchar{pop\n"
	"		.600 0 0 0 .600 .750 setcachedevice\n"
	"		.050 dup scale 5.8 7 nt0}bind put\n"
	"	}{\n"
	"	 CharStrings /sharpchar{pop\n"
	"		650 0 0 -50 650 750 setcachedevice\n"
	"		50 dup scale 5.8 7 sh0}bind put\n"
	"	 CharStrings /flatchar{pop\n"
	"		600 0 0 0 600 750 setcachedevice\n"
	"		50 dup scale 5.8 6 ft0}bind put\n"
	"	 CharStrings /natchar{pop\n"
	"		600 0 0 0 600 750 setcachedevice\n"
	"		50 dup scale 5.8 7 nt0}bind put\n"
	"	 }ifelse\n"
	/*if RoPS and Font Type 3, change BuildChar*/
	"	product(RoPS)eq FontType 3 eq and{\n"
	"		/TTBuildChar /BuildChar load def\n"
	"		/BuildChar{1 index begin\n"
	"			dup Encoding exch get\n"
	"			CharStrings exch get\n"
	"			end\n"
	"			dup type/integertype eq{\n"
	"				pop 1 index/TTBuildChar get exec\n"
	"			}{\n"
	"				exec pop\n"
	"			}ifelse\n"
	"		}bind def\n"
	"	}if\n"
	"	}!\n"
#else
	/* accidentals in guitar chord */
	"/sharp_glyph{\n"
	"	fh .4 mul 0 RM currentpoint\n"
	"	gsave T fh .08 mul dup scale 0 7 sh0 grestore\n"
	"	fh .4 mul 0 RM}!\n"
	"/flat_glyph{\n"
	"	fh .4 mul 0 RM currentpoint\n"
	"	gsave T fh .08 mul dup scale 0 5 ft0 grestore\n"
	"	fh .4 mul 0 RM}!\n"
	"/nat_glyph{\n"
	"	fh .4 mul 0 RM currentpoint\n"
	"	gsave T fh .08 mul dup scale 0 7 nt0 grestore\n"
	"	fh .4 mul 0 RM}!\n"
#endif
	/* str gcshow - guitar chord */
	"/gcshow{show}!\n"
	/* x y w h box - draw a box */
	"/box{.6 SLW rectstroke}!\n"
	/* str w gxshow - expand a guitar chord */
	"/find{search{pop 3 -1 roll 1 add 3 1 roll}{pop exit}ifelse}!\n"
	"/gxshow{1 index stringwidth pop\n"
	"	sub 0 2 index(	){find}loop div\n"
	"	0 9 4 -1 roll widthshow}!\n"

	/* str anshow - annotation */
	"/anshow{show}!\n"

	/* -- lyrics under notes -- */
	/* l x y wln - underscore line */
	"/wln{M .8 SLW 0 RL stroke}!\n"
	/* l x y hyph - hyphen */
	"/hyph{	.8 SLW 3 add M\n"
	"	dup cvi 40 idiv 1 add 1 index exch div\n"	/* l dx */
	"	dup 3 sub 3 1 roll\n"		/* (dx-3) l dx */
	"	dup .5 mul dup 1.5 sub 0 RM\n"	/* (dx-3) l dx (dx/2) */
	"	exch 3 -1 roll\n"		/* (dx-3) (dx/2) dx l */
	"	{pop 3 0 RL dup 0 RM}for stroke pop}!\n"
	/* str lyshow - lyrics */
	"/lyshow{show}!\n"

	/* -- bars -- */
	/* h x y bar - thin bar */
	"/bar{M dlw 0 exch RL stroke}!\n"
	/* h x y dotbar - dotted bar */
	"/dotbar{[5] 0 setdash bar [] 0 setdash}!\n"
	/* h x y thbar - thick bar */
	"/thbar{3 -1 roll 3 exch rectfill}!\n"
	/* x y rdots - repeat dots */
	"/rdots{	2 copy 9 add M currentpoint 1.2 0 360 arc\n"
	"	15 add M currentpoint 1.2 0 360 arc fill}!\n"

	/* x y csig - C timesig */
	"/csig{	M\n"
	"	1 17.3 RM\n"
	"	0.9 0 2.3 -0.7 2.4 -2.2 RC\n"
	"	-1.2 2 -3.6 -0.1 -1.6 -1.7 RC\n"
	"	2 -1 3.8 3.5 -0.8 4.7 RC\n"
	"	-2 0.4 -6.4 -1.3 -5.8 -7 RC\n"
	"	0.4 -6.4 7.9 -6.8 9.1 -0.7 RC\n"
	"	-2.3 -5.6 -6.7 -5.1 -6.8 0 RC\n"
	"	-0.5 4.4 0.7 7.5 3.5 6.9 RC\n"
	"	fill}!\n"

	/* x y ctsig - C| timesig */
	"/ctsig{dlw 2 copy csig 4 add M 0 16 RL stroke}!\n"

	/* (top) (bot) x y tsig - time signature */
	"/tsig{	M gsave/Times-Bold 16 selectfont 1.2 1 scale\n"
	"	0 1 RM currentpoint 3 -1 roll showc\n"
	"	12 add M showc grestore}!\n"

	/* (meter) x y stsig - single time signature */
	"/stsig{	M gsave/Times-Bold 18 selectfont 1.2 1 scale\n"
	"	0 7 RM showc grestore}!\n"

	/* width n x y staff - staff with n lines*/
	"/staff{	dlw M{dup 0 RL dup neg 6 RM}repeat\n"
	"	pop stroke}!\n"

	/* l x sep0 - hline separator */
	"/sep0{dlw 0 M 0 RL stroke}!\n"

	"/hbrce{	-2.5 1 RM\n"
	"	-4.5 -4.6 -7.5 -12.2 -4.4 -26.8 RC\n"
	"	3.5 -14.3 3.2 -21.7 -2.1 -24.2 RC\n"
	"	7.4 2.4 7.3 14.2 3.5 29.5 RC\n"
	"	-2.7 9.5 -1.5 16.2 3 21.5 RC\n"
	"	fill}!\n"
	/* h x y brace */
	"/brace{	gsave T 0 0 M .01 mul 1 exch scale hbrce\n"
	"	0 -100 M 1 -1 scale hbrce grestore}!\n"

	/* h x y bracket */
	"/bracket{M dlw -5 2 RM currentpoint\n"
	"	-1.7 2 RM 10.5 -1 12 4.5 12 3.5 RC\n"
	"	0 -1 -3.5 -5.5 -8.5 -5.5 RC fill\n"
	"	3 SLW M 0 2 RM\n"
	"	0 exch neg -8 add RL currentpoint stroke\n"
	"	dlw M -1.7 0 RM\n"
	"	10.5 1 12 -4.5 12 -3.5 RC\n"
	"	0 1 -3.5 5.5 -8.5 5.5 RC fill}!\n"

	/* nb_measures x y mrest */
	"/mrest{	gsave T 1 SLW\n"
	"	-20 6 M 0 12 RL 20 6 M 0 12 RL stroke\n"
	"	5 SLW -20 12 M 40 0 RL stroke\n"
	"	/Times-Bold 15 selectfont 0 28 M showc grestore}!\n"

	/* x y mrep - measure repeat */
	"/mrep{	2 copy 2 copy\n"
	"	M -5 16 RM currentpoint 1.4 0 360 arc\n"
	"	M 5 8 RM currentpoint 1.4 0 360 arc\n"
	"	M -7 6 RM 11 12 RL 3 0 RL -11 -12 RL -3 0 RL\n"
	"	fill}!\n"

	/* x y mrep2 - measure repeat 2 times */
	"/mrep2{	2 copy 2 copy\n"
	"	M -5 18 RM currentpoint 1.4 0 360 arc\n"
	"	M 5 6 RM currentpoint 1.4 0 360 arc fill\n"
	"	M 1.8 SLW\n"
	"	-7 4 RM 14 10 RL -14 -4 RM 14 10 RL\n"
	"	stroke}!\n"

	/* str dy bracket_type dx x y repbra - repeat bracket */
	"/repbra{gsave dlw T 0 -20 M\n"
	"	0 20 3 index 1 ne{RL}{RM}ifelse 0 RL 0 ne{0 -20 RL}if stroke\n"
	"	4 exch M show grestore}!\n"

	/* pp2x pp1x p1 pp1 pp2 p2 p1 SL - slur / tie */
	"/SL{M curveto RL curveto closepath fill}!\n"

	/* pp2x pp1x p1 pp1 pp2 p2 p1 dSL - dotted slur / tie */
	"/dSL{	M [4] 0 setdash .8 SLW\n"
	"	curveto stroke [] 0 setdash\n"
	"	pop pop pop pop pop pop pop pop}!\n"

	/* -- text -- */
	"/strw{stringwidth pop w add/w exch def}!\n"
	"/jshow{w 0 32 4 -1 roll widthshow}!\n"

	/* texts which may be translated */
	"/wbook{0 0 M(Book:)show}!\n"
	"/wdisco{0 0 M(Discography:)show}!\n"
	"/whisto{0 0 M(History:)show}!\n"
	"/wnotes{0 0 M(Notes:)show}!\n"
	"/wrhythm{0 0 M(Rhythm:)show}!\n"
	"/wsource{0 0 M(Source:)show}!\n"
	"/wtrans{0 0 M(Transcription:)show}!\n"

	/* -- note heads -- */
	/* x y hd - full head */
	"/hd{	xymove\n"
	"	3.5 2 RM\n"
	"	-2 3.5 -9 -0.5 -7 -4 RC\n"
	"	2 -3.5 9 0.5 7 4 RC fill}!\n"
	/* x y Hd - open head for half */
	"/Hd{	xymove\n"
	"	3 1.6 RM\n"
	"	-1 1.8 -7 -1.4 -6 -3.2 RC\n"
	"	1 -1.8 7 1.4 6 3.2 RC\n"
	"	0.5 0.3 RM\n"
	"	2 -3.8 -5 -7.6 -7 -3.8 RC\n"
	"	-2 3.8 5 7.6 7 3.8 RC\n"
	"	fill}!\n"
	/* x y HD - open head for whole */
	"/HD{	xymove\n"
	"	-1.6 2.4 RM\n"
	"	2.8 1.6 6 -3.2 3.2 -4.8 RC\n"
	"	-2.8 -1.6 -6 3.2 -3.2 4.8 RC\n"
	"	7.2 -2.4 RM\n"
	"	0 1.8 -2.2 3.2 -5.6 3.2 RC\n"
	"	-3.4 0 -5.6 -1.4 -5.6 -3.2 RC\n"
	"	0 -1.8 2.2 -3.2 5.6 -3.2 RC\n"
	"	3.4 0 5.6 1.4 5.6 3.2 RC\n"
	"	fill}!\n"
	/* x y HDD - round breve */
	"/HDD{	dlw HD\n"
	"	x y M -6 -4 RM 0 8 RL\n"
	"	12 0 RM 0 -8 RL stroke}!\n"
	/* x y breve - square breve */
	"/breve{	xymove\n"
	"	2.5 SLW -6 -2.7 RM 12 0 RL\n"
	"	0 5.4 RM -12 0 RL stroke\n"
	"	dlw x y M -6 -5 RM 0 10 RL\n"
	"	12 0 RM 0 -10 RL stroke}!\n"
	/* x y longa */
	"/longa{	xymove\n"
	"	2.5 SLW -6 -2.7 RM 12 0 RL\n"
	"	0 5.4 RM -12 0 RL stroke\n"
	"	dlw x y M -6 -5 RM 0 10 RL\n"
	"	12 0 RM 0 -16 RL stroke}!\n"

	/* -- default percussion heads -- */
	/* x y pshhd - percussion sharp head */
	"/pshhd{/x 2 index def/y 1 index def dsh0}!\n"
	/* x y pfthd - percussion flat head */
	"/pfthd{/x 2 index def/y 1 index def dsh0\n"
	"	.7 SLW x y 4 0 360 arc stroke}!\n"

	/* x y ghd - grace note head */
	"/ghd{	xymove\n"
	"	1.7 1 RM\n"
	"	-1 1.7 -4.5 -0.2 -3.4 -2 RC\n"
	"	1 -1.7 4.5 0.2 3.4 2 RC fill}!\n"

	/* dx dy gua / gda - acciaccatura */
	"/gua{x y M -1 4 RM RL stroke}!\n"
	"/gda{x y M -5 -4 RM RL stroke}!\n"

	/* y ghl - grace note helper line */
	"/ghl{	.6 SLW x -3 add exch M\n"
	"	6 0 RL stroke}!\n"

	/* x1 y2 x2 y2 x3 y3 x0 y0 gsl - grace note slur */
	"/gsl{dlw M curveto stroke}!\n"

	/* tin whistle */
	"/tw_head{/Helvetica 8 selectfont\n"
	"	0 -45 M 90 rotate(WHISTLE)show -90 rotate\n"
	"	/Helvetica-Bold 36 selectfont\n"
	"	0 -45 M show .5 SLW newpath}!\n"
	"/tw_under{\n"
	"	1 index 2.5 sub -4 M 2.5 -2.5 RL 2.5 2.5 RL\n"
	"	-2.5 -2.5 RM 0 6 RL stroke}!\n"
	"/tw_over{\n"
	"	1 index 2.5 sub -3 M 2.5 2.5 RL 2.5 -2.5 RL\n"
	"	-2.5 2.5 RM 0 -6 RL stroke}!\n"
	"/tw_0{7 sub 2 copy 3.5 sub 3 0 360 arc stroke}!\n"
	"/tw_1{7 sub 2 copy 3.5 sub 2 copy 3 90 270 arc fill 3 270 90 arc stroke}!\n"
	"/tw_2{7 sub 2 copy 3.5 sub 3 0 360 arc fill}!\n"
	"/tw_p{pop -55 M 0 6 RL -3 -3 RM 6 0 RL stroke}!\n"
	"/tw_pp{	pop 3 sub -53.5 M 6 0 RL\n"
	"	-1.5 -1.5 RM 0 3 RL\n"
	"	-3 0 RM 0 -3 RL stroke}!\n"

	/* x y showerror */
	"/showerror{	gsave 1 0.7 0.7 setrgbcolor 2.5 SLW 2 copy newpath\n"
	"	30 0 360 arc stroke grestore}!\n"

	"0 setlinecap 0 setlinejoin\n";

static char *enc_tb[MAXENC] = {
	/* 1 */
	"/space/exclamdown/cent/sterling/currency/yen/brokenbar/section\n"
	"/dieresis/copyright/ordfeminine/guillemotleft/logicalnot/hyphen/registered/macron\n"
/*	"/degree/plusminus/twosuperior/threesuperior/acute/mu/paragraph/bullet\n" */
	"/degree/plusminus/twosuperior/threesuperior/acute/mu/paragraph/periodcentered\n"
/*	"/cedilla/dotlessi/ordmasculine/guillemotright/onequarter/onehalf/threequarters/questiondown\n" */
	"/cedilla/onesuperior/ordmasculine/guillemotright/onequarter/onehalf/threequarters/questiondown\n"
	/* (300) */
	"/Agrave/Aacute/Acircumflex/Atilde/Adieresis/Aring/AE/Ccedilla\n"
	"/Egrave/Eacute/Ecircumflex/Edieresis/Igrave/Iacute/Icircumflex/Idieresis\n"
	"/Eth/Ntilde/Ograve/Oacute/Ocircumflex/Otilde/Odieresis/multiply\n"
	"/Oslash/Ugrave/Uacute/Ucircumflex/Udieresis/Yacute/Thorn/germandbls\n"
	"/agrave/aacute/acircumflex/atilde/adieresis/aring/ae/ccedilla\n"
	"/egrave/eacute/ecircumflex/edieresis/igrave/iacute/icircumflex/idieresis\n"
	"/eth/ntilde/ograve/oacute/ocircumflex/otilde/odieresis/divide\n"
	"/oslash/ugrave/uacute/ucircumflex/udieresis/yacute/thorn/ydieresis",
	/* 2 */
	"/space/Aogonek/breve/Lslash/currency/Lcaron/Sacute/section\n"
	"/dieresis/Scaron/Scedilla/Tcaron/Zacute/hyphen/Zcaron/Zdotaccent\n"
	"/degree/aogonek/ogonek/lslash/acute/lcaron/sacute/caron\n"
	"/cedilla/scaron/scedilla/tcaron/zacute/hungarumlaut/zcaron/zdotaccent\n"
	/* (300) */
	"/Racute/Aacute/Acircumflex/Abreve/Adieresis/Lacute/Cacute/Ccedilla\n"
	"/Ccaron/Eacute/Eogonek/Edieresis/Ecaron/Iacute/Icircumflex/Dcaron\n"
	"/Dbar/Nacute/Ncaron/Oacute/Ocircumflex/Ohungarumlaut/Odieresis/multiply\n"
	"/Rcaron/Uring/Uacute/Uhungarumlaut/Udieresis/Yacute/Tcedilla/germandbls\n"
	"/racute/aacute/acircumflex/abreve/adieresis/lacute/cacute/ccedilla\n"
	"/ccaron/eacute/eogonek/edieresis/ecaron/iacute/icircumflex/dcaron\n"
	"/dbar/nacute/ncaron/oacute/ocircumflex/ohungarumlaut/odieresis/divide\n"
	"/rcaron/uring/uacute/uhungarumlaut/udieresis/yacute/tcedilla/dotaccent",
	/* 3 */
	"/space/Hstroke/breve/sterling/currency/yen/Hcircumflex/section\n"
	"/dieresis/Idotaccent/Scedilla/Gbreve/Jcircumflex/hyphen/registered/Zdotaccent\n"
	"/degree/hstroke/twosuperior/threesuperior/acute/mu/hcircumflex/bullet\n"
	"/cedilla/dotlessi/scedilla/gbreve/jcircumflex/onehalf/threequarters/zdotaccent\n"
	/* (300) */
	"/Agrave/Aacute/Acircumflex/Atilde/Adieresis/Cdotaccent/Ccircumflex/Ccedilla\n"
	"/Egrave/Eacute/Ecircumflex/Edieresis/Igrave/Iacute/Icircumflex/Idieresis\n"
	"/Eth/Ntilde/Ograve/Oacute/Ocircumflex/Gdotaccent/Odieresis/multiply\n"
	"/Gcircumflex/Ugrave/Uacute/Ucircumflex/Udieresis/Ubreve/Scircumflex/germandbls\n"
	"/agrave/aacute/acircumflex/atilde/adieresis/cdotaccent/ccircumflex/ccedilla\n"
	"/egrave/eacute/ecircumflex/edieresis/igrave/iacute/icircumflex/idieresis\n"
	"/eth/ntilde/ograve/oacute/ocircumflex/gdotaccent/odieresis/divide\n"
	"/gcircumflex/ugrave/uacute/ucircumflex/udieresis/ubreve/scircumflex/dotaccent",
	/* 4 */
	"/space/Aogonek/kra/Rcedilla/currency/Itilde/Lcedilla/section\n"
	"/dieresis/Scaron/Emacron/Gcedilla/Tbar/hyphen/Zcaron/macron\n"
	"/degree/aogonek/ogonek/rcedilla/acute/itilde/lcedilla/caron\n"
	"/cedilla/scaron/emacron/gcedilla/tbar/Eng/zcaron/eng\n"
	/* (300) */
	"/Amacron/Aacute/Acircumflex/Atilde/Adieresis/Aring/AE/Iogonek\n"
	"/Ccaron/Eacute/Eogonek/Edieresis/Edotaccent/Iacute/Icircumflex/Imacron\n"
	"/Eth/Ncedilla/Omacron/Kcedilla/Ocircumflex/Otilde/Odieresis/multiply\n"
	"/Oslash/Uogonek/Uacute/Ucircumflex/Udieresis/Utilde/Umacron/germandbls\n"
	"/amacron/aacute/acircumflex/atilde/adieresis/aring/ae/iogonek\n"
	"/ccaron/eacute/eogonek/edieresis/edotaccent/iacute/icircumflex/imacron\n"
	"/dbar/ncedilla/omacron/kcedilla/ocircumflex/otilde/odieresis/divide\n"
	"/oslash/uogonek/uacute/ucircumflex/udieresis/utilde/umacron/dotaccent",
	/* 5 */
	"/space/exclamdown/cent/sterling/currency/yen/brokenbar/section\n"
	"/dieresis/copyright/ordfeminine/guillemotleft/logicalnot/hyphen/registered/macron\n"
	"/degree/plusminus/twosuperior/threesuperior/acute/mu/paragraph/bullet\n"
	"/cedilla/dotlessi/ordmasculine/guillemotright/onequarter/onehalf/threequarters/questiondown\n"
	/* (300) */
	"/Agrave/Aacute/Acircumflex/Atilde/Adieresis/Aring/AE/Ccedilla\n"
	"/Egrave/Eacute/Ecircumflex/Edieresis/Igrave/Iacute/Icircumflex/Idieresis\n"
	"/Gbreve/Ntilde/Ograve/Oacute/Ocircumflex/Otilde/Odieresis/multiply\n"
	"/Oslash/Ugrave/Uacute/Ucircumflex/Udieresis/Idotaccent/Scedilla/germandbls\n"
	"/agrave/aacute/acircumflex/atilde/adieresis/aring/ae/ccedilla\n"
	"/egrave/eacute/ecircumflex/edieresis/igrave/iacute/icircumflex/idieresis\n"
	"/gbreve/ntilde/ograve/oacute/ocircumflex/otilde/odieresis/divide\n"
	"/oslash/ugrave/uacute/ucircumflex/udieresis/dotlessi/scedilla/ydieresis",
	/* 6 */
	"/space/Aogonek/Emacron/Gcedilla/Imacron/Itilde/Kcedilla/Lcedilla\n"
	"/acute/Rcedilla/Scaron/Tbar/Zcaron/hyphen/kra/Eng\n"
	"/dbar/aogonek/emacron/gcedilla/imacron/itilde/kcedilla/lcedilla\n"
	"/nacute/rcedilla/scaron/tbar/zcaron/section/germandbls/eng\n"
	/* (300) */
	"/Amacron/Aacute/Acircumflex/Atilde/Adieresis/Aring/AE/Iogonek\n"
	"/Ccaron/Eacute/Eogonek/Edieresis/Edotaccent/Iacute/Icircumflex/Idieresis\n"
	"/Dbar/Ncedilla/Omacron/Oacute/Ocircumflex/Otilde/Odieresis/Utilde\n"
	"/Oslash/Uogonek/Uacute/Ucircumflex/Udieresis/Yacute/Thorn/Umacron\n"
	"/amacron/aacute/acircumflex/atilde/adieresis/aring/ae/iogonek\n"
	"/ccaron/eacute/eogonek/edieresis/edotaccent/iacute/icircumflex/idieresis\n"
	"/eth/ncedilla/omacron/oacute/ocircumflex/otilde/odieresis/utilde\n"
	"/oslash/uogonek/uacute/ucircumflex/udieresis/yacute/thorn/umacron"
};

/* -- define an encoding -- */
void define_encoding(int enc,		/* index */
		     char *ename)	/* name */
{
	char enc_txt[30];

	if (enc == ENC_NATIVE)
		return;
	if (enc < ENC_NATIVE) {
		sprintf(enc_txt, "ISOLatin%dEncoding", enc);
		ename = enc_txt;
		if (enc > 0)
		    fprintf(fout, "/%s [\n"
			"StandardEncoding 0 45 getinterval aload pop\n"
			"/minus\n"
			"StandardEncoding 46 82 getinterval aload pop\n"
			/* (200) */
			"/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef\n"
			"/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef\n"
			"/dotlessi/grave/acute/circumflex/tilde/macron/breve/dotaccent\n"
			"/dieresis/.notdef/ring/cedilla/.notdef/hungarumlaut/ogonek/caron\n"
			"%s\n"
			"] def\n",
			ename, enc_tb[enc - 1]);
		else fprintf(fout,
			"/%s ISOLatin1Encoding dup length array copy def\n",
			ename);
		fprintf(fout,
			"%s dup 8#201 /sharpchar put\n"
			"dup 8#202 /flatchar put\n"
			"8#203 /natchar put\n",
			ename);
	}
	fprintf(fout, "/mkfontext%d{\n"
		"	findfont dup length\n"
		"	 product(RoPS)eq{1 add}if\n"
		"	 dict begin\n"
		"		{1 index/FID ne{def}{pop pop}ifelse}forall\n",
		enc);
	fprintf(fout,
		"		/Encoding %s def\n",
		ename);
	if (enc < ENC_NATIVE)
		fprintf(fout,
			"		accfont\n");
	fprintf(fout,
		"		currentdict\n"
		"	end\n"
		"	definefont pop}!\n");
}

/* -- define_font -- */
void define_font(char name[],
		 int num,
		 int enc)
{
	if (enc == ENC_NATIVE || strcmp(name, "Symbol") == 0) {
		fprintf(fout, "/F%d{dup .8 mul/fh exch def"
			"/%s exch selectfont}!\n",
			num, name);
		return;
	}
	fprintf(fout, "/%s%d/%s mkfontext%d\n"
		"/F%d{dup .8 mul/fh exch def/%s%d exch selectfont}!\n",
		name, enc, name, enc, num, name, enc);
}

/* -- output the symbol definitions -- */
void define_symbols(void)
{
	fputs(ps_head, fout);

	/* len su - up stem */
	fprintf(fout, "/su{dlw x y M %.1f %.1f RM %.1f sub 0 exch RL stroke}!\n",
		STEM_XOFF, STEM_YOFF, STEM_YOFF);

	/* len sd - down stem */
	fprintf(fout, "/sd{dlw x y M %.1f %.1f RM %.1f add 0 exch RL stroke}!\n",
		-STEM_XOFF, -STEM_YOFF, STEM_YOFF);

	/* n len sfu - stem and n flags up */
	fprintf(fout, "/sfu{	dlw x y M %.1f %.1f RM\n"
		"	%.1f sub 0 exch RL currentpoint stroke\n"
		"	M dup 1 eq{\n"
		"		pop\n"
		"		0.6 -5.6 9.6 -9 5.6 -18.4 RC\n"
		"		1.6 6 -1.3 11.6 -5.6 12.8 RC\n"
		"		fill\n"
		"	  }{\n"
		"		1 sub{	currentpoint\n"
		"			0.9 -3.7 9.1 -6.4 6 -12.4 RC\n"
		"			1 5.4 -4.2 8.4 -6 8.4 RC\n"
		"			fill -5.4 add M\n"
		"		}repeat\n"
		"		1.2 -3.2 9.6 -5.7 5.6 -14.6 RC\n"
		"		1.6 5.4 -1 10.2 -5.6 11.4 RC\n"
		"		fill\n"
		"	  }ifelse}!\n",
		STEM_XOFF, STEM_YOFF, STEM_YOFF);

	/* n len sfd - stem and n flags down */
	fprintf(fout, "/sfd{	dlw x y M %.1f %.1f RM\n"
		"	%.1f add 0 exch RL currentpoint stroke\n"
		"	M dup 1 eq{\n"
		"		pop\n"
		"		0.6 5.6 9.6 9 5.6 18.4 RC\n"
		"		1.6 -6 -1.3 -11.6 -5.6 -12.8 RC\n"
		"		fill\n"
		"	  }{\n"
		"		1 sub{	currentpoint\n"
		"			0.9 3.7 9.1 6.4 6 12.4 RC\n"
		"			1 -5.4 -4.2 -8.4 -6 -8.4 RC\n"
		"			fill 5.4 add M\n"
		"		}repeat\n"
		"		1.2 3.2 9.6 5.7 5.6 14.6 RC\n"
		"		1.6 -5.4 -1 -10.2 -5.6 -11.4 RC\n"
		"		fill\n"
		"	  }ifelse}!\n",
		-STEM_XOFF, -STEM_YOFF, STEM_YOFF);

	/* n len sfs - stem and n straight flag down */
	fprintf(fout, "/sfs{	dup 0 lt{\n"
		"		dlw x y M -%.1f -%.1f RM\n"
		"		%.1f add 0 exch RL currentpoint stroke\n"
		"		M{	currentpoint\n"
		"			7 %.1f RL\n"
		"			0 %.1f RL\n"
		"			-7 -%.1f RL\n"
		"			fill 5.4 add M\n"
		"		}repeat\n"
		"	}{\n"
		"		dlw x y M %.1f %.1f RM\n"
		"		-%.1f add 0 exch RL currentpoint stroke\n"
		"		M{	currentpoint\n"
		"			7 -%.1f RL\n"
		"			0 -%.1f RL\n"
		"			-7 %.1f RL\n"
		"			fill -5.4 add M\n"
		"		}repeat\n"
		"	}ifelse}!\n",
		STEM_XOFF, STEM_YOFF, STEM_YOFF,
		BEAM_DEPTH, BEAM_DEPTH, BEAM_DEPTH,
		STEM_XOFF, STEM_YOFF, STEM_YOFF,
		BEAM_DEPTH, BEAM_DEPTH, BEAM_DEPTH);

	/* len gu - grace note stem up */
	fprintf(fout, "/gu{	.6 SLW x y M\n"
		"	%.1f 0 RM 0 exch RL stroke}!\n"

	/* len gd - grace note stem down */
		"/gd{	.6 SLW x y M\n"
		"	%.1f 0 RM 0 exch RL stroke}!\n",
		GSTEM_XOFF, -GSTEM_XOFF);

	/* n len sgu - gnote stem and n flag up */
	fprintf(fout, "/sgu{	.6 SLW x y M %.1f 0 RM\n"
		"	0 exch RL currentpoint stroke\n"
		"	M dup 1 eq{\n"
		"		pop\n"
		"		0.6 -3.4 5.6 -3.8 3 -10 RC\n"
		"		1.2 4.4 -1.4 7 -3 7 RC\n"
		"		fill\n"
		"	  }{\n"
		"		{	currentpoint\n"
		"			1 -3.2 5.6 -2.8 3.2 -8 RC\n"
		"			1.4 4.8 -2.4 5.4 -3.2 5.2 RC\n"
		"			fill -3.5 add M\n"
		"		}repeat\n"
		"	  }\n"
		"	ifelse}!\n",
		GSTEM_XOFF);

	/* n len sgd - gnote stem and n flag down */
	fprintf(fout, "/sgd{	.6 SLW x y M %.1f 0 RM\n"
		"	0 exch RL currentpoint stroke\n"
		"	M dup 1 eq{\n"
		"		pop\n"
		"		0.6 3.4 5.6 3.8 3 10 RC\n"
		"		1.2 -4.4 -1.4 -7 -3 -7 RC\n"
		"		fill\n"
		"	  }{\n"
		"		{	currentpoint\n"
		"			1 3.2 5.6 2.8 3.2 8 RC\n"
		"			1.4 -4.8 -2.4 -5.4 -3.2 -5.2 RC\n"
		"			fill 3.5 add M\n"
		"		}repeat\n"
		"	  }\n"
		"	ifelse}!\n",
		-GSTEM_XOFF);

	/* n len sgs - gnote stem and n straight flag up */
	fprintf(fout, "/sgs{	.6 SLW x y M %.1f 0 RM\n"
		"	0 exch RL currentpoint stroke\n"
		"	M{	currentpoint\n"
		"		3 -1.5 RL 0 -2 RL -3 1.5 RL\n"
		"		closepath fill -3 add M\n"
		"	}repeat}!\n",
		GSTEM_XOFF);
}
