/*
 * Low-level utilities.
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

//fixme: more work to do
//#define HEX_USER

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#ifdef HAVE_PANGO
#include <pango/pangocairo.h>
#include <pango/pangofc-font.h>
//fixme: needed for FT_Has_PS_Glyph_Names
//#include <freetype/t1tables.h>
#endif

#include "abc2ps.h" 

char tex_buf[TEX_BUF_SZ];	/* result of tex_str() */
int outft = -1;			/* last font in the output file */

static char *strop;		/* current string output operation */
static float strlw;		/* line width */
static float strtw = -1;	/* current text width */
static int strns;		/* number of spaces (justify) */
static int curft;		/* current (wanted) font */
static int defft;		/* default font */
static int strtx;		/* PostScript text outputing */

/* width of characters according to the encoding */
/* these are the widths for Times-Roman, extracted from the 'a2ps' package */
/*fixme: does not work with utf-8*/
static short cw_tb[] = {
	  0,  0,  0,  0,  0,  0,  0,  0,
	  0,  0,  0,  0,  0,  0,  0,  0,
	  0,  0,  0,  0,  0,  0,  0,  0,
	  0,  0,  0,  0,  0,  0,  0,  0,
	250,333,408,500,500,833,778,333,
	333,333,500,564,250,564,250,278,
	500,500,500,500,500,500,500,500,
	500,500,278,278,564,564,564,444,
	921,722,667,667,722,611,556,722,
	722,333,389,722,611,889,722,722,
	556,722,667,556,611,722,722,944,
	722,722,611,333,278,333,469,500,
	333,444,500,444,500,444,333,500,
	500,278,278,500,278,778,500,500,
	500,500,333,389,278,500,500,722,
	500,500,444,480,200,480,541,  0,
	  0,  0,  0,  0,  0,  0,  0,  0,
	  0,  0,  0,  0,  0,  0,  0,  0,
	  0,  0,  0,  0,  0,  0,  0,  0,
	  0,  0,  0,  0,  0,  0,  0,  0,
	250,333,500,500,500,500,200,500,
	333,760,276,500,564,333,760,333,
	400,564,300,300,333,500,453,350,
	333,278,310,500,750,750,750,444,
	722,722,722,722,722,722,889,667,
	611,611,611,611,333,333,333,333,
	722,722,722,722,722,722,722,564,
	722,722,722,722,722,722,556,500,
	444,444,444,444,444,444,667,444,
	444,444,444,444,278,278,278,278,
	500,500,500,500,500,500,500,564,
	500,500,500,500,500,500,500,500,
};

/* translation of tex sequences */
/* first 2 characters: input sequence
 * next 2 characters: UTF-8 sequence */
static char tex_tb[][4] = {
/* translation table from the IETF rfc1345 */
	{'N', 'S',	0xc2, 0xa0},
	{'!', 'I',	0xc2, 0xa1},
	{'C', 't',	0xc2, 0xa2},
	{'P', 'd',	0xc2, 0xa3},
	{'C', 'u',	0xc2, 0xa4},
	{'Y', 'e',	0xc2, 0xa5},
	{'B', 'B',	0xc2, 0xa6},
	{'S', 'E',	0xc2, 0xa7},
	{'\'', ':',	0xc2, 0xa8},
	{'C', 'o',	0xc2, 0xa9},
	{'-', 'a',	0xc2, 0xaa},
	{'<', '<',	0xc2, 0xab},
	{'N', 'O',	0xc2, 0xac},
	{'-', '-',	0xc2, 0xad},
	{'R', 'g',	0xc2, 0xae},
	{'\'', 'm',	0xc2, 0xaf},
	{'D', 'G',	0xc2, 0xb0},
	{'+', '-',	0xc2, 0xb1},
	{'2', 'S',	0xc2, 0xb2},
	{'3', 'S',	0xc2, 0xb3},
	{'\'', '\'',	0xc2, 0xb4},
	{'M', 'y',	0xc2, 0xb5},
	{'P', 'I',	0xc2, 0xb6},
	{'.', 'M',	0xc2, 0xb7},
	{'\'', ',',	0xc2, 0xb8},
	{'1', 'S',	0xc2, 0xb9},
	{'-', 'o',	0xc2, 0xba},
	{'>', '>',	0xc2, 0xbb},
	{'1', '4',	0xc2, 0xbc},
	{'1', '2',	0xc2, 0xbd},
	{'3', '4',	0xc2, 0xbe},
//	{'?', 'I',	0xc2, 0xbf},	/*fixme: questiondown - conflict with 'I?' */
	{'A', '!',	0xc3, 0x80},
	{'A', '\'',	0xc3, 0x81},
	{'A', '>',	0xc3, 0x82},
	{'A', '?',	0xc3, 0x83},
	{'A', ':',	0xc3, 0x84},
	{'A', 'A',	0xc3, 0x85},
	{'A', 'E',	0xc3, 0x86},
	{'C', ',',	0xc3, 0x87},
	{'E', '!',	0xc3, 0x88},
	{'E', '\'',	0xc3, 0x89},
	{'E', '>',	0xc3, 0x8a},
	{'E', ':',	0xc3, 0x8b},
	{'I', '!',	0xc3, 0x8c},
	{'I', '\'',	0xc3, 0x8d},
	{'I', '>',	0xc3, 0x8e},
	{'I', ':',	0xc3, 0x8f},
	{'D', '-',	0xc3, 0x90},
	{'N', '?',	0xc3, 0x91},
	{'O', '!',	0xc3, 0x92},
	{'O', '\'',	0xc3, 0x93},
	{'O', '>',	0xc3, 0x94},
	{'O', '?',	0xc3, 0x95},
	{'O', ':',	0xc3, 0x96},	//fixme
	{'*', 'X',	0xc3, 0x97},
	{'O', '/',	0xc3, 0x98},
	{'U', '!',	0xc3, 0x99},
	{'U', '\'',	0xc3, 0x9a},
	{'U', '>',	0xc3, 0x9b},
	{'U', ':',	0xc3, 0x9c},	//fixme
	{'Y', '\'',	0xc3, 0x9d},
	{'T', 'H',	0xc3, 0x9e},
	{'s', 's',	0xc3, 0x9f},
	{'a', '!',	0xc3, 0xa0},
	{'a', '\'',	0xc3, 0xa1},
	{'a', '>',	0xc3, 0xa2},
	{'a', '?',	0xc3, 0xa3},
	{'a', ':',	0xc3, 0xa4},
	{'a', 'a',	0xc3, 0xa5},
	{'a', 'e',	0xc3, 0xa6},
	{'c', ',',	0xc3, 0xa7},
	{'e', '!',	0xc3, 0xa8},
	{'e', '\'',	0xc3, 0xa9},
	{'e', '>',	0xc3, 0xaa},
	{'e', ':',	0xc3, 0xab},
	{'i', '!',	0xc3, 0xac},
	{'i', '\'',	0xc3, 0xad},
	{'i', '>',	0xc3, 0xae},
	{'i', ':',	0xc3, 0xaf},
	{'d', '-',	0xc3, 0xb0},
	{'n', '?',	0xc3, 0xb1},
	{'o', '!',	0xc3, 0xb2},
	{'o', '\'',	0xc3, 0xb3},
	{'o', '>',	0xc3, 0xb4},
	{'o', '?',	0xc3, 0xb5},
	{'o', ':',	0xc3, 0xb6},	// fixme
	{'-', ':',	0xc3, 0xb7},
	{'o', '/',	0xc3, 0xb8},
	{'u', '!',	0xc3, 0xb9},
	{'u', '\'',	0xc3, 0xba},
	{'u', '>',	0xc3, 0xbb},
	{'u', ':',	0xc3, 0xbc},	//fixme
	{'y', '\'',	0xc3, 0xbd},
	{'t', 'h',	0xc3, 0xbe},
	{'y', ':',	0xc3, 0xbf},
	{'A', '-',	0xc4, 0x80},
	{'a', '-',	0xc4, 0x81},
	{'A', '(',	0xc4, 0x82},
	{'a', '(',	0xc4, 0x83},
	{'A', ';',	0xc4, 0x84},
	{'a', ';',	0xc4, 0x85},
	{'C', '\'',	0xc4, 0x86},
	{'c', '\'',	0xc4, 0x87},
	{'C', '>',	0xc4, 0x88},
	{'c', '>',	0xc4, 0x89},
	{'C', '.',	0xc4, 0x8a},
	{'c', '.',	0xc4, 0x8b},
	{'C', '<',	0xc4, 0x8c},
	{'c', '<',	0xc4, 0x8d},
	{'D', '<',	0xc4, 0x8e},
	{'d', '<',	0xc4, 0x8f},
	{'D', '/',	0xc4, 0x90},
	{'d', '/',	0xc4, 0x91},
	{'E', '-',	0xc4, 0x92},
	{'e', '-',	0xc4, 0x93},
	{'E', '.',	0xc4, 0x96},
	{'e', '.',	0xc4, 0x97},
	{'E', ';',	0xc4, 0x98},
	{'e', ';',	0xc4, 0x99},
	{'E', '<',	0xc4, 0x9a},
	{'e', '<',	0xc4, 0x9b},
	{'G', '>',	0xc4, 0x9c},
	{'g', '>',	0xc4, 0x9d},
	{'G', '(',	0xc4, 0x9e},
	{'g', '(',	0xc4, 0x9f},
	{'G', '.',	0xc4, 0xa0},
	{'g', '.',	0xc4, 0xa1},
	{'G', ',',	0xc4, 0xa2},
	{'g', ',',	0xc4, 0xa3},
	{'H', '>',	0xc4, 0xa4},
	{'h', '>',	0xc4, 0xa5},
	{'H', '/',	0xc4, 0xa6},
	{'h', '/',	0xc4, 0xa7},
	{'I', '?',	0xc4, 0xa8},
	{'i', '?',	0xc4, 0xa9},
	{'I', '-',	0xc4, 0xaa},
	{'i', '-',	0xc4, 0xab},
	{'I', ';',	0xc4, 0xae},
	{'i', ';',	0xc4, 0xaf},
	{'I', '.',	0xc4, 0xb0},
	{'i', '.',	0xc4, 0xb1},
	{'I', 'J',	0xc4, 0xb2},
	{'i', 'j',	0xc4, 0xb3},
	{'J', '>',	0xc4, 0xb4},
	{'j', '>',	0xc4, 0xb5},
	{'K', ',',	0xc4, 0xb6},
	{'k', ',',	0xc4, 0xb7},
	{'k', 'k',	0xc4, 0xb8},
	{'L', '\'',	0xc4, 0xb9},
	{'l', '\'',	0xc4, 0xba},
	{'L', ',',	0xc4, 0xbb},
	{'l', ',',	0xc4, 0xbc},
	{'L', '<',	0xc4, 0xbd},
	{'l', '<',	0xc4, 0xbe},
	{'L', '.',	0xc4, 0xbf},
	{'l', '.',	0xc5, 0x80},
	{'L', '/',	0xc5, 0x81},
	{'l', '/',	0xc5, 0x82},
	{'N', '\'',	0xc5, 0x83},
	{'n', '\'',	0xc5, 0x84},
	{'N', ',',	0xc5, 0x85},
	{'n', ',',	0xc5, 0x86},
	{'N', '<',	0xc5, 0x87},
	{'n', '<',	0xc5, 0x88},
	{'N', 'G',	0xc5, 0x8a},
	{'n', 'g',	0xc5, 0x8b},
	{'O', '-',	0xc5, 0x8c},
	{'o', '-',	0xc5, 0x8d},
	{'O', '(',	0xc5, 0x8e},
	{'o', '(',	0xc5, 0x8f},
//	{'O', '"',	0xc5, 0x90},	//fixme
//	{'o', '"',	0xc5, 0x91},
	{'O', 'E',	0xc5, 0x92},
	{'o', 'e',	0xc5, 0x93},
	{'R', '\'',	0xc5, 0x94},
	{'r', '\'',	0xc5, 0x95},
	{'R', ',',	0xc5, 0x96},
	{'r', ',',	0xc5, 0x97},
	{'R', '<',	0xc5, 0x98},
	{'r', '<',	0xc5, 0x99},
	{'S', '\'',	0xc5, 0x9a},
	{'s', '\'',	0xc5, 0x9b},
	{'S', '>',	0xc5, 0x9c},
	{'s', '>',	0xc5, 0x9d},
	{'S', ',',	0xc5, 0x9e},
	{'s', ',',	0xc5, 0x9f},
	{'S', '<',	0xc5, 0xa0},
	{'s', '<',	0xc5, 0xa1},
	{'T', ',',	0xc5, 0xa2},
	{'t', ',',	0xc5, 0xa3},
	{'T', '<',	0xc5, 0xa4},
	{'t', '<',	0xc5, 0xa5},
	{'T', '/',	0xc5, 0xa6},
	{'t', '/',	0xc5, 0xa7},
	{'U', '?',	0xc5, 0xa8},
	{'u', '?',	0xc5, 0xa9},
	{'U', '-',	0xc5, 0xaa},
	{'u', '-',	0xc5, 0xab},
	{'U', '(',	0xc5, 0xac},
	{'u', '(',	0xc5, 0xad},
	{'U', '0',	0xc5, 0xae},
	{'u', '0',	0xc5, 0xaf},
//	{'U', '"',	0xc5, 0xb0},	//fixme
//	{'u', '"',	0xc5, 0xb1},
	{'U', ';',	0xc5, 0xb2},
	{'u', ';',	0xc5, 0xb3},
	{'W', '>',	0xc5, 0xb4},
	{'w', '>',	0xc5, 0xb5},
	{'Y', '>',	0xc5, 0xb6},
	{'y', '>',	0xc5, 0xb7},
	{'Y', ':',	0xc5, 0xb8},
	{'Z', '\'',	0xc5, 0xb9},
	{'z', '\'',	0xc5, 0xba},
	{'Z', '.',	0xc5, 0xbb},
	{'z', '.',	0xc5, 0xbc},
	{'Z', '<',	0xc5, 0xbd},
	{'z', '<',	0xc5, 0xbe},
	{'\'', '<',	0xcb, 0x87},
	{'\'', '(',	0xcb, 0x98},
	{'\'', '.',	0xcb, 0x99},
	{'\'', ';',	0xcb, 0x9b},
	{'\'', '"',	0xcb, 0x9d},
/* X11 compose sequences (/usr/share/X11/locale/en_US.UTF-8/Compose) */
	{',', '-',	0xc2, 0xac},
	{'^', '2',	0xc2, 0xb2},
	{'^', '3',	0xc2, 0xb3},
	{'M', 'u',	0xc2, 0xb5},
	{'^', '1',	0xc2, 0xb9},
	{'D', 'H',	0xc3, 0x90},
	{'O', '"',	0xc3, 0x96},
	{'x', 'x',	0xc3, 0x97},
	{'U', '"',	0xc3, 0x9c},
	{'o', '"',	0xc3, 0xb6},
	{'u', '"',	0xc3, 0xbc},
	{'O', '=',	0xc5, 0x90},
	{'o', '=',	0xc5, 0x91},
	{'U', '=',	0xc5, 0xb0},
	{'u', '=',	0xc5, 0xb1},
#if 1
/*
 * translation table from the ABC draft version 2
 *	` grave
 *	' acute
 *	^ circumflex
 *	, cedilla
 *	" umlaut
 *	~ tilde
 *	o ring
 *	= macron or stroke
 *	/ slash
 *	; ogonek
 *	v caron
 *	u breve
 *	: long Hungarian umlaut
 *	. dot / dotless
 * else, ligatures
 *	ae ss ng
 */
#if 0
	{'`', 'A',	0xc3, 0x80},
	{'`', 'E',	0xc3, 0x88},
	{'`', 'I',	0xc3, 0x8c},
	{'`', 'O',	0xc3, 0x92},
	{'`', 'U',	0xc3, 0x99},
	{'`', 'a',	0xc3, 0xa0},
	{'`', 'e',	0xc3, 0xa8},
	{'`', 'i',	0xc3, 0xac},
	{'`', 'o',	0xc3, 0xb2},
	{'`', 'u',	0xc3, 0xb9},

	{'\'', 'A',	0xc3, 0x81},
	{'\'', 'E',	0xc3, 0x89},
	{'\'', 'I',	0xc3, 0x8d},
	{'\'', 'O',	0xc3, 0x93},
	{'\'', 'U',	0xc3, 0x9a},
	{'\'', 'Y',	0xc3, 0x9d},
	{'\'', 'a',	0xc3, 0xa1},
	{'\'', 'e',	0xc3, 0xa9},
	{'\'', 'i',	0xc3, 0xad},
	{'\'', 'o',	0xc3, 0xb3},
	{'\'', 'u',	0xc3, 0xba},
	{'\'', 'y',	0xc3, 0xbd},
	{'\'', 'S',	0xc5, 0x9a},
	{'\'', 'Z',	0xc5, 0xb9},
	{'\'', 's',	0xc5, 0x9b},
	{'\'', 'z',	0xc5, 0xba},
	{'\'', 'R',	0xc5, 0x94},
	{'\'', 'L',	0xc4, 0xb9},
	{'\'', 'C',	0xc4, 0x86},
	{'\'', 'N',	0xc5, 0x83},
	{'\'', 'r',	0xc5, 0x95},
	{'\'', 'l',	0xc4, 0xba},
	{'\'', 'c',	0xc4, 0x87},
	{'\'', 'n',	0xc5, 0x84},

	{'^', 'A',	0xc3, 0x82},
	{'^', 'E',	0xc3, 0x8a},
	{'^', 'I',	0xc3, 0x8e},
	{'^', 'O',	0xc3, 0x94},
	{'^', 'U',	0xc3, 0x9b},
	{'^', 'a',	0xc3, 0xa2},
	{'^', 'e',	0xc3, 0xaa},
	{'^', 'i',	0xc3, 0xae},
	{'^', 'o',	0xc3, 0xb4},
	{'^', 'u',	0xc3, 0xbb},
	{'^', 'H',	0xc4, 0xa4},
	{'^', 'J',	0xc4, 0xb4},
	{'^', 'h',	0xc4, 0xa5},
	{'^', 'j',	0xc4, 0xb5},
	{'^', 'C',	0xc4, 0x88},
	{'^', 'G',	0xc4, 0x9c},
	{'^', 'S',	0xc5, 0x9c},
	{'^', 'c',	0xc4, 0x89},
	{'^', 'g',	0xc4, 0x9d},
	{'^', 's',	0xc5, 0x9d},

	{',', 'C',	0xc3, 0x87},
	{',', 'c',	0xc3, 0xa7},
	{',', 'S',	0xc5, 0x9e},
	{',', 's',	0xc5, 0x9f},
	{',', 'T',	0xc5, 0xa2},
	{',', 't',	0xc5, 0xa3},
	{',', 'R',	0xc5, 0x96},
	{',', 'L',	0xc4, 0xbb},
	{',', 'G',	0xc4, 0xa2},
	{',', 'r',	0xc5, 0x97},
	{',', 'l',	0xc4, 0xbc},
	{',', 'g',	0xc4, 0xa3},
	{',', 'N',	0xc5, 0x85},
	{',', 'K',	0xc4, 0xb6},
	{',', 'n',	0xc5, 0x86},
	{',', 'k',	0xc4, 0xb7},

#endif
	{'"', 'A',	0xc3, 0x84},
	{'"', 'E',	0xc3, 0x8b},
	{'"', 'I',	0xc3, 0x8f},
#if 0
	{'"', 'O',	0xc3, 0x96},
	{'"', 'U',	0xc3, 0x9c},
#endif
	{'"', 'a',	0xc3, 0xa4},
	{'"', 'e',	0xc3, 0xab},
	{'"', 'i',	0xc3, 0xaf},
#if 0
	{'"', 'o',	0xc3, 0xb6},
	{'"', 'u',	0xc3, 0xbc},
#endif
	{'"', 'y',	0xc3, 0xbf},
#if 0
	{'~', 'A',	0xc3, 0x83},
	{'~', 'N',	0xc3, 0x91},
	{'~', 'O',	0xc3, 0x95},
	{'~', 'a',	0xc3, 0xa3},
	{'~', 'n',	0xc3, 0xb1},
	{'~', 'o',	0xc3, 0xb5},
	{'~', 'I',	0xc4, 0xa8},
	{'~', 'i',	0xc4, 0xa9},
	{'~', 'U',	0xc5, 0xa8},
	{'~', 'u',	0xc5, 0xa9},
#endif
	{'o', 'A',	0xc3, 0x85},
	{'o', 'a',	0xc3, 0xa5},
	{'o', 'U',	0xc5, 0xae},
	{'o', 'u',	0xc5, 0xaf},
	{'=', 'A',	0xc4, 0x80},
	{'=', 'D',	0xc4, 0x90},
	{'=', 'E',	0xc4, 0x92},
	{'=', 'H',	0xc4, 0xa6},
	{'=', 'I',	0xc4, 0xaa},
#if 0
	{'=', 'O',	0xc5, 0x8c},
#endif
	{'=', 'T',	0xc5, 0xa6},
#if 0
	{'=', 'U',	0xc5, 0xaa},
#endif
	{'=', 'a',	0xc4, 0x81},
	{'=', 'd',	0xc4, 0x91},
	{'=', 'e',	0xc4, 0x93},
	{'=', 'h',	0xc4, 0xa7},
	{'=', 'i',	0xc4, 0xab},
#if 0
	{'=', 'o',	0xc5, 0x8d},
#endif
	{'=', 't',	0xc5, 0xa7},
#if 0
	{'=', 'u',	0xc5, 0xab},
#endif
#if 0
	{'/', 'O',	0xc3, 0x98},
	{'/', 'o',	0xc3, 0xb8},
	{'/', 'D',	0xc4, 0x90},
	{'/', 'd',	0xc4, 0x91},
	{'/', 'L',	0xc5, 0x81},
	{'/', 'l',	0xc5, 0x82},
	{';', 'A',	0xc4, 0x84},
	{';', 'a',	0xc4, 0x85},
	{';', 'E',	0xc4, 0x98},
	{';', 'e',	0xc4, 0x99},
	{';', 'I',	0xc4, 0xae},
	{';', 'U',	0xc5, 0xb2},
	{';', 'i',	0xc4, 0xaf},
	{';', 'u',	0xc5, 0xb3},
#endif
	{'v', 'L',	0xc4, 0xbd},
	{'v', 'S',	0xc5, 0xa0},
	{'v', 'T',	0xc5, 0xa4},
	{'v', 'Z',	0xc5, 0xbd},
	{'v', 'l',	0xc4, 0xbe},
	{'v', 's',	0xc5, 0xa1},
	{'v', 't',	0xc5, 0xa5},
	{'v', 'z',	0xc5, 0xbe},
	{'v', 'C',	0xc4, 0x8c},
	{'v', 'E',	0xc4, 0x9a},
	{'v', 'D',	0xc4, 0x8e},
	{'v', 'N',	0xc5, 0x87},
	{'v', 'R',	0xc5, 0x98},
	{'v', 'c',	0xc4, 0x8d},
	{'v', 'e',	0xc4, 0x9b},
	{'v', 'd',	0xc4, 0x8f},
	{'v', 'n',	0xc5, 0x88},
	{'v', 'r',	0xc5, 0x99},
	{'u', 'A',	0xc4, 0x82},
	{'u', 'a',	0xc4, 0x83},
	{'u', 'E',	0xc4, 0x94},
	{'u', 'e',	0xc4, 0x95},
	{'u', 'G',	0xc4, 0x9e},
	{'u', 'g',	0xc4, 0x9f},
	{'u', 'I',	0xc4, 0xac},
	{'u', 'i',	0xc4, 0xad},
	{'u', 'O',	0xc5, 0x8e},
	{'u', 'o',	0xc5, 0x8f},
	{'u', 'U',	0xc5, 0xac},
	{'u', 'u',	0xc5, 0xad},
#if 0
	{':', 'O',	0xc5, 0x90},	/*fixme*/
	{':', 'U',	0xc5, 0xb0},	/*fixme*/
	{':', 'o',	0xc5, 0x91},	/*fixme*/
	{':', 'u',	0xc5, 0xb1},	/*fixme*/
	{'.', 'Z',	0xc5, 0xbb},
	{'.', 'z',	0xc5, 0xbc},
	{'.', 'I',	0xc4, 0xb0},
	{'.', 'i',	0xc4, 0xb1},
	{'.', 'C',	0xc4, 0x8a},
	{'.', 'G',	0xc4, 0xa0},
	{'.', 'c',	0xc4, 0x8b},
	{'.', 'g',	0xc4, 0xa1},
	{'.', 'E',	0xc4, 0x96},
	{'.', 'e',	0xc4, 0x97},
#endif
/*	{'(C)',	0xc2, 0xa9},		fixme: 3 characters */
#endif
};

static struct u_ps {
	struct u_ps *next;
	char text[2];
} *user_ps;

/* -- print message for internal error and maybe stop -- */
void bug(char *msg, int fatal)
{
	error(1, 0, "Internal error: %s.", msg);
	if (fatal) {
		fprintf(stderr, "Emergency stop.\n\n");
		exit(3);
	}
	fprintf(stderr, "Trying to continue...\n");
}

/* -- print an error message -- */
void error(int sev,	/* 0: warning, 1: error */
	   struct SYMBOL *s,
	   char *fmt, ...)
{
	va_list args;
static struct SYMBOL *t;

	if (t != info['T' - 'A']) {
		char *p;

		t = info['T' - 'A'];
		p = &t->as.text[2];
		while (isspace((unsigned char) *p))
			p++;
		fprintf(stderr, "   - In tune '%s':\n", p);
	}
	fprintf(stderr, sev == 0 ? "Warning " : "Error ");
	if (s != 0) {
		fprintf(stderr, "in line %d.%d",
			s->as.linenum, s->as.colnum);
		if (showerror) {
			s->as.flags |= ABC_F_ERROR;
			showerror++;
		}
	}
	fprintf(stderr, ": ");
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
}

/* -- read a number with a unit -- */
float scan_u(char *str)
{
	float a;
	int nch;

	if (sscanf(str, "%f%n", &a, &nch) == 1) {
		if (str[nch] == '\0' || str[nch] == ' ')
			return a PT;
		if (!strncasecmp(str + nch, "cm", 2))
			return a CM;
		if (!strncasecmp(str + nch, "in", 2))
			return a IN;
		if (!strncasecmp(str + nch, "pt", 2))
			return a PT;
	}
	error(1, 0, "\n++++ Unknown unit value \"%s\"", str);
	return 20 PT;
}

/* -- capitalize a string -- */
static void cap_str(char *p)
{
	while (*p != '\0') {
#if 1
/* pb with toupper - works with ASCII only */
		unsigned char c;

		c = (unsigned char) *p;
		if (c >= 'a' && c <= 'z')
/*fixme: KO with utf-8*/
//		    || (c >= 0xe0 && c <= 0xfe))
			*p = c & ~0x20;
#else
		*p = toupper((unsigned char) *p);
#endif
		p++;
	}
}

/* -- return the character width -- */
float cwid(unsigned short c)
{
	if (c > sizeof cw_tb / sizeof cw_tb[0])
		c = 'A';
	return (float) cw_tb[c] / 1000.;
}

/* -- change string taking care of some tex-style codes -- */
/* Return an estimated width of the string. */
float tex_str(char *s)
{
	char *d, c2;
	signed char c1;
	unsigned maxlen, i, j, v;
	float w, swfac;

	w = 0;
	d = tex_buf;
	maxlen = sizeof tex_buf - 1;		/* have room for EOS */
	if ((i = curft) <= 0)
		i = defft;
	swfac = cfmt.font_tb[i].swfac;
	while ((c1 = *s++) != '\0') {
		switch (c1) {
		case '\\':			/* backslash sequences */
			if (*s == '\0')
				continue;
			c1 = *s++;
			switch (c1) {
			case ' ':
				goto addchar;
			case 't':
				c1 = '\t';
				goto addchar;
			case 'u':		/* unicode 16 bits */
				i = 4;
				goto hexuni;
			case 'U':		/* unicode 32 bits */
				i = 8;
		hexuni:

				/* check if hexadecimal digits */
				for (j = 0; j < i; j++)
					if (!isxdigit((unsigned char) s[j]))
						break;
				if (j != i)
					break;
				v = 0;		/* get the unicode value */
				while (--i >= 0
				    && isxdigit((unsigned char) *s)) {
					v <<= 4;
					if (*s <= '9')
						v += *s - '0';
					else if (*s <= 'F')
						v += *s - 'A' + 10;
					else
						v += *s - 'a' + 10;
					s++;
				}
				if ((v & 0xd800) == 0xd800) {	/* surrogates */
					if (*s != '\\' || s[1] != 'u')
						break;
					s += 2;
					for (j = 0; j < i; j++)
						if (!isxdigit((unsigned char) s[j]))
							break;
					if (j != i)
						break;
					v = (v - 0xd7c0) << (10 - 4);
					while (--i >= 0
					    && isxdigit((unsigned char) *s)) {
						v <<= 4;
						if (*s <= '9')
							v += *s - '0';
						else if (*s <= 'F')
							v += *s - 'A' + 10;
						else
							v += *s - 'a' + 10;
						s++;
					}
					v -= 0xdc00;
				}
				if (v < 0x80) {	/* convert to UTF-8 */
					c1 = v;
				} else if (v < 0x800) {
					if (--maxlen <= 0)
						goto addchar;
					*d++ = 0xc0 | (v >> 6);
					c1 = 0x80 | (v & 0x3f);
				} else if (v < 0x10000) {
					if (--maxlen <= 0)
						goto addchar;
					*d++ = 0xe0 | (v >> 12);
					if (--maxlen <= 0)
						goto addchar;
					*d++ = 0x80 | ((v >> 6) & 0x3f);
					c1 = 0x80 | (v & 0x3f);
				} else {
					if (--maxlen <= 0)
						goto addchar;
					*d++ = 0xf0 | (v >> 18);
					if (--maxlen <= 0)
						goto addchar;
					*d++ = 0x80 | ((v >> 12) & 0x3f);
					if (--maxlen <= 0)
						goto addchar;
					*d++ = 0x80 | ((v >> 6) & 0x3f);
					c1 = 0x80 | (v & 0x3f);
				}
				goto addchar;
			}
			c2 = *s;

			/* treat escape with octal value */
			if ((unsigned) (c1 - '0') <= 3
			    && (unsigned) (c2 - '0') <= 7
			    && (unsigned) (s[1] - '0') <= 7) {
				c1 = ((c1 - '0') << 6) + ((c2 - '0') << 3) + s[1] - '0';
				s += 2;

				/* accidental compatibility */
				switch (c1) {
				case '\201':
				case '\202':
				case '\203':
				case '\204':
				case '\205':
					c1 &= 0x07;
					break;
				default:
					goto compat;
				}
				break;
			}

			/* convert to rfc1345 */
			switch (c1) {
			case '`': c1 = '!'; break;
			case '^': c1 = '>'; break;
			case '~': c1 = '?'; break;
//			case '"': c1 = ':'; break;	/* KO with some chars */
			}

			switch (c2) {
			case '`': c2 = '!'; break;
			case '^': c2 = '>'; break;
			case '~': c2 = '?'; break;
//			case '"': c2 = ':'; break;
			}

			/* check the sequence in both orders */
			for (i = 0; i < sizeof tex_tb / sizeof tex_tb[0]; i++) {
				if ((tex_tb[i][0] == c1 &&
						tex_tb[i][1] == c2) ||
				    (tex_tb[i][0] == c2 &&
						tex_tb[i][1] == c1)) {
					if (--maxlen <= 0)
						goto addchar_nowidth;;
					s++;
					*d++ = tex_tb[i][2];
					c1 = tex_tb[i][3];
					goto addchar;
				}
			}
			break;
		case '$':
			if (isdigit((unsigned char) *s)
			    && (unsigned) (*s - '0') < FONT_UMAX) {
				i = *s - '0';
				if (i == 0)
					i = defft;
				swfac = cfmt.font_tb[i].swfac;
				if (--maxlen <= 0)
					break;
				*d++ = c1;
				c1 = *s++;
				goto addchar_nowidth;
			}
			if (*s == '$') {
				if (--maxlen <= 0)
					break;
				*d++ = c1;
				s++;
			}
			break;
		default:
compat:
			/* latin1 compatibility */
			if (c1 >= 0)
				break;		/* ascii */
//			if ((unsigned) ((c1 & 0xff) - 0xa0) > 0x100 - 0xa0)
//				break;
			if ((c1 & 0xff) > 0xc0) {
				if ((*s & 0xc0) == 0x80)
					break;
				if (*s == '\\' && s[1] == '2')
					break;
			} else {
				if (d[-1] < 0)
					break;
			}

			/* replace the character by the utf-8 value */
			if (--maxlen <= 0)
				break;
			*d++ = 0xc0 | ((c1 >> 6) & 0x03);
			c1 = 0x80 | (c1 & 0x3f);
			break;
		}
	addchar:
		if (c1 < 0) {
			if ((c1 & 0xc0) == 0x80) {
				unsigned short unicode;

/*fixme: does not work with utf-8 on 3 characters*/
				unicode = ((d[-1] & 0x0f) << 6) | (c1 & 0x3f);
				w += cwid(unicode) * swfac;
			}
		} else if (c1 <= 5) {		/* accidentals */
#if 1
			if (--maxlen <= 0)
				break;
			*d++ = 0xc2;
			c1 |= 0x80;		/* pseudo utf8 c201 .. c205 */
#else
			if (c1 < 4) {
				if (--maxlen <= 0)
					break;
				*d++ = 0xe2;
				if (--maxlen <= 0)
					break;
				*d++ = 0x99;
				switch (c1) {
				case 1:
					c1 = 0xaf;
					break;
				case 2:
					c1 = 0xad;
					break;
				default:
/*				case 3: */
					c1 = 0xae;
					break;
				}
			} else {
				if (--maxlen <= 0)
					break;
				*d++ = 0xf0;
				if (--maxlen <= 0)
					break;
				*d++ = 0x9d;
				if (--maxlen <= 0)
					break;
				*d++ = 0x84;
				c1 += 0xaa - 4;	/* aa / ab */
			}
#endif
			w += cwid('A') * swfac;
		} else
			w += cwid((unsigned short) c1) * swfac;
	addchar_nowidth:
		if (--maxlen <= 0)
			break;
		*d++ = c1;
	}
	*d = '\0';
	if (maxlen <= 0)
		error(0, 0, "Text too large - ignored part: '%s'", s);
	return w;
}

#ifdef HAVE_PANGO
#define PG_SCALE (PANGO_SCALE * 72 / 96)	/* 96 DPI */

static PangoFontDescription *desc_tb[MAXFONTS];
static PangoLayout *layout = (PangoLayout *) -1;
static PangoAttrList *attrs;
static int out_pg_ft = -1;		/* current pango font */
static GString *pg_str;


/* -- initialize the pango mechanism -- */
void pg_init(void)
{
	static PangoContext *context;

	/* initialize glib */
	g_type_init();

	context = pango_font_map_create_context(
			pango_cairo_font_map_get_default());
	if (context != NULL)
		layout = pango_layout_new(context);
	if (layout == NULL) {
		fprintf(stderr, "pango disabled\n");
		cfmt.pango = 0;
	} else {
		pango_layout_set_wrap(layout, PANGO_WRAP_WORD);
//		pango_layout_set_spacing(layout, 0);
		pg_str = g_string_sized_new(256);
	}
}
void pg_reset_font(void)
{
	out_pg_ft = -1;
}

static void desc_font(int fnum)
{
	char font_name[128], *p;

	if (desc_tb[fnum] == 0) {
		p = font_name;
		sprintf(p, "%s 10", fontnames[fnum]);
		while (*p != '\0') {
			if (*p == '-')
				*p = ' ';
			p++;
		}
		desc_tb[fnum] = pango_font_description_from_string(font_name);
	}
}

/* output a line */
static void pg_line_output(PangoLayoutLine *line)
{
	GSList *runs_list;
	PangoGlyphInfo *glyph_info;
	char tmp[256];
	const char *fontname = 0;
	int ret, glypharray;

	outft = -1;
	glypharray = 0;
	for (runs_list = line->runs; runs_list; runs_list = runs_list->next) {
		PangoLayoutRun *run = runs_list->data;
		PangoItem *item = run->item;
		PangoGlyphString *glyphs = run->glyphs;
		PangoAnalysis *analysis = &item->analysis;
		PangoFont *font = analysis->font;
		PangoFcFont *fc_font = PANGO_FC_FONT(font);
		FT_Face face = pango_fc_font_lock_face(fc_font);
		PangoFontDescription *ftdesc =
				pango_font_describe(font);
		int wi = pango_font_description_get_size(ftdesc);
		int i, c;

		if (pango_font_description_get_size(ftdesc) != wi) {
			wi = pango_font_description_get_size(ftdesc);
			fontname = 0;
		}
		for (i = 0; i < glyphs->num_glyphs; i++) {
			glyph_info = &glyphs->glyphs[i];
			c = glyph_info->glyph;
			if (c == PANGO_GLYPH_EMPTY)
				continue;
			if (c & PANGO_GLYPH_UNKNOWN_FLAG) {
//fixme: works only for extra chars (accidentals)
				c &= ~PANGO_GLYPH_UNKNOWN_FLAG;
				if ((unsigned) (c - 0x80) >= 0x20) {
					fprintf(stderr, "char %04x not treated\n", c);
					continue;
				}
				if (glypharray)
					a2b("]glypharray");
//				else if (mbf[-1] != '\n')
					a2b("\n");
				a2b("/ExtraFont %.1f selectfont(%c)show",
					(float) wi / PG_SCALE,
					c - 0x80);
				glypharray = 0;
				fontname = 0;
				continue;
			}

			ret = FT_Load_Glyph(face,
					c,		// PangoGlyph = index
					FT_LOAD_NO_SCALE);
			if (ret != 0) {
				fprintf(stdout, "%%%% freetype error %d\n", ret);
			} else if (FT_HAS_GLYPH_NAMES(face)) {
				if (FT_Get_Postscript_Name(face) != fontname) {
					fontname = FT_Get_Postscript_Name(face);
					if (glypharray)
						a2b("]glypharray");
//					else if (mbf[-1] != '\n')
						a2b("\n");
					a2b("/%s %.1f selectfont[",
						fontname,
						(float) wi / PG_SCALE);
					glypharray = 1;
				}
				FT_Get_Glyph_Name((FT_FaceRec *) face, c,
						tmp, sizeof tmp);
				a2b("/%s", tmp);
			} else {
				a2b("%% glyph: %s %d\n",
					FT_Get_Postscript_Name(face), c);
			}
		}
		pango_fc_font_unlock_face(fc_font);
	}
	if (glypharray)
		a2b("]glypharray");
}

static void str_font_change(int start,
			int end)
{
	struct FONTSPEC *f;
	int fnum;
	PangoAttribute *attr1, *attr2;;

	f = &cfmt.font_tb[curft];
	fnum = f->fnum;
	if (f->size == 0) {
		error(0, 0, "Font \"%s\" with a null size - set to 8",
		      fontnames[fnum]);
		f->size = 8;
	}
	desc_font(fnum);
	
	attr1 = pango_attr_font_desc_new(desc_tb[fnum]);
	attr1->start_index = start;
	attr1->end_index = end;
	pango_attr_list_insert(attrs, attr1);
//	pango_attr_list_change(attrs, attr1);
	attr2 = pango_attr_size_new((int) (f->size * PG_SCALE));
	attr2->start_index = start;
	attr2->end_index = end;
	pango_attr_list_insert(attrs, attr2);
//	pango_attr_list_change(attrs, attr2);
}

static GString *str_set_font(char *p, GString *str)
{
	char *q;
	int start;

	start = str->len;
	q = p;
	while (*p != '\0') {
		switch (*p) {
		case '$':
			if (isdigit((unsigned char) p[1])
			    && (unsigned) (p[1] - '0') < FONT_UMAX) {
				if (p > q)
					str = g_string_append_len(str, q, p - q);
				if (curft != p[1] - '0') {
					str_font_change(start, str->len);
					start = str->len;
					curft = p[1] - '0';
					if (curft == 0)
						curft = defft;
				}
				p += 2;
				q = p;
				continue;
			}
			if (p[1] == '$') {
				str = g_string_append_len(str, q, p - q);
				q = ++p;
			}
			break;
		}
		p++;
	}
	if (p > q) {
		str = g_string_append_len(str, q, p - q);
		str_font_change(start, str->len);
	}
	return str;
}

/* -- output a string using the pango and freetype libraries -- */
static void str_pg_out(char *p, int action)
{
	PangoLayoutLine *line;

//fixme: test
//PUT1("\n%% t: '%s'\n", p);
	if (out_pg_ft != curft)
		out_pg_ft = -1;

	attrs = pango_attr_list_new();
	pg_str = str_set_font(p, pg_str);

	pango_layout_set_text(layout, pg_str->str, pg_str->len);
	pango_layout_set_attributes(layout, attrs);

	/* only one line */
	line = pango_layout_get_line_readonly(layout, 0);
	switch (action) {
	case A_CENTER:
	case A_RIGHT: {
		int wi;
		float w;

		pango_layout_get_size(layout, &wi, NULL);
//printf("w: %d\n", wi);
		if (action == A_CENTER)
			wi /= 2;
//		w = (float) wi / PG_SCALE;
		w = (float) wi / PANGO_SCALE;
		a2b("-%.1f 0 RM ", w);
		break;
	    }
	}
	pg_line_output(line);
	pango_layout_set_attributes(layout, NULL);
	pg_str = g_string_truncate(pg_str, 0);
	pango_attr_list_unref(attrs);
}

/* output a justified or filled paragraph */
static void pg_para_output(int job)
{
	GSList *lines, *runs_list;
	PangoLayoutLine *line;
	PangoGlyphInfo *glyph_info;
	char tmp[256];
	const char *fontname = 0;
	int ret, glypharray;
	int wi;
	float y;

	pango_layout_set_text(layout, pg_str->str,
			pg_str->len - 1);	/* remove the last space */
	pango_layout_set_attributes(layout, attrs);
	outft = -1;
	glypharray = 0;
	wi = 0;
	y = 0;
	lines = pango_layout_get_lines_readonly(layout);

	for (; lines; lines = lines->next) {
		PangoRectangle pos;

		line = lines->data;
		pango_layout_line_get_extents(line, NULL, &pos);
//		y += (float) (pos.height - pos.y) / PG_SCALE;
		y += (float) pos.height
				* .87		/* magic! */
				/ PANGO_SCALE;

		for (runs_list = line->runs; runs_list; runs_list = runs_list->next) {
			PangoLayoutRun *run = runs_list->data;
			PangoItem *item = run->item;
			PangoGlyphString *glyphs = run->glyphs;
			PangoAnalysis *analysis = &item->analysis;
			PangoFont *font = analysis->font;
			PangoFcFont *fc_font = PANGO_FC_FONT(font);
			FT_Face face = pango_fc_font_lock_face(fc_font);
			PangoFontDescription *ftdesc =
					pango_font_describe(font);
			int i, g, set_move, x;

			if (pango_font_description_get_size(ftdesc) != wi) {
				wi = pango_font_description_get_size(ftdesc);
				fontname = 0;
			}
//printf("font size: %.2f\n", (float) wi / PG_SCALE);

			pango_layout_index_to_pos(layout, item->offset, &pos);
			x = pos.x;
			set_move = 1;
			for (i = 0; i < glyphs->num_glyphs; i++) {
				glyph_info = &glyphs->glyphs[i];
				g = glyph_info->glyph;
				if (g == PANGO_GLYPH_EMPTY)
					continue;
				if (set_move) {
					set_move = 0;
					if (glypharray) {
						a2b("]glypharray");
						glypharray = 0;
					}
//					   else if (mbf[-1] != '\n') {
						a2b("\n");
//					}
					a2b("%.2f %.2f M ",
						(float) x / PANGO_SCALE, -y);
				}
				x += glyph_info->geometry.width;
				if (g & PANGO_GLYPH_UNKNOWN_FLAG) {
//fixme: works only for extra chars (accidentals)
					g &= ~PANGO_GLYPH_UNKNOWN_FLAG;
					if ((unsigned) (g - 0x80) >= 0x20) {
						fprintf(stderr, "char %04x not treated\n", g);
						continue;
					}
					if (glypharray)
						a2b("]glypharray");
//					else if (mbf[-1] != '\n')
						a2b("\n");
					a2b("/ExtraFont %.1f selectfont(%c)show",
						(float) wi / PG_SCALE,
						g - 0x80);
					glypharray = 0;
					fontname = 0;
					continue;
				}

				ret = FT_Load_Glyph(face,
						g,		// PangoGlyph = index
						FT_LOAD_NO_SCALE);
				if (ret != 0) {
					fprintf(stdout, "%%%% freetype error %d\n", ret);
				} else if (FT_HAS_GLYPH_NAMES(face)) {
					if (FT_Get_Postscript_Name(face) != fontname) {
						fontname = FT_Get_Postscript_Name(face);
						if (glypharray)
							a2b("]glypharray");
//						else if (mbf[-1] != '\n')
							a2b("\n");
						a2b("/%s %.1f selectfont[",
							fontname,
							(float) wi / PG_SCALE);
						glypharray = 1;
					}
					FT_Get_Glyph_Name((FT_FaceRec *) face, g,
							tmp, sizeof tmp);
					if (job == T_JUSTIFY
					 && strcmp(tmp, "space") == 0) {
						set_move = 1;
						continue;
					}
					if (!glypharray) {
						a2b("[");
						glypharray = 1;
					}
					a2b("/%s", tmp);
				} else {
					a2b("%% glyph: %s %d\n",
						FT_Get_Postscript_Name(face), g);
				}
			}
			pango_fc_font_unlock_face(fc_font);
			if (glypharray) {
				a2b("]glypharray\n");
				glypharray = 0;
			}
		}
		if (glypharray) {
			a2b("]glypharray\n");
			glypharray = 0;
		}
	}
	bskip(y);
	pango_layout_set_attributes(layout, NULL);
	pg_str = g_string_truncate(pg_str, 0);
	pango_attr_list_unref(attrs);
}

/* concatenate text for fill and justify */
static void pg_add_text(char *s, int job, float baseskip)
{
	if (strtw <= 0) {			/* new paragraph */
		if (strtw < 0) {		/* first line */
			curft = defft;
			pango_layout_set_width(layout, strlw * PANGO_SCALE);
			pango_layout_set_justify(layout, job == T_JUSTIFY);
		}
		attrs = pango_attr_list_new();
	}
	if (*s == '\0') {
		if (pg_str->len > 0)
			pg_para_output(job);
		bskip(baseskip * 0.5);
		buffer_eob();
		strtw = 0;
		return;
	}
	tex_str(s);
	s = &tex_buf[strlen(tex_buf)];
	*s++ = ' ';
	*s = '\0';
	pg_str = str_set_font(tex_buf, pg_str);
	strtw = strlw;
}
#endif

/* -- set the default font of a string -- */
void str_font(int ft)
{
	curft = defft = ft;
}

/* -- get the current and default fonts -- */
void get_str_font(int *cft, int *dft)
{
	*cft = curft;
	*dft = defft;
}

/* -- set the current and default fonts -- */
void set_str_font(int cft, int dft)
{
	curft = cft;
	defft = dft;
}

/* -- output one string -- */
static void str_ft_out1(char *p, int l)
{
	if (curft != outft) {
		if (strtx) {
			PUT1(")%s ", strop);
			strtx = 0;
		}
//		if (curft == 0)
//			curft = defft;
		set_font(curft);
	}
	if (!strtx) {
		PUT0("(");
		strtx = 1;
	}
	PUT2("%.*s", l, p);
}

/* -- output a string and the font changes -- */
static void str_ft_out(char *p, int end)
{
	char *q;

	q = p;
	while (*p != '\0') {
		switch (*p) {
		case '$':
			if (isdigit((unsigned char) p[1])
			    && (unsigned) (p[1] - '0') < FONT_UMAX) {
				if (p > q)
					str_ft_out1(q, p - q);
				if (curft != p[1] - '0') {
					curft = p[1] - '0';
					if (curft == 0)
						curft = defft;
				}
				p += 2;
				q = p;
				continue;
			}
			if (p[1] == '$') {
				str_ft_out1(q, p - q);
				q = ++p;
			}
			break;
		case '(':
		case ')':
		case '\\':
			if (p > q)
				str_ft_out1(q, p - q);
			str_ft_out1("\\", 1);
			q = p;
			break;
		}
		p++;
	}
	if (p > q)
		str_ft_out1(q, p - q);
	if (end && strtx) {
		PUT1(")%s ", strop);
		strtx = 0;
	}
}

/* -- output a string, handling the font changes -- */
void str_out(char *p, int action)
{
	static char *strop_tb[] = {	/* index = action (A_xxxx) */
		"show",
		"showc",
		"showr",
		"lyshow",
		"gcshow",
		"anshow",
		"gxshow",
	};
	if (curft <= 0)		/* first call */
		curft = defft;

	/* special case when font change at start of text */
/*---fixme: authorize 2 chars?*/
	if (*p == '$' && isdigit((unsigned char) p[1])
	    && (unsigned) (p[1] - '0') < FONT_UMAX) {
		if (curft != p[1] - '0') {
			curft = p[1] - '0';
			if (curft == 0)
				curft = defft;
		}
		p += 2;
	}

#ifdef HAVE_PANGO
//fixme: pango KO if user modification of ly/gc/an/gxshow
	/* use pango if some characters are out of the utf-array (in syms.c) */
	if (cfmt.pango) {
		unsigned char *q;

		if (cfmt.pango == 2) {
			str_pg_out(p, action);	/* output the string */
			return;
		}
		for (q = (unsigned char *) p; *q != '\0'; q++) {
			if (*q >= 0xc6) {
				str_pg_out(p, action);	/* output the string */
				return;
			}
		}
	}
#endif

	/* direct output if no font change */
	if (strchr(p, '$') == 0) {
		strop = strop_tb[action];
		str_ft_out(p, 1);		/* output the string */
		return;
	}

	/* if not left aligned, build a PS function */
	switch (action) {
	case A_CENTER:
	case A_RIGHT:
		if (!svg && epsf != 2) {
			PUT0("/str{");
			outft = -1;
			strop = "strop";
			break;
		}
		/* fall thru */
	default:
		strop = strop_tb[action];
		break;
	}

	str_ft_out(p, 1);		/* output the string */

	/* if not left aligned, call the PS function */
	if (svg || epsf == 2)
		return;
	if (action == A_CENTER || action == A_RIGHT) {
		PUT0("}def\n"
			"/strop/strw load def/w 0 def str w ");
		if (action == A_CENTER)
			PUT0("0.5 mul ");
		PUT0("neg 0 RM/strop/show load def str ");
	}
}

/* -- output a string with TeX translation -- */
void put_str(char *str, int action)
{
	tex_str(str);
	str_out(tex_buf, action);
	PUT0("\n");
}

/* -- output a header information -- */
static void put_inf(struct SYMBOL *s)
{
	char *p;

	p = s->as.text;
	if (p[1] == ':')
		p += 2;
	while (isspace((unsigned char) *p))
		p++;
	put_str(p, A_LEFT);
}

/* -- output a header format '111 (222)' -- */
static void put_inf2r(struct SYMBOL *s1,
		      struct SYMBOL *s2,
		      int action)
{
	char buf[256], *p, *q;

	if (s1 == 0) {
		s1 = s2;
		s2 = 0;
	}
	p = &s1->as.text[2];
	if (s1->as.text[0] == 'T')
		p = trim_title(p, s1 == info['T' - 'A']);
	if (s2 != 0) {
		buf[sizeof buf - 1] = '\0';
		strncpy(buf, p, sizeof buf - 1);
		q = buf + strlen(buf);
		if (q < buf + sizeof buf - 4) {
			*q++ = ' ';
			*q++ = '(';
			p = &s2->as.text[2];
			strncpy(q, p, buf + sizeof buf - 2 - q);
			q += strlen(q);
			*q++ = ')';
			*q = '\0';
		}
		p = buf;
	}
	put_str(p, action);
}

/* -- add text to a block -- */
#ifdef HAVE_PANGO
void add_to_text_block(char *s, int job, int do_pango)
#else
void add_to_text_block(char *s, int job)
#endif
{
	float baseskip, lw;
	char *p, sep;
	struct FONTSPEC *f;

	/* if first line, set the fonts */
	if (strtw < 0) {
		str_font(TEXTFONT);
		strlw = ((cfmt.landscape ? cfmt.pageheight : cfmt.pagewidth)
			- cfmt.leftmargin - cfmt.rightmargin) / cfmt.scale;
	}

	if (curft > 0)
		f = &cfmt.font_tb[curft];
	else
		f = &cfmt.font_tb[defft];
	baseskip = f->size * cfmt.lineskipfac;

	/* follow lines */
	switch (job) {
	case T_LEFT:
	case T_CENTER:
	case T_RIGHT:
		if (*s != '\0') {
			bskip(baseskip);
			switch (job) {
			case T_LEFT:
				PUT0("0 0 M ");
#if T_LEFT != A_LEFT
				job = A_LEFT;
#endif
				break;
			case T_CENTER:
				PUT1("%.1f 0 M ", strlw * 0.5);
#if T_CENTER != A_CENTER
				job = A_CENTER;
#endif
				break;
			default:
#if T_RIGHT != A_RIGHT
				job = A_RIGHT;
#endif
				PUT1("%.1f 0 M ", strlw);
				break;
			}
			put_str(s, job);
		} else {
			bskip(baseskip * 0.5);
			buffer_eob();
		}
		strtw = 0;
		return;
	}

	/* fill or justify lines */
#ifdef HAVE_PANGO
	if (do_pango) {
		pg_add_text(s, job, baseskip);
		return;
	}
#endif
	if (strtw <= 0) {		/* if first line */
		if (strtw < 0)
			curft = defft;
		bskip(baseskip);
		PUT0("0 0 M ");
		if (job == T_FILL) {
			strop = "show";
		} else {
			PUT0("/str{");
			outft = -1;
			strop = "strop";
		}
	}

	if (*s == '\0') {			/* empty line */
		if (strtx) {
			PUT1(")%s", strop);
			strtx = 0;
		}
		if (job == T_JUSTIFY)
			PUT0("}def\n"
				"/strop/show load def str");
		a2b("\n");
		bskip(baseskip * 0.5);
		buffer_eob();
		strtw = 0;
		return;
	}

	p = s;
	for (;;) {
		if (strtw <= 0) {
			strns = 0;
			strtw = 0;
		} else {
			str_ft_out(" ", 0);
			strtw += cwid(' ') * cfmt.font_tb[curft].swfac;
			strns++;
		}
		while (*p != ' ' && *p != '\0')
			p++;
		sep = *p;
		*p = '\0';
		lw = tex_str(s);
		if (strtw + lw > strlw) {
			if (strtx) {
				if (mbf[-1] == ' ')
					mbf--;
				PUT1(")%s ", strop);
				strtx = 0;
			}
			if (job == T_JUSTIFY) {
				if (--strns <= 0)
					strns = 1;
				if (svg || epsf == 2)
					a2b("}def\n"
						"%.1f jshow"
						"/strop/show load def str ",
						strlw);
				else
					a2b("}def\n"
						"/strop/strw load def/w 0 def str"
						"/w %.1f w sub %d div def"
						"/strop/jshow load def str ",
						strlw, strns);
				strns = 0;
			}
			bskip(cfmt.font_tb[curft].size * cfmt.lineskipfac);
			a2b("0 0 M ");
			if (job == T_JUSTIFY) {
				PUT0("/str{");
				outft = -1;
			}
			strtw = 0;
		}
		str_ft_out(tex_buf, 0);
		strtw += lw;
		*p = sep;
		while (*p == ' ')
			p++;
		if (*p == '\0')
			break;
		s = p;
	}
}

/* -- write a text block -- */
void write_text_block(int job, int abc_state)
{
	if (strtw < 0)
		return;

#ifdef HAVE_PANGO
	if (pg_str->len > 0) {
		pg_para_output(job);
	} else
#endif
	if (strtw > 0) {
		if (strtx) {
			PUT1(")%s", strop);
			strtx = 0;
		}
		if (job == T_JUSTIFY)
			PUT0("}def\n"
				"/strop/show load def str");
//		if (mbf[-1] != '\n')
			a2b("\n");
	}
	bskip(cfmt.font_tb[TEXTFONT].size * cfmt.parskipfac);
	buffer_eob();

	/* next line to allow pagebreak after each paragraph */
	if (!epsf && abc_state != ABC_S_TUNE)
		write_buffer();
	strtw = -1;
}

/* -- output a line of words after tune -- */
static int put_wline(char *p,
		     float x,
		     int right)
{
	char *q, *r, sep;

	while (isspace((unsigned char) *p))
		p++;
	if (*p == '$' && isdigit((unsigned char) p[1])
	    && (unsigned) (p[1] - '0') < FONT_UMAX) {
		if (curft != p[1] - '0') {
			curft = p[1] - '0';
			if (curft == 0)
				curft = defft;
		}
		p += 2;
	}
	r = 0;
	q = p;
	if (isdigit((unsigned char) *p) || p[1] == '.') {
		while (*p != '\0') {
			p++;
			if (*p == ' '
			    || p[-1] == ':'
			    || p[-1] == '.')
				break;
		}
		r = p;
		while (*p == ' ')
			p++;
	}

	/* on the left side, permit page break at empty lines or stanza start */
	if (!right
	   && (*p == '\0' || r != 0))
		buffer_eob();

	if (r != 0) {
		sep = *r;
		*r = '\0';
		PUT1("%.1f 0 M ", x);
		put_str(q,  A_RIGHT);
		*r = sep;
	}
	if (*p != '\0') {
		PUT1("%.1f 0 M ", x + 5);
		put_str(p, A_LEFT);
	}
	return *p == '\0' && r == 0;
}

/* -- output the words after tune -- */
void put_words(struct SYMBOL *words)
{
	struct SYMBOL *s, *s_end, *s2;
	char *p;
	int i, n, have_text, max2col;
	float middle;

	str_font(WORDSFONT);

	/* see if we may have 2 columns */
	middle = 0.5 * ((cfmt.landscape ? cfmt.pageheight : cfmt.pagewidth)
		- cfmt.leftmargin - cfmt.rightmargin) / cfmt.scale;
	max2col = (int) ((middle - 45.) / (cwid('a') * cfmt.font_tb[WORDSFONT].swfac));
	n = 0;
	have_text = 0;
	for (s = words; s != 0; s = s->next) {
		p = &s->as.text[2];
/*fixme:utf8*/
		if ((int) strlen(p) > max2col) {
			n = 0;
			break;
		}
		if (*p == '\0') {
			if (have_text) {
				n++;
				have_text = 0;
			}
		} else	have_text = 1;
	}
	if (n > 0) {
		n++;
		n /= 2;
		i = n;
		have_text = 0;
		s_end = words;
		for (;;) {
			p = &s_end->as.text[2];
			while (isspace((unsigned char) *p))
				p++;
			if (*p == '\0') {
				if (have_text && --i <= 0)
					break;
				have_text = 0;
			} else	have_text = 1;
			s_end = s_end->next;
		}
		s2 = s_end->next;
	} else {
		s_end = 0;
		s2 = 0;
	}

	/* output the text */
	bskip(cfmt.wordsspace);
	for (s = words; s != 0 || s2 != 0; ) {
		bskip(cfmt.lineskipfac * cfmt.font_tb[WORDSFONT].size);
		if (s != 0) {
			put_wline(&s->as.text[2], 45., 0);
			s = s->next;
			if (s == s_end)
				s = 0;
		}
		if (s2 != 0) {
			if (put_wline(&s2->as.text[2], 20. + middle, 1)) {
				if (--n == 0) {
					if (s != 0)
						n++;
					else if (s2->next != 0) {

						/* center the last words */
/*fixme: should compute the width average.. */
						middle *= 0.6;
					}
				}
			}
			s2 = s2->next;
		}
	}
	buffer_eob();
}

/* -- output history -- */
void put_history(void)
{
	struct SYMBOL *s, *s2;
	int font;
	unsigned u;
	float w, h;
	char tmp[265];

	font = 0;
	for (s = info['I' - 'A']; s != 0; s = s->next) {
		u = s->as.text[0] - 'A';
		if (!(cfmt.fields[0] & (1 << u)))
			continue;
		if ((s2 = info[u]) == 0)
			continue;
		if (!font) {
			bskip(cfmt.textspace);
			str_font(HISTORYFONT);
			font = 1;
		}
		get_str(tmp, &s->as.text[1], sizeof tmp);
		w = tex_str(tmp);
		h = cfmt.font_tb[HISTORYFONT].size * cfmt.lineskipfac;
		set_font(HISTORYFONT);
		PUT1("0 0 M(%s)show ", tex_buf);
		for (;;) {
			put_inf(s2);
			if ((s2 = s2->next) == 0)
				break;
			bskip(h);
			a2b("%.2f 0 M ", w);
		}
		bskip(h * 1.2);
		buffer_eob();
	}
}

/* -- move trailing "The" to front, set to uppercase letters or add xref -- */
char *trim_title(char *p, int first)
{
	char *b, *q, *r;
static char buf[STRL1];

	q = 0;
	if (cfmt.titletrim) {
		q = strrchr(p, ',');
		if (q != 0) {
			if (q[1] != ' ' || !isupper((unsigned char) q[2])
			    || strchr(q + 2, ' ') != 0)
				q = 0;
		}
	}
	if (q == 0
	 && !cfmt.titlecaps
	 && !(first && (cfmt.fields[0] & (1 << ('X' - 'A')))))
		return p;		/* keep the title as it is */
	b = buf;
	r = &info['X' - 'A']->as.text[2];
	if (first
	 && (cfmt.fields[0] & (1 << ('X' - 'A')))
	 && r[2] != '\0') {
		if (strlen(p) + strlen(r) + 3 >= sizeof buf) {
			error(1, 0, "Title or X: too long");
			return p;
		}
		b += sprintf(b, "%s.  ", r);
	} else {
		if (strlen(p) >= sizeof buf) {
			error(1, 0, "Title too long");
			return p;
		}
	}
	if (q != 0)
		sprintf(b, "%s %.*s", q + 2, (int) (q - p), p);
	else
		strcpy(b, p);
	if (cfmt.titlecaps)
		cap_str(buf);
	return buf;
}

/* -- write a title -- */
void write_title(struct SYMBOL *s)
{
	char *p;

	if (!(cfmt.fields[0] & (1 << ('T' - 'A'))))
		return;
	p = &s->as.text[2];
	if (*p == '\0')
		return;
	p = trim_title(p, s == info['T' - 'A']);
	if (s == info['T' - 'A']) {
		bskip(cfmt.titlespace + cfmt.font_tb[TITLEFONT].size);
		str_font(TITLEFONT);
	} else {
		bskip(cfmt.subtitlespace + cfmt.font_tb[SUBTITLEFONT].size);
		str_font(SUBTITLEFONT);
	}
	if (cfmt.titleleft)
		PUT0("0 0 M ");
	else	PUT1("%.1f 0 M ",
		     0.5 * ((cfmt.landscape ? cfmt.pageheight : cfmt.pagewidth)
		     - cfmt.leftmargin - cfmt.rightmargin) / cfmt.scale);
	put_str(p, cfmt.titleleft ? A_LEFT : A_CENTER);
}

/* -- write heading with format -- */
static void write_headform(float lwidth)
{
	char *p, *q;
	struct SYMBOL *s;
	struct FONTSPEC *f;
	int align, i;
	unsigned j;
	float x, y, xa[3], ya[3], sz, yb[3];	/* !! see action A_xxx */
	char inf_nb[26];
	INFO inf_s;
	char inf_ft[26];
	float inf_sz[26];
	char fmt[64];

	memset(inf_nb, 0, sizeof inf_nb);
	memset(inf_ft, HISTORYFONT, sizeof inf_ft);
	inf_ft['A' - 'A'] = INFOFONT;
	inf_ft['C' - 'A'] = COMPOSERFONT;
	inf_ft['O' - 'A'] = COMPOSERFONT;
	inf_ft['P' - 'A'] = PARTSFONT;
	inf_ft['Q' - 'A'] = TEMPOFONT;
	inf_ft['R' - 'A'] = INFOFONT;
	inf_ft['T' - 'A'] = TITLEFONT;
	inf_ft['X' - 'A'] = TITLEFONT;
	memcpy(inf_s, info, sizeof inf_s);
	memset(inf_sz, 0, sizeof inf_sz);
	inf_sz['A' - 'A'] = cfmt.infospace;
	inf_sz['C' - 'A'] = cfmt.composerspace;
	inf_sz['O' - 'A'] = cfmt.composerspace;
	inf_sz['R' - 'A'] = cfmt.infospace;
	p = cfmt.titleformat;
	j = 0;
	for (;;) {
		while (isspace((unsigned char) *p))
			p++;
		if (*p == '\0')
			break;
		i = *p - 'A';
		if ((unsigned) i < 26) {
			inf_nb[i]++;
			switch (p[1]) {
			default:
				align = A_CENTER;
				break;
			case '1':
				align = A_RIGHT;
				p++;
				break;
			case '-':
				align = A_LEFT;
				p++;
				break;
			}
			if (j < sizeof fmt - 4) {
				fmt[j++] = i;
				fmt[j++] = align;
			}
		} else if (*p == ',') {
			if (j < sizeof fmt - 3)
				fmt[j++] = 126;		/* next line */
		} else if (*p == '+') {
			if (j > 0 && fmt[j - 1] < 125
			    && j < sizeof fmt - 3)
				fmt[j++] = 125;		/* concatenate */
/*new fixme: add free text "..." ?*/
		}
		p++;
	}
	fmt[j++] = 126;			/* newline */
	fmt[j] = 127;			/* end of format */

	ya[0] = ya[1] = ya[2] = cfmt.titlespace;;
	xa[0] = 0;
	xa[1] = lwidth * 0.5;
	xa[2] = lwidth;

	p = fmt;
	for (;;) {
		yb[0] = yb[1] = yb[2] = y = 0;
		q = p;
		for (;;) {
			i = *q++;
			if (i >= 126)		/* if newline */
				break;
			align = *q++;
			if (yb[align] != 0)
				continue;
			s = inf_s[i];
			if (s == 0 || inf_nb[i] == 0)
				continue;
			j = inf_ft[i];
			f = &cfmt.font_tb[j];
			sz = f->size * 1.1 + inf_sz[i];
			if (y < sz)
				y = sz;
			yb[align] = sz;
/*fixme:should count the height of the concatenated field*/
			if (*q == 125)
				q++;
		}
		for (i = 0; i < 3; i++)
			ya[i] += y - yb[i];
		for (;;) {
			i = *p++;
			if (i >= 126)		/* if newline */
				break;
			align = *p++;
			if (!(cfmt.fields[0] & (1 << (unsigned) i)))
				continue;
			s = inf_s[i];
			if (s == 0 || inf_nb[i] == 0)
				continue;
			j = inf_ft[i];
			str_font(j);
			x = xa[align];
			f = &cfmt.font_tb[j];
			sz = f->size * 1.1 + inf_sz[i];
			y = ya[align] + sz;
			PUT2("%.1f %.1f M ", x, -y);
			if (*p == 125) {	/* concatenate */
			    p++;
/*fixme: do it work with different fields*/
			    if (*p == i && p[1] == align
				&& s->next != 0) {
				char buf[256], *r;

				q = s->as.text;
				if (q[1] == ':')
					q += 2;
				while (isspace((unsigned char) *q))
					q++;
				if (i == 'T' - 'A')
					q = trim_title(q, s == inf_s['T' - 'A']);
				strncpy(buf, q, sizeof buf - 1);
				buf[sizeof buf - 1] = '\0';
				j = strlen(buf);
				if (j < sizeof buf - 1) {
					buf[j] = ' ';
					buf[j + 1] = '\0';
				}
				s = s->next;
				q = s->as.text;
				if (q[1] == ':')
					q += 2;
				while (isspace((unsigned char) *q))
					q++;
				if (s->as.text[0] == 'T' && s->as.text[1] == ':')
					q = trim_title(q, 0);
				r = buf + strlen(buf);
				strncpy(r, q, buf + sizeof buf - r - 1);
				tex_str(buf);
				str_out(tex_buf, align);
				PUT0("\n");
				inf_nb[i]--;
				p += 2;
			    }
			} else if (i == 'Q' - 'A') {	/* special case for tempo */
				if (align != A_LEFT) {
					float w;

					w = tempo_width(s);
					if (align == A_CENTER)
						PUT1("-%.1f 0 RM ", w * 0.5);
					else	PUT1("-%.1f 0 RM ", w);
				}
				write_tempo(s, 0, 0.75);
			} else {
				put_inf2r(s, 0, align);
			}
			if (inf_s[i] == info['T' - 'A']) {
				inf_ft[i] = SUBTITLEFONT;
				str_font(SUBTITLEFONT);
				f = &cfmt.font_tb[SUBTITLEFONT];
				inf_sz[i] = cfmt.subtitlespace;
				sz = f->size * 1.1 + inf_sz[i];
			}
			s = s->next;
			if (inf_nb[i] == 1) {
				while (s != 0) {
					y += sz;
					PUT2("%.1f %.1f M ", x, -y);
					put_inf2r(s, 0, align);
					s = s->next;
				}
			}
			inf_s[i] = s;
			inf_nb[i]--;
			ya[align] = y;
		}
		if (ya[1] > ya[0])
			ya[0] = ya[1];
		if (ya[2] > ya[0])
			ya[0] = ya[2];
		if (*p == 127) {
			bskip(ya[0]);
			break;
		}
		ya[1] = ya[2] = ya[0];
	}
}

/* -- output the tune heading -- */
void write_heading(struct abctune *t)
{
	struct SYMBOL *s, *rhythm, *area, *author, *composer, *origin;
	float lwidth, down1, down2;

	lwidth = ((cfmt.landscape ? cfmt.pageheight : cfmt.pagewidth)
		- cfmt.leftmargin - cfmt.rightmargin) / cfmt.scale;

	if (cfmt.titleformat != 0) {
		write_headform(lwidth);
		bskip(cfmt.musicspace);
		return;
	}

	/* titles */
	for (s = info['T' - 'A']; s != 0; s = s->next)
		write_title(s);

	/* rhythm, composer, origin */
	down1 = cfmt.composerspace + cfmt.font_tb[COMPOSERFONT].size;
	rhythm = (first_voice->key.bagpipe && !cfmt.infoline) ? info['R' - 'A'] : 0;
	if (!(cfmt.fields[0] & (1 << ('R' - 'A'))))
		rhythm = 0;
	if (rhythm) {
		str_font(COMPOSERFONT);
		PUT1("0 %.1f M ",
		     -(cfmt.composerspace + cfmt.font_tb[COMPOSERFONT].size));
		put_inf(rhythm);
		down1 -= cfmt.font_tb[COMPOSERFONT].size;
	}
	area = author = 0;
	if (t->abc_vers != (2 << 16))
		area = info['A' - 'A'];
	else
		author = info['A' - 'A'];
	if (!(cfmt.fields[0] & (1 << ('A' - 'A'))))
		area = author = 0;
	composer = info['C' - 'A'];
	if (!(cfmt.fields[0] & (1 << ('C' - 'A'))))
		composer = 0;
	origin = info['O' - 'A'];
	if (!(cfmt.fields[0] & (1 << ('O' - 'A'))))
		origin = 0;
	if (composer != 0 || origin != 0 || author != 0) {
		float xcomp;
		int align;

		str_font(COMPOSERFONT);
		bskip(cfmt.composerspace);
		if (cfmt.aligncomposer < 0) {
			xcomp = 0;
			align = A_LEFT;
		} else if (cfmt.aligncomposer == 0) {
			xcomp = lwidth * 0.5;
			align = A_CENTER;
		} else {
			xcomp = lwidth;
			align = A_RIGHT;
		}
		down2 = down1;
		if (author != 0) {
			for (;;) {
				bskip(cfmt.font_tb[COMPOSERFONT].size);
				down2 += cfmt.font_tb[COMPOSERFONT].size;
				PUT0("0 0 M ");
				put_inf(author);
				if ((author = author->next) == 0)
					break;
			}
		}
		if (composer != 0 || origin != 0) {
			if (cfmt.aligncomposer >= 0
			    && down1 != down2)
				bskip(down1 - down2);
			s = composer;
			for (;;) {
				bskip(cfmt.font_tb[COMPOSERFONT].size);
				PUT1("%.1f 0 M ", xcomp);
				put_inf2r(s,
					  (s == 0 || s->next == 0) ? origin : 0,
					  align);
				if (s == 0)
					break;
				if ((s = s->next) == 0)
					break;
				down1 += cfmt.font_tb[COMPOSERFONT].size;
			}
			if (down2 > down1)
				bskip(down2 - down1);
		}

		rhythm = rhythm ? 0 : info['R' - 'A'];
		if (!(cfmt.fields[0] & (1 << ('R' - 'A'))))
			rhythm = 0;
		if ((rhythm || area) && cfmt.infoline) {

			/* if only one of rhythm or area then do not use ()'s
			 * otherwise output 'rhythm (area)' */
			str_font(INFOFONT);
			bskip(cfmt.font_tb[INFOFONT].size + cfmt.infospace);
			PUT1("%.1f 0 M ", lwidth);
			put_inf2r(rhythm, area, A_RIGHT);
			down1 += cfmt.font_tb[INFOFONT].size + cfmt.infospace;
		}
		down2 = 0;
	} else {
		down2 = cfmt.composerspace + cfmt.font_tb[COMPOSERFONT].size;
	}

	/* parts */
	if (info['P' - 'A']
	 && (cfmt.fields[0] & (1 << ('P' - 'A')))) {
		down1 = cfmt.partsspace + cfmt.font_tb[PARTSFONT].size - down1;
		if (down1 > 0)
			down2 += down1;
		if (down2 > 0.01)
			bskip(down2);
		str_font(PARTSFONT);
		PUT0("0 0 M ");
		put_inf(info['P' - 'A']);
		down2 = 0;
	}
	bskip(down2 + cfmt.musicspace);
}

/* -- memorize a PS line -- */
void user_ps_add(char *s)
{
	struct u_ps *t, *r;
	int l;

	l = strlen(s);
	t = malloc(sizeof *user_ps - sizeof user_ps->text + l + 1);
	strcpy(t->text, s);
	t->next = 0;
	if ((r = user_ps) == 0)
		user_ps = t;
	else {
		while (r->next != 0)
			r = r->next;
		r->next = t;
	}
}

/* -- output the user defined postscript sequences -- */
void user_ps_write(void)
{
	struct u_ps *t;

	for (t = user_ps; t != 0; t = t->next) {
		if (t->text[0] == '\001') {	/* PS file */
			FILE *f;
			char line[BSIZE];

			if ((f = fopen(&t->text[1], "r")) == 0) {
				error(1, 0, "Cannot open PS file '%s'",
					&t->text[1]);
			} else {
				while (fgets(line, sizeof line, f))	/* copy the file */
					fwrite(line, 1, strlen(line), fout);
				fclose(f);
			}
		} else {
			output(fout, "%s\n", t->text);
		}
	}
}
