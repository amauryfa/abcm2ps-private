/* -- general macros -- */

#include "config.h"

#define OUTPUTFILE	"Out.ps"	/* standard output file */
#if defined(unix) || defined(__unix__)
#define DIRSEP '/'
#else
#define DIRSEP '\\'
#endif

#define CM		* 28.35	/* factor to transform cm to pt */
#define PT			/* factor to transform pt to pt */
#define IN		* 72.0	/* factor to transform inch to pt */

/* basic page dimensions */
#ifdef A4_FORMAT
#define PAGEHEIGHT	(29.7 CM)
#define PAGEWIDTH	(21.0 CM)
#define MARGIN		(1.8 CM)
#else
#define PAGEHEIGHT	(11.0 IN)
#define PAGEWIDTH	(8.5 IN)
#define MARGIN		(0.7 IN)
#endif

/* -- macros controlling music typesetting -- */

#define STEM_YOFF	1.0	/* offset stem from note center */
#define STEM_XOFF	3.5
#define STEM		20	/* default stem height */
#define STEM_MIN	16	/* min stem height under beams */
#define STEM_MIN2	14	/* ... for notes with two beams */
#define STEM_MIN3	12	/* ... for notes with three beams */
#define STEM_MIN4	10	/* ... for notes with four beams */
#define STEM_CH_MIN	14	/* min stem height for chords under beams */
#define STEM_CH_MIN2	10	/* ... for notes with two beams */
#define STEM_CH_MIN3	 9	/* ... for notes with three beams */
#define STEM_CH_MIN4	 8	/* ... for notes with four beams */
#define BEAM_DEPTH	3.2	/* width of a beam stroke (was 2.6) */
#define BEAM_OFFSET	0.25	/* pos of flat beam relative to staff line */
#define BEAM_SHIFT	5.0	/* shift of second and third beams (was 5.3) */
/*  To align the 4th beam as the 1st: shift=6-(depth-2*offset)/3  */
#define BEAM_FLATFAC	0.6	/* factor to decrease slope of long beams */
#define BEAM_THRESH	0.06	/* flat beam if slope below this threshold */
#define BEAM_SLOPE	0.5	/* max slope of a beam */
#define BEAM_STUB	6.0	/* length of stub for flag under beam */ 
#define SLUR_SLOPE	1.0	/* max slope of a slur */
#define DOTSHIFT	5	/* dot shift when up flag on note */
#define GSTEM		14	/* grace note stem length */
#define GSTEM_XOFF	1.6	/* x offset for grace note stem */
#define GSPACE0		12.0	/* space from grace note to big note */
#define GSPACE		8.0	/* space between grace notes */

#define BETA_C		0.1	/* max expansion for flag -c */
#define ALFA_X		1.0	/* max compression before complaining */
#define BETA_X		1.0	/* max expansion before complaining */

#define VOCPRE		0.4	/* portion of vocals word before note */
#define GCHPRE		0.4	/* portion of guitar chord before note */

/* -- Parameters for note spacing -- */
/* -- fnn multiplies the spacing under a beam, to compress the notes a bit
 */

#define fnnp 0.9

/* -- macros for program internals -- */

#define STRL1		256	/* string length for file names */
#define MAXSTAFF	16	/* max staves */
#define BSIZE		512	/* buffer size for one input string */
#define BUFFSZ		64000	/* size of output buffer */
#define BUFFSZ1		5000	/* buffer reserved for one staff */

#define BREVE		(BASE_LEN * 2)	/* double note (square note) */
#define SEMIBREVE	BASE_LEN	/* whole note */
#define MINIM		(BASE_LEN / 2)	/* half note (white note) */
#define CROTCHET 	(BASE_LEN / 4)	/* quarter note (black note) */
#define QUAVER		(BASE_LEN / 8)	/* 1/8 note */
#define SEMIQUAVER	(BASE_LEN / 16)	/* 1/16 note */

#define MAXFONTS	30	/* max number of fonts */
#define MAXENC		6	/* max number of ISO-Latin encodings */
#define ENC_NATIVE	7	/* (see format.c) */

#define T_LEFT		0
#define T_JUSTIFY	1
#define T_FILL		2
#define T_CENTER	3
#define T_SKIP		4
#define T_RIGHT		5

#define YSTEP	128		/* number of steps for y offsets */

extern unsigned char deco_glob[256], deco_tune[256];

struct FONTSPEC {
	int fnum;		/* index to font tables in format.c */
	float size;
	float swfac;
};
extern char font_enc[MAXFONTS];	/* font encoding */

/* lyrics */
#define MAXLY	16	/* max number of lyrics */
struct lyl {
	struct FONTSPEC *f;	/* font */
	float w;		/* width */
	float s;		/* shift / note */
	char t[1];		/* word */
};
struct lyrics {
	struct lyl *lyl[MAXLY];	/* ptr to lyric lines */
};

/* music element */
struct SYMBOL { 		/* struct for a drawable symbol */
	struct abcsym as;	/* abc symbol !!must be the first field!! */
	struct SYMBOL *next, *prev;	/* voice linkage */
	unsigned char type;	/* symbol type */
#define NO_TYPE		0	/* invalid type */
#define NOTE		1	/* valid symbol types */
#define REST		2
#define BAR		3
#define CLEF		4
#define TIMESIG 	5
#define KEYSIG		6
#define TEMPO		7
#define STAVES		8
#define MREST		9
#define PART		10
#define GRACE		11
#define FMTCHG		12
#define TUPLET		13
#define WHISTLE		14
	unsigned char seq;	/* sequence # - see parse.c */
	unsigned char voice;	/* voice (0..nvoice) */
	unsigned char staff;	/* staff (0..nstaff) */
	int dur;		/* main note duration */
	signed char pits[MAXHD]; /* pitches for notes */
	struct SYMBOL *ts_next, *ts_prev; /* time linkage */
	int time;		/* starting time */
	unsigned int sflags;	/* symbol flags */
#define S_EOLN		0x0001		/* end of line */
#define S_WORD_ST	0x0002		/* word starts here */
#define S_BEAM_BR1	0x0004		/* 2nd beam must restart here */
#define S_OTHER_HEAD	0x0008		/* don't draw any note head */
#define S_IN_TUPLET	0x0010		/* in a tuplet */
#define S_TREM		0x0020		/* tremolo (when note) */
#define S_RRBAR		0x0040		/* right repeat bar (when bar) */
#define S_XSTEM		0x0080		/* cross-staff stem (when note) */
#define S_NOREPBAR	0x0100		/* don't draw the repeat bar (when bar) */
#define S_BEAM_ON	0x0200		/* continue beaming */
#define S_SL1		0x0400		/* some chord slur start */
#define S_SL2		0x0800		/* some chord slur end */
#define S_TI1		0x1000		/* some chord tie start */
#define S_DYNUP		0x2000		/* dynamic marks above the staff */
#define S_DYNDN		0x4000		/* dynamic marks below the staff */
#define S_RBSTOP	0x8000		/* repeat bracket stop */
#define S_BEAM_BR2	0x00010000	/* 3rd beam must restart here */
	unsigned char nhd;	/* number of notes in chord - 1 */
	signed char stem;	/* 1 / -1 for stem up / down */
	signed char nflags;	/* number of note flags when > 0 */
	char dots;		/* number of dots */
	unsigned char head;	/* head type */
#define H_FULL		0
#define H_EMPTY 	1
#define H_OVAL		2
#define H_SQUARE	3
	signed char multi;	/* multi voice in the staff (+1, 0, -1) */
	signed char doty;	/* dot y pos when voices overlap */
	signed char nohdix;	/* no head index */
	unsigned char gcf;	/* font for guitar chords */
	unsigned char anf;	/* font for annotations */
	short u;		/* auxillary information:
				 *	- CLEF: small clef
				 *	- KEYSIG: old key signature
				 *	- BAR: new bar number
				 *	- TUPLET: tuplet format
				 *	- FMTCHG (format change): subtype */
#define STBRK 0				/* staff break
					 *	xmx: width */
#define PSSEQ 1				/* postscript sequence */
	float x;		/* x offset */
	short y;
	short ymn, ymx, yav;	/* min, max, avg note head height */
	float xmx;		/* max h-pos of a head rel to top */
	float xs, ys;		/* offset of stem end */
	float wl, wr;		/* left, right min width */
	float space;		/* natural space before symbol */
	float xmin, xmax;	/* min and max x offsets */
	float shhd[MAXHD];	/* horizontal shift for heads */
	float shac[MAXHD];	/* horizontal shift for accidentals */
	struct lyrics *ly;	/* lyrics */
	struct SYMBOL *grace;	/* grace notes */
};

/* bar types !tied to abcparse.h! */
#define B_SINGLE B_BAR		/* |	single bar */
#define B_DOUBLE 0x11		/* ||	thin double bar */
#define B_THIN_THICK 0x13	/* |]	thick at section end  */
#define B_THICK_THIN 0x21	/* [|	thick at section start */
#define B_LREP 0x14		/* |:	left repeat bar */
#define B_RREP 0x41		/* :|	right repeat bar */
#define B_DREP 0x44		/* ::	double repeat bar */
#define B_DASH 0x04		/* :	dashed bar */

extern unsigned short *micro_tb; /* ptr to the microtone table of the tune */

struct FORMAT { 		/* struct for page layout */
	float pageheight, pagewidth;
	float topmargin, botmargin, leftmargin, rightmargin;
	float topspace, wordsspace, titlespace, subtitlespace, partsspace;
	float composerspace, musicspace, staffsep, vocalspace, textspace;
	float scale, maxshrink, lineskipfac, parskipfac, sysstaffsep;
	float indent, infospace, slurheight, notespacingfactor;
	float maxstaffsep, maxsysstaffsep, stemheight;
	int abc2pscompat, alignbars, aligncomposer, autoclef;
	int barsperstaff, breathlow, bstemdown;
	int combinevoices, contbarnb, continueall, dynalign;
	int encoding, exprabove, exprbelow, flatbeams, freegchord;
	int infoline, gchordbox, graceslurs, comball, hyphencont;
	int landscape, measurebox, measurefirst, measurenb, musiconly;
	int oneperpage, partsbox, printparts, printtempo;
	int setdefl, shiftunisson, splittune, squarebreve, staffnonote;
	int straightflags, stretchstaff, stretchlast;
	int textoption, titlecaps, titleleft, titletrim, timewarn, tuplets;
	int vocalabove, withxrefs, writehistory;
	char *dateformat, *header, *footer, *titleformat;
#define FONT_UMAX 5		/* max number of user fonts */
#define ANNOTATIONFONT 5
#define COMPOSERFONT 6
#define FOOTERFONT 7
#define GCHORDFONT 8
#define HEADERFONT 9
#define HISTORYFONT 10
#define INFOFONT 11
#define MEASUREFONT 12
#define PARTSFONT 13
#define REPEATFONT 14
#define SUBTITLEFONT 15
#define TEMPOFONT 16
#define TEXTFONT 17
#define TITLEFONT 18
#define VOCALFONT 19
#define VOICEFONT 20
#define WORDSFONT 21
#define FONT_DYN 22		/* index of dynamic fonts (gch, an, ly) */
#define FONT_DYNX 12		/* number of dynamic fonts */
#define FONT_MAX (FONT_DYN+FONT_DYNX)		/* whole number of fonts */
	struct FONTSPEC font_tb[FONT_MAX];
	char ndfont;		/* current index of dynamic fonts */
	unsigned char gcf, anf, vof;	/* fonts for
				* guitar chords, annotations and lyrics */
};

extern struct FORMAT cfmt;	/* current local format for output */
extern struct FORMAT dfmt;	/* current global format for output */

struct ISTRUCT {		/* information fields */
	struct SYMBOL *area;
	struct SYMBOL *book;
	struct SYMBOL *comp;
	struct SYMBOL *disco;
	struct SYMBOL *histo;
	struct SYMBOL *notes;
	struct SYMBOL *orig;
	struct SYMBOL *parts;
	struct SYMBOL *rhythm;
	struct SYMBOL *src;
	struct SYMBOL *tempo;
	struct SYMBOL *title;
	char *xref;
	struct SYMBOL *trans;
};

extern struct ISTRUCT info, default_info;

extern char *mbf;		/* where to PUTx() */
extern int nbuf;		/* number of bytes buffered */
extern int use_buffer;		/* 1 if lines are being accumulated */

extern int outft;		/* last font in the output file */
extern int tunenum;		/* number of current tune */
extern int pagenum;		/* current page number */
extern int nbar;		/* current measure number */
extern int nbar_rep;		/* last repeat bar number */
extern int in_page;
extern int defl;		/* decoration flags */
#define DEF_NOST 0x01		/* long deco with no start */
#define DEF_NOEN 0x02		/* long deco with no end */
#define DEF_STEMUP 0x04		/* stem up (1) or down (0) */

		/* switches modified by flags: */
extern int pagenumbers; 	/* write page numbers */
extern int epsf;		/* for EPSF postscript output */
extern int showerror;		/* show the errors */

extern char outfn[STRL1];	/* output file name */
extern char *in_fname;		/* current input file name */
extern char *styd;		/* format search directory */
extern time_t mtime;		/* last modification time of the input file */

extern int file_initialized;	/* for output file */
extern FILE *fout;		/* output file */

#define MAXWHISTLE	4	/* max number of whistle tablature */
struct WHISTLE_S {
	short voice;		/* voice number */
	short pitch;		/* absolute key pitch */
};
extern struct WHISTLE_S whistle_tb[MAXWHISTLE];
extern int nwhistle;

extern int s_argc;		/* command line arguments */
extern char **s_argv;

struct STAFF_S {
	struct clef_s clef;	/* base clef */
	char flags[2];		/* brace and bracket flags (from %%staves) */
	char forced_clef;	/* explicit clef */
	char empty;		/* no symbol on this staff */
	short botbar, topbar;	/* bottom and top of bar */
	float y;		/* y position */
	float bar_height;	/* height of measure bars */
	float sep;		/* distance to the next staff */
	float maxsep;		/* max distance to the next staff */
	float top[YSTEP], bot[YSTEP];	/* top/bottom y offsets */
};
extern struct STAFF_S staff_tb[MAXSTAFF];
extern int nstaff;		/* (0..MAXSTAFF-1) */

struct VOICE_S {
	struct SYMBOL *sym;	/* associated symbols */
	struct SYMBOL *last_symbol;	/* last symbol while scanning */
	struct SYMBOL *s_anc;	/* ancillary symbol pointer */
	struct VOICE_S *next, *prev;	/* staff links */
	char *name;		/* voice id */
	char *nm;		/* voice name */
	char *snm;		/* voice subname */
	char *bar_text;		/* bar text at start of staff when bar_start */
	struct SYMBOL *tie;	/* note with ties of previous line */
	struct SYMBOL *rtie;	/* note with ties before 1st repeat bar */
	char *tabhead;		/* tablature:	PS head function */
	char *tabnote;		/*		note function */
	char *tabbar;		/*		bar function */
	float tabha;		/*		height above the staff */
	float tabhu;		/*		height under the staff */
	float scale;		/* scale */
	float sep;		/* distance to the next staff */
	float maxsep;		/* max distance to the next staff */
	int time;		/* current time while parsing */
	struct clef_s clef;	/* current clef */
	struct key_s key;	/* current key signature */
	struct meter_s meter;	/* current time signature */
	unsigned hy_st;		/* lyrics hyphens at start of line (bit array) */
	unsigned forced_clef:1;	/* explicit clef */
	unsigned second:1;	/* secondary voice in a brace/parenthesis */
	unsigned floating:1;	/* floating voice in a brace */
	unsigned selected:1;	/* selected while sorting by time (music.c) */
	unsigned bar_repeat:1;	/* bar at start of staff is a repeat bar */
	unsigned norepbra:1;	/* don't display the repeat brackets */
	unsigned have_ly:1;	/* some lyrics in this voice */
	unsigned whistle:1;	/* tin whistle for this voice */
	unsigned new_name:1;	/* redisplay the voice name */
	short wmeasure;		/* measure duration while parsing */
	signed char bar_start;	/* bar type at start of staff / 0 */
	signed char clone;	/* duplicate from this voice number */
	unsigned char staff;	/* staff (0..n-1) */
	unsigned char cstaff;	/* staff while parsing */
	signed char sfp;	/* key signature while parsing */
	signed char stem;	/* stem direction while parsing */
	signed char gstem;	/* grace stem direction while parsing */
	signed char dyn;	/* place of dynamic marks while parsing */
	signed char ly_pos;	/* place of lyrics (above / below) */
	unsigned char slur_st;	/* slurs at start of staff */
};
extern struct VOICE_S voice_tb[MAXVOICE]; /* voice table */
extern struct VOICE_S *first_voice; /* first_voice */

extern struct SYMBOL *tsnext;	/* next line when cut */
extern float realwidth;		/* real staff width while generating */

#define NFLAGS_SZ 10		/* size of note flags tables */
#define C_XFLAGS 5		/* index of crotchet in flags tables */
extern float space_tb[NFLAGS_SZ]; /* note spacing */

/* PUTn: add to buffer with n arguments */
#define PUT0(f) do {sprintf(mbf,f); a2b(); } while (0)
#define PUT1(f,a) do {sprintf(mbf,f,a); a2b(); } while (0)
#define PUT2(f,a,b) do {sprintf(mbf,f,a,b); a2b(); } while (0)
#define PUT3(f,a,b,c) do {sprintf(mbf,f,a,b,c); a2b(); } while (0)
#define PUT4(f,a,b,c,d) do {sprintf(mbf,f,a,b,c,d); a2b(); } while (0)
#define PUT5(f,a,b,c,d,e) do {sprintf(mbf,f,a,b,c,d,e); a2b(); } while (0)

/* -- external routines -- */
/* abc2ps.c */
void clrarena(int level);
void lvlarena(int level);
char *getarena(int len);
void strext(char *fid, char *ext);
/* buffer.c */
void a2b(void);
void abskip(float h);
void buffer_eob(void);
void bskip(float h);
void check_buffer(void);
void clear_buffer(void);
void close_output_file(void);
void close_page(void);
float get_bposy(void);
void write_buffer(void);
void open_output_file(void);
void write_eps(void);
/* deco.c */
void deco_add(char *text);
void deco_cnv(struct deco *dc, struct SYMBOL *s);
unsigned char deco_intern(unsigned char deco);
void deco_update(struct SYMBOL *s, float dx);
float deco_width(struct SYMBOL *s);
void draw_all_deco(void);
int draw_deco_head(int deco, float x, float y, int stem);
void draw_all_deco_head(struct SYMBOL *s, float x, float y);
void draw_deco_near(void);
void draw_deco_note(void);
void draw_deco_staff(void);
float draw_partempo(float top,
		    int any_part,
		    int any_tempo);
void draw_measnb(void);
void reset_deco(void);
void set_defl(int new_defl);
float tempo_width(struct SYMBOL *s);
void write_tempo(struct SYMBOL *s,
		int beat,
		float sc);
float y_get(struct SYMBOL *s,
	    int up,
	    float x,
	    float w,
	    float h);
void y_set(struct SYMBOL *s,
	   int up,
	   float x,
	   float w,
	   float y);
/* draw.c */
void draw_sym_near(void);
void draw_all_symb(void);
void draw_vname(float indent);
void draw_whistle(void);
void set_scale(int staff);
void putf(float f);
void putx(float x);
void puty(float y);
void putxy(float x, float y);
/* format.c */
void define_fonts(void);
void define_encodings(void);
int get_textopt(char *p);
void interpret_fmt_line(char *w, char *p, int lock);
void lock_fmt(void *fmt);
void make_font_list(void);
FILE *open_file(char *fn,
		char *ext,
		char *rfn);
void print_format(void);
int read_fmt_file(char *filename);
void set_format(void);
/* music.c */
void output_music(void);
void reset_gen(void);
/* parse.c */
extern float multicol_start;
struct SYMBOL *add_sym(struct VOICE_S *p_voice,
		       int type);
void voice_dup(void);
void do_tune(struct abctune *t,
	     int header_only);
void identify_note(struct SYMBOL *s,
		   int len,
		   int *p_head,
		   int *p_dots,
		   int *p_flags);
/* subs.c */
void bug(char *msg, int fatal);
void error(int sev, struct SYMBOL *s, char *fmt, ...);
float scan_u(char *str);
void add_to_text_block(char *s, int job);
float cwid(unsigned char c);
int get_str_font(void);
void put_history(void);
void put_words(struct SYMBOL *words);
void set_font(int ft);
void str_font(int ft);
#define A_LEFT -1
#define A_CENTER 0
#define A_RIGHT 1
void str_out(char *p, int action);
void put_str(char *str, int action);
float tex_str(char *s);
extern char tex_buf[];	/* result of tex_str() */
#define TEX_BUF_SZ 512
void user_ps_add(char *s);
void user_ps_write(void);
void write_title(struct SYMBOL *s);
void write_heading(struct abctune *t);
void write_user_ps(void);
void write_text_block(int job, int abc_state);
/* syms.c */
void define_encoding(int enc, char *ename);
void define_font(char *name, int num, int enc);
void define_symbols(void);
