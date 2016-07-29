/*++
 * Declarations for abcparse.c.
 *
 *-*/

#define MAXVOICE 32	/* max number of voices */

#define MAXHD	8	/* max heads in a chord */
#define MAXDC	45	/* max decorations per note/chord/bar */
#define MAXMICRO 32	/* max microtone values (5 bits in accs[]) */

#define BASE_LEN 1536	/* basic note length (semibreve or whole note - same as MIDI) */

/* accidentals */
enum accidentals {
	A_NULL,		/* none */
	A_SH,		/* sharp */
	A_NT,		/* natural */
	A_FT,		/* flat */
	A_DS,		/* double sharp */
	A_DF		/* double flat */
};

/* bar types - 4 bits per symbol */
#define B_BAR 1		/* | */
#define B_OBRA 2	/* [ */
#define B_CBRA 3	/* ] */
#define B_COL 4		/* : */

/* slur types (2 bits) */
#define SL_ABOVE 0x01
#define SL_BELOW 0x02
#define SL_AUTO 0x03

/* note structure */
struct deco {		/* decorations */
	char n;			/* whole number of decorations */
	char h;			/* start of head decorations */
	char s;			/* start of decorations from s: (d:) */
	unsigned char t[MAXDC];	/* decoration type */
};

struct note {		/* note or rest */
	signed char pits[MAXHD]; /* pitches */
	short lens[MAXHD];	/* note lengths (# pts in [1] if space) */
	unsigned char accs[MAXHD]; /* code for accidentals & index in micro_tb */
	unsigned char sl1[MAXHD]; /* slur start per head */
	char sl2[MAXHD];	/* number of slur end per head */
	char ti1[MAXHD];	/* flag to start tie here */
	unsigned char decs[MAXHD]; /* head decorations (index: 5 bits, len: 3 bits) */
	short chlen;		/* chord length */
	char nhd;		/* number of notes in chord - 1 */
	unsigned char slur_st;	/* slurs starting here (2 bits array) */
	char slur_end;		/* number of slurs ending here */
	signed char brhythm;	/* broken rhythm */
	struct deco dc;		/* decorations */
};

/* symbol definition */
struct abctune;
struct abcsym {
	struct abctune *tune;	/* tune */
	struct abcsym *next, *prev; /* next / previous symbol */
	char type;		/* symbol type */
#define ABC_T_NULL	0
#define ABC_T_INFO 	1		/* (text[0] gives the info type) */
#define ABC_T_PSCOM	2
#define ABC_T_CLEF	3
#define ABC_T_NOTE	4
#define ABC_T_REST	5
#define ABC_T_BAR	6
#define ABC_T_EOLN	7
#define ABC_T_INFO2	8		/* (info without header - H:) */
#define ABC_T_MREST	9		/* multi-measure rest */
#define ABC_T_MREP	10		/* measure repeat */
#define ABC_T_V_OVER	11		/* voice overlay */
#define ABC_T_TUPLET	12
	char state;		/* symbol state in file/tune */
#define ABC_S_GLOBAL 0			/* global */
#define ABC_S_HEAD 1			/* in header (after X:) */
#define ABC_S_TUNE 2			/* in tune (after K:) */
#define ABC_S_EMBED 3			/* embedded header (between [..]) */
	unsigned short colnum;	/* ABC source column number */
	unsigned short flags;
#define ABC_F_ERROR	0x0001		/* error around this symbol */
#define ABC_F_INVIS	0x0002		/* invisible symbol */
#define ABC_F_WORD_END	0x0004		/* 1 if word ends here */
#define ABC_F_STEMLESS	0x0008		/* note with no stem */
#define ABC_F_LYRIC_START 0x0010	/* may start a lyric here */
#define ABC_F_GRACE	0x0020		/* grace note */
#define ABC_F_GR_END	0x0040		/* end of grace note sequence */
#define ABC_F_SAPPO	0x0080		/* short appoggiatura */
#define ABC_F_DOTTED_SLUR 0x0100	/* dotted slur */
#define ABC_F_DOTTED_TIE 0x0200		/* dotted tie */
	unsigned short free;
	int linenum;		/* ABC source line number */
	char *text;		/* main text (INFO, PSCOM),
				 * guitar chord (NOTE, REST, BAR) */
	char *comment;		/* comment part (when keep_comment) */
	union {			/* type dependent part */
		struct key_s {		/* K: info */
			signed char sf;		/* sharp (> 0) flats (< 0) */
			char bagpipe;		/* HP or Hp */
			char minor;		/* major (0) / minor (1) */
			char empty;		/* clef alone if 1, 'none' if 2 */
			signed char nacc;	/* explicit accidentals */
			char pits[8];
			char accs[8];
		} key;
		struct {		/* L: info */
			int base_length;	/* basic note length */
		} length;
		struct meter_s {	/* M: info */
			short wmeasure;		/* duration of a measure */
			char nmeter;		/* number of meter elements */
			char expdur;		/* explicit measure duration */
#define MAX_MEASURE 6
			struct {
				char top[8];	/* top value */
				char bot[2];	/* bottom value */
			} meter[MAX_MEASURE];
		} meter;
		struct {		/* Q: info */
			char *str1;		/* string before */
			short length[4];	/* up to 4 note lengths */
			char *value;		/* tempo value */
			char *str2;		/* string after */
		} tempo;
		struct {		/* V: info */
			char *name;		/* name */
			char *fname;		/* full name */
			char *nname;		/* nick name */
			float scale;		/* != 0 when change */
			unsigned char voice;	/* voice number */
			char merge;		/* merge with previous voice */
			signed char stem;	/* have stems up or down (2 = auto) */
			signed char gstem;	/* have grace stems up or down (2 = auto) */
			signed char dyn;	/* have dynamic marks above or below the staff */
			signed char lyrics;	/* have lyrics above or below the staff */
		} voice;
		struct {		/* bar, mrest or mrep */
			struct deco dc;		/* decorations */
			int type;
			char repeat_bar;
			char len;		/* len if mrest or mrep */
			char dotted;
		} bar;
		struct clef_s {		/* clef (and staff!) */
			float staffscale;	/* != 0 when change */
			signed char stafflines;	/* >= 0 when change */
			signed char type;	/* no clef if < 0 */
#define TREBLE 0
#define ALTO 1
#define BASS 2
#define PERC 3
			char line;
			signed char octave;
			signed char transpose;
			char invis;
			char check_pitch;	/* check if old abc2ps transposition */
		} clef;
		struct note note;	/* note, rest */
		struct {		/* user defined accent */
			unsigned char symbol;
			unsigned char value;
		} user;
		struct {
			char type;	/* 0: end of line
					 * 1: continuation ('\')
					 * 2: line break ('!') */
		} eoln;
		struct staff_s {	/* %%staves */
			short voice;
			unsigned char flags[2];
#define OPEN_BRACE 0x01
#define CLOSE_BRACE 0x02
#define OPEN_BRACKET 0x04
#define CLOSE_BRACKET 0x08
#define OPEN_PARENTH 0x10
#define CLOSE_PARENTH 0x20
#define STOP_BAR 0x40
#define FL_VOICE 0x80
		} staves[MAXVOICE];
		struct {		/* voice overlay */
			char type;
#define V_OVER_V 0				/* & */
#define V_OVER_S 1				/* (& */
#define V_OVER_E 2				/* &) */
			unsigned char voice;
		} v_over;
		struct {		/* tuplet */
			char p_plet, q_plet, r_plet;
		} tuplet;
	} u;
};

/* tune definition */
struct abctune {
	struct abctune *next;	/* next tune */
	struct abctune *prev;	/* previous tune */
	struct abcsym *first_sym; /* first symbol */
	struct abcsym *last_sym; /* last symbol */
	int abc_vers;		/* ABC version */
	void *client_data;	/* client data */
	unsigned short micro_tb[MAXMICRO]; /* microtone values [ (n-1) | (d-1) ] */
};

#ifdef WIN32
#define strcasecmp stricmp
#define strncasecmp strnicmp
#endif

#if defined(__cplusplus)
extern "C" {
#endif
extern char *deco_tb[];
extern int severity;

void abc_delete(struct abcsym *as);
void abc_free(struct abctune *first_tune);
void abc_init(void *alloc_f_api(int size),
	      void free_f_api(void *ptr),
	      void level_f_api(int level),
	      int client_sz_api,
	      int keep_comment_api);
void abc_insert(char *file_api,
		struct abcsym *s);
struct abcsym *abc_new(struct abctune *t,
		       char *p,
		       char *comment);
struct abctune *abc_parse(char *file_api);
char *get_str(char *d,
	      char *s,
	      int maxlen);
char *parse_deco(char *p,
		 struct deco *deco);
#if defined(__cplusplus)
}
#endif
