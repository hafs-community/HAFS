
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with grib_yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 12 "griby.y"


#include "grib_api_internal.h"
/* #include "grib_parser.h" */

extern int grib_yylex();
extern int grib_yyerror(const char*);

extern   grib_action*           grib_parser_all_actions;
extern   grib_concept_value*    grib_parser_concept;
extern   grib_context*          grib_parser_context;
extern   grib_rule*             grib_parser_rules;

static grib_concept_value* reverse(grib_concept_value* r);
static grib_concept_value *reverse_concept(grib_concept_value *r,grib_concept_value *s);

/* typedef int (*testp_proc)(long,long); */
/* typedef long (*grib_op_proc)(long,long);   */




/* Line 189 of yacc.c  */
#line 96 "y.tab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum grib_yytokentype {
     LOWERCASE = 258,
     IF = 259,
     IF_TRANSIENT = 260,
     ELSE = 261,
     END = 262,
     UNSIGNED = 263,
     TEMPLATE = 264,
     TEMPLATE_NOFAIL = 265,
     TRIGGER = 266,
     ASCII = 267,
     KSEC1EXPVER = 268,
     LABEL = 269,
     LIST = 270,
     WHILE = 271,
     IBMFLOAT = 272,
     SIGNED = 273,
     UINT8 = 274,
     INT8 = 275,
     UINT16 = 276,
     INT16 = 277,
     UINT16_LITTLE_ENDIAN = 278,
     INT16_LITTLE_ENDIAN = 279,
     UINT32 = 280,
     INT32 = 281,
     UINT32_LITTLE_ENDIAN = 282,
     INT32_LITTLE_ENDIAN = 283,
     UINT64 = 284,
     INT64 = 285,
     UINT64_LITTLE_ENDIAN = 286,
     INT64_LITTLE_ENDIAN = 287,
     BLOB = 288,
     BYTE = 289,
     CODETABLE = 290,
     COMPLEX_CODETABLE = 291,
     LOOKUP = 292,
     ALIAS = 293,
     UNALIAS = 294,
     META = 295,
     POS = 296,
     INTCONST = 297,
     TRANS = 298,
     FLAGBIT = 299,
     CONCEPT = 300,
     GETENV = 301,
     CONCEPT_NOFAIL = 302,
     NIL = 303,
     DUMMY = 304,
     MODIFY = 305,
     READ_ONLY = 306,
     STRING_TYPE = 307,
     LONG_TYPE = 308,
     DOUBLE_TYPE = 309,
     NO_COPY = 310,
     DUMP = 311,
     NO_FAIL = 312,
     EDITION_SPECIFIC = 313,
     OVERRIDE = 314,
     HIDDEN = 315,
     CAN_BE_MISSING = 316,
     MISSING = 317,
     CONSTRAINT = 318,
     COPY_OK = 319,
     WHEN = 320,
     SET = 321,
     SET_NOFAIL = 322,
     WRITE = 323,
     APPEND = 324,
     PRINT = 325,
     EXPORT = 326,
     REMOVE = 327,
     SKIP = 328,
     PAD = 329,
     SECTION_PADDING = 330,
     MESSAGE = 331,
     PADTO = 332,
     PADTOEVEN = 333,
     PADTOMULTIPLE = 334,
     G1_HALF_BYTE = 335,
     G1_MESSAGE_LENGTH = 336,
     G1_SECTION4_LENGTH = 337,
     SECTION_LENGTH = 338,
     FLAG = 339,
     ITERATOR = 340,
     NEAREST = 341,
     BOX = 342,
     KSEC = 343,
     ASSERT = 344,
     CASE = 345,
     SWITCH = 346,
     DEFAULT = 347,
     EQ = 348,
     NE = 349,
     GE = 350,
     LE = 351,
     LT = 352,
     GT = 353,
     BIT = 354,
     BITOFF = 355,
     AND = 356,
     OR = 357,
     NOT = 358,
     IS = 359,
     IDENT = 360,
     STRING = 361,
     INTEGER = 362,
     FLOAT = 363
   };
#endif
/* Tokens.  */
#define LOWERCASE 258
#define IF 259
#define IF_TRANSIENT 260
#define ELSE 261
#define END 262
#define UNSIGNED 263
#define TEMPLATE 264
#define TEMPLATE_NOFAIL 265
#define TRIGGER 266
#define ASCII 267
#define KSEC1EXPVER 268
#define LABEL 269
#define LIST 270
#define WHILE 271
#define IBMFLOAT 272
#define SIGNED 273
#define UINT8 274
#define INT8 275
#define UINT16 276
#define INT16 277
#define UINT16_LITTLE_ENDIAN 278
#define INT16_LITTLE_ENDIAN 279
#define UINT32 280
#define INT32 281
#define UINT32_LITTLE_ENDIAN 282
#define INT32_LITTLE_ENDIAN 283
#define UINT64 284
#define INT64 285
#define UINT64_LITTLE_ENDIAN 286
#define INT64_LITTLE_ENDIAN 287
#define BLOB 288
#define BYTE 289
#define CODETABLE 290
#define COMPLEX_CODETABLE 291
#define LOOKUP 292
#define ALIAS 293
#define UNALIAS 294
#define META 295
#define POS 296
#define INTCONST 297
#define TRANS 298
#define FLAGBIT 299
#define CONCEPT 300
#define GETENV 301
#define CONCEPT_NOFAIL 302
#define NIL 303
#define DUMMY 304
#define MODIFY 305
#define READ_ONLY 306
#define STRING_TYPE 307
#define LONG_TYPE 308
#define DOUBLE_TYPE 309
#define NO_COPY 310
#define DUMP 311
#define NO_FAIL 312
#define EDITION_SPECIFIC 313
#define OVERRIDE 314
#define HIDDEN 315
#define CAN_BE_MISSING 316
#define MISSING 317
#define CONSTRAINT 318
#define COPY_OK 319
#define WHEN 320
#define SET 321
#define SET_NOFAIL 322
#define WRITE 323
#define APPEND 324
#define PRINT 325
#define EXPORT 326
#define REMOVE 327
#define SKIP 328
#define PAD 329
#define SECTION_PADDING 330
#define MESSAGE 331
#define PADTO 332
#define PADTOEVEN 333
#define PADTOMULTIPLE 334
#define G1_HALF_BYTE 335
#define G1_MESSAGE_LENGTH 336
#define G1_SECTION4_LENGTH 337
#define SECTION_LENGTH 338
#define FLAG 339
#define ITERATOR 340
#define NEAREST 341
#define BOX 342
#define KSEC 343
#define ASSERT 344
#define CASE 345
#define SWITCH 346
#define DEFAULT 347
#define EQ 348
#define NE 349
#define GE 350
#define LE 351
#define LT 352
#define GT 353
#define BIT 354
#define BITOFF 355
#define AND 356
#define OR 357
#define NOT 358
#define IS 359
#define IDENT 360
#define STRING 361
#define INTEGER 362
#define FLOAT 363




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 34 "griby.y"

    char                    *str;
    long                    lval;
    double                  dval;
    grib_darray             *dvalue;
    grib_iarray             *ivalue;
    grib_action             *act;
    grib_arguments          *explist;
    grib_expression         *exp;
    grib_concept_condition  *concept_condition;
    grib_concept_value      *concept_value;
	grib_case               *case_value;
  grib_rule               *rules;
  grib_rule_entry         *rule_entry;



/* Line 214 of yacc.c  */
#line 366 "y.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define grib_yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 378 "y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 grib_yytype_uint8;
#else
typedef unsigned char grib_yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 grib_yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char grib_yytype_int8;
#else
typedef short int grib_yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 grib_yytype_uint16;
#else
typedef unsigned short int grib_yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 grib_yytype_int16;
#else
typedef short int grib_yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int grib_yyi)
#else
static int
YYID (grib_yyi)
    int grib_yyi;
#endif
{
  return grib_yyi;
}
#endif

#if ! defined grib_yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined grib_yyoverflow || YYERROR_VERBOSE */


#if (! defined grib_yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union grib_yyalloc
{
  grib_yytype_int16 grib_yyss_alloc;
  YYSTYPE grib_yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union grib_yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (grib_yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T grib_yyi;				\
	  for (grib_yyi = 0; grib_yyi < (Count); grib_yyi++)	\
	    (To)[grib_yyi] = (From)[grib_yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T grib_yynewbytes;						\
	YYCOPY (&grib_yyptr->Stack_alloc, Stack, grib_yysize);			\
	Stack = &grib_yyptr->Stack_alloc;					\
	grib_yynewbytes = grib_yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	grib_yyptr += grib_yynewbytes / sizeof (*grib_yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  192
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1480

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  126
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  45
/* YYNRULES -- Number of rules.  */
#define YYNRULES  232
/* YYNRULES -- Number of states.  */
#define YYNSTATES  752

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   363

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? grib_yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const grib_yytype_uint8 grib_yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,   124,     2,     2,
     113,   114,   122,   125,   109,   120,   116,   123,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   119,   110,
       2,   115,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   111,     2,   112,   121,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   117,     2,   118,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const grib_yytype_uint16 grib_yyprhs[] =
{
       0,     0,     3,     5,     7,     9,    11,    13,    14,    16,
      20,    22,    26,    28,    31,    35,    38,    41,    43,    45,
      47,    49,    51,    53,    55,    57,    60,    62,    64,    66,
      70,    72,    80,    91,    99,   110,   118,   126,   134,   145,
     153,   161,   172,   180,   191,   200,   213,   224,   233,   242,
     253,   262,   272,   280,   283,   286,   291,   296,   301,   306,
     311,   316,   321,   326,   331,   336,   341,   346,   351,   356,
     361,   369,   376,   384,   387,   393,   399,   404,   411,   419,
     422,   428,   437,   446,   450,   456,   462,   468,   474,   481,
     485,   489,   493,   499,   502,   510,   515,   524,   535,   541,
     547,   553,   559,   562,   567,   571,   576,   581,   588,   593,
     596,   598,   604,   609,   612,   614,   620,   625,   628,   634,
     636,   644,   656,   664,   676,   683,   691,   703,   708,   713,
     716,   720,   722,   725,   727,   730,   732,   736,   738,   740,
     742,   744,   746,   748,   750,   752,   754,   756,   758,   760,
     762,   764,   766,   775,   783,   791,   798,   808,   821,   836,
     847,   862,   875,   884,   896,   903,   913,   926,   937,   952,
     965,   974,   986,   988,   991,   993,   996,  1001,  1012,  1022,
    1030,  1036,  1042,  1048,  1054,  1056,  1059,  1064,  1066,  1068,
    1070,  1072,  1074,  1076,  1078,  1082,  1085,  1089,  1094,  1098,
    1100,  1104,  1108,  1112,  1116,  1120,  1122,  1126,  1130,  1132,
    1136,  1140,  1144,  1148,  1152,  1156,  1160,  1163,  1165,  1169,
    1171,  1175,  1177,  1179,  1181,  1183,  1188,  1191,  1193,  1196,
    1198,  1206,  1208
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const grib_yytype_int16 grib_yyrhs[] =
{
     127,     0,    -1,   128,    -1,   149,    -1,   130,    -1,   170,
      -1,     1,    -1,    -1,   108,    -1,   129,   109,   108,    -1,
     107,    -1,   129,   109,   107,    -1,   131,    -1,   131,   130,
      -1,   131,   110,   130,    -1,   131,   110,    -1,   136,   110,
      -1,   137,    -1,   145,    -1,   146,    -1,   147,    -1,   148,
      -1,   138,    -1,   152,    -1,   110,    -1,   132,   110,    -1,
     128,    -1,   134,    -1,   135,    -1,   135,   109,   134,    -1,
     164,    -1,     8,   111,   107,   112,   105,   141,   142,    -1,
       8,   111,   107,   112,   105,   111,   133,   112,   141,   142,
      -1,     8,   113,   107,   114,   105,   141,   142,    -1,     8,
     113,   107,   114,   105,   111,   133,   112,   141,   142,    -1,
      12,   111,   107,   112,   105,   141,   142,    -1,    12,   111,
     107,   112,   106,   141,   142,    -1,    34,   111,   107,   112,
     105,   141,   142,    -1,    34,   111,   107,   112,   105,   111,
     133,   112,   141,   142,    -1,    13,   111,   107,   112,   105,
     141,   142,    -1,    18,   111,   107,   112,   105,   141,   142,
      -1,    18,   111,   107,   112,   105,   111,   133,   112,   141,
     142,    -1,    18,   113,   107,   114,   105,   141,   142,    -1,
      18,   113,   107,   114,   105,   111,   133,   112,   141,   142,
      -1,    35,   111,   107,   112,   105,   135,   141,   142,    -1,
      35,   111,   107,   112,   105,   135,   141,    66,   113,   105,
     114,   142,    -1,    35,   111,   107,   112,   105,   113,   133,
     114,   141,   142,    -1,   105,   115,    46,   113,   133,   114,
     141,   142,    -1,    36,   111,   107,   112,   105,   135,   141,
     142,    -1,    36,   111,   107,   112,   105,   113,   133,   114,
     141,   142,    -1,    84,   111,   107,   112,   105,   135,   141,
     142,    -1,    37,   111,   107,   112,   105,   113,   133,   114,
     142,    -1,    44,   105,   113,   133,   114,   141,   142,    -1,
      14,   105,    -1,    14,   106,    -1,    17,   105,   141,   142,
      -1,    20,   105,   141,   142,    -1,    19,   105,   141,   142,
      -1,    22,   105,   141,   142,    -1,    21,   105,   141,   142,
      -1,    24,   105,   141,   142,    -1,    23,   105,   141,   142,
      -1,    26,   105,   141,   142,    -1,    25,   105,   141,   142,
      -1,    28,   105,   141,   142,    -1,    27,   105,   141,   142,
      -1,    30,   105,   141,   142,    -1,    29,   105,   141,   142,
      -1,    32,   105,   141,   142,    -1,    31,   105,   141,   142,
      -1,    33,   105,   111,   133,   112,   141,   142,    -1,    17,
     105,   116,   105,   141,   142,    -1,    17,   105,   111,   135,
     112,   141,   142,    -1,    41,   105,    -1,    42,   105,   115,
     135,   142,    -1,    43,   105,   115,   135,   142,    -1,   108,
     105,   141,   142,    -1,   108,   105,   116,   105,   141,   142,
      -1,   108,   105,   111,   135,   112,   141,   142,    -1,    80,
     105,    -1,    83,   111,   107,   112,   105,    -1,    81,   111,
     107,   112,   105,   113,   133,   114,    -1,    82,   111,   107,
     112,   105,   113,   133,   114,    -1,    88,   105,   135,    -1,
      74,   105,   113,   133,   114,    -1,    77,   105,   113,   133,
     114,    -1,    78,   105,   113,   133,   114,    -1,    79,   105,
     113,   133,   114,    -1,    76,   111,   107,   112,   105,   142,
      -1,    75,   105,   142,    -1,     9,   105,   106,    -1,    10,
     105,   106,    -1,    38,   105,   115,   105,   142,    -1,    39,
     105,    -1,    38,   105,   116,   105,   115,   105,   142,    -1,
      39,   105,   116,   105,    -1,    40,   105,   105,   113,   133,
     114,   141,   142,    -1,    40,   105,   116,   105,   105,   113,
     133,   114,   141,   142,    -1,    85,   105,   113,   133,   114,
      -1,    86,   105,   113,   133,   114,    -1,    87,   105,   113,
     133,   114,    -1,    71,   105,   113,   133,   114,    -1,    72,
     133,    -1,    89,   113,   164,   114,    -1,    50,   105,   142,
      -1,    66,   105,   115,    62,    -1,    66,   105,   115,   164,
      -1,    66,   105,   115,   117,   129,   118,    -1,    67,   105,
     115,   164,    -1,    68,   106,    -1,    68,    -1,    68,   113,
     107,   114,   106,    -1,    68,   113,   107,   114,    -1,    69,
     106,    -1,    69,    -1,    69,   113,   107,   114,   106,    -1,
      69,   113,   107,   114,    -1,    70,   106,    -1,    70,   113,
     106,   114,   106,    -1,    70,    -1,     4,   113,   164,   114,
     117,   130,   118,    -1,     4,   113,   164,   114,   117,   130,
     118,     6,   117,   130,   118,    -1,     5,   113,   164,   114,
     117,   130,   118,    -1,     5,   113,   164,   114,   117,   130,
     118,     6,   117,   130,   118,    -1,    65,   113,   164,   114,
     139,   132,    -1,    65,   113,   164,   114,   117,   140,   118,
      -1,    65,   113,   164,   114,   117,   140,   118,     6,   117,
     140,   118,    -1,    66,   105,   115,   164,    -1,    67,   105,
     115,   164,    -1,   139,   132,    -1,   140,   139,   132,    -1,
     128,    -1,   115,   133,    -1,   128,    -1,   119,   143,    -1,
     144,    -1,   143,   109,   144,    -1,    51,    -1,     3,    -1,
      56,    -1,    55,    -1,    57,    -1,    60,    -1,    58,    -1,
      61,    -1,    63,    -1,    59,    -1,    64,    -1,    43,    -1,
      52,    -1,    53,    -1,    54,    -1,   105,    15,   113,   164,
     114,   117,   130,   118,    -1,    16,   113,   164,   114,   117,
     130,   118,    -1,    11,   113,   133,   114,   117,   130,   118,
      -1,    45,   105,   117,   149,   118,   142,    -1,    45,   105,
     113,   105,   114,   117,   149,   118,   142,    -1,    45,   105,
     113,   105,   109,   106,   109,   105,   109,   105,   114,   142,
      -1,    45,   105,   113,   105,   109,   106,   109,   105,   109,
     105,   109,   105,   114,   142,    -1,    45,   105,   113,   105,
     109,   106,   109,   105,   114,   142,    -1,    45,   105,   116,
     105,   113,   105,   109,   106,   109,   105,   109,   105,   114,
     142,    -1,    45,   105,   116,   105,   113,   105,   109,   106,
     109,   105,   114,   142,    -1,    45,   105,   116,   105,   117,
     149,   118,   142,    -1,    45,   105,   116,   105,   113,   105,
     114,   117,   149,   118,   142,    -1,    47,   105,   117,   149,
     118,   142,    -1,    47,   105,   113,   105,   114,   117,   149,
     118,   142,    -1,    47,   105,   113,   105,   109,   106,   109,
     105,   109,   105,   114,   142,    -1,    47,   105,   113,   105,
     109,   106,   109,   105,   114,   142,    -1,    47,   105,   116,
     105,   113,   105,   109,   106,   109,   105,   109,   105,   114,
     142,    -1,    47,   105,   116,   105,   113,   105,   109,   106,
     109,   105,   114,   142,    -1,    47,   105,   116,   105,   117,
     149,   118,   142,    -1,    47,   105,   116,   105,   113,   105,
     114,   117,   149,   118,   142,    -1,   153,    -1,   149,   153,
      -1,   151,    -1,   150,   151,    -1,    90,   134,   119,   130,
      -1,    91,   113,   133,   114,   117,   150,    92,   119,   130,
     118,    -1,    91,   113,   133,   114,   117,   150,    92,   119,
     118,    -1,    91,   113,   133,   114,   117,   150,   118,    -1,
     106,   115,   117,   154,   118,    -1,   105,   115,   117,   154,
     118,    -1,   107,   115,   117,   154,   118,    -1,   108,   115,
     117,   154,   118,    -1,   155,    -1,   155,   154,    -1,   105,
     115,   164,   110,    -1,   105,    -1,   106,    -1,   156,    -1,
     107,    -1,   108,    -1,    48,    -1,    49,    -1,   113,   164,
     114,    -1,   120,   157,    -1,   105,   113,   114,    -1,   105,
     113,   133,   114,    -1,   157,   121,   158,    -1,   157,    -1,
     159,   122,   158,    -1,   159,   123,   158,    -1,   159,   124,
     158,    -1,   159,    99,   158,    -1,   159,   100,   158,    -1,
     158,    -1,   160,   125,   159,    -1,   160,   120,   159,    -1,
     159,    -1,   161,    98,   160,    -1,   161,    93,   160,    -1,
     161,    97,   160,    -1,   161,    95,   160,    -1,   161,    96,
     160,    -1,   161,    94,   160,    -1,   156,   104,   156,    -1,
     103,   161,    -1,   160,    -1,   162,   101,   161,    -1,   161,
      -1,   163,   102,   162,    -1,   162,    -1,   163,    -1,   168,
      -1,   169,    -1,   105,   115,   164,   110,    -1,    73,   110,
      -1,   166,    -1,   166,   167,    -1,   166,    -1,     4,   113,
     164,   114,   117,   167,   118,    -1,   165,    -1,   165,   170,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const grib_yytype_uint16 grib_yyrline[] =
{
       0,   231,   231,   232,   233,   234,   236,   239,   242,   243,
     244,   245,   247,   248,   249,   250,   253,   254,   255,   256,
     257,   258,   259,   260,   264,   265,   268,   269,   272,   273,
     276,   280,   283,   286,   289,   292,   296,   299,   302,   305,
     308,   311,   314,   317,   320,   323,   327,   330,   333,   336,
     339,   342,   345,   348,   351,   354,   358,   361,   364,   367,
     370,   373,   376,   379,   382,   385,   388,   391,   394,   397,
     400,   404,   407,   410,   413,   416,   419,   422,   425,   428,
     431,   434,   437,   440,   443,   446,   449,   452,   455,   458,
     460,   462,   465,   468,   471,   475,   479,   482,   485,   497,
     509,   521,   524,   527,   530,   533,   534,   535,   537,   540,
     541,   542,   543,   544,   545,   546,   547,   549,   550,   551,
     555,   556,   557,   558,   562,   563,   564,   567,   568,   571,
     572,   576,   577,   580,   581,   584,   585,   588,   589,   590,
     591,   592,   593,   594,   595,   596,   597,   598,   599,   600,
     601,   602,   605,   608,   611,   614,   615,   616,   617,   618,
     619,   620,   621,   622,   623,   624,   625,   626,   627,   628,
     629,   630,   634,   635,   638,   639,   642,   646,   647,   648,
     651,   653,   655,   657,   661,   662,   665,   668,   669,   672,
     673,   674,   676,   677,   678,   679,   680,   681,   685,   686,
     689,   690,   691,   692,   693,   694,   697,   698,   699,   702,
     704,   705,   706,   707,   708,   709,   714,   715,   718,   719,
     722,   723,   726,   732,   733,   736,   737,   740,   741,   744,
     748,   751,   752
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const grib_yytname[] =
{
  "$end", "error", "$undefined", "LOWERCASE", "IF", "IF_TRANSIENT",
  "ELSE", "END", "UNSIGNED", "TEMPLATE", "TEMPLATE_NOFAIL", "TRIGGER",
  "ASCII", "KSEC1EXPVER", "LABEL", "LIST", "WHILE", "IBMFLOAT", "SIGNED",
  "UINT8", "INT8", "UINT16", "INT16", "UINT16_LITTLE_ENDIAN",
  "INT16_LITTLE_ENDIAN", "UINT32", "INT32", "UINT32_LITTLE_ENDIAN",
  "INT32_LITTLE_ENDIAN", "UINT64", "INT64", "UINT64_LITTLE_ENDIAN",
  "INT64_LITTLE_ENDIAN", "BLOB", "BYTE", "CODETABLE", "COMPLEX_CODETABLE",
  "LOOKUP", "ALIAS", "UNALIAS", "META", "POS", "INTCONST", "TRANS",
  "FLAGBIT", "CONCEPT", "GETENV", "CONCEPT_NOFAIL", "NIL", "DUMMY",
  "MODIFY", "READ_ONLY", "STRING_TYPE", "LONG_TYPE", "DOUBLE_TYPE",
  "NO_COPY", "DUMP", "NO_FAIL", "EDITION_SPECIFIC", "OVERRIDE", "HIDDEN",
  "CAN_BE_MISSING", "MISSING", "CONSTRAINT", "COPY_OK", "WHEN", "SET",
  "SET_NOFAIL", "WRITE", "APPEND", "PRINT", "EXPORT", "REMOVE", "SKIP",
  "PAD", "SECTION_PADDING", "MESSAGE", "PADTO", "PADTOEVEN",
  "PADTOMULTIPLE", "G1_HALF_BYTE", "G1_MESSAGE_LENGTH",
  "G1_SECTION4_LENGTH", "SECTION_LENGTH", "FLAG", "ITERATOR", "NEAREST",
  "BOX", "KSEC", "ASSERT", "CASE", "SWITCH", "DEFAULT", "EQ", "NE", "GE",
  "LE", "LT", "GT", "BIT", "BITOFF", "AND", "OR", "NOT", "IS", "IDENT",
  "STRING", "INTEGER", "FLOAT", "','", "';'", "'['", "']'", "'('", "')'",
  "'='", "'.'", "'{'", "'}'", "':'", "'-'", "'^'", "'*'", "'/'", "'%'",
  "'+'", "$accept", "all", "empty", "dvalues", "instructions",
  "instruction", "semi", "argument_list", "arguments", "argument",
  "simple", "if_block", "when_block", "set", "set_list", "default",
  "flags", "flag_list", "flag", "list_block", "while_block",
  "trigger_block", "concept_block", "concept_list", "case_list",
  "case_value", "switch_block", "concept_value", "concept_conditions",
  "concept_condition", "string_or_ident", "atom", "power", "factor",
  "term", "condition", "conjonction", "disjonction", "expression", "rule",
  "rule_entry", "rule_entries", "fact", "conditional_rule", "rules", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const grib_yytype_uint16 grib_yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,    44,
      59,    91,    93,    40,    41,    61,    46,   123,   125,    58,
      45,    94,    42,    47,    37,    43
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const grib_yytype_uint8 grib_yyr1[] =
{
       0,   126,   127,   127,   127,   127,   127,   128,   129,   129,
     129,   129,   130,   130,   130,   130,   131,   131,   131,   131,
     131,   131,   131,   131,   132,   132,   133,   133,   134,   134,
     135,   136,   136,   136,   136,   136,   136,   136,   136,   136,
     136,   136,   136,   136,   136,   136,   136,   136,   136,   136,
     136,   136,   136,   136,   136,   136,   136,   136,   136,   136,
     136,   136,   136,   136,   136,   136,   136,   136,   136,   136,
     136,   136,   136,   136,   136,   136,   136,   136,   136,   136,
     136,   136,   136,   136,   136,   136,   136,   136,   136,   136,
     136,   136,   136,   136,   136,   136,   136,   136,   136,   136,
     136,   136,   136,   136,   136,   136,   136,   136,   136,   136,
     136,   136,   136,   136,   136,   136,   136,   136,   136,   136,
     137,   137,   137,   137,   138,   138,   138,   139,   139,   140,
     140,   141,   141,   142,   142,   143,   143,   144,   144,   144,
     144,   144,   144,   144,   144,   144,   144,   144,   144,   144,
     144,   144,   145,   146,   147,   148,   148,   148,   148,   148,
     148,   148,   148,   148,   148,   148,   148,   148,   148,   148,
     148,   148,   149,   149,   150,   150,   151,   152,   152,   152,
     153,   153,   153,   153,   154,   154,   155,   156,   156,   157,
     157,   157,   157,   157,   157,   157,   157,   157,   158,   158,
     159,   159,   159,   159,   159,   159,   160,   160,   160,   161,
     161,   161,   161,   161,   161,   161,   161,   161,   162,   162,
     163,   163,   164,   165,   165,   166,   166,   167,   167,   168,
     169,   170,   170
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const grib_yytype_uint8 grib_yyr2[] =
{
       0,     2,     1,     1,     1,     1,     1,     0,     1,     3,
       1,     3,     1,     2,     3,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     1,     3,
       1,     7,    10,     7,    10,     7,     7,     7,    10,     7,
       7,    10,     7,    10,     8,    12,    10,     8,     8,    10,
       8,     9,     7,     2,     2,     4,     4,     4,     4,     4,
       4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
       7,     6,     7,     2,     5,     5,     4,     6,     7,     2,
       5,     8,     8,     3,     5,     5,     5,     5,     6,     3,
       3,     3,     5,     2,     7,     4,     8,    10,     5,     5,
       5,     5,     2,     4,     3,     4,     4,     6,     4,     2,
       1,     5,     4,     2,     1,     5,     4,     2,     5,     1,
       7,    11,     7,    11,     6,     7,    11,     4,     4,     2,
       3,     1,     2,     1,     2,     1,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     8,     7,     7,     6,     9,    12,    14,    10,
      14,    12,     8,    11,     6,     9,    12,    10,    14,    12,
       8,    11,     1,     2,     1,     2,     4,    10,     9,     7,
       5,     5,     5,     5,     1,     2,     4,     1,     1,     1,
       1,     1,     1,     1,     3,     2,     3,     4,     3,     1,
       3,     3,     3,     3,     3,     1,     3,     3,     1,     3,
       3,     3,     3,     3,     3,     3,     2,     1,     3,     1,
       3,     1,     1,     1,     1,     4,     2,     1,     2,     1,
       7,     1,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const grib_yytype_uint8 grib_yydefact[] =
{
       0,     6,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,   114,   119,     0,
       7,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     2,     4,    12,     0,    17,    22,
      18,    19,    20,    21,     3,    23,   172,   231,   229,   223,
     224,     5,     0,     0,     0,     0,     0,     0,     7,     0,
       0,    53,    54,     0,     7,     0,     0,     7,     7,     7,
       7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
       7,     0,     0,     0,     0,     0,     0,    93,     0,    73,
       0,     0,     0,     0,     0,     7,     0,     0,     0,   109,
       0,   113,     0,   117,     0,     0,   192,   193,     0,   187,
     188,   190,   191,     0,     0,    26,   102,    27,    28,   189,
     199,   205,   208,   217,   219,   221,   222,    30,   226,     0,
       7,     0,     0,     0,     0,    79,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     7,     0,     0,     0,     0,
       7,     0,     1,     0,     0,     0,    15,    13,    16,     0,
       0,   173,     0,     0,   232,     0,     0,     0,     0,    90,
      91,     0,     0,     0,     0,     0,     7,     0,   131,     7,
       0,     0,     7,     7,     7,     7,     7,     7,     7,     7,
       7,     7,     7,     7,     7,     7,     7,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     7,     0,
       0,     0,     0,     0,     0,     0,   133,   104,     0,     0,
       0,     0,     0,     0,     7,   216,     0,     0,   189,   195,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     7,    89,
       0,     7,     7,     7,     0,     0,     0,     0,     7,     7,
       7,    83,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     7,     0,     0,     0,    14,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   132,
       7,    55,     0,     0,    57,    56,    59,    58,    61,    60,
      63,    62,    65,    64,    67,    66,    69,    68,     0,     0,
       0,     0,     0,     7,     0,    95,     7,     0,     7,     7,
       0,     0,     0,     0,     0,     0,     0,   138,   148,   137,
     149,   150,   151,   140,   139,   141,   143,   146,   142,   144,
     145,   147,   134,   135,     0,   105,     0,   106,   108,   112,
     116,     0,     0,   196,     0,   194,    29,   187,   215,   198,
     203,   204,   200,   201,   202,   207,   206,   210,   214,   212,
     213,   211,   209,   218,   220,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   103,     0,     0,
       7,     0,     0,   184,   225,     0,     0,     0,     7,    76,
       0,     0,     0,     0,     0,     7,     7,     0,     7,     7,
       7,     0,     7,     7,     7,     7,     7,     7,     0,     0,
       0,    92,     0,     0,     0,    74,    75,     7,     0,     0,
       0,     0,     7,     0,     0,     0,     0,     7,     0,     0,
       0,     0,     0,    10,     8,     0,   111,   115,   118,   101,
     197,    84,     7,    85,    86,    87,     0,     0,    80,     0,
      98,    99,   100,     0,     0,     0,     0,   181,   185,   180,
     182,     7,     7,   183,     0,     0,     0,     0,   227,     0,
       0,     7,     7,     7,     7,     0,     7,     7,     7,     0,
       7,    71,     7,     7,     7,     7,     7,     7,     7,     7,
       7,     7,     7,     7,     7,     7,     7,     7,     0,     0,
       0,     0,   155,     0,     0,     0,     0,   164,   136,     0,
       0,     0,     0,    24,   124,     0,   107,    88,     7,     7,
       7,     0,     0,   174,     0,     7,     0,     7,    77,     0,
       0,     0,   120,   228,   230,   122,     0,    31,     0,    33,
     154,    35,    36,    39,   153,    72,     0,    40,     0,    42,
      70,     0,    37,     0,    30,     7,     0,     7,     0,    94,
       7,     0,    52,     0,     0,     0,     0,     7,     0,     0,
       0,     0,     7,     0,     0,   129,   125,     0,    25,    11,
       9,     0,     0,     7,     0,     0,   179,   175,     0,     7,
     186,    78,     0,     0,     7,     7,     7,     7,     7,     7,
       0,    44,     7,    48,     7,    96,     7,     0,     7,     0,
       0,   162,     0,     7,     0,     0,   170,   127,   128,     0,
     130,    81,    82,    50,     0,     0,   152,    47,     0,     0,
       7,     7,     7,     7,     7,     7,     0,     7,    51,     7,
       0,     7,   156,     0,     0,     0,     7,   165,     0,     0,
       0,   176,   178,     0,     0,     0,    32,    34,    41,    43,
      38,    46,     0,    49,    97,     0,   159,     0,     7,     0,
     167,     0,     7,     0,   177,   121,   123,     7,     0,     7,
       0,     7,   163,     7,     0,     7,   171,   126,    45,     0,
     157,     0,   161,   166,     0,   169,     7,     7,     7,   158,
     160,   168
};

/* YYDEFGOTO[NTERM-NUM].  */
static const grib_yytype_int16 grib_yydefgoto[] =
{
      -1,    73,   256,   485,   517,    76,   564,   156,   157,   158,
      77,    78,    79,   561,   562,   219,   257,   382,   383,    80,
      81,    82,    83,    84,   572,   573,    85,    86,   432,   433,
     159,   160,   161,   162,   163,   164,   165,   166,   167,    87,
     518,   519,    89,    90,    91
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -547
static const grib_yytype_int16 grib_yypact[] =
{
    1058,  -547,   -95,   -89,    80,   -64,   -17,   -36,   -67,   -20,
      68,    28,     5,   159,    54,    60,    89,    97,   116,   145,
     160,   172,   187,   189,   196,   199,   205,   207,   213,   176,
     223,   225,   229,   255,   256,   257,   262,   265,   267,   277,
     278,   279,   280,   273,   284,   299,   -61,   -34,   -23,   315,
     161,   311,   318,   324,   319,   326,   327,   329,   330,   325,
     328,   331,   332,   333,   336,   341,   343,   345,   347,    -6,
      67,   339,   -72,   465,  -547,  -547,  1163,   356,  -547,  -547,
    -547,  -547,  -547,  -547,   272,  -547,  -547,     1,  -547,  -547,
    -547,  -547,   161,   161,   360,   362,   364,   365,   161,   368,
     369,  -547,  -547,   161,    -3,   370,   371,   357,   357,   357,
     357,   357,   357,   357,   357,   357,   357,   357,   357,   357,
     357,   373,   372,   374,   375,   379,    69,   392,   -82,  -547,
     396,   397,   400,   103,   120,   395,   161,   401,   402,  -547,
     380,  -547,   408,  -547,   413,   407,  -547,  -547,   161,   409,
    -547,  -547,  -547,   161,   320,  -547,  -547,  -547,   414,   420,
     411,  -547,   -69,   -25,   249,   428,   431,  -547,  -547,   421,
     395,   430,   422,   425,   427,  -547,   435,   436,   438,   444,
     441,   442,   443,   161,   161,   161,   445,   -21,   440,   447,
      22,   448,  -547,   449,    14,   454,  1372,  -547,  -547,   451,
     455,  -547,   450,   456,  -547,   458,   459,   457,   464,  -547,
    -547,   466,   467,   469,   470,   161,   161,   478,  -547,   395,
     473,   472,   395,   395,   395,   395,   395,   395,   395,   395,
     395,   395,   395,   395,   395,   395,   161,   476,   479,   480,
     481,   485,   489,   490,   483,   492,   161,   161,   161,   494,
     496,   272,   497,   499,   272,   446,  -547,  -547,   500,   121,
     161,   501,   503,   504,   161,   249,   139,   505,  -547,  -547,
     161,   134,   320,   320,   320,   320,   320,   320,   320,   320,
     320,   320,   320,   320,   320,   320,   161,   161,   161,  -547,
     498,   161,   161,   161,   508,   510,   511,   512,   161,   161,
     161,  -547,   513,   515,   161,   493,   520,   516,   520,   520,
     161,   525,   395,   520,   161,   514,  -547,   491,   161,   161,
     519,   521,   526,   527,   522,   170,   528,   524,   530,  -547,
     357,  -547,   532,   538,  -547,  -547,  -547,  -547,  -547,  -547,
    -547,  -547,  -547,  -547,  -547,  -547,  -547,  -547,   534,   539,
     542,   543,   544,   395,   535,  -547,   161,   552,   395,   395,
     547,    -7,   -19,   -57,    25,   143,   -43,  -547,  -547,  -547,
    -547,  -547,  -547,  -547,  -547,  -547,  -547,  -547,  -547,  -547,
    -547,  -547,   452,  -547,   -28,  -547,   177,  -547,  -547,   507,
     556,   558,   551,  -547,   553,  -547,  -547,  -547,  -547,  -547,
    -547,  -547,  -547,  -547,  -547,   -69,   -69,   -25,   -25,   -25,
     -25,   -25,   -25,   249,   428,   554,   561,   555,   557,   559,
     565,   567,   571,   572,   568,   569,   570,  -547,   541,   573,
     161,   566,   575,   520,  -547,   579,   581,   574,   357,  -547,
     582,   577,   588,  1270,  1372,   -55,   146,  1372,   357,   357,
     357,  1372,   357,   395,   182,   200,   357,   220,   183,   268,
     576,  -547,   580,   591,   594,  -547,  -547,   357,   602,   592,
     583,   272,   395,   604,   596,   606,   272,   395,   446,   609,
     611,    -8,   607,  -547,  -547,   -40,  -547,  -547,  -547,  -547,
    -547,  -547,   395,  -547,  -547,  -547,   605,   608,  -547,   161,
    -547,  -547,  -547,   629,   603,   610,   161,  -547,  -547,  -547,
    -547,   357,   395,  -547,   612,   613,    17,   614,   -59,   615,
     616,   161,   395,   161,   395,   617,   395,   395,   395,   619,
     395,  -547,   161,   395,   161,   395,   395,   161,   395,   161,
     357,   161,   357,   161,   395,   357,   161,   395,   618,   272,
      26,    71,  -547,   622,   272,    57,    93,  -547,  -547,   623,
     624,   607,   -47,  -547,   630,   217,  -547,  -547,   161,   161,
     357,   161,   -50,  -547,  1372,   357,   631,   395,  -547,  1372,
     -59,    55,   716,  -547,  -547,   717,   637,  -547,   639,  -547,
    -547,  -547,  -547,  -547,  -547,  -547,   644,  -547,   645,  -547,
    -547,   646,  -547,   628,   505,   -49,   647,   395,   648,  -547,
     395,   649,  -547,   620,   100,   653,   643,   395,   621,   107,
     658,   650,   395,   161,   161,   630,   722,   607,  -547,  -547,
    -547,   652,   654,   395,   651,   655,  -547,  -547,   659,   395,
    -547,  -547,   662,   663,   357,   357,   357,   357,   357,   357,
     656,  -547,   357,  -547,   395,  -547,   357,    58,   395,   667,
     272,  -547,    72,   395,   672,   272,  -547,  -547,  -547,   665,
     630,  -547,  -547,  -547,  1372,   947,  -547,  -547,  1372,  1372,
     395,   395,   395,   395,   395,   395,   666,   395,  -547,   395,
     678,   395,  -547,   679,   215,   681,   395,  -547,   682,   221,
      -8,  -547,  -547,   670,   671,   673,  -547,  -547,  -547,  -547,
    -547,  -547,   680,  -547,  -547,    81,  -547,   108,   395,   683,
    -547,   140,   395,   -45,  -547,  -547,  -547,   395,   687,   395,
     688,   395,  -547,   395,   690,   395,  -547,  -547,  -547,   684,
    -547,   685,  -547,  -547,   686,  -547,   395,   395,   395,  -547,
    -547,  -547
};

/* YYPGOTO[NTERM-NUM].  */
static const grib_yytype_int16 grib_yypgoto[] =
{
    -547,  -547,     7,  -547,     0,  -547,  -546,    66,  -262,  -180,
    -547,  -547,  -547,  -382,    96,   298,   168,  -547,   294,  -547,
    -547,  -547,  -547,  -241,  -547,   230,  -547,   -83,  -297,  -547,
    -128,   657,    76,    77,   210,  -144,   517,  -547,   -56,  -547,
       6,   285,  -547,  -547,   718
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -8
static const grib_yytype_int16 grib_yytable[] =
{
      75,   201,   482,   301,   265,   202,    88,    74,   396,   186,
     363,   435,   436,   366,    51,   625,   440,   650,    92,   479,
     480,   479,   480,   244,    93,   305,   268,   146,   147,   186,
     273,   274,   186,   190,   245,   328,   205,   206,   479,   480,
     571,    96,   635,   191,    99,   139,   203,   214,   199,    70,
      71,   200,   140,   275,   276,   277,   521,   155,   479,   480,
     216,   472,   199,    70,    71,   200,   358,   359,   636,   565,
     255,   626,   141,   737,    51,   477,   197,    98,   566,   142,
     258,   670,   148,   143,   149,   150,   151,   152,    97,   481,
     144,   100,   153,    88,   470,   278,   306,   267,   471,   154,
     279,   305,   468,   146,   147,   155,   203,   469,   215,   187,
     104,   218,   216,   217,   218,   218,   218,   218,   218,   218,
     218,   218,   218,   218,   218,   218,   218,   218,   302,   315,
     437,   307,   581,   310,   473,   615,   508,   216,   311,   474,
     616,   103,   413,   398,   268,   268,   268,   268,   268,   268,
     268,   268,   268,   268,   268,   268,   268,   268,   148,   107,
     149,   150,   151,   152,   211,   108,   620,   690,   153,   146,
     147,   621,   691,   101,   102,   154,   199,    70,    71,   200,
     627,   695,   188,   385,   241,   242,   696,   146,   147,   617,
     728,    94,   155,    95,   109,   729,   316,   218,   199,    70,
      71,   200,   110,   387,   388,   199,    70,    71,   200,   146,
     147,   622,   199,    70,    71,   200,   249,   730,   658,   250,
     251,   111,   731,   155,   148,   663,   149,   150,   151,   152,
     551,   146,   147,   252,   153,   556,   253,   254,   386,   397,
     150,   154,   148,   155,   149,   150,   151,   152,   429,   734,
     112,   303,   153,   393,   735,   155,   475,   523,   441,   154,
     476,   216,   442,   307,   148,   113,   149,   150,   151,   152,
     105,   155,   106,   155,   153,   448,   449,   114,   540,   542,
     201,   154,   329,   201,   483,   484,   148,   122,   149,   150,
     151,   152,   115,   532,   116,   155,   539,   216,   155,   155,
     155,   117,   348,   154,   118,   155,   155,   155,   614,   634,
     119,   534,   120,   619,   360,   216,   146,   147,   121,   570,
     199,    70,    71,   200,   629,   630,   199,    70,    71,   200,
     392,   537,   394,   718,   123,   216,   124,   218,   289,   722,
     125,   627,   280,   281,   282,   283,   284,   285,   399,   400,
     401,   402,   403,   404,   415,   405,   406,   417,   418,   419,
     126,   127,   128,   155,   424,   425,   426,   129,   146,   147,
     130,   148,   131,   149,   150,   151,   152,   199,    70,    71,
     200,   541,   132,   133,   134,   135,   136,   331,   154,   137,
     334,   335,   336,   337,   338,   339,   340,   341,   342,   343,
     344,   345,   346,   347,   138,   222,   223,   224,   225,   226,
     227,   228,   229,   230,   231,   232,   233,   234,   235,   694,
     145,   168,   463,   169,   699,   149,   150,   151,   152,   170,
     171,   172,   173,   153,   174,   175,   176,   155,   180,   177,
     154,   181,   178,   179,   520,   218,   182,   525,   183,   367,
     576,   529,   218,   218,   189,   218,   218,   218,   184,   218,
     185,   218,   218,   218,   218,   192,   198,   207,   201,   208,
     209,   210,   216,   201,   218,   212,   213,   220,   221,   237,
     439,   238,   239,   604,   236,   604,   240,   261,   312,   368,
     407,   408,   409,   410,   411,   412,   505,   369,   370,   371,
     372,   373,   374,   375,   376,   377,   378,   379,   243,   380,
     381,   246,   247,   248,   255,   262,   259,   260,   218,   263,
     264,   461,   266,   270,   271,   307,   465,   466,   155,   286,
     155,   201,   272,   287,   288,   291,   201,   290,   292,   155,
     293,   155,   294,   295,   155,   296,   155,   218,   155,   218,
     155,   297,   218,   155,   298,   299,   300,   308,   304,   190,
     305,   478,   314,   318,   309,   313,   317,   667,   668,   322,
     191,   319,   320,   321,   638,   155,   155,   218,   323,   325,
     324,   326,   218,   330,   327,   332,   333,   586,   349,   588,
     353,   350,   351,   352,   354,   355,   356,   357,   596,   361,
     598,   362,   364,   601,   365,   603,   430,   606,   306,   608,
     416,   201,   611,   486,   384,   389,   201,   390,   391,   395,
     420,   531,   421,   422,   423,   431,   434,   427,   453,   428,
     438,   445,   446,   450,   631,   632,   443,   454,   444,   447,
     552,   451,   452,   455,   457,   557,   456,   458,   459,   460,
     462,   218,   218,   218,   218,   218,   218,   464,   503,   218,
     567,   467,   487,   218,   488,   489,   492,   490,   491,   493,
     496,   494,   497,   495,   701,   703,   498,   499,   704,   705,
     578,   506,   500,   501,   502,   544,   511,   504,   550,   543,
     587,   514,   589,   507,   591,   592,   593,   509,   595,   510,
     513,   597,   515,   599,   600,   545,   602,   546,   548,   549,
     553,   555,   609,   554,   559,   612,   560,   563,   568,   571,
     574,   569,   642,   643,   575,   657,   662,   613,   669,   579,
     580,   618,   582,   584,   585,   590,   512,   594,   623,   624,
     628,   640,   649,   522,   524,   641,   526,   527,   528,   644,
     530,   645,   533,   535,   536,   538,   646,   647,   648,   659,
     660,   652,   654,   656,   664,   547,   671,   665,   672,   686,
     674,   712,   558,   651,   675,   653,   693,   676,   655,   678,
     679,   698,   700,   715,   717,   661,   719,   721,   724,   725,
     666,   726,   739,   741,   727,   744,   723,   733,   746,   747,
     748,   673,   637,   583,   414,   204,     0,   677,     0,   577,
       0,   269,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   688,     0,     0,     0,   692,     0,     0,     0,
       0,   697,     0,     0,     0,     0,     0,     0,   605,     0,
     607,     0,     0,   610,     0,     0,     0,     0,   706,   707,
     708,   709,   710,   711,     0,   713,     0,   714,     0,   716,
       0,     0,     0,     0,   720,     0,     0,     0,   633,     0,
       0,     0,     0,   639,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   732,     0,     0,     0,
     736,     0,     0,     0,     0,   738,     0,   740,     0,   742,
       0,   743,     0,   745,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   749,   750,   751,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   680,   681,   682,   683,   684,   685,     0,     0,
     687,   193,     3,     0,   689,     4,     5,     6,     7,     8,
       9,    10,     0,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,     0,    41,     0,     0,    42,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    43,    44,    45,    46,    47,    48,    49,    50,
       0,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,     0,    68,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   194,     0,     0,   195,     0,     0,    -7,     1,
       0,     0,     2,     3,     0,   702,     4,     5,     6,     7,
       8,     9,    10,     0,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,     0,    41,     0,     0,    42,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,     0,    68,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,    71,    72,   193,     3,     0,
       0,     4,     5,     6,     7,     8,     9,    10,     0,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,     0,
      41,     0,     0,    42,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    43,    44,
      45,    46,    47,    48,    49,    50,     0,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,     0,    68,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   194,     0,
       0,   195,     0,   196,   193,     3,     0,     0,     4,     5,
       6,     7,     8,     9,    10,     0,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,     0,    41,     0,     0,
      42,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
       0,    68,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   516,   193,     3,   195,     0,
       4,     5,     6,     7,     8,     9,    10,     0,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,     0,    41,
       0,     0,    42,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    43,    44,    45,
      46,    47,    48,    49,    50,     0,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      66,    67,     0,    68,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   194,     0,     0,
     195
};

static const grib_yytype_int16 grib_yycheck[] =
{
       0,    84,   384,   183,   148,     4,     0,     0,   270,    15,
     251,   308,   309,   254,    73,   561,   313,    66,   113,    66,
      67,    66,    67,   105,   113,    46,   154,    48,    49,    15,
      99,   100,    15,   105,   116,   215,    92,    93,    66,    67,
      90,   105,    92,   115,   111,   106,   105,   103,   105,   106,
     107,   108,   113,   122,   123,   124,   111,    50,    66,    67,
     115,   118,   105,   106,   107,   108,   246,   247,   118,   109,
     119,   118,   106,   118,    73,   118,    76,   113,   118,   113,
     136,   627,   103,   106,   105,   106,   107,   108,   105,   117,
     113,   111,   113,    87,   113,   120,   117,   153,   117,   120,
     125,    46,   109,    48,    49,    98,   105,   114,   111,   115,
     105,   104,   115,   116,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   184,   115,
     310,   187,   115,   111,   109,   109,   433,   115,   116,   114,
     114,   113,   286,   271,   272,   273,   274,   275,   276,   277,
     278,   279,   280,   281,   282,   283,   284,   285,   103,   105,
     105,   106,   107,   108,    98,   105,   109,   109,   113,    48,
      49,   114,   114,   105,   106,   120,   105,   106,   107,   108,
     562,   109,   115,    62,   115,   116,   114,    48,    49,   118,
     109,   111,   185,   113,   105,   114,   196,   190,   105,   106,
     107,   108,   105,   259,   260,   105,   106,   107,   108,    48,
      49,   118,   105,   106,   107,   108,   113,   109,   118,   116,
     117,   105,   114,   216,   103,   118,   105,   106,   107,   108,
     471,    48,    49,   113,   113,   476,   116,   117,   117,   105,
     106,   120,   103,   236,   105,   106,   107,   108,   304,   109,
     105,   185,   113,   114,   114,   248,   113,   111,   314,   120,
     117,   115,   318,   319,   103,   105,   105,   106,   107,   108,
     111,   264,   113,   266,   113,   105,   106,   105,   458,   459,
     363,   120,   216,   366,   107,   108,   103,   111,   105,   106,
     107,   108,   105,   111,   105,   288,   113,   115,   291,   292,
     293,   105,   236,   120,   105,   298,   299,   300,   549,   571,
     105,   111,   105,   554,   248,   115,    48,    49,   105,   499,
     105,   106,   107,   108,   107,   108,   105,   106,   107,   108,
     264,   111,   266,   118,   111,   115,   111,   330,   170,   118,
     111,   723,    93,    94,    95,    96,    97,    98,   272,   273,
     274,   275,   276,   277,   288,   278,   279,   291,   292,   293,
     105,   105,   105,   356,   298,   299,   300,   105,    48,    49,
     105,   103,   105,   105,   106,   107,   108,   105,   106,   107,
     108,   113,   105,   105,   105,   105,   113,   219,   120,   105,
     222,   223,   224,   225,   226,   227,   228,   229,   230,   231,
     232,   233,   234,   235,   105,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   660,
     105,   110,   356,   105,   665,   105,   106,   107,   108,   105,
     111,   105,   105,   113,   105,   105,   111,   430,   105,   111,
     120,   105,   111,   111,   444,   438,   105,   447,   105,     3,
     506,   451,   445,   446,   115,   448,   449,   450,   113,   452,
     113,   454,   455,   456,   457,     0,   110,   107,   551,   107,
     106,   106,   115,   556,   467,   107,   107,   107,   107,   107,
     312,   107,   107,   539,   111,   541,   107,   107,   190,    43,
     280,   281,   282,   283,   284,   285,   430,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,   116,    63,
      64,   115,   115,   113,   119,   107,   115,   115,   511,   106,
     113,   353,   113,   109,   104,   581,   358,   359,   521,   101,
     523,   614,   121,   102,   113,   113,   619,   107,   113,   532,
     113,   534,   107,   107,   537,   107,   539,   540,   541,   542,
     543,   107,   545,   546,   113,   113,   113,   117,   113,   105,
      46,   109,   113,   113,   117,   117,   115,   623,   624,   112,
     115,   115,   114,   114,   574,   568,   569,   570,   114,   112,
     114,   112,   575,   105,   114,   112,   114,   521,   112,   523,
     105,   112,   112,   112,   105,   105,   113,   105,   532,   105,
     534,   105,   105,   537,   105,   539,   113,   541,   117,   543,
     112,   694,   546,   106,   114,   114,   699,   114,   114,   114,
     112,   453,   112,   112,   112,   105,   110,   114,   330,   114,
     105,   105,   105,   105,   568,   569,   117,   105,   117,   117,
     472,   117,   112,   105,   105,   477,   112,   105,   105,   105,
     115,   644,   645,   646,   647,   648,   649,   105,   117,   652,
     492,   114,   106,   656,   106,   114,   105,   114,   114,   114,
     105,   114,   105,   114,   674,   675,   105,   105,   678,   679,
     512,   115,   114,   114,   114,   105,   112,   114,   105,   113,
     522,   114,   524,   118,   526,   527,   528,   118,   530,   118,
     118,   533,   114,   535,   536,   114,   538,   113,   106,   117,
     106,   105,   544,   117,   105,   547,   105,   110,   113,    90,
     117,   113,     6,     6,   114,   105,   105,   109,     6,   117,
     117,   109,   118,   118,   118,   118,   438,   118,   115,   115,
     110,   110,   114,   445,   446,   577,   448,   449,   450,   112,
     452,   112,   454,   455,   456,   457,   112,   112,   112,   106,
     117,   114,   114,   114,   106,   467,   114,   117,   114,   113,
     119,   105,   478,   605,   119,   607,   109,   118,   610,   117,
     117,   109,   117,   105,   105,   617,   105,   105,   118,   118,
     622,   118,   105,   105,   114,   105,   700,   114,   114,   114,
     114,   633,   572,   518,   287,    87,    -1,   639,    -1,   511,
      -1,   154,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   654,    -1,    -1,    -1,   658,    -1,    -1,    -1,
      -1,   663,    -1,    -1,    -1,    -1,    -1,    -1,   540,    -1,
     542,    -1,    -1,   545,    -1,    -1,    -1,    -1,   680,   681,
     682,   683,   684,   685,    -1,   687,    -1,   689,    -1,   691,
      -1,    -1,    -1,    -1,   696,    -1,    -1,    -1,   570,    -1,
      -1,    -1,    -1,   575,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   718,    -1,    -1,    -1,
     722,    -1,    -1,    -1,    -1,   727,    -1,   729,    -1,   731,
      -1,   733,    -1,   735,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   746,   747,   748,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   644,   645,   646,   647,   648,   649,    -1,    -1,
     652,     4,     5,    -1,   656,     8,     9,    10,    11,    12,
      13,    14,    -1,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    47,    -1,    -1,    50,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    65,    66,    67,    68,    69,    70,    71,    72,
      -1,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    -1,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   105,    -1,    -1,   108,    -1,    -1,     0,     1,
      -1,    -1,     4,     5,    -1,   118,     8,     9,    10,    11,
      12,    13,    14,    -1,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    47,    -1,    -1,    50,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    -1,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,   106,   107,   108,     4,     5,    -1,
      -1,     8,     9,    10,    11,    12,    13,    14,    -1,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      47,    -1,    -1,    50,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    65,    66,
      67,    68,    69,    70,    71,    72,    -1,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    -1,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,
      -1,   108,    -1,   110,     4,     5,    -1,    -1,     8,     9,
      10,    11,    12,    13,    14,    -1,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    47,    -1,    -1,
      50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,     4,     5,   108,    -1,
       8,     9,    10,    11,    12,    13,    14,    -1,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    47,
      -1,    -1,    50,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    65,    66,    67,
      68,    69,    70,    71,    72,    -1,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,
     108
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const grib_yytype_uint8 grib_yystos[] =
{
       0,     1,     4,     5,     8,     9,    10,    11,    12,    13,
      14,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    47,    50,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    91,   105,
     106,   107,   108,   127,   128,   130,   131,   136,   137,   138,
     145,   146,   147,   148,   149,   152,   153,   165,   166,   168,
     169,   170,   113,   113,   111,   113,   105,   105,   113,   111,
     111,   105,   106,   113,   105,   111,   113,   105,   105,   105,
     105,   105,   105,   105,   105,   105,   105,   105,   105,   105,
     105,   105,   111,   111,   111,   111,   105,   105,   105,   105,
     105,   105,   105,   105,   105,   105,   113,   105,   105,   106,
     113,   106,   113,   106,   113,   105,    48,    49,   103,   105,
     106,   107,   108,   113,   120,   128,   133,   134,   135,   156,
     157,   158,   159,   160,   161,   162,   163,   164,   110,   105,
     105,   111,   105,   105,   105,   105,   111,   111,   111,   111,
     105,   105,   105,   105,   113,   113,    15,   115,   115,   115,
     105,   115,     0,     4,   105,   108,   110,   130,   110,   105,
     108,   153,     4,   105,   170,   164,   164,   107,   107,   106,
     106,   133,   107,   107,   164,   111,   115,   116,   128,   141,
     107,   107,   141,   141,   141,   141,   141,   141,   141,   141,
     141,   141,   141,   141,   141,   141,   111,   107,   107,   107,
     107,   115,   116,   116,   105,   116,   115,   115,   113,   113,
     116,   117,   113,   116,   117,   119,   128,   142,   164,   115,
     115,   107,   107,   106,   113,   161,   113,   164,   156,   157,
     109,   104,   121,    99,   100,   122,   123,   124,   120,   125,
      93,    94,    95,    96,    97,    98,   101,   102,   113,   142,
     107,   113,   113,   113,   107,   107,   107,   107,   113,   113,
     113,   135,   164,   133,   113,    46,   117,   164,   117,   117,
     111,   116,   141,   117,   113,   115,   130,   115,   113,   115,
     114,   114,   112,   114,   114,   112,   112,   114,   135,   133,
     105,   142,   112,   114,   142,   142,   142,   142,   142,   142,
     142,   142,   142,   142,   142,   142,   142,   142,   133,   112,
     112,   112,   112,   105,   105,   105,   113,   105,   135,   135,
     133,   105,   105,   149,   105,   105,   149,     3,    43,    51,
      52,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      63,    64,   143,   144,   114,    62,   117,   164,   164,   114,
     114,   114,   133,   114,   133,   114,   134,   105,   156,   158,
     158,   158,   158,   158,   158,   159,   159,   160,   160,   160,
     160,   160,   160,   161,   162,   133,   112,   133,   133,   133,
     112,   112,   112,   112,   133,   133,   133,   114,   114,   164,
     113,   105,   154,   155,   110,   154,   154,   135,   105,   142,
     154,   164,   164,   117,   117,   105,   105,   117,   105,   106,
     105,   117,   112,   141,   105,   105,   112,   105,   105,   105,
     105,   142,   115,   133,   105,   142,   142,   114,   109,   114,
     113,   117,   118,   109,   114,   113,   117,   118,   109,    66,
      67,   117,   139,   107,   108,   129,   106,   106,   106,   114,
     114,   114,   105,   114,   114,   114,   105,   105,   105,   105,
     114,   114,   114,   117,   114,   133,   115,   118,   154,   118,
     118,   112,   141,   118,   114,   114,   105,   130,   166,   167,
     130,   111,   141,   111,   141,   130,   141,   141,   141,   130,
     141,   142,   111,   141,   111,   141,   141,   111,   141,   113,
     135,   113,   135,   113,   105,   114,   113,   141,   106,   117,
     105,   149,   142,   106,   117,   105,   149,   142,   144,   105,
     105,   139,   140,   110,   132,   109,   118,   142,   113,   113,
     135,    90,   150,   151,   117,   114,   164,   141,   142,   117,
     117,   115,   118,   167,   118,   118,   133,   142,   133,   142,
     118,   142,   142,   142,   118,   142,   133,   142,   133,   142,
     142,   133,   142,   133,   164,   141,   133,   141,   133,   142,
     141,   133,   142,   109,   149,   109,   114,   118,   109,   149,
     109,   114,   118,   115,   115,   132,   118,   139,   110,   107,
     108,   133,   133,   141,   134,    92,   118,   151,   130,   141,
     110,   142,     6,     6,   112,   112,   112,   112,   112,   114,
      66,   142,   114,   142,   114,   142,   114,   105,   118,   106,
     117,   142,   105,   118,   106,   117,   142,   164,   164,     6,
     132,   114,   114,   142,   119,   119,   118,   142,   117,   117,
     141,   141,   141,   141,   141,   141,   113,   141,   142,   141,
     109,   114,   142,   109,   149,   109,   114,   142,   109,   149,
     117,   130,   118,   130,   130,   130,   142,   142,   142,   142,
     142,   142,   105,   142,   142,   105,   142,   105,   118,   105,
     142,   105,   118,   140,   118,   118,   118,   114,   109,   114,
     109,   114,   142,   114,   109,   114,   142,   118,   142,   105,
     142,   105,   142,   142,   105,   142,   114,   114,   114,   142,
     142,   142
};

#define grib_yyerrok		(grib_yyerrstatus = 0)
#define grib_yyclearin	(grib_yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto grib_yyacceptlab
#define YYABORT		goto grib_yyabortlab
#define YYERROR		goto grib_yyerrorlab


/* Like YYERROR except do call grib_yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto grib_yyerrlab

#define YYRECOVERING()  (!!grib_yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (grib_yychar == YYEMPTY && grib_yylen == 1)				\
    {								\
      grib_yychar = (Token);						\
      grib_yylval = (Value);						\
      grib_yytoken = YYTRANSLATE (grib_yychar);				\
      YYPOPSTACK (1);						\
      goto grib_yybackup;						\
    }								\
  else								\
    {								\
      grib_yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `grib_yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX grib_yylex (YYLEX_PARAM)
#else
# define YYLEX grib_yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (grib_yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (grib_yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      grib_yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
grib_yy_symbol_value_print (FILE *grib_yyoutput, int grib_yytype, YYSTYPE const * const grib_yyvaluep)
#else
static void
grib_yy_symbol_value_print (grib_yyoutput, grib_yytype, grib_yyvaluep)
    FILE *grib_yyoutput;
    int grib_yytype;
    YYSTYPE const * const grib_yyvaluep;
#endif
{
  if (!grib_yyvaluep)
    return;
# ifdef YYPRINT
  if (grib_yytype < YYNTOKENS)
    YYPRINT (grib_yyoutput, grib_yytoknum[grib_yytype], *grib_yyvaluep);
# else
  YYUSE (grib_yyoutput);
# endif
  switch (grib_yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
grib_yy_symbol_print (FILE *grib_yyoutput, int grib_yytype, YYSTYPE const * const grib_yyvaluep)
#else
static void
grib_yy_symbol_print (grib_yyoutput, grib_yytype, grib_yyvaluep)
    FILE *grib_yyoutput;
    int grib_yytype;
    YYSTYPE const * const grib_yyvaluep;
#endif
{
  if (grib_yytype < YYNTOKENS)
    YYFPRINTF (grib_yyoutput, "token %s (", grib_yytname[grib_yytype]);
  else
    YYFPRINTF (grib_yyoutput, "nterm %s (", grib_yytname[grib_yytype]);

  grib_yy_symbol_value_print (grib_yyoutput, grib_yytype, grib_yyvaluep);
  YYFPRINTF (grib_yyoutput, ")");
}

/*------------------------------------------------------------------.
| grib_yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
grib_yy_stack_print (grib_yytype_int16 *grib_yybottom, grib_yytype_int16 *grib_yytop)
#else
static void
grib_yy_stack_print (grib_yybottom, grib_yytop)
    grib_yytype_int16 *grib_yybottom;
    grib_yytype_int16 *grib_yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; grib_yybottom <= grib_yytop; grib_yybottom++)
    {
      int grib_yybot = *grib_yybottom;
      YYFPRINTF (stderr, " %d", grib_yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (grib_yydebug)							\
    grib_yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
grib_yy_reduce_print (YYSTYPE *grib_yyvsp, int grib_yyrule)
#else
static void
grib_yy_reduce_print (grib_yyvsp, grib_yyrule)
    YYSTYPE *grib_yyvsp;
    int grib_yyrule;
#endif
{
  int grib_yynrhs = grib_yyr2[grib_yyrule];
  int grib_yyi;
  unsigned long int grib_yylno = grib_yyrline[grib_yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     grib_yyrule - 1, grib_yylno);
  /* The symbols being reduced.  */
  for (grib_yyi = 0; grib_yyi < grib_yynrhs; grib_yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", grib_yyi + 1);
      grib_yy_symbol_print (stderr, grib_yyrhs[grib_yyprhs[grib_yyrule] + grib_yyi],
		       &(grib_yyvsp[(grib_yyi + 1) - (grib_yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (grib_yydebug)				\
    grib_yy_reduce_print (grib_yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int grib_yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef grib_yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define grib_yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
grib_yystrlen (const char *grib_yystr)
#else
static YYSIZE_T
grib_yystrlen (grib_yystr)
    const char *grib_yystr;
#endif
{
  YYSIZE_T grib_yylen;
  for (grib_yylen = 0; grib_yystr[grib_yylen]; grib_yylen++)
    continue;
  return grib_yylen;
}
#  endif
# endif

# ifndef grib_yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define grib_yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
grib_yystpcpy (char *grib_yydest, const char *grib_yysrc)
#else
static char *
grib_yystpcpy (grib_yydest, grib_yysrc)
    char *grib_yydest;
    const char *grib_yysrc;
#endif
{
  char *grib_yyd = grib_yydest;
  const char *grib_yys = grib_yysrc;

  while ((*grib_yyd++ = *grib_yys++) != '\0')
    continue;

  return grib_yyd - 1;
}
#  endif
# endif

# ifndef grib_yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for grib_yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from grib_yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
grib_yytnamerr (char *grib_yyres, const char *grib_yystr)
{
  if (*grib_yystr == '"')
    {
      YYSIZE_T grib_yyn = 0;
      char const *grib_yyp = grib_yystr;

      for (;;)
	switch (*++grib_yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++grib_yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (grib_yyres)
	      grib_yyres[grib_yyn] = *grib_yyp;
	    grib_yyn++;
	    break;

	  case '"':
	    if (grib_yyres)
	      grib_yyres[grib_yyn] = '\0';
	    return grib_yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! grib_yyres)
    return grib_yystrlen (grib_yystr);

  return grib_yystpcpy (grib_yyres, grib_yystr) - grib_yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
grib_yysyntax_error (char *grib_yyresult, int grib_yystate, int grib_yychar)
{
  int grib_yyn = grib_yypact[grib_yystate];

  if (! (YYPACT_NINF < grib_yyn && grib_yyn <= YYLAST))
    return 0;
  else
    {
      int grib_yytype = YYTRANSLATE (grib_yychar);
      YYSIZE_T grib_yysize0 = grib_yytnamerr (0, grib_yytname[grib_yytype]);
      YYSIZE_T grib_yysize = grib_yysize0;
      YYSIZE_T grib_yysize1;
      int grib_yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *grib_yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int grib_yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *grib_yyfmt;
      char const *grib_yyf;
      static char const grib_yyunexpected[] = "syntax error, unexpected %s";
      static char const grib_yyexpecting[] = ", expecting %s";
      static char const grib_yyor[] = " or %s";
      char grib_yyformat[sizeof grib_yyunexpected
		    + sizeof grib_yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof grib_yyor - 1))];
      char const *grib_yyprefix = grib_yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int grib_yyxbegin = grib_yyn < 0 ? -grib_yyn : 0;

      /* Stay within bounds of both grib_yycheck and grib_yytname.  */
      int grib_yychecklim = YYLAST - grib_yyn + 1;
      int grib_yyxend = grib_yychecklim < YYNTOKENS ? grib_yychecklim : YYNTOKENS;
      int grib_yycount = 1;

      grib_yyarg[0] = grib_yytname[grib_yytype];
      grib_yyfmt = grib_yystpcpy (grib_yyformat, grib_yyunexpected);

      for (grib_yyx = grib_yyxbegin; grib_yyx < grib_yyxend; ++grib_yyx)
	if (grib_yycheck[grib_yyx + grib_yyn] == grib_yyx && grib_yyx != YYTERROR)
	  {
	    if (grib_yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		grib_yycount = 1;
		grib_yysize = grib_yysize0;
		grib_yyformat[sizeof grib_yyunexpected - 1] = '\0';
		break;
	      }
	    grib_yyarg[grib_yycount++] = grib_yytname[grib_yyx];
	    grib_yysize1 = grib_yysize + grib_yytnamerr (0, grib_yytname[grib_yyx]);
	    grib_yysize_overflow |= (grib_yysize1 < grib_yysize);
	    grib_yysize = grib_yysize1;
	    grib_yyfmt = grib_yystpcpy (grib_yyfmt, grib_yyprefix);
	    grib_yyprefix = grib_yyor;
	  }

      grib_yyf = YY_(grib_yyformat);
      grib_yysize1 = grib_yysize + grib_yystrlen (grib_yyf);
      grib_yysize_overflow |= (grib_yysize1 < grib_yysize);
      grib_yysize = grib_yysize1;

      if (grib_yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (grib_yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *grib_yyp = grib_yyresult;
	  int grib_yyi = 0;
	  while ((*grib_yyp = *grib_yyf) != '\0')
	    {
	      if (*grib_yyp == '%' && grib_yyf[1] == 's' && grib_yyi < grib_yycount)
		{
		  grib_yyp += grib_yytnamerr (grib_yyp, grib_yyarg[grib_yyi++]);
		  grib_yyf += 2;
		}
	      else
		{
		  grib_yyp++;
		  grib_yyf++;
		}
	    }
	}
      return grib_yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
grib_yydestruct (const char *grib_yymsg, int grib_yytype, YYSTYPE *grib_yyvaluep)
#else
static void
grib_yydestruct (grib_yymsg, grib_yytype, grib_yyvaluep)
    const char *grib_yymsg;
    int grib_yytype;
    YYSTYPE *grib_yyvaluep;
#endif
{
  YYUSE (grib_yyvaluep);

  if (!grib_yymsg)
    grib_yymsg = "Deleting";
  YY_SYMBOL_PRINT (grib_yymsg, grib_yytype, grib_yyvaluep, grib_yylocationp);

  switch (grib_yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int grib_yyparse (void *YYPARSE_PARAM);
#else
int grib_yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int grib_yyparse (void);
#else
int grib_yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int grib_yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE grib_yylval;

/* Number of syntax errors so far.  */
int grib_yynerrs;



/*-------------------------.
| grib_yyparse or grib_yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
grib_yyparse (void *YYPARSE_PARAM)
#else
int
grib_yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
grib_yyparse (void)
#else
int
grib_yyparse ()

#endif
#endif
{


    int grib_yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int grib_yyerrstatus;

    /* The stacks and their tools:
       `grib_yyss': related to states.
       `grib_yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow grib_yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    grib_yytype_int16 grib_yyssa[YYINITDEPTH];
    grib_yytype_int16 *grib_yyss;
    grib_yytype_int16 *grib_yyssp;

    /* The semantic value stack.  */
    YYSTYPE grib_yyvsa[YYINITDEPTH];
    YYSTYPE *grib_yyvs;
    YYSTYPE *grib_yyvsp;

    YYSIZE_T grib_yystacksize;

  int grib_yyn;
  int grib_yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int grib_yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE grib_yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char grib_yymsgbuf[128];
  char *grib_yymsg = grib_yymsgbuf;
  YYSIZE_T grib_yymsg_alloc = sizeof grib_yymsgbuf;
#endif

#define YYPOPSTACK(N)   (grib_yyvsp -= (N), grib_yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int grib_yylen = 0;

  grib_yytoken = 0;
  grib_yyss = grib_yyssa;
  grib_yyvs = grib_yyvsa;
  grib_yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  grib_yystate = 0;
  grib_yyerrstatus = 0;
  grib_yynerrs = 0;
  grib_yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  grib_yyssp = grib_yyss;
  grib_yyvsp = grib_yyvs;

  goto grib_yysetstate;

/*------------------------------------------------------------.
| grib_yynewstate -- Push a new state, which is found in grib_yystate.  |
`------------------------------------------------------------*/
 grib_yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  grib_yyssp++;

 grib_yysetstate:
  *grib_yyssp = grib_yystate;

  if (grib_yyss + grib_yystacksize - 1 <= grib_yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T grib_yysize = grib_yyssp - grib_yyss + 1;

#ifdef grib_yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *grib_yyvs1 = grib_yyvs;
	grib_yytype_int16 *grib_yyss1 = grib_yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if grib_yyoverflow is a macro.  */
	grib_yyoverflow (YY_("memory exhausted"),
		    &grib_yyss1, grib_yysize * sizeof (*grib_yyssp),
		    &grib_yyvs1, grib_yysize * sizeof (*grib_yyvsp),
		    &grib_yystacksize);

	grib_yyss = grib_yyss1;
	grib_yyvs = grib_yyvs1;
      }
#else /* no grib_yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto grib_yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= grib_yystacksize)
	goto grib_yyexhaustedlab;
      grib_yystacksize *= 2;
      if (YYMAXDEPTH < grib_yystacksize)
	grib_yystacksize = YYMAXDEPTH;

      {
	grib_yytype_int16 *grib_yyss1 = grib_yyss;
	union grib_yyalloc *grib_yyptr =
	  (union grib_yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (grib_yystacksize));
	if (! grib_yyptr)
	  goto grib_yyexhaustedlab;
	YYSTACK_RELOCATE (grib_yyss_alloc, grib_yyss);
	YYSTACK_RELOCATE (grib_yyvs_alloc, grib_yyvs);
#  undef YYSTACK_RELOCATE
	if (grib_yyss1 != grib_yyssa)
	  YYSTACK_FREE (grib_yyss1);
      }
# endif
#endif /* no grib_yyoverflow */

      grib_yyssp = grib_yyss + grib_yysize - 1;
      grib_yyvsp = grib_yyvs + grib_yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) grib_yystacksize));

      if (grib_yyss + grib_yystacksize - 1 <= grib_yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", grib_yystate));

  if (grib_yystate == YYFINAL)
    YYACCEPT;

  goto grib_yybackup;

/*-----------.
| grib_yybackup.  |
`-----------*/
grib_yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  grib_yyn = grib_yypact[grib_yystate];
  if (grib_yyn == YYPACT_NINF)
    goto grib_yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (grib_yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      grib_yychar = YYLEX;
    }

  if (grib_yychar <= YYEOF)
    {
      grib_yychar = grib_yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      grib_yytoken = YYTRANSLATE (grib_yychar);
      YY_SYMBOL_PRINT ("Next token is", grib_yytoken, &grib_yylval, &grib_yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  grib_yyn += grib_yytoken;
  if (grib_yyn < 0 || YYLAST < grib_yyn || grib_yycheck[grib_yyn] != grib_yytoken)
    goto grib_yydefault;
  grib_yyn = grib_yytable[grib_yyn];
  if (grib_yyn <= 0)
    {
      if (grib_yyn == 0 || grib_yyn == YYTABLE_NINF)
	goto grib_yyerrlab;
      grib_yyn = -grib_yyn;
      goto grib_yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (grib_yyerrstatus)
    grib_yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", grib_yytoken, &grib_yylval, &grib_yylloc);

  /* Discard the shifted token.  */
  grib_yychar = YYEMPTY;

  grib_yystate = grib_yyn;
  *++grib_yyvsp = grib_yylval;

  goto grib_yynewstate;


/*-----------------------------------------------------------.
| grib_yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
grib_yydefault:
  grib_yyn = grib_yydefact[grib_yystate];
  if (grib_yyn == 0)
    goto grib_yyerrlab;
  goto grib_yyreduce;


/*-----------------------------.
| grib_yyreduce -- Do a reduction.  |
`-----------------------------*/
grib_yyreduce:
  /* grib_yyn is the number of a rule to reduce with.  */
  grib_yylen = grib_yyr2[grib_yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  grib_yyval = grib_yyvsp[1-grib_yylen];


  YY_REDUCE_PRINT (grib_yyn);
  switch (grib_yyn)
    {
        case 2:

/* Line 1455 of yacc.c  */
#line 231 "griby.y"
    { grib_parser_all_actions = 0;grib_parser_concept=0; grib_parser_rules=0; }
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 232 "griby.y"
    { grib_parser_concept     = reverse((grib_yyvsp[(1) - (1)].concept_value)); }
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 233 "griby.y"
    { grib_parser_all_actions = (grib_yyvsp[(1) - (1)].act); }
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 234 "griby.y"
    { grib_parser_rules       = (grib_yyvsp[(1) - (1)].rules); }
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 236 "griby.y"
    { grib_parser_all_actions = 0; grib_parser_concept=0; grib_parser_rules=0; }
    break;

  case 8:

/* Line 1455 of yacc.c  */
#line 242 "griby.y"
    { (grib_yyval.dvalue)=grib_darray_push(0,0,(grib_yyvsp[(1) - (1)].dval));}
    break;

  case 9:

/* Line 1455 of yacc.c  */
#line 243 "griby.y"
    { (grib_yyval.dvalue)=grib_darray_push(0,(grib_yyvsp[(1) - (3)].dvalue),(grib_yyvsp[(3) - (3)].dval));}
    break;

  case 10:

/* Line 1455 of yacc.c  */
#line 244 "griby.y"
    { (grib_yyval.dvalue)=grib_darray_push(0,0,(grib_yyvsp[(1) - (1)].lval));}
    break;

  case 11:

/* Line 1455 of yacc.c  */
#line 245 "griby.y"
    { (grib_yyval.dvalue)=grib_darray_push(0,(grib_yyvsp[(1) - (3)].dvalue),(grib_yyvsp[(3) - (3)].lval));}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 248 "griby.y"
    { (grib_yyvsp[(1) - (2)].act)->next = (grib_yyvsp[(2) - (2)].act); (grib_yyval.act) = (grib_yyvsp[(1) - (2)].act); }
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 249 "griby.y"
    { (grib_yyvsp[(1) - (3)].act)->next = (grib_yyvsp[(3) - (3)].act); (grib_yyval.act) = (grib_yyvsp[(1) - (3)].act); }
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 250 "griby.y"
    {  (grib_yyval.act) = (grib_yyvsp[(1) - (2)].act);}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 268 "griby.y"
    { (grib_yyval.explist) = 0; }
    break;

  case 29:

/* Line 1455 of yacc.c  */
#line 273 "griby.y"
    { (grib_yyvsp[(1) - (3)].explist)->next = (grib_yyvsp[(3) - (3)].explist); (grib_yyval.explist) = (grib_yyvsp[(1) - (3)].explist); }
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 276 "griby.y"
    { (grib_yyval.explist) = grib_arguments_new(grib_parser_context,(grib_yyvsp[(1) - (1)].exp),NULL); }
    break;

  case 31:

/* Line 1455 of yacc.c  */
#line 281 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (7)].str),"unsigned",(grib_yyvsp[(3) - (7)].lval),NULL,(grib_yyvsp[(6) - (7)].explist),(grib_yyvsp[(7) - (7)].lval),NULL,NULL);        free((grib_yyvsp[(5) - (7)].str));  }
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 284 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (10)].str),"unsigned",(grib_yyvsp[(3) - (10)].lval),(grib_yyvsp[(7) - (10)].explist),(grib_yyvsp[(9) - (10)].explist),(grib_yyvsp[(10) - (10)].lval),NULL,NULL);        free((grib_yyvsp[(5) - (10)].str));  }
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 287 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (7)].str),"unsigned_bits",(grib_yyvsp[(3) - (7)].lval),NULL,(grib_yyvsp[(6) - (7)].explist),(grib_yyvsp[(7) - (7)].lval),NULL,NULL);        free((grib_yyvsp[(5) - (7)].str));  }
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 290 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (10)].str),"unsigned_bits",(grib_yyvsp[(3) - (10)].lval),(grib_yyvsp[(7) - (10)].explist),(grib_yyvsp[(9) - (10)].explist),(grib_yyvsp[(10) - (10)].lval),NULL,NULL);        free((grib_yyvsp[(5) - (10)].str));  }
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 293 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (7)].str),"ascii",(grib_yyvsp[(3) - (7)].lval),NULL,(grib_yyvsp[(6) - (7)].explist),(grib_yyvsp[(7) - (7)].lval),NULL,NULL);  free((grib_yyvsp[(5) - (7)].str));  }
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 297 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (7)].str),"ascii",(grib_yyvsp[(3) - (7)].lval),NULL,(grib_yyvsp[(6) - (7)].explist),(grib_yyvsp[(7) - (7)].lval),NULL,NULL);  free((grib_yyvsp[(5) - (7)].str));  }
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 300 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (7)].str),"bytes",(grib_yyvsp[(3) - (7)].lval),NULL,(grib_yyvsp[(6) - (7)].explist),(grib_yyvsp[(7) - (7)].lval),NULL,NULL);      free((grib_yyvsp[(5) - (7)].str));  }
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 303 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (10)].str),"bytes",(grib_yyvsp[(3) - (10)].lval),(grib_yyvsp[(7) - (10)].explist),(grib_yyvsp[(9) - (10)].explist),(grib_yyvsp[(10) - (10)].lval),NULL,NULL);      free((grib_yyvsp[(5) - (10)].str));  }
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 306 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (7)].str),"ksec1expver",(grib_yyvsp[(3) - (7)].lval),NULL,(grib_yyvsp[(6) - (7)].explist),(grib_yyvsp[(7) - (7)].lval),NULL,NULL);  free((grib_yyvsp[(5) - (7)].str));  }
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 309 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (7)].str),"signed",(grib_yyvsp[(3) - (7)].lval),NULL,(grib_yyvsp[(6) - (7)].explist),(grib_yyvsp[(7) - (7)].lval),NULL,NULL);      free((grib_yyvsp[(5) - (7)].str));  }
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 312 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (10)].str),"signed",(grib_yyvsp[(3) - (10)].lval),(grib_yyvsp[(7) - (10)].explist),(grib_yyvsp[(9) - (10)].explist),(grib_yyvsp[(10) - (10)].lval),NULL,NULL);      free((grib_yyvsp[(5) - (10)].str));  }
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 315 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (7)].str),"signed_bits",(grib_yyvsp[(3) - (7)].lval),NULL,(grib_yyvsp[(6) - (7)].explist),(grib_yyvsp[(7) - (7)].lval),NULL,NULL);      free((grib_yyvsp[(5) - (7)].str));  }
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 318 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (10)].str),"signed_bits",(grib_yyvsp[(3) - (10)].lval),(grib_yyvsp[(7) - (10)].explist),(grib_yyvsp[(9) - (10)].explist),(grib_yyvsp[(10) - (10)].lval),NULL,NULL);      free((grib_yyvsp[(5) - (10)].str));  }
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 321 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (8)].str),"codetable",(grib_yyvsp[(3) - (8)].lval), (grib_yyvsp[(6) - (8)].explist),(grib_yyvsp[(7) - (8)].explist),(grib_yyvsp[(8) - (8)].lval),NULL,NULL);    free((grib_yyvsp[(5) - (8)].str)); }
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 324 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (12)].str),"codetable",(grib_yyvsp[(3) - (12)].lval), (grib_yyvsp[(6) - (12)].explist),(grib_yyvsp[(7) - (12)].explist),(grib_yyvsp[(12) - (12)].lval),NULL,(grib_yyvsp[(10) - (12)].str));
					free((grib_yyvsp[(5) - (12)].str));free((grib_yyvsp[(10) - (12)].str)); }
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 328 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (10)].str),"codetable",(grib_yyvsp[(3) - (10)].lval), (grib_yyvsp[(7) - (10)].explist),(grib_yyvsp[(9) - (10)].explist),(grib_yyvsp[(10) - (10)].lval),NULL,NULL);    free((grib_yyvsp[(5) - (10)].str)); }
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 331 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(1) - (8)].str),"getenv",0,(grib_yyvsp[(5) - (8)].explist),(grib_yyvsp[(7) - (8)].explist),(grib_yyvsp[(8) - (8)].lval),NULL,NULL);    free((grib_yyvsp[(1) - (8)].str)); }
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 334 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (8)].str),"complex_codetable",(grib_yyvsp[(3) - (8)].lval), (grib_yyvsp[(6) - (8)].explist),(grib_yyvsp[(7) - (8)].explist),(grib_yyvsp[(8) - (8)].lval),NULL,NULL);    free((grib_yyvsp[(5) - (8)].str)); }
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 337 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (10)].str),"complex_codetable",(grib_yyvsp[(3) - (10)].lval), (grib_yyvsp[(7) - (10)].explist),(grib_yyvsp[(9) - (10)].explist),(grib_yyvsp[(10) - (10)].lval),NULL,NULL);    free((grib_yyvsp[(5) - (10)].str)); }
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 340 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (8)].str),"codeflag",(grib_yyvsp[(3) - (8)].lval), (grib_yyvsp[(6) - (8)].explist),(grib_yyvsp[(7) - (8)].explist),(grib_yyvsp[(8) - (8)].lval),NULL,NULL);  free((grib_yyvsp[(5) - (8)].str)); }
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 343 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (9)].str),"lookup",(grib_yyvsp[(3) - (9)].lval),(grib_yyvsp[(7) - (9)].explist),NULL,(grib_yyvsp[(9) - (9)].lval),NULL,NULL); free((grib_yyvsp[(5) - (9)].str)); }
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 346 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (7)].str),"bit",0,(grib_yyvsp[(4) - (7)].explist),(grib_yyvsp[(6) - (7)].explist),(grib_yyvsp[(7) - (7)].lval),NULL,NULL); free((grib_yyvsp[(2) - (7)].str)); }
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 349 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (2)].str),"label",0,NULL,NULL,0,NULL,NULL);   free((grib_yyvsp[(2) - (2)].str));  }
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 352 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (2)].str),"label",0,NULL,NULL,0,NULL,NULL);   free((grib_yyvsp[(2) - (2)].str));  }
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 355 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"ibmfloat",4,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 359 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"int8",1,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 362 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"uint8",1,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 365 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"int16",2,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 368 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"uint16",2,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 371 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"int16_little_endian",2,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 374 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"uint16_little_endian",2,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 377 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"int32",4,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 380 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"uint32",4,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 383 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"int32_little_endian",4,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 386 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"uint32_little_endian",4,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 389 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"int64",8,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 392 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"uint64",8,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 395 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"int64_little_endian",8,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 398 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"uint64_little_endian",8,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 401 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (7)].str),"blob",0,(grib_yyvsp[(4) - (7)].explist),(grib_yyvsp[(6) - (7)].explist),(grib_yyvsp[(7) - (7)].lval),NULL,NULL);        free((grib_yyvsp[(2) - (7)].str));  }
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 405 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(4) - (6)].str),"ibmfloat",4,NULL,(grib_yyvsp[(5) - (6)].explist),(grib_yyvsp[(6) - (6)].lval),(grib_yyvsp[(2) - (6)].str),NULL);free((grib_yyvsp[(4) - (6)].str)); free((grib_yyvsp[(2) - (6)].str)); }
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 408 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (7)].str),"ibmfloat",4,(grib_yyvsp[(4) - (7)].explist),(grib_yyvsp[(6) - (7)].explist),(grib_yyvsp[(7) - (7)].lval),NULL,NULL);free((grib_yyvsp[(2) - (7)].str));  }
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 411 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (2)].str),"position",0,NULL,NULL,0,NULL,NULL);     free((grib_yyvsp[(2) - (2)].str));  }
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 414 "griby.y"
    { (grib_yyval.act) = grib_action_create_variable(grib_parser_context,(grib_yyvsp[(2) - (5)].str),"constant",0,(grib_yyvsp[(4) - (5)].explist),NULL,(grib_yyvsp[(5) - (5)].lval),NULL);free((grib_yyvsp[(2) - (5)].str)); }
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 417 "griby.y"
    { (grib_yyval.act) = grib_action_create_variable(grib_parser_context,(grib_yyvsp[(2) - (5)].str),"transient",0,(grib_yyvsp[(4) - (5)].explist),(grib_yyvsp[(4) - (5)].explist),(grib_yyvsp[(5) - (5)].lval),NULL);   free((grib_yyvsp[(2) - (5)].str)); }
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 420 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (4)].str),"ieeefloat",4,NULL,(grib_yyvsp[(3) - (4)].explist),(grib_yyvsp[(4) - (4)].lval),NULL,NULL);   free((grib_yyvsp[(2) - (4)].str));  }
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 423 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(4) - (6)].str),"ieeefloat",4,NULL,(grib_yyvsp[(5) - (6)].explist),(grib_yyvsp[(6) - (6)].lval),(grib_yyvsp[(2) - (6)].str),NULL);  free((grib_yyvsp[(4) - (6)].str));free((grib_yyvsp[(2) - (6)].str));}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 426 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (7)].str),"ieeefloat",4,(grib_yyvsp[(4) - (7)].explist),(grib_yyvsp[(6) - (7)].explist),(grib_yyvsp[(7) - (7)].lval),NULL,NULL);free((grib_yyvsp[(2) - (7)].str));  }
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 429 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (2)].str),"g1_half_byte_codeflag",0,NULL,NULL,0,NULL,NULL);free((grib_yyvsp[(2) - (2)].str));  }
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 432 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (5)].str),"section_length",(grib_yyvsp[(3) - (5)].lval),NULL,NULL,0,NULL,NULL);free((grib_yyvsp[(5) - (5)].str));  }
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 435 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (8)].str),"g1_message_length",(grib_yyvsp[(3) - (8)].lval),(grib_yyvsp[(7) - (8)].explist),NULL,0,NULL,NULL);free((grib_yyvsp[(5) - (8)].str));  }
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 438 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (8)].str),"g1_section4_length",(grib_yyvsp[(3) - (8)].lval),(grib_yyvsp[(7) - (8)].explist),NULL,0,NULL,NULL);free((grib_yyvsp[(5) - (8)].str));  }
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 441 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (3)].str),"ksec",0,(grib_yyvsp[(3) - (3)].explist),NULL,0,NULL,NULL);free((grib_yyvsp[(2) - (3)].str)); }
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 444 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (5)].str),"pad",0,(grib_yyvsp[(4) - (5)].explist),0,0,NULL,NULL);   free((grib_yyvsp[(2) - (5)].str)); }
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 447 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (5)].str),"padto",0,(grib_yyvsp[(4) - (5)].explist),0,0,NULL,NULL);   free((grib_yyvsp[(2) - (5)].str)); }
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 450 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (5)].str),"padtoeven",0,(grib_yyvsp[(4) - (5)].explist),0,0,NULL,NULL);   free((grib_yyvsp[(2) - (5)].str)); }
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 453 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (5)].str),"padtomultiple",0,(grib_yyvsp[(4) - (5)].explist),0,0,NULL,NULL);   free((grib_yyvsp[(2) - (5)].str)); }
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 456 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(5) - (6)].str),"message",(grib_yyvsp[(3) - (6)].lval),0,0,(grib_yyvsp[(6) - (6)].lval),NULL,NULL);   free((grib_yyvsp[(5) - (6)].str));  }
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 459 "griby.y"
    { (grib_yyval.act) = grib_action_create_gen(grib_parser_context,(grib_yyvsp[(2) - (3)].str),"section_padding",0,0,0,(grib_yyvsp[(3) - (3)].lval),NULL,NULL);   free((grib_yyvsp[(2) - (3)].str));  }
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 461 "griby.y"
    { (grib_yyval.act) = grib_action_create_template(grib_parser_context,0,(grib_yyvsp[(2) - (3)].str),(grib_yyvsp[(3) - (3)].str)); free((grib_yyvsp[(2) - (3)].str)); free((grib_yyvsp[(3) - (3)].str));}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 463 "griby.y"
    { (grib_yyval.act) = grib_action_create_template(grib_parser_context,1,(grib_yyvsp[(2) - (3)].str),(grib_yyvsp[(3) - (3)].str)); free((grib_yyvsp[(2) - (3)].str)); free((grib_yyvsp[(3) - (3)].str));}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 466 "griby.y"
    { (grib_yyval.act) = grib_action_create_alias(grib_parser_context,(grib_yyvsp[(2) - (5)].str),(grib_yyvsp[(4) - (5)].str),NULL,(grib_yyvsp[(5) - (5)].lval));  free((grib_yyvsp[(2) - (5)].str)); free((grib_yyvsp[(4) - (5)].str)); }
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 469 "griby.y"
    { (grib_yyval.act) = grib_action_create_alias(grib_parser_context,(grib_yyvsp[(2) - (2)].str),NULL,NULL,0);  free((grib_yyvsp[(2) - (2)].str)); }
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 472 "griby.y"
    {
         (grib_yyval.act) = grib_action_create_alias(grib_parser_context,(grib_yyvsp[(4) - (7)].str),(grib_yyvsp[(6) - (7)].str),(grib_yyvsp[(2) - (7)].str),(grib_yyvsp[(7) - (7)].lval));  free((grib_yyvsp[(2) - (7)].str)); free((grib_yyvsp[(4) - (7)].str)); free((grib_yyvsp[(6) - (7)].str));
    }
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 476 "griby.y"
    {
         (grib_yyval.act) = grib_action_create_alias(grib_parser_context,(grib_yyvsp[(4) - (4)].str),NULL,(grib_yyvsp[(2) - (4)].str),0);  free((grib_yyvsp[(2) - (4)].str)); free((grib_yyvsp[(4) - (4)].str));
    }
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 480 "griby.y"
    { (grib_yyval.act) = grib_action_create_meta(grib_parser_context,(grib_yyvsp[(2) - (8)].str),(grib_yyvsp[(3) - (8)].str),(grib_yyvsp[(5) - (8)].explist),(grib_yyvsp[(7) - (8)].explist),(grib_yyvsp[(8) - (8)].lval),NULL); free((grib_yyvsp[(2) - (8)].str));free((grib_yyvsp[(3) - (8)].str));}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 483 "griby.y"
    { (grib_yyval.act) = grib_action_create_meta(grib_parser_context,(grib_yyvsp[(4) - (10)].str),(grib_yyvsp[(5) - (10)].str),(grib_yyvsp[(7) - (10)].explist),(grib_yyvsp[(9) - (10)].explist),(grib_yyvsp[(10) - (10)].lval),(grib_yyvsp[(2) - (10)].str)); free((grib_yyvsp[(4) - (10)].str));free((grib_yyvsp[(5) - (10)].str));free((grib_yyvsp[(2) - (10)].str));}
    break;

  case 98:

/* Line 1455 of yacc.c  */
#line 486 "griby.y"
    {
      grib_arguments* a = grib_arguments_new(
        grib_parser_context,
        new_accessor_expression(grib_parser_context,(grib_yyvsp[(2) - (5)].str)),
		NULL
        );
      a->next=(grib_yyvsp[(4) - (5)].explist);
      (grib_yyval.act) = grib_action_create_meta(grib_parser_context,
      "ITERATOR","iterator",a,NULL,
      GRIB_ACCESSOR_FLAG_HIDDEN|GRIB_ACCESSOR_FLAG_READ_ONLY,NULL); free((grib_yyvsp[(2) - (5)].str));
    }
    break;

  case 99:

/* Line 1455 of yacc.c  */
#line 498 "griby.y"
    {
      grib_arguments* a = grib_arguments_new(
        grib_parser_context,
        new_accessor_expression(grib_parser_context,(grib_yyvsp[(2) - (5)].str)),
		NULL
        );
      a->next=(grib_yyvsp[(4) - (5)].explist);
      (grib_yyval.act) = grib_action_create_meta(grib_parser_context,
      "NEAREST","nearest",a,NULL,
      GRIB_ACCESSOR_FLAG_HIDDEN|GRIB_ACCESSOR_FLAG_READ_ONLY,NULL); free((grib_yyvsp[(2) - (5)].str));
    }
    break;

  case 100:

/* Line 1455 of yacc.c  */
#line 510 "griby.y"
    {
      grib_arguments* a = grib_arguments_new(
        grib_parser_context,
        new_accessor_expression(grib_parser_context,(grib_yyvsp[(2) - (5)].str)),
		NULL
        );
      a->next=(grib_yyvsp[(4) - (5)].explist);
      (grib_yyval.act) = grib_action_create_meta(grib_parser_context,
      "BOX","box",a,NULL,
      GRIB_ACCESSOR_FLAG_HIDDEN|GRIB_ACCESSOR_FLAG_READ_ONLY,NULL); free((grib_yyvsp[(2) - (5)].str));
    }
    break;

  case 101:

/* Line 1455 of yacc.c  */
#line 522 "griby.y"
    { (grib_yyval.act) = grib_action_create_put(grib_parser_context,(grib_yyvsp[(2) - (5)].str),(grib_yyvsp[(4) - (5)].explist));free((grib_yyvsp[(2) - (5)].str));}
    break;

  case 102:

/* Line 1455 of yacc.c  */
#line 525 "griby.y"
    { (grib_yyval.act) = grib_action_create_remove(grib_parser_context,(grib_yyvsp[(2) - (2)].explist));}
    break;

  case 103:

/* Line 1455 of yacc.c  */
#line 528 "griby.y"
    { (grib_yyval.act) = grib_action_create_assert(grib_parser_context,(grib_yyvsp[(3) - (4)].exp));}
    break;

  case 104:

/* Line 1455 of yacc.c  */
#line 531 "griby.y"
    { (grib_yyval.act) = grib_action_create_modify(grib_parser_context,(grib_yyvsp[(2) - (3)].str),(grib_yyvsp[(3) - (3)].lval)); free((grib_yyvsp[(2) - (3)].str));}
    break;

  case 105:

/* Line 1455 of yacc.c  */
#line 533 "griby.y"
    { (grib_yyval.act) = grib_action_create_set_missing(grib_parser_context,(grib_yyvsp[(2) - (4)].str)); free((grib_yyvsp[(2) - (4)].str)); }
    break;

  case 106:

/* Line 1455 of yacc.c  */
#line 534 "griby.y"
    { (grib_yyval.act) = grib_action_create_set(grib_parser_context,(grib_yyvsp[(2) - (4)].str),(grib_yyvsp[(4) - (4)].exp),0); free((grib_yyvsp[(2) - (4)].str)); }
    break;

  case 107:

/* Line 1455 of yacc.c  */
#line 535 "griby.y"
    { (grib_yyval.act) = grib_action_create_set_darray(grib_parser_context,(grib_yyvsp[(2) - (6)].str),(grib_yyvsp[(5) - (6)].dvalue)); free((grib_yyvsp[(2) - (6)].str)); }
    break;

  case 108:

/* Line 1455 of yacc.c  */
#line 537 "griby.y"
    { (grib_yyval.act) = grib_action_create_set(grib_parser_context,(grib_yyvsp[(2) - (4)].str),(grib_yyvsp[(4) - (4)].exp),1); free((grib_yyvsp[(2) - (4)].str)); }
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 540 "griby.y"
    { (grib_yyval.act) = grib_action_create_write(grib_parser_context,(grib_yyvsp[(2) - (2)].str),0,0); free((grib_yyvsp[(2) - (2)].str));}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 541 "griby.y"
    { (grib_yyval.act) = grib_action_create_write(grib_parser_context,"",0,0); }
    break;

  case 111:

/* Line 1455 of yacc.c  */
#line 542 "griby.y"
    { (grib_yyval.act) = grib_action_create_write(grib_parser_context,(grib_yyvsp[(5) - (5)].str),0,(grib_yyvsp[(3) - (5)].lval)); free((grib_yyvsp[(5) - (5)].str));}
    break;

  case 112:

/* Line 1455 of yacc.c  */
#line 543 "griby.y"
    { (grib_yyval.act) = grib_action_create_write(grib_parser_context,"",0,(grib_yyvsp[(3) - (4)].lval)); }
    break;

  case 113:

/* Line 1455 of yacc.c  */
#line 544 "griby.y"
    { (grib_yyval.act) = grib_action_create_write(grib_parser_context,(grib_yyvsp[(2) - (2)].str),1,0); free((grib_yyvsp[(2) - (2)].str));}
    break;

  case 114:

/* Line 1455 of yacc.c  */
#line 545 "griby.y"
    { (grib_yyval.act) = grib_action_create_write(grib_parser_context,"",1,0); }
    break;

  case 115:

/* Line 1455 of yacc.c  */
#line 546 "griby.y"
    { (grib_yyval.act) = grib_action_create_write(grib_parser_context,(grib_yyvsp[(5) - (5)].str),1,(grib_yyvsp[(3) - (5)].lval)); free((grib_yyvsp[(5) - (5)].str));}
    break;

  case 116:

/* Line 1455 of yacc.c  */
#line 547 "griby.y"
    { (grib_yyval.act) = grib_action_create_write(grib_parser_context,"",1,(grib_yyvsp[(3) - (4)].lval)); }
    break;

  case 117:

/* Line 1455 of yacc.c  */
#line 549 "griby.y"
    { (grib_yyval.act) = grib_action_create_print(grib_parser_context,(grib_yyvsp[(2) - (2)].str),0); free((grib_yyvsp[(2) - (2)].str)); }
    break;

  case 118:

/* Line 1455 of yacc.c  */
#line 550 "griby.y"
    { (grib_yyval.act) = grib_action_create_print(grib_parser_context,(grib_yyvsp[(5) - (5)].str),(grib_yyvsp[(3) - (5)].str)); free((grib_yyvsp[(5) - (5)].str)); free((grib_yyvsp[(3) - (5)].str));}
    break;

  case 119:

/* Line 1455 of yacc.c  */
#line 551 "griby.y"
    { (grib_yyval.act) = grib_action_create_print(grib_parser_context,"",0);  }
    break;

  case 120:

/* Line 1455 of yacc.c  */
#line 555 "griby.y"
    { (grib_yyval.act) = grib_action_create_if(grib_parser_context,(grib_yyvsp[(3) - (7)].exp),(grib_yyvsp[(6) - (7)].act),0,0); }
    break;

  case 121:

/* Line 1455 of yacc.c  */
#line 556 "griby.y"
    { (grib_yyval.act) = grib_action_create_if(grib_parser_context,(grib_yyvsp[(3) - (11)].exp),(grib_yyvsp[(6) - (11)].act),(grib_yyvsp[(10) - (11)].act),0); }
    break;

  case 122:

/* Line 1455 of yacc.c  */
#line 557 "griby.y"
    { (grib_yyval.act) = grib_action_create_if(grib_parser_context,(grib_yyvsp[(3) - (7)].exp),(grib_yyvsp[(6) - (7)].act),0,1); }
    break;

  case 123:

/* Line 1455 of yacc.c  */
#line 558 "griby.y"
    { (grib_yyval.act) = grib_action_create_if(grib_parser_context,(grib_yyvsp[(3) - (11)].exp),(grib_yyvsp[(6) - (11)].act),(grib_yyvsp[(10) - (11)].act),1); }
    break;

  case 124:

/* Line 1455 of yacc.c  */
#line 562 "griby.y"
    { (grib_yyval.act) = grib_action_create_when(grib_parser_context,(grib_yyvsp[(3) - (6)].exp),(grib_yyvsp[(5) - (6)].act),NULL); }
    break;

  case 125:

/* Line 1455 of yacc.c  */
#line 563 "griby.y"
    { (grib_yyval.act) = grib_action_create_when(grib_parser_context,(grib_yyvsp[(3) - (7)].exp),(grib_yyvsp[(6) - (7)].act),NULL); }
    break;

  case 126:

/* Line 1455 of yacc.c  */
#line 564 "griby.y"
    { (grib_yyval.act) = grib_action_create_when(grib_parser_context,(grib_yyvsp[(3) - (11)].exp),(grib_yyvsp[(6) - (11)].act),(grib_yyvsp[(10) - (11)].act)); }
    break;

  case 127:

/* Line 1455 of yacc.c  */
#line 567 "griby.y"
    { (grib_yyval.act) = grib_action_create_set(grib_parser_context,(grib_yyvsp[(2) - (4)].str),(grib_yyvsp[(4) - (4)].exp),0); free((grib_yyvsp[(2) - (4)].str)); }
    break;

  case 128:

/* Line 1455 of yacc.c  */
#line 568 "griby.y"
    { (grib_yyval.act) = grib_action_create_set(grib_parser_context,(grib_yyvsp[(2) - (4)].str),(grib_yyvsp[(4) - (4)].exp),1); free((grib_yyvsp[(2) - (4)].str)); }
    break;

  case 130:

/* Line 1455 of yacc.c  */
#line 572 "griby.y"
    { (grib_yyvsp[(1) - (3)].act)->next = (grib_yyvsp[(2) - (3)].act); (grib_yyval.act) = (grib_yyvsp[(1) - (3)].act); }
    break;

  case 131:

/* Line 1455 of yacc.c  */
#line 576 "griby.y"
    { (grib_yyval.explist) = NULL ;}
    break;

  case 132:

/* Line 1455 of yacc.c  */
#line 577 "griby.y"
    { (grib_yyval.explist) = (grib_yyvsp[(2) - (2)].explist) ;}
    break;

  case 133:

/* Line 1455 of yacc.c  */
#line 580 "griby.y"
    { (grib_yyval.lval) = 0 ; }
    break;

  case 134:

/* Line 1455 of yacc.c  */
#line 581 "griby.y"
    { (grib_yyval.lval) = (grib_yyvsp[(2) - (2)].lval); }
    break;

  case 136:

/* Line 1455 of yacc.c  */
#line 585 "griby.y"
    { (grib_yyval.lval) = (grib_yyvsp[(1) - (3)].lval) | (grib_yyvsp[(3) - (3)].lval); }
    break;

  case 137:

/* Line 1455 of yacc.c  */
#line 588 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_READ_ONLY; }
    break;

  case 138:

/* Line 1455 of yacc.c  */
#line 589 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_LOWERCASE; }
    break;

  case 139:

/* Line 1455 of yacc.c  */
#line 590 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_DUMP; }
    break;

  case 140:

/* Line 1455 of yacc.c  */
#line 591 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_NO_COPY; }
    break;

  case 141:

/* Line 1455 of yacc.c  */
#line 592 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_NO_FAIL; }
    break;

  case 142:

/* Line 1455 of yacc.c  */
#line 593 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_HIDDEN; }
    break;

  case 143:

/* Line 1455 of yacc.c  */
#line 594 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_EDITION_SPECIFIC; }
    break;

  case 144:

/* Line 1455 of yacc.c  */
#line 595 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_CAN_BE_MISSING; }
    break;

  case 145:

/* Line 1455 of yacc.c  */
#line 596 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_CONSTRAINT; }
    break;

  case 146:

/* Line 1455 of yacc.c  */
#line 597 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_OVERRIDE; }
    break;

  case 147:

/* Line 1455 of yacc.c  */
#line 598 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_COPY_OK; }
    break;

  case 148:

/* Line 1455 of yacc.c  */
#line 599 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_TRANSIENT; }
    break;

  case 149:

/* Line 1455 of yacc.c  */
#line 600 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_STRING_TYPE; }
    break;

  case 150:

/* Line 1455 of yacc.c  */
#line 601 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_LONG_TYPE; }
    break;

  case 151:

/* Line 1455 of yacc.c  */
#line 602 "griby.y"
    { (grib_yyval.lval) = GRIB_ACCESSOR_FLAG_DOUBLE_TYPE; }
    break;

  case 152:

/* Line 1455 of yacc.c  */
#line 605 "griby.y"
    { (grib_yyval.act) = grib_action_create_list(grib_parser_context,(grib_yyvsp[(1) - (8)].str),(grib_yyvsp[(4) - (8)].exp),(grib_yyvsp[(7) - (8)].act)); free((grib_yyvsp[(1) - (8)].str)); }
    break;

  case 153:

/* Line 1455 of yacc.c  */
#line 608 "griby.y"
    { (grib_yyval.act) = grib_action_create_while(grib_parser_context,(grib_yyvsp[(3) - (7)].exp),(grib_yyvsp[(6) - (7)].act));  }
    break;

  case 154:

/* Line 1455 of yacc.c  */
#line 611 "griby.y"
    { (grib_yyval.act) = grib_action_create_trigger(grib_parser_context,(grib_yyvsp[(3) - (7)].explist),(grib_yyvsp[(6) - (7)].act));  }
    break;

  case 155:

/* Line 1455 of yacc.c  */
#line 614 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(2) - (6)].str),(grib_yyvsp[(4) - (6)].concept_value),0,0,0,0,0,0,(grib_yyvsp[(6) - (6)].lval),0);  free((grib_yyvsp[(2) - (6)].str)); }
    break;

  case 156:

/* Line 1455 of yacc.c  */
#line 615 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(2) - (9)].str),(grib_yyvsp[(7) - (9)].concept_value),0,0,(grib_yyvsp[(4) - (9)].str),0,0,0,(grib_yyvsp[(9) - (9)].lval),0);  free((grib_yyvsp[(2) - (9)].str));free((grib_yyvsp[(4) - (9)].str)); }
    break;

  case 157:

/* Line 1455 of yacc.c  */
#line 616 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(2) - (12)].str),0,(grib_yyvsp[(6) - (12)].str),0,(grib_yyvsp[(4) - (12)].str),(grib_yyvsp[(8) - (12)].str),(grib_yyvsp[(10) - (12)].str),0,(grib_yyvsp[(12) - (12)].lval),0);  free((grib_yyvsp[(2) - (12)].str));free((grib_yyvsp[(6) - (12)].str));free((grib_yyvsp[(4) - (12)].str));free((grib_yyvsp[(8) - (12)].str));free((grib_yyvsp[(10) - (12)].str)); }
    break;

  case 158:

/* Line 1455 of yacc.c  */
#line 617 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(2) - (14)].str),0,(grib_yyvsp[(6) - (14)].str),0,(grib_yyvsp[(4) - (14)].str),(grib_yyvsp[(8) - (14)].str),(grib_yyvsp[(10) - (14)].str),(grib_yyvsp[(12) - (14)].str),(grib_yyvsp[(14) - (14)].lval),0);  free((grib_yyvsp[(2) - (14)].str));free((grib_yyvsp[(6) - (14)].str));free((grib_yyvsp[(4) - (14)].str));free((grib_yyvsp[(8) - (14)].str));free((grib_yyvsp[(10) - (14)].str));free((grib_yyvsp[(12) - (14)].str)); }
    break;

  case 159:

/* Line 1455 of yacc.c  */
#line 618 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(2) - (10)].str),0,(grib_yyvsp[(6) - (10)].str),0,(grib_yyvsp[(4) - (10)].str),(grib_yyvsp[(8) - (10)].str),0,0,(grib_yyvsp[(10) - (10)].lval),0);  free((grib_yyvsp[(2) - (10)].str));free((grib_yyvsp[(6) - (10)].str));free((grib_yyvsp[(4) - (10)].str));free((grib_yyvsp[(8) - (10)].str)); }
    break;

  case 160:

/* Line 1455 of yacc.c  */
#line 619 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(4) - (14)].str),0,(grib_yyvsp[(8) - (14)].str),(grib_yyvsp[(2) - (14)].str),(grib_yyvsp[(6) - (14)].str),(grib_yyvsp[(10) - (14)].str),(grib_yyvsp[(12) - (14)].str),0,(grib_yyvsp[(14) - (14)].lval),0);  free((grib_yyvsp[(4) - (14)].str));free((grib_yyvsp[(8) - (14)].str));free((grib_yyvsp[(6) - (14)].str));free((grib_yyvsp[(10) - (14)].str)); free((grib_yyvsp[(12) - (14)].str)); free((grib_yyvsp[(2) - (14)].str));}
    break;

  case 161:

/* Line 1455 of yacc.c  */
#line 620 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(4) - (12)].str),0,(grib_yyvsp[(8) - (12)].str),(grib_yyvsp[(2) - (12)].str),(grib_yyvsp[(6) - (12)].str),(grib_yyvsp[(10) - (12)].str),0,0,(grib_yyvsp[(12) - (12)].lval),0);  free((grib_yyvsp[(4) - (12)].str));free((grib_yyvsp[(8) - (12)].str));free((grib_yyvsp[(6) - (12)].str));free((grib_yyvsp[(10) - (12)].str)); free((grib_yyvsp[(2) - (12)].str));}
    break;

  case 162:

/* Line 1455 of yacc.c  */
#line 621 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(4) - (8)].str),(grib_yyvsp[(6) - (8)].concept_value),0,(grib_yyvsp[(2) - (8)].str),0,0,0,0,(grib_yyvsp[(8) - (8)].lval),0);  free((grib_yyvsp[(2) - (8)].str));free((grib_yyvsp[(4) - (8)].str)); }
    break;

  case 163:

/* Line 1455 of yacc.c  */
#line 622 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(4) - (11)].str),(grib_yyvsp[(9) - (11)].concept_value),0,(grib_yyvsp[(2) - (11)].str),(grib_yyvsp[(6) - (11)].str),0,0,0,(grib_yyvsp[(11) - (11)].lval),0);  free((grib_yyvsp[(2) - (11)].str));free((grib_yyvsp[(4) - (11)].str));free((grib_yyvsp[(6) - (11)].str)); }
    break;

  case 164:

/* Line 1455 of yacc.c  */
#line 623 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(2) - (6)].str),(grib_yyvsp[(4) - (6)].concept_value),0,0,0,0,0,0,(grib_yyvsp[(6) - (6)].lval),1);  free((grib_yyvsp[(2) - (6)].str)); }
    break;

  case 165:

/* Line 1455 of yacc.c  */
#line 624 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(2) - (9)].str),(grib_yyvsp[(7) - (9)].concept_value),0,0,(grib_yyvsp[(4) - (9)].str),0,0,0,(grib_yyvsp[(9) - (9)].lval),1);  free((grib_yyvsp[(2) - (9)].str));free((grib_yyvsp[(4) - (9)].str)); }
    break;

  case 166:

/* Line 1455 of yacc.c  */
#line 625 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(2) - (12)].str),0,(grib_yyvsp[(6) - (12)].str),0,(grib_yyvsp[(4) - (12)].str),(grib_yyvsp[(8) - (12)].str),(grib_yyvsp[(10) - (12)].str),0,(grib_yyvsp[(12) - (12)].lval),1);  free((grib_yyvsp[(2) - (12)].str));free((grib_yyvsp[(6) - (12)].str));free((grib_yyvsp[(4) - (12)].str));free((grib_yyvsp[(8) - (12)].str));free((grib_yyvsp[(10) - (12)].str)); }
    break;

  case 167:

/* Line 1455 of yacc.c  */
#line 626 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(2) - (10)].str),0,(grib_yyvsp[(6) - (10)].str),0,(grib_yyvsp[(4) - (10)].str),(grib_yyvsp[(8) - (10)].str),0,0,(grib_yyvsp[(10) - (10)].lval),1);  free((grib_yyvsp[(2) - (10)].str));free((grib_yyvsp[(6) - (10)].str));free((grib_yyvsp[(4) - (10)].str));free((grib_yyvsp[(8) - (10)].str)); }
    break;

  case 168:

/* Line 1455 of yacc.c  */
#line 627 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(4) - (14)].str),0,(grib_yyvsp[(8) - (14)].str),(grib_yyvsp[(2) - (14)].str),(grib_yyvsp[(6) - (14)].str),(grib_yyvsp[(10) - (14)].str),(grib_yyvsp[(12) - (14)].str),0,(grib_yyvsp[(14) - (14)].lval),1);  free((grib_yyvsp[(4) - (14)].str));free((grib_yyvsp[(8) - (14)].str));free((grib_yyvsp[(6) - (14)].str));free((grib_yyvsp[(10) - (14)].str));free((grib_yyvsp[(12) - (14)].str)); free((grib_yyvsp[(2) - (14)].str));}
    break;

  case 169:

/* Line 1455 of yacc.c  */
#line 628 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(4) - (12)].str),0,(grib_yyvsp[(8) - (12)].str),(grib_yyvsp[(2) - (12)].str),(grib_yyvsp[(6) - (12)].str),(grib_yyvsp[(10) - (12)].str),0,0,(grib_yyvsp[(12) - (12)].lval),1);  free((grib_yyvsp[(4) - (12)].str));free((grib_yyvsp[(8) - (12)].str));free((grib_yyvsp[(6) - (12)].str));free((grib_yyvsp[(10) - (12)].str)); free((grib_yyvsp[(2) - (12)].str));}
    break;

  case 170:

/* Line 1455 of yacc.c  */
#line 629 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(4) - (8)].str),(grib_yyvsp[(6) - (8)].concept_value),0,(grib_yyvsp[(2) - (8)].str),0,0,0,0,(grib_yyvsp[(8) - (8)].lval),1);  free((grib_yyvsp[(2) - (8)].str));free((grib_yyvsp[(4) - (8)].str)); }
    break;

  case 171:

/* Line 1455 of yacc.c  */
#line 630 "griby.y"
    { (grib_yyval.act) = grib_action_create_concept(grib_parser_context,(grib_yyvsp[(4) - (11)].str),(grib_yyvsp[(9) - (11)].concept_value),0,(grib_yyvsp[(2) - (11)].str),(grib_yyvsp[(6) - (11)].str),0,0,0,(grib_yyvsp[(11) - (11)].lval),1);  free((grib_yyvsp[(2) - (11)].str));free((grib_yyvsp[(4) - (11)].str));free((grib_yyvsp[(6) - (11)].str)); }
    break;

  case 173:

/* Line 1455 of yacc.c  */
#line 635 "griby.y"
    { (grib_yyval.concept_value) = (grib_yyvsp[(2) - (2)].concept_value); (grib_yyvsp[(2) - (2)].concept_value)->next = (grib_yyvsp[(1) - (2)].concept_value);   }
    break;

  case 175:

/* Line 1455 of yacc.c  */
#line 639 "griby.y"
    { (grib_yyval.case_value) = (grib_yyvsp[(2) - (2)].case_value); (grib_yyvsp[(2) - (2)].case_value)->next = (grib_yyvsp[(1) - (2)].case_value);   }
    break;

  case 176:

/* Line 1455 of yacc.c  */
#line 642 "griby.y"
    { (grib_yyval.case_value) = grib_case_new(grib_parser_context,(grib_yyvsp[(2) - (4)].explist),(grib_yyvsp[(4) - (4)].act));  }
    break;

  case 177:

/* Line 1455 of yacc.c  */
#line 646 "griby.y"
    { (grib_yyval.act) = grib_action_create_switch(grib_parser_context,(grib_yyvsp[(3) - (10)].explist),(grib_yyvsp[(6) - (10)].case_value),(grib_yyvsp[(9) - (10)].act)); }
    break;

  case 178:

/* Line 1455 of yacc.c  */
#line 647 "griby.y"
    { (grib_yyval.act) = grib_action_create_switch(grib_parser_context,(grib_yyvsp[(3) - (9)].explist),(grib_yyvsp[(6) - (9)].case_value),grib_action_create_noop(grib_parser_context,"continue")); }
    break;

  case 179:

/* Line 1455 of yacc.c  */
#line 648 "griby.y"
    { (grib_yyval.act) = grib_action_create_switch(grib_parser_context,(grib_yyvsp[(3) - (7)].explist),(grib_yyvsp[(6) - (7)].case_value),0); }
    break;

  case 180:

/* Line 1455 of yacc.c  */
#line 651 "griby.y"
    {
	  				(grib_yyval.concept_value) = grib_concept_value_new(grib_parser_context,(grib_yyvsp[(1) - (5)].str),(grib_yyvsp[(4) - (5)].concept_condition)); free((grib_yyvsp[(1) - (5)].str));}
    break;

  case 181:

/* Line 1455 of yacc.c  */
#line 653 "griby.y"
    {
	  				(grib_yyval.concept_value) = grib_concept_value_new(grib_parser_context,(grib_yyvsp[(1) - (5)].str),(grib_yyvsp[(4) - (5)].concept_condition)); free((grib_yyvsp[(1) - (5)].str));}
    break;

  case 182:

/* Line 1455 of yacc.c  */
#line 655 "griby.y"
    {
					char buf[80]; sprintf(buf,"%ld",(long)(grib_yyvsp[(1) - (5)].lval)); (grib_yyval.concept_value) = grib_concept_value_new(grib_parser_context,buf,(grib_yyvsp[(4) - (5)].concept_condition));}
    break;

  case 183:

/* Line 1455 of yacc.c  */
#line 657 "griby.y"
    {
					char buf[80]; sprintf(buf,"%g",(double)(grib_yyvsp[(1) - (5)].dval)); (grib_yyval.concept_value) = grib_concept_value_new(grib_parser_context,buf,(grib_yyvsp[(4) - (5)].concept_condition));}
    break;

  case 185:

/* Line 1455 of yacc.c  */
#line 662 "griby.y"
    { (grib_yyvsp[(1) - (2)].concept_condition)->next = (grib_yyvsp[(2) - (2)].concept_condition); (grib_yyval.concept_condition) = (grib_yyvsp[(1) - (2)].concept_condition); }
    break;

  case 186:

/* Line 1455 of yacc.c  */
#line 665 "griby.y"
    { (grib_yyval.concept_condition) = grib_concept_condition_new(grib_parser_context,(grib_yyvsp[(1) - (4)].str),(grib_yyvsp[(3) - (4)].exp)); free((grib_yyvsp[(1) - (4)].str)); }
    break;

  case 187:

/* Line 1455 of yacc.c  */
#line 668 "griby.y"
    { (grib_yyval.exp) = new_accessor_expression(grib_parser_context,(grib_yyvsp[(1) - (1)].str)); free((grib_yyvsp[(1) - (1)].str)); }
    break;

  case 188:

/* Line 1455 of yacc.c  */
#line 669 "griby.y"
    { (grib_yyval.exp) = new_string_expression(grib_parser_context,(grib_yyvsp[(1) - (1)].str));  free((grib_yyvsp[(1) - (1)].str)); }
    break;

  case 190:

/* Line 1455 of yacc.c  */
#line 673 "griby.y"
    { (grib_yyval.exp) = new_long_expression(grib_parser_context,(grib_yyvsp[(1) - (1)].lval));  }
    break;

  case 191:

/* Line 1455 of yacc.c  */
#line 674 "griby.y"
    { (grib_yyval.exp) = new_double_expression(grib_parser_context,(grib_yyvsp[(1) - (1)].dval));  /* TODO: change to new_float_expression*/}
    break;

  case 192:

/* Line 1455 of yacc.c  */
#line 676 "griby.y"
    { (grib_yyval.exp) = NULL; }
    break;

  case 193:

/* Line 1455 of yacc.c  */
#line 677 "griby.y"
    { (grib_yyval.exp) = new_true_expression(grib_parser_context); }
    break;

  case 194:

/* Line 1455 of yacc.c  */
#line 678 "griby.y"
    { (grib_yyval.exp) = (grib_yyvsp[(2) - (3)].exp); }
    break;

  case 195:

/* Line 1455 of yacc.c  */
#line 679 "griby.y"
    { (grib_yyval.exp) = new_unop_expression(grib_parser_context,&grib_op_neg,&grib_op_neg_d,(grib_yyvsp[(2) - (2)].exp)); }
    break;

  case 196:

/* Line 1455 of yacc.c  */
#line 680 "griby.y"
    { (grib_yyval.exp) = new_func_expression(grib_parser_context,(grib_yyvsp[(1) - (3)].str),NULL); free((grib_yyvsp[(1) - (3)].str));}
    break;

  case 197:

/* Line 1455 of yacc.c  */
#line 681 "griby.y"
    { (grib_yyval.exp) = new_func_expression(grib_parser_context,(grib_yyvsp[(1) - (4)].str),(grib_yyvsp[(3) - (4)].explist)); free((grib_yyvsp[(1) - (4)].str));}
    break;

  case 198:

/* Line 1455 of yacc.c  */
#line 685 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_pow,NULL,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 200:

/* Line 1455 of yacc.c  */
#line 689 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_mul,&grib_op_mul_d,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 201:

/* Line 1455 of yacc.c  */
#line 690 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_div,&grib_op_div_d,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 202:

/* Line 1455 of yacc.c  */
#line 691 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_modulo,NULL,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 203:

/* Line 1455 of yacc.c  */
#line 692 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_bit,NULL,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 204:

/* Line 1455 of yacc.c  */
#line 693 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_bitoff,NULL,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 206:

/* Line 1455 of yacc.c  */
#line 697 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_add,&grib_op_add_d,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 207:

/* Line 1455 of yacc.c  */
#line 698 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_sub,&grib_op_sub_d,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 209:

/* Line 1455 of yacc.c  */
#line 702 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_gt,&grib_op_gt_d,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 210:

/* Line 1455 of yacc.c  */
#line 704 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_eq,&grib_op_eq_d,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 211:

/* Line 1455 of yacc.c  */
#line 705 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_lt,&grib_op_lt_d,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 212:

/* Line 1455 of yacc.c  */
#line 706 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_ge,&grib_op_ge_d,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 213:

/* Line 1455 of yacc.c  */
#line 707 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_le,&grib_op_le_d,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 214:

/* Line 1455 of yacc.c  */
#line 708 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_ne,&grib_op_ne_d,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 215:

/* Line 1455 of yacc.c  */
#line 709 "griby.y"
    { (grib_yyval.exp) = new_string_compare_expression(grib_parser_context,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 216:

/* Line 1455 of yacc.c  */
#line 714 "griby.y"
    { (grib_yyval.exp) = new_unop_expression(grib_parser_context,&grib_op_not,NULL,(grib_yyvsp[(2) - (2)].exp)); }
    break;

  case 218:

/* Line 1455 of yacc.c  */
#line 718 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_and,NULL,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp)); }
    break;

  case 220:

/* Line 1455 of yacc.c  */
#line 722 "griby.y"
    { (grib_yyval.exp) = new_binop_expression(grib_parser_context,&grib_op_or,NULL,(grib_yyvsp[(1) - (3)].exp),(grib_yyvsp[(3) - (3)].exp));}
    break;

  case 225:

/* Line 1455 of yacc.c  */
#line 736 "griby.y"
    { (grib_yyval.rule_entry) = grib_new_rule_entry(grib_parser_context,(grib_yyvsp[(1) - (4)].str),(grib_yyvsp[(3) - (4)].exp)); free((grib_yyvsp[(1) - (4)].str)); }
    break;

  case 226:

/* Line 1455 of yacc.c  */
#line 737 "griby.y"
    { (grib_yyval.rule_entry) = grib_new_rule_entry(grib_parser_context,"skip",0);}
    break;

  case 228:

/* Line 1455 of yacc.c  */
#line 741 "griby.y"
    { (grib_yyvsp[(1) - (2)].rule_entry)->next = (grib_yyvsp[(2) - (2)].rule_entry); (grib_yyval.rule_entry) = (grib_yyvsp[(1) - (2)].rule_entry); }
    break;

  case 229:

/* Line 1455 of yacc.c  */
#line 744 "griby.y"
    { (grib_yyval.rules) = grib_new_rule(grib_parser_context,NULL,(grib_yyvsp[(1) - (1)].rule_entry)); }
    break;

  case 230:

/* Line 1455 of yacc.c  */
#line 748 "griby.y"
    { (grib_yyval.rules) = grib_new_rule(grib_parser_context,(grib_yyvsp[(3) - (7)].exp),(grib_yyvsp[(6) - (7)].rule_entry)); }
    break;

  case 232:

/* Line 1455 of yacc.c  */
#line 752 "griby.y"
    { (grib_yyvsp[(1) - (2)].rules)->next = (grib_yyvsp[(2) - (2)].rules); (grib_yyval.rules) = (grib_yyvsp[(1) - (2)].rules); }
    break;



/* Line 1455 of yacc.c  */
#line 3786 "y.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", grib_yyr1[grib_yyn], &grib_yyval, &grib_yyloc);

  YYPOPSTACK (grib_yylen);
  grib_yylen = 0;
  YY_STACK_PRINT (grib_yyss, grib_yyssp);

  *++grib_yyvsp = grib_yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  grib_yyn = grib_yyr1[grib_yyn];

  grib_yystate = grib_yypgoto[grib_yyn - YYNTOKENS] + *grib_yyssp;
  if (0 <= grib_yystate && grib_yystate <= YYLAST && grib_yycheck[grib_yystate] == *grib_yyssp)
    grib_yystate = grib_yytable[grib_yystate];
  else
    grib_yystate = grib_yydefgoto[grib_yyn - YYNTOKENS];

  goto grib_yynewstate;


/*------------------------------------.
| grib_yyerrlab -- here on detecting error |
`------------------------------------*/
grib_yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!grib_yyerrstatus)
    {
      ++grib_yynerrs;
#if ! YYERROR_VERBOSE
      grib_yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T grib_yysize = grib_yysyntax_error (0, grib_yystate, grib_yychar);
	if (grib_yymsg_alloc < grib_yysize && grib_yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T grib_yyalloc = 2 * grib_yysize;
	    if (! (grib_yysize <= grib_yyalloc && grib_yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      grib_yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (grib_yymsg != grib_yymsgbuf)
	      YYSTACK_FREE (grib_yymsg);
	    grib_yymsg = (char *) YYSTACK_ALLOC (grib_yyalloc);
	    if (grib_yymsg)
	      grib_yymsg_alloc = grib_yyalloc;
	    else
	      {
		grib_yymsg = grib_yymsgbuf;
		grib_yymsg_alloc = sizeof grib_yymsgbuf;
	      }
	  }

	if (0 < grib_yysize && grib_yysize <= grib_yymsg_alloc)
	  {
	    (void) grib_yysyntax_error (grib_yymsg, grib_yystate, grib_yychar);
	    grib_yyerror (grib_yymsg);
	  }
	else
	  {
	    grib_yyerror (YY_("syntax error"));
	    if (grib_yysize != 0)
	      goto grib_yyexhaustedlab;
	  }
      }
#endif
    }



  if (grib_yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (grib_yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (grib_yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  grib_yydestruct ("Error: discarding",
		      grib_yytoken, &grib_yylval);
	  grib_yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto grib_yyerrlab1;


/*---------------------------------------------------.
| grib_yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
grib_yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label grib_yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto grib_yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (grib_yylen);
  grib_yylen = 0;
  YY_STACK_PRINT (grib_yyss, grib_yyssp);
  grib_yystate = *grib_yyssp;
  goto grib_yyerrlab1;


/*-------------------------------------------------------------.
| grib_yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
grib_yyerrlab1:
  grib_yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      grib_yyn = grib_yypact[grib_yystate];
      if (grib_yyn != YYPACT_NINF)
	{
	  grib_yyn += YYTERROR;
	  if (0 <= grib_yyn && grib_yyn <= YYLAST && grib_yycheck[grib_yyn] == YYTERROR)
	    {
	      grib_yyn = grib_yytable[grib_yyn];
	      if (0 < grib_yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (grib_yyssp == grib_yyss)
	YYABORT;


      grib_yydestruct ("Error: popping",
		  grib_yystos[grib_yystate], grib_yyvsp);
      YYPOPSTACK (1);
      grib_yystate = *grib_yyssp;
      YY_STACK_PRINT (grib_yyss, grib_yyssp);
    }

  *++grib_yyvsp = grib_yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", grib_yystos[grib_yyn], grib_yyvsp, grib_yylsp);

  grib_yystate = grib_yyn;
  goto grib_yynewstate;


/*-------------------------------------.
| grib_yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
grib_yyacceptlab:
  grib_yyresult = 0;
  goto grib_yyreturn;

/*-----------------------------------.
| grib_yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
grib_yyabortlab:
  grib_yyresult = 1;
  goto grib_yyreturn;

#if !defined(grib_yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| grib_yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
grib_yyexhaustedlab:
  grib_yyerror (YY_("memory exhausted"));
  grib_yyresult = 2;
  /* Fall through.  */
#endif

grib_yyreturn:
  if (grib_yychar != YYEMPTY)
     grib_yydestruct ("Cleanup: discarding lookahead",
		 grib_yytoken, &grib_yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (grib_yylen);
  YY_STACK_PRINT (grib_yyss, grib_yyssp);
  while (grib_yyssp != grib_yyss)
    {
      grib_yydestruct ("Cleanup: popping",
		  grib_yystos[*grib_yyssp], grib_yyvsp);
      YYPOPSTACK (1);
    }
#ifndef grib_yyoverflow
  if (grib_yyss != grib_yyssa)
    YYSTACK_FREE (grib_yyss);
#endif
#if YYERROR_VERBOSE
  if (grib_yymsg != grib_yymsgbuf)
    YYSTACK_FREE (grib_yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (grib_yyresult);
}



/* Line 1675 of yacc.c  */
#line 756 "griby.y"


static grib_concept_value *reverse_concept(grib_concept_value *r,grib_concept_value *s)
{
    grib_concept_value *v;

    if(r == NULL) return s;

    v         = r->next;
    r->next   = s;
    return reverse_concept(v,r);
}


static grib_concept_value* reverse(grib_concept_value* r)
{
    return reverse_concept(r,NULL);
}


