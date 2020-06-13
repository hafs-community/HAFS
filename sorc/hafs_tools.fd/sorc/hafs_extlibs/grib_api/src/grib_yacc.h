
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
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

/* Line 1676 of yacc.c  */
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



/* Line 1676 of yacc.c  */
#line 286 "y.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define grib_yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE grib_yylval;


