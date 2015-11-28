%{
open Query
open Interpret
%}

(* value tokens *)
%token <float> FLOAT
%token <int> INT
%token <string> COLUMN
%token <string> STRING
%token <Date.t> DATE
%token <string> ID
%token TRUE
%token FALSE

(* symbols *)
%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON

(* where ops *)
%token GREATER
%token LESS
%token EQUAL
%token GREATER_EQUAL
%token LESS_EQUAL
%token NOT_EQ
%token LIKE_REGEX

(* expression keywords *)
%token SELECT
%token FROM
%token WHERE
%token INSERT
%token JOIN
%token CREATETABLE
%token CREATEDB
%token UPDATE
%token DELETE
%token DROPTABLE
%token DROPDB
%token SET

%token EOF
%start <Interpret.expr> prog
%%
prog:
  | s = statement; EOF              {s}
  | s = statement; SEMICOLON; EOF   {s}
  ;

statement:
  | q1 = statement; JOIN; q2 = statement; ON; c1 = COLUMN; EQUAL; c2 = COLUMN
      {JoinQueries(q1, q2, (c1, c2))}
  | t1 = ID; JOIN; t2 = ID; ON; c1 = COLUMN; EQUAL; c2 = COLUMN
      {JoinTables(t1, t2, (c1, c2))}
  | tab = ID; JOIN; query = statement; ON; c1 = COLUMN; EQUAL; c2 = COLUMN
      {JoinTabQuer(tab, query, (c1, c2))}
  | query = statement; JOIN; tab = ID; ON; c1 = COLUM; EQUAL; c2 = COLUMN
      {JoinQueries(query, tab, (c1, c2))}
  | SELECT; cols = col_list; FROM; tab = ID; WHERE; w = where_condition
      {Select(cols, tab, w)}
  | SELECT; cols = col_list; FROM; tab = ID; w = where_condition
      {Select(cols, tab, w)}
  | INSERT; tab = ID; cols = col_list; vals = val_list
      {Insert(tab, cols, vals)}
  | UPDATE; tab = ID; SET; pairs = pair_list; WHERE; w = where_condition
      {Update(tab, pairs, w)}
  | DELETE; tab = ID; SET; pairs = pair_list; WHERE; w = where_condition
      {Delete(tab, pairs, w)}
  | CREATETABLE; tab = ID; LEFT_PAREN; decs = dec_list; RIGHT_PAREN
      {CreateTable(tab, decs)}
  | CREATEDB; db = ID
      {CreateDb(db)}
  | DROPTABLE; tab = ID
      {DropTable(db)}
  | DROPDB; db = ID
      {DropDb(db)}
  ;

col_list: cols = rev_col_list  {List.rev cols};

val_list: vals = rev_val_list  {List.rev vals};

dec_list: decs = rev_dec_list  {List.rev decs};

rev_col_list:
  | (* empty *)                               {[]}
  | col = COLUMN                              {[col]}
  | cols = rev_col_list; COMMA; col = COLUMN  {col::cols}
  ;

rev_val_list:
  | (* empty *)                                 {[]}
  | val = value                                 {[val]}
  | vals = rev_val_list; COMMA; val = value     {val::vals}
  ;

rev_dec_list:
  | (* empty *)                                             {[]}
  | col = COLUMN; val = value                               {[(col, val)]}
  | decs = rev_dec_list; COMMA; col = COLUMN; val = value   {(col, val)::decs}
  ;

where_condition:
  | (* empty *)                                {Null}
  | col = COLUMN; o = operator; v = value      {Condition(col, o, v)}
  ;

operator:
  | GREATER         {Gt}
  | LESS            {Lt}
  | EQUAL           {Eq}
  | GREATER_EQUAL   {GtEq}
  | LESS_EQUAL      {LtEq}
  | NOT_EQ          {NotEq}
  | LIKE_REGEX      {Like}
  ;

value:
  | b = TRUE        {VBool(true)}
  | b = FALSE       {VBool(false)}
  | i = INT         {VInt(i)}
  | str = STRING    {VString(str)}
  | f = FLOAT       {VFloat(f)}
  | v = DATE        {VDate(v)}
  ;