%{
open Ast
open Typs
open Lexing
%}

%token <float> FLOAT
%token <int> INT
%token <string> STRING
%token <string> ID
%token TRUE
%token FALSE

%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token SEMICOLON
%token ASTERISK
%token PERCENTAGE
%token QUOTE
%token INTEGERS
%token STRINGS
%token BOOLS
%token FLOATS
%token GREATER
%token LESS
%token EQUAL
%token GREATER_EQUAL
%token LESS_EQUAL
%token NOT_EQ
%token LK
%token NT

%token SELECT
%token FROM
%token WHERE
%token INSERT
%token JOIN
%token CREATE
%token UPDATE
%token DELETE
%token DROP
%token SET
%token ON
%token INTO
%token DATABASE
%token TABLE
%token VALUES
%token USE
%token SHOW
%token DATABASES
%token EXIT

%token EOF

%start prog
%type <Ast.expr> prog

%%

prog:
  | s = statement; EOF              {s}
  | s = statement; SEMICOLON; EOF   {s}
  ;

statement:
  | q1 = statement; JOIN; q2 = statement; ON; c1 = ID; EQUAL; c2 = ID
      {JoinQueries(q1, q2, (c1, c2))}
  | t1 = ID; JOIN; t2 = ID; ON; c1 = ID; EQUAL; c2 = ID
      {JoinTables(t1, t2, (c1, c2))}
  | tab = ID; JOIN; query = statement; ON; c1 = ID; EQUAL; c2 = ID
      {JoinTabQuer(tab, query, (c1, c2))}
  | query = statement; JOIN; tab = ID; ON; c1 = ID; EQUAL; c2 = ID
      {JoinQuerTab(query, tab, (c1, c2))}
  | SELECT; cols = col_list; FROM; tab = ID; w = where_condition
      {Select(cols, tab, w)}
  | SELECT; ASTERISK; FROM; tab = ID; w = where_condition
      {SelectAll(tab, w)}
  | INSERT; INTO; tab = ID; LEFT_PAREN; cols = col_list; RIGHT_PAREN;
      VALUES; LEFT_PAREN; vals = val_list RIGHT_PAREN
        {Insert(tab, cols, vals)}
  | INSERT; INTO; tab = ID; VALUES; LEFT_PAREN; vals = val_list; RIGHT_PAREN
      {InsertAll(tab, vals)}
  | UPDATE; tab = ID; SET; pairs = pair_list; w = where_condition
      {Update(tab, pairs, w)}
  | DELETE; FROM; tab = ID; WHERE; w = where_condition
      {Delete(tab, w)}
  | DELETE; FROM; tab = ID; w = where_condition
      {Delete(tab, w)}
  | CREATE; TABLE; tab = ID; LEFT_PAREN; decs = dec_list; RIGHT_PAREN
      {CreateTable(tab, decs)}
  | CREATE; DATABASE; db = ID       {CreateDb(db)}
  | DROP; TABLE; tab = ID           {DropTable(tab)}
  | DROP; DATABASE; db = ID         {DropDb(db)}
  | USE; db = ID                    {Use(db)}
  | SHOW; DATABASES                 {ShowDatabases}
  | EXIT                            {ExitDb}
  ;

col_list: cols = rev_col_list  {List.rev cols};

val_list: vals = rev_val_list  {List.rev vals};

dec_list: decs = rev_dec_list  {List.rev decs};

pair_list: pairs = rev_pair_list {List.rev pairs};

rev_col_list:
  | (* empty *)                           {[]}
  | col = ID                              {[col]}
  | cols = rev_col_list; COMMA; col = ID  {col::cols}
  ;

rev_val_list:
  | (* empty *)                                 {[]}
  | v = value                                   {[v]}
  | vals = rev_val_list; COMMA; v = value       {v::vals}
  ;

rev_dec_list:
  | col = ID; t = supported                                {[(col, t)]}
  | decs = rev_dec_list; COMMA; col = ID; t = supported    {(col, t)::decs}
  ;

rev_pair_list:
  | (* empty *)                                            {[]}
  | col = ID; EQUAL; v = value                             {[(col, v)]}
  | ps = rev_pair_list; COMMA; col = ID; EQUAL; v = value  {(col, v)::ps}
  ;

where_condition:
  | (* empty *)
      {Null}
  | WHERE; col = ID; LK; QUOTE; PERCENTAGE; v = ID; QUOTE
      {Condition(col, LikeEnd, VString(v))}
  | WHERE; col = ID; LK; QUOTE; v = ID; PERCENTAGE; QUOTE
      {Condition(col, LikeBegin, VString(v))}
  | WHERE; col = ID; LK; QUOTE; PERCENTAGE; v = ID; PERCENTAGE; QUOTE
      {Condition(col, LikeSubstring, VString(v))}
  | WHERE; col = ID; NT; LK; QUOTE; PERCENTAGE; v = ID; QUOTE
      {Condition(col, NotLikeEnd, VString(v))}
  | WHERE; col = ID; NT; LK; QUOTE; v = ID; PERCENTAGE; QUOTE
      {Condition(col, NotLikeBegin, VString(v))}
  | WHERE; col = ID; NT; LK; QUOTE; PERCENTAGE; v = ID; PERCENTAGE; QUOTE
      {Condition(col, NotLikeSubstring, VString(v))}
  | WHERE; col = ID; o = operator; v = value
      {Condition(col, o, v)}
  ;

operator:
  | GREATER         {Gt}
  | LESS            {Lt}
  | EQUAL           {Eq}
  | GREATER_EQUAL   {GtEq}
  | LESS_EQUAL      {LtEq}
  | NOT_EQ          {NotEq}
  ;

value:
  | TRUE            {VBool(true)}
  | FALSE           {VBool(false)}
  | i = INT         {VInt(i)}
  | str = STRING    {VString(str)}
  | f = FLOAT       {VFloat(f)}
  ;

supported:
  | INTEGERS        {VInt(0)}
  | BOOLS           {VBool(true)}
  | FLOATS          {VFloat(0.)}
  | STRINGS         {VString("")}
  ;