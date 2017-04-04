%{

%}

%token <string> STRING
%token <int> NEG_INT
%token <int> POS_INT
%token <float> FLOAT

%token CAT
%token NUM
%token LCURLY
%token RCURLY
%token COMMA
%token EOL
%token EOF
%token <string> COMMENT

%start header
%start row

%type <Csv_types.header> header
%type <Csv_types.row> row

%%

header:
| columns EOL { $1 }
| columns COMMENT EOL { $1 }
| newlines columns EOL { $2 }
| newlines columns COMMENT EOL { $2 }

columns:
| STRING COMMA columns { ($1, `Untyped) :: $3 }
| STRING { [ ($1, `Untyped) ] }
| STRING CAT COMMA columns { ($1, `Cat) :: $4 }
| STRING CAT { [ ($1, `Cat) ] }
| STRING NUM COMMA columns { ($1, `Num) :: $4 }
| STRING NUM { [ ($1, `Num) ] }

value:
| NEG_INT { (`Int $1) }
| POS_INT { (`Int $1) }
| FLOAT { (`Float $1) }
| STRING { (`String $1) }

value_opt:
| value { Some $1}
| { None }

values:
| value_opt COMMA values { $1 :: $3 }
| value_opt { [ $1 ] }

row:
| row_sans_nl EOL { $1 }
| row_sans_nl COMMENT EOL { $1 }
| newlines row_sans_nl EOL { $2 }
| newlines row_sans_nl COMMENT EOL { $2 }
| newlines EOF { `EOF }
| EOF { `EOF }

row_sans_nl:
| dense_row { $1 }
| sparse_row { `Sparse $1 }

dense_row:
| values { Csv_types.parse_opt_row $1 }

sparse_row:
| LCURLY pairs RCURLY { $2 }
| LCURLY RCURLY { [] }

pairs:
| pair COMMA pairs { $1 :: $3 }
| pair { [ $1 ] }

pair:
| POS_INT value { $1, $2 }

newlines:
| EOL { () }
| EOL newlines { () }
| COMMENT newlines { () }
