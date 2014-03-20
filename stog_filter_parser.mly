/* Filter parser */

%token <string> Attribute
%token <string> String

%token LPAR RPAR
%token EQUAL
%token AND OR NOT
%token COLON

%token EOF

%type <Stog_filter_types.t> filter
%start filter

%%

filter: filter_exp EOF { $1 }

filter_exp:
| LPAR filter_exp RPAR { $2 }
| Attribute EQUAL String { Stog_filter_types.Pred (("", $1), $3) }
| Attribute COLON Attribute EQUAL String { Stog_filter_types.Pred (($1, $3), $5) }
| filter_exp OR filter_exp { Stog_filter_types.Or ($1, $3) }
| filter_exp AND filter_exp { Stog_filter_types.And ($1, $3) }
| NOT filter_exp { Stog_filter_types.Not $2 }
