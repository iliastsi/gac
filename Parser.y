{
module Parser where
import Lexer
}

%partial parser Prog
%tokentype { Token }
%error { happyError }
%monad { P }
%lexer { lexer } { T_EOF }

%left '|'
%left '&'
%nonassoc '==' '!=' '>' '<' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%nonassoc '(' ')'

%token
    kwByte      { T_kwByte      }
    return      { T_Return      }
    else        { T_Else        }
    while       { T_While       }
    false       { T_False       }
    true        { T_True        }
    if          { T_If          }
    kwInt       { T_kwInt       }
    proc        { T_Proc        }
    reference   { T_Reference   }
    id          { T_Id      $$  }
    int         { T_Int     $$  }
    char        { T_Char    $$  }
    string      { T_String  $$  }
    '='         { T_Assign      }
    '+'         { T_Plus        }
    '-'         { T_Minus       }
    '*'         { T_Times       }
    '/'         { T_Div         }
    '%'         { T_Mod         }
    '!'         { T_Not         }
    '&'         { T_And         }
    '|'         { T_Or          }
    '=='        { T_Eq          }
    '!='        { T_Ne          }
    '<'         { T_Lt          }
    '>'         { T_Gt          }
    '<='        { T_Le          }
    '>='        { T_Ge          }
    '('         { T_Op          }
    ')'         { T_Cp          }
    '['         { T_Os          }
    ']'         { T_Cs          }
    '{'         { T_Oc          }
    '}'         { T_Cc          }
    ','         { T_Comma       }
    ';'         { T_SemiColon   }



%%

Prog
  : Exp             { $1 }

Exp :
    '(' Exp ')'     { $2 }
  | Exp '*' Exp     { $1 * $3 }
  | Exp '+' Exp     { $1 + $3 }
  | Exp '-' Exp     { $1 - $3 }
  | int             { $1 }


{

happyError :: Token -> P a
happyError _ = P $ \inp sc -> error ("We have to put something in here")

}
