#lang br
(require brag/support)

(define barley-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   [(from/to "--" "\n")
    (token 'COMMENT lexeme #:skip? #t)]
   [whitespace
    (token 'WHITESPACE lexeme #:skip? #t)]
   [(:or "add" "mul" "eq" "less" "not" "call" "fix" "if" "resume"
         "{" "}" "=" "=>" ";" "(" ")"
         "bind" "let" "handle" "with" "otherwise"
         "true" "false")
    (token lexeme lexeme)]
   [(:seq alphabetic (:* (:or alphabetic numeric)))
    (token 'VAR (string->symbol lexeme))]
   [(:seq "%" alphabetic (:* (:or alphabetic numeric)))
    (token 'OPNAME (string->symbol lexeme))]
   [(:+ (char-set "0123456789"))
    (token 'INT (string->number lexeme))]))

(provide barley-lexer)