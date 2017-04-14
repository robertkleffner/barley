#lang br
(require "lexer.rkt" brag/support rackunit)

(define (lex str)
  (apply-lexer barley-lexer str))

(check-equal? (lex "") empty)
(check-equal?
 (lex "12")
 (list (srcloc-token (token 'INT 12)
                     (srcloc 'string #f #f 1 2))))

(check-exn exn:fail:read? (lambda () (lex "+")))