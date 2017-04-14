#lang br/quicklang

(require "eval.rkt")

(define-macro (barley-module-begin PARSE-TREE)
  #'(#%module-begin PARSE-TREE))
(provide (rename-out [barley-module-begin #%module-begin]))

(define-macro (bly-program EXPR)
  #'(eval-barley EXPR))
(provide bly-program)

(define-macro (bly-expr WORDS ...)
  #'(list WORDS ...))
(provide bly-expr)

(define-macro (bly-word WORD)
  #'WORD)
(provide bly-word)

(define-macro (bly-prim PRIM)
  #'(cond
      [(eq? PRIM "true") #t]
      [(eq? PRIM "false") #f]
      [#t (ast-prim PRIM)]))
(provide bly-prim)

(define-macro (bly-block "{" WORDS "}")
  #'(ast-block WORDS))
(provide bly-block)

(define-macro (bly-bind "bind" VAR "(" WORDS ")")
  #'(ast-bind VAR WORDS))
(provide bly-bind)

(define-macro (bly-let "let" VAR "=" ARGS "(" BODY ")")
  #'(ast-let VAR ARGS BODY))
(provide bly-let)

(define-macro (bly-handle "handle" BODY "with" HANDLERS ... DEFAULT)
  #'(ast-handle BODY (list HANDLERS ... DEFAULT)))
(provide bly-handle)

(define-macro (bly-op-case OPNAME "=>" BODY ";")
  #'(list OPNAME BODY))
(provide bly-op-case)

(define-macro (bly-default-case "otherwise" "=>" BODY ";")
  #'(list "otherwise" BODY))
(provide bly-default-case)