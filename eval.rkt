#lang racket



(struct ast-prim (name) #:transparent)
(struct ast-block (body) #:transparent)
(struct ast-bind (name body) #:transparent)
(struct ast-let (name arg body) #:transparent)
(struct ast-handle (body clauses) #:transparent)
(provide ast-prim ast-block ast-bind
         ast-let ast-handle)

;; ===================================================
;; ===================================================
;; MACHINE
;; ===================================================
;; ===================================================

;; type Effect = String
;; An effect is a operator that engages a handler in
;; the handler stack. If there is no handler in the stack
;; that mentions the effect by it's name, then the effect
;; cannot be discharged.

;; type Expression = Word
;; An expression is a list of operators, commonly called
;; 'words' in Forth and related languages.

;; type MachineState = (Stack Value, Expression, Stack HandlerSet, Stack Expression)
;; A machine consists of:
;; 1. a stack of values, which will end up as the result
;;    of evaluation
;; 2. an expression to evaluate
;; 3. a stack of effect handlers, which contain pushed
;;    handlers to discharge any effects that arise
;; 4. a stack of 'delayed' expressions, which are something
;;    like continuations that have (one?) use (I think)
(struct machine (stack expr handlers delay) #:transparent)

;; ===================================================
;; ===================================================
;; EVALUATION
;; ===================================================
;; ===================================================

;; subst :: Var, Expr, Expr -> Expr
(define (subst name rep target)
  (define (subst-clause clause)
    (match clause
      [(list n b)
       (list n (subst-rec b))]))
  (define (subst-rec t)
    (cond
      [(list? target) (flatten (map (lambda (x) (subst-rec x)) target))]
      [(string? target) (if (eq? name target) rep name)]
      [(ast-block? target) (ast-block (subst-rec (ast-block-body target)))]
      [(ast-bind? target)
       (if (eq? name (ast-bind-name target))
           target
           (ast-bind (ast-bind-name target) (subst-rec (ast-bind-body target))))]
      [(ast-let? target)
       (ast-let (ast-let-name target)
                (subst-rec (ast-let-arg target))
                (if (eq? name (ast-let-name target))
                    (ast-let-body target)
                    (subst-rec (ast-let-body target))))]
      [(ast-handle? target)
       (ast-handle (subst-rec (ast-handle-body target))
                   (map (lambda (x) (subst-clause x)) (ast-handle-clauses target)))]
      [#t target]))
  (subst-rec target))

;; find-handler-set :: Effect, Stack HandlerSet -> HandlerSet
(define (find-handler-set op hs)
  (match hs
    [(list) (error (string-append "couldn't find effect: " op))]
    [(list h hs ...)
     (if (assoc op h)
         h
         (find-handler-set op hs))]))

;; remove-handlers-until :: Effect, Stack HandlerSet -> Stack HandlerSet
(define (remove-handlers-until op hs)
  (match hs
    ;; error will be caught by find-handler-set first
    [(list h hs ...)
     (if (assoc op h)
         hs
         (remove-handlers-until op hs))]))

;; eval-barley :: Expression -> Stack | Error
;; Runs the expression e on an initial (empty)
;; machine state with some effect specifications.
;; The machine terminates when the expression is
;; 'empty' (expressions are just a list of operators)
;; and there are no remaining handlers to discharge.
;; Alternatively, a machine can terminate when an
;; effect is not discharged by a handler; this is
;; considered an error, or 'stuck', state.
(define (eval-barley e)
  ;; step-eval :: MachineState -> Stack | Error
  (define (step-eval state)
    (if (and (empty? (machine-expr state))
             (empty? (machine-handlers state)))
        (machine-stack state)
        (step-eval (step state))))
  ;; step :: MachineState -> MachineState | Error
  (define (step state)
    (displayln state)
    (match state
      ;; THE FIRST FOUR CASES ARE THE BIG ONES THAT HANDLE
      ;; EFFECTS. NOTE HOW EASILY WE CAN ADD THEM ON TOP OF
      ;; OUR OTHER OPERATIONS. THE OTHER OPS DO NOT CARE ABOUT
      ;; HANDLERS OR DELAYED EXPRESSIONS.
      ;; We have an effect op on top of the stack... discharge it
      [(machine (list op s ...) e h (list e1 es ...))
       #:when (effect? op)
       (machine s
                (append (assoc op (find-handler-set op h)) e1)
                (remove-handlers-until op h)
                es)]
      ;; We have an empty expression, but some handlers left, discharge
      ;; the one on top of the stack
      [(machine s (list) h (list e1 es ...))
       (machine s
                (append (assoc "otherwise" h) e1)
                (rest h)
                es)]
      ;; Resume from within a handler clause
      [(machine s (list (ast-prim "resume") e ...) h (list e1 es ...))
       (machine s (append e1 e) h es)]
      ;; Evaluate an expression within a handler
      [(machine s (list (ast-handle body hs) e ...) h (list es ...))
       (machine s body (cons hs h) (list* e es))]


      ;; AND NOW THE SIMPLE REDUCTION RULES FROM lambda-o
      ;; Pushing values or effects
      [(machine s (list c e ...) h es)
       #:when (or (number? c)
                  (boolean? c)
                  (ast-block? c)
                  (effect? c))
       (machine (cons c s) e h es)]
      
      ;; Value binding
      [(machine (list v s ...) (list (ast-bind name body) e ...) h es)
       (machine s (append (subst name (list v) body) e) h es)]
      
      ;; Let binding
      [(machine s (list (ast-let name arg body) e ...) h es)
       (machine s (append (subst name arg body) e) h es)]
      
      ;; Primitives
      [(machine (list n1 n2 s ...) (list (ast-prim "add") e ...) h es)
       (machine (list* (+ n1 n2) s) e h es)]
      
      [(machine (list b s ...) (list (ast-prim "not") e ...) h es)
       (machine (list* (not b) s) e h es)]
      
      [(machine (list (ast-block exp) s ...) (list (ast-prim "call") e ...) h es)
       (machine s (append exp e) h es)]
      
      [(machine (list (ast-block exp) s ...) (list (ast-prim "fix") e ...) h es)
       (machine (list (ast-block (list* (ast-block exp) (ast-prim "fix"))) s)
                (append exp e) h es)]
      
      [(machine (list #t v1 v2 s ...) (list (ast-prim "if") e ...) h es)
       (machine (list* v1 s) e h es)]
      
      [(machine (list #f v1 v2 s ...) (list (ast-prim "if") e ...) h es)
       (machine (list* v2 s) e h es)]
      
      [(machine (list n1 n2 s ...) (list (ast-prim "eq") e ...) h es)
       #:when (and (number? n1) (number? n2))
       (machine (list* (eq? n1 n2) s) e h es)]
      
      [(machine (list n1 n2 s ...) (list (ast-prim "less") e ...) h es)
       #:when (and (number? n1) (number? n2))
       (machine (list* (< n2 n1) s) e h es)]))
    
      
  (step-eval (machine (list) e (list) (list))))
(provide eval-barley)


(define (effect? word)
  (and (string? word)
       (string-prefix? word "%")))