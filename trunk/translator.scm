#!/usr/bin/guile -s
!#
(define-module (smile std))
(use-syntax (ice-9 syncase))
(use-modules
  (oop goops)
  (ice-9 receive)
  (srfi srfi-1)
  (ice-9 optargs)
  (ice-9 q))
(define-macro
  (simple-syntax header . body)
  (let ((name (car header))
        (bindings (cdr header))
        (x (gensym)))
    `(define-syntax
       ,name
       (lambda ((unquote x))
         (syntax-case
           ,x
           ()
           ((_ (unquote-splicing bindings))
            (syntax (unquote-splicing body))))))))
(export simple-syntax)
(define-macro
  (gensyms a . rest)
  (unless
    (list? a)
    (error "gensyms expects list of symbols as it's first argument"))
  `(let ,(%~ map i in a (list i (quote (gensym))))
     ,@rest))
(export gensyms)
(define-macro
  (with woot arg . thunk)
  (let ((with-woot (symbol-append (quote with-) woot)))
    `(,with-woot
      arg
      (lambda () (unquote-splicing thunk)))))
(export with)
(simple-syntax
  (%% fn (x y ...) (a b ...) c1 c2 ...)
  (fn (lambda (x y ...) c1 c2 ...) a b ...))
(export %%)
(define-macro
  (% fn bindings . rest)
  (receive
    (collections body)
    (split-at rest (length bindings))
    `(%% ,fn
         ,bindings
         ,collections
         ,@body)))
(export %)
(define (make-eqv y) (lambda (x) (eqv? x y)))
(export make-eqv)
(define-macro
  (%~ fn . args)
  (receive
    (bindings rest)
    (break! (make-eqv (quote in)) args)
    `(% ,fn
        ,bindings
        ,@(cdr rest))))
(export %~)
(define (compliment fn)
  (lambda x (not (apply fn x))))
(export compliment)
(define-public ~ compliment)
(simple-syntax
  (with-cc cc a b ...)
  (call-with-current-continuation
    (lambda (cc) a b ...)))
(export with-cc)
(define-macro
  (for . x)
  (append (quote (%~ for-each)) x))
(export for)
(define-method
  (for-each fn (s1 <promise>))
  (stream-for-each fn s1))
(define-method
  (for-each fn (s1 <promise>) (s2 <promise>))
  (stream-for-each fn s1 s2))
(define-method
  (for-each
    fn
    (s1 <promise>)
    (s2 <promise>)
    (s3 <promise>))
  (stream-for-each fn s1 s2 s3))
(define-method
  (for-each fn (s1 <hashtable>))
  (hash-for-each (lambda (x y) (fn (cons x y))) s1))
(define-macro
  (calc-for . x)
  (append (quote (%~ map)) x))
(export calc-for)
(simple-syntax
  (do-times count c1 c2 ...)
  (let req ((n count))
    (when (> n 0) (begin c1 c2 ...) (req (1- n)))))
(export do-times)
(simple-syntax
  (when cnd c1 c2 ...)
  (if cnd (begin c1 c2 ...) *unspecified*))
(export when)
(simple-syntax
  (unless cnd c1 c2 ...)
  (if cnd *unspecified* (begin c1 c2 ...)))
(export unless)
(simple-syntax
  (bind-vars lst (a b ...) c d ...)
  (receive (a b ...) (apply values lst) c d ...))
(export bind-vars)
(define-macro
  (lambda-bind-vars vars . body)
  (gensyms
    (tmp)
    `(lambda ((unquote tmp))
       (bind-vars
         ,tmp
         ,vars
         ,@body))))
(export lambda-bind-vars)
(define (positional-only args)
  (let ((tmp (make-q)))
    (let req ((args args))
      (cond ((null? args) (q->list tmp))
            ((keyword? (car args)) (req (cl-cddr args)))
            (else (enq! tmp (car args)) (req (cdr args)))))))
(export positional-only)
(define*
  (print #:key
         (port (current-output-port))
         (func display)
         (before "")
         (delim " ")
         (after "
")
         #:rest
         args)
  (display before port)
  (let ((args (positional-only args)))
    (unless
      (null? args)
      (let req ((args args))
        (func (car args) port)
        (let ((tail (cdr args)))
          (unless
            (null? tail)
            (display delim port)
            (req tail))))))
  (display after port))
(export print)
(define-macro
  (set-slots! instance . pairs)
  (unless
    (symbol? instance)
    (error "set-slots! requires symbol as it's first argument"))
  (unless
    (list? pairs)
    (error "set-slots! expects list of lists as it's second argument"))
  (cons 'begin
        (append
          (%~ map
              pair
              in
              pairs
              (if ($ ,(pair? pair)
                     and
                     ,(pair? (cdr pair)))
                (let ((symbol (car pair)) (value (cadr pair)))
                  (if (symbol? symbol)
                    `(slot-set!
                       ,instance
                       ',symbol
                       ,value)
                    (error "set-slots! syntax error")))
                (error "set-slots! expects list of lists as it's second argument")))
          `((unquote instance)))))
(export set-slots!)
(define-macro
  (make-instance-and-setup class . initial-vals)
  (unless
    (symbol? class)
    (error "make-instance-and-setup requires class name as it's first argument"))
  (unless
    (list? initial-vals)
    (error "make-instance-and-setup expects list of lists as it's second argument"))
  (gensyms
    (result)
    `(let ((,result
            (make-instance (unquote class))))
       (set-slots!
         ,result
         ,@initial-vals))))
(export make-instance-and-setup)
(define-public rest cdr)
(define-public q->list car)
(define-public q-top q-front)
(define-public
  (list->q lst)
  (let ((q (cons lst #f))) (sync-q! q) q))
(define-public
  (cl-car x)
  (if (null? x) (quote ()) (car x)))
(define-public
  (cl-cdr x)
  (if (null? x) (quote ()) (cdr x)))
(define-macro
  (cl-c___r ___ x)
  (let* ((lst (map string->symbol
                   (map string (string->list (symbol->string ___))))))
    (let calc ((lst lst))
      (if (null? lst)
        x
        (let ((fun (symbol-append (quote cl-c) (car lst) (quote r))))
          (list fun (calc (cdr lst))))))))
(define-macro
  (define-cl-c___rs a b)
  (let ((result (make-q)))
    (enq! result (quote begin))
    (let req ((level 0) (mid-symbol (quote #{}#)))
      (when (>= level a)
            (enq! result
                  `(define-public
                     (,(symbol-append (quote cl-c) mid-symbol (quote r))
                      __x__)
                     (cl-c___r (unquote mid-symbol) __x__))))
      (when (< level b)
            (req (1+ level)
                 (symbol-append mid-symbol (quote a)))
            (req (1+ level)
                 (symbol-append mid-symbol (quote d)))))
    (q->list result)))
(define-cl-c___rs 2 5)
(define (range a b)
  (make-stream
    (lambda (state)
      (if (<= state b)
        (cons state (1+ state))
        '()))
    a))
(export range)
(define (list-range a b)
  (stream->list (range a b)))
(export list-range)
(define-macro
  (define-nth-fn fn)
  (let ((fun-name (symbol-append (quote nth-) fn)))
    (gensyms
      (lst i)
      `(define-public
         ((unquote fun-name) (unquote lst) (unquote i))
         (if (zero? (unquote i))
           ((unquote fn) (unquote lst))
           (,fun-name
            (cdr (unquote lst))
            (1- (unquote i))))))))
(define-nth-fn car)
(define-nth-fn cdr)
(define-nth-fn car+cdr)
(simple-syntax
  (m->fn m)
  (lambda x (primitive-eval (cons m x))))
(export m->fn)
(define (none . args) (not (apply any args)))
(export none)
(define-public any? any)
(define-public every? every)
(define-public none? none)
(define (-- x) (- x))
(define ^ expt)
(define (!= . args) (not (apply = args)))
(define (<=> . args)
  (cond ((apply < args) -1)
        ((apply = args) 0)
        (else 1)))
(define *smile-dollar-functions*
  '(sin cos tan exp fact sqrt -- truncate))
(define *smile-dollar-operator-precedence*
  '((%function-application)
    (^)
    (* /)
    (+ -)
    (= < > <= >= != <=>)
    (%negation)
    (or and)))
(define (infix->prefix lst)
  (define (left-associative? operator)
    (not (member
           operator
           '(^ %function-application))))
  (define (operator-priority operator)
    (let loop ((answer 1)
               (part *smile-dollar-operator-precedence*))
      (cond ((null? part) #f)
            ((member operator (car part)) answer)
            (else (loop (1+ answer) (cdr part))))))
  (define (operator? x) (operator-priority x))
  (define (function? x)
    (member x *smile-dollar-functions*))
  (define (negation? x) (eqv? x (quote not)))
  (define (operator-first? s c)
    (let ((ops (operator-priority s))
          (opc (operator-priority c)))
      (or (< ops opc)
          (and (= ops opc) (left-associative? s)))))
  (let ((operators (make-q)) (stack (make-q)))
    (define (step)
      (let ((b (q-pop! stack))
            (a (q-pop! stack))
            (o (q-pop! operators)))
        (cond ((eqv? o (quote %function-application))
               (q-push! stack (list a b)))
              ((eqv? o (quote %negation))
               (q-push! stack (list a b)))
              (else (q-push! stack (list o a b))))))
    (do ((args lst (cdr args)))
        ((null? args)
         (let reduce ()
           (if (q-empty? operators)
             (q-top stack)
             (begin (step) (reduce)))))
      (let ((current (car args)))
        (cond ((pair? current)
               (if (member (car current) (quote (quote unquote)))
                 (q-push! stack (cadr current))
                 (q-push! stack (infix->prefix current))))
              ((operator? current)
               (while (and (not (q-empty? operators))
                           (<= 2 (q-length stack))
                           (operator-first? (q-top operators) current))
                      (step))
               (q-push! operators current))
              ((function? current)
               (q-push! stack current)
               (q-push! operators (quote %function-application)))
              ((negation? current)
               (q-push! stack current)
               (q-push! operators (quote %negation)))
              (else (q-push! stack current)))))))
(define (repair-unary-minus arg)
  (define oper-fun
    (append
      (apply append
             (cdr *smile-dollar-operator-precedence*))
      *smile-dollar-functions*))
  (let ((q (make-q)))
    (let loop ((start #t) (lst arg))
      (if (null? lst)
        (q->list q)
        (let ((x (car lst)))
          (cond ((and (pair? x)
                      (not (eqv? (quote quote) (car x))))
                 (enq! q (repair-unary-minus x)))
                ((and start (eqv? (quote -) (car lst)))
                 (enq! q (quote --)))
                (else (enq! q (car lst))))
          (loop (member x oper-fun) (cdr lst)))))))
(define (math->prefix args)
  (infix->prefix (repair-unary-minus args)))
(define-macro ($ . args) (math->prefix args))
(define-module (smile internals))
(use-modules
  (oop goops)
  (srfi srfi-1)
  (srfi srfi-34)
  (ice-9 pretty-print)
  (ice-9 rdelim)
  (ice-9 readline)
  (ice-9 buffered-input)
  (ice-9 streams)
  (ice-9 q)
  (ice-9 receive)
  (ice-9 optargs)
  (ice-9 format)
  (ice-9 syncase)
  (smile std))
(define (lexer-error lexer . args)
  (define (str arg) (format #f "~a" arg))
  (raise (list 'lexer-error
               (string-join (map str args) " ")
               (cons (port-line (lexer-port lexer))
                     (port-column (lexer-port lexer))))))
(define-class
  <lexer>
  ()
  (port #:accessor
        lexer-port
        #:init-keyword
        #:port)
  (interactive?
    #:accessor
    lexer-interactive?
    #:init-keyword
    #:interactive?)
  (indentation
    #:accessor
    lexer-indentation
    #:init-value
    '(0))
  (empty-line?
    #:accessor
    lexer-empty-line?
    #:init-value
    #t)
  (braces
    #:accessor
    lexer-braces
    #:init-value
    '())
  (token-buffer
    #:accessor
    lexer-token-buffer
    #:init-thunk
    make-q))
(define (copy-lexer parser)
  (let ((port (lexer-port parser))
        (interactive (lexer-interactive? parser)))
    (make <lexer>
          #:port
          port
          #:interactive?
          interactive)))
(export copy-lexer)
(define-macro
  (reset-lexer lexer)
  `(set! ,lexer
     (copy-lexer (unquote lexer))))
(export reset-lexer)
(define (setup-lexer port interactive)
  (make <lexer>
        #:port
        port
        #:interactive?
        interactive))
(export setup-lexer)
(define (read-token lexer)
  (let ((port (lexer-port lexer)))
    (define (on-empty-line?)
      (lexer-empty-line? lexer))
    (define (set-on-empty-line! x)
      (set! (lexer-empty-line? lexer) x))
    (define (indentation) (lexer-indentation lexer))
    (define (set-indentation! new-one)
      (set! (lexer-indentation lexer) new-one))
    (define (add-to-indentation x)
      (set! (lexer-indentation lexer)
        (cons x (lexer-indentation lexer))))
    (define (token-buffer)
      (lexer-token-buffer lexer))
    (define ignored (char-set #\space #\ht))
    (define separators
      (char-set-union
        (apply char-set (map first *brace-tokens*))
        (char-set #\space #\newline #\ht #\" #\;)))
    (define (separator? chr)
      (char-set-contains? separators chr))
    (define (read-until-predicate pred result)
      (let ((chr (read-char port)))
        (if (pred chr)
          (begin
            (unread-char chr port)
            (reverse-list->string result))
          (read-until-predicate pred (cons chr result)))))
    (define (string->thing str)
      (cond ((assoc str *token-strings*) => second)
            (else (read (open-input-string str)))))
    (define (open-parenthesis type)
      (set! (lexer-braces lexer)
        (cons type (lexer-braces lexer))))
    (define (close-parenthesis type)
      (cond ((null? (lexer-braces lexer))
             (lexer-error lexer "unexcepted closing brace"))
            ((eq? type (car (lexer-braces lexer)))
             (set! (lexer-braces lexer)
               (cdr (lexer-braces lexer))))
            (else (lexer-error lexer "incompatible braces"))))
    (define (on-flat?)
      (and (not (on-empty-line?))
           (null? (lexer-braces lexer))))
    (define (read-slash)
      (let ((chr (read-char port)))
        (cond ((char=? chr #\newline) (eat-whitespace))
              ((char-set-contains? ignored chr) (read-slash))
              (else (unread-char chr port) (start-new-line)))))
    (define (read-string)
      (unread-char #\" port)
      (read port))
    (define (read-real-token chr)
      (set-on-empty-line! #f)
      (cond ((char=? chr #\#)
             (unread-char #\# port)
             (read port))
            ((char=? chr #\") (read-string))
            ((char=? chr #\\) (read-slash))
            ((char=? chr #\') SINGLE_QUOTE)
            ((char=? chr #\`) QUASI_QUOTE)
            ((char=? chr #\,)
             (let ((tmp (read-char port)))
               (if (char=? tmp #\@)
                 COMMA_AT
                 (begin (unread-char tmp port) COMMA))))
            ((assoc chr *brace-tokens*)
             =>
             (lambda-bind-vars
               (chr token type open)
               (if open
                 (open-parenthesis type)
                 (close-parenthesis type))
               token))
            (else
             (string->thing
               (read-until-predicate separator? (list chr))))))
    (define (do-at-eof)
      (unless
        (on-empty-line?)
        (enq! (token-buffer) ENDLINE))
      (do-times
        (1- (length (indentation)))
        (enq! (token-buffer) DEDENT))
      (enq! (token-buffer) EOF)
      (set-indentation! (quote (1)))
      (deq! (token-buffer)))
    (define (start-new-line)
      (if (on-empty-line?)
        (eat-whitespace)
        (let position ()
          (let ((chr (read-char port)))
            (cond ((eof-object? chr) (do-at-eof))
                  ((and (char=? chr #\newline)
                        (= 0 (port-column port))
                        (lexer-interactive? lexer))
                   (do-at-eof))
                  ((char-whitespace? chr) (position))
                  (else
                   (set-on-empty-line! #t)
                   (unread-char chr port)
                   (let ((col (port-column port)))
                     (cond ((> col (first (indentation)))
                            (add-to-indentation col)
                            (enq! (token-buffer) INDENT))
                           ((member col (indentation))
                            =>
                            (lambda (whats-left)
                              (let ((prev-len (length (indentation)))
                                    (curr-len (length whats-left)))
                                (set-indentation! whats-left)
                                (do ((countdown
                                       (- prev-len curr-len)
                                       (1- countdown)))
                                    ((= countdown 0))
                                  (enq! (token-buffer) DEDENT)))))
                           (else
                            (lexer-error
                              lexer
                              "bad indentation"
                              col
                              (indentation)))))
                   ENDLINE))))))
    (define (read-part)
      (let ((chr (quote _)))
        (let pre-loop ()
          (set! chr (read-char port))
          (when (char-whitespace? chr) (pre-loop)))
        (let loop ((result (list chr)))
          (set! chr (read-char port))
          (if (char-whitespace? chr)
            (reverse-list->string result)
            (loop (cons chr result))))))
    (define (eat-whitespace)
      (define (ignore-comment)
        (do ((tmp #\_ (read-char port)))
            ((char=? tmp #\newline)
             (unread-char tmp port)
             (eat-whitespace))
          'do-nothing))
      (let ((chr (read-char port)))
        (if (eof-object? chr)
          (do-at-eof)
          (case chr
            ((#\space #\ht) (eat-whitespace))
            ((#\newline)
             (if (on-flat?) (start-new-line) (eat-whitespace)))
            ((#\;) (ignore-comment))
            ((#\#)
             (let ((chr2 (read-char port)))
               (case chr2
                 ((#\!) (ignore-comment))
                 ((#\|)
                  (let read-comment ((depth 1))
                    (let ((current (read-part)))
                      (cond ((string=? current "#|")
                             (read-comment (1+ depth)))
                            ((string=? current "|#")
                             (if (= depth 1)
                               (eat-whitespace)
                               (read-comment (1- depth))))
                            (else (read-comment depth))))))
                 (else
                  (unread-char chr2 port)
                  (read-real-token chr)))))
            (else (read-real-token chr))))))
    (if (q-empty? (token-buffer))
      (eat-whitespace)
      (deq! (token-buffer)))))
(export read-token)
(define (unread-token lexer tkn)
  (q-push! (lexer-token-buffer lexer) tkn))
(export unread-token)
(define-class <token> () (type))
(define-method
  (initialize (tkn <token>) args)
  (bind-vars
    args
    (val)
    (slot-set! tkn (quote type) val)))
(define-method
  (write (tkn <token>) (port <port>))
  (display
    (format #f "TKN_~a" (slot-ref tkn (quote type)))
    port))
(define-macro
  (register-token . lst)
  (cons 'begin
        (calc-for
          tkn
          in
          lst
          (let ((str (symbol->string tkn)))
            `(define ,tkn
               (make <token> (unquote str)))))))
(define (special-token? x) (is-a? x <token>))
(define (datum? x) (not (special-token? x)))
(register-token INDENT DEDENT ENDLINE)
(register-token DOT BAR BAR_BAR BAR_BAR_BAR)
(register-token AMP_DOT AMP AMP_AMP AMP_AMP_AMP)
(register-token
  SINGLE_QUOTE
  COMMA
  COMMA_AT
  QUASI_QUOTE)
(register-token OPEN_PAREN CLOSE_PAREN)
(register-token OPEN_BRACKET CLOSE_BRACKET)
(register-token EOF WRONG)
(define (parser-error parser . args)
  (define (str arg) (format #f "~a" arg))
  (let ((port (slot-ref parser (quote port))))
    (raise (list 'parser-error
                 (string-join (map str args) " ")
                 (cons (port-line port) (port-column port))
                 (read-line port)))))
(define *brace-tokens*
  `((#\( (unquote OPEN_PAREN) ROUND #t)
    (#\) (unquote CLOSE_PAREN) ROUND #f)
    (#\[ (unquote OPEN_BRACKET) SQUARE #t)
    (#\] (unquote CLOSE_BRACKET) SQUARE #f)))
(define *token-strings*
  `(("." (unquote DOT))
    ("&." (unquote AMP_DOT))
    ("&" (unquote AMP))
    ("&&" (unquote AMP_AMP))
    ("&&&" (unquote AMP_AMP_AMP))
    ("|" (unquote BAR))
    ("||" (unquote BAR_BAR))
    ("|||" (unquote BAR_BAR_BAR))
    ("~EOF~" (unquote EOF))))
(define *quotation-tokens*
  (list (list SINGLE_QUOTE (quote quote))
        (list QUASI_QUOTE (quote quasiquote))
        (list COMMA (quote unquote))
        (list COMMA_AT (quote unquote-splicing))))
(define (get-lexer parser) parser)
(define (parse parser)
  (let ((lexer (get-lexer parser)))
    (define (true-procedure-body)
      (let ((part (parse-or-allow parse-line EOF)))
        (if (wrong? part)
          (parser-error parser "error when parsing line")
          part)))
    (define (wrong? x) (eqv? WRONG x))
    (define (eat-token tkn)
      (let ((eaten (read-token lexer)))
        (unless
          (eqv? tkn eaten)
          (parser-error
            parser
            "excepting"
            tkn
            "but"
            eaten
            "encountered"))))
    (define (eaten-token? tkn)
      (let ((eaten (read-token lexer)))
        (if (eqv? tkn eaten)
          #t
          (begin (unread-token lexer eaten) #f))))
    (define (eat-and-return food return)
      (eat-token food)
      return)
    (define (kleene-star proc)
      (let ((q (make-q)))
        (let read-them ()
          (let ((tmp_ (proc)))
            (unless (wrong? tmp_) (enq! q tmp_) (read-them))))
        (q->list q)))
    (define (kleene-plus proc)
      (let ((lst (kleene-star proc)))
        (if (null? lst) WRONG lst)))
    (define (parse-or-allow parser-fn . allowed)
      (let ((tmp__ (read-token lexer)))
        (if (member tmp__ allowed)
          tmp__
          (begin (unread-token lexer tmp__) (parser-fn)))))
    (define (force-parsing proc)
      (let ((tmp (proc)))
        (when (wrong? tmp)
              (parser-error
                parser
                (procedure-name proc)
                "failed"))
        tmp))
    (define (parse-line)
      (let ((tkn (read-token lexer)))
        (cond ((eqv? DOT tkn)
               (eat-and-return ENDLINE (parse-element)))
              ((eqv? ENDLINE tkn)
               (eat-token INDENT)
               (eat-and-return DEDENT (parse-list-of-lines)))
              (else
               (unread-token lexer tkn)
               (let ((flat (parse-list-inside)))
                 (if (wrong? flat)
                   WRONG
                   (begin
                     (eat-token ENDLINE)
                     (if (eaten-token? INDENT)
                       (eat-and-return
                         DEDENT
                         (append flat (parse-list-of-lines)))
                       flat))))))))
    (define (parse-element)
      (let ((tkn (read-token lexer)))
        (cond ((eqv? OPEN_PAREN tkn)
               (let ((inside (parse-list-inside)))
                 (eat-token CLOSE_PAREN)
                 (if (wrong? inside) (quote ()) inside)))
              ((eqv? OPEN_BRACKET tkn)
               (let ((inside (parse-list-inside)))
                 (eat-token CLOSE_BRACKET)
                 (if (wrong? inside) (quote ()) inside)))
              ((eqv? AMP_DOT tkn)
               (cons (force-parsing parse-element)
                     (force-parsing parse-element)))
              ((eqv? AMP tkn)
               (list (force-parsing parse-element)))
              ((eqv? AMP_AMP tkn)
               (list (force-parsing parse-element)
                     (force-parsing parse-element)))
              ((eqv? AMP_AMP_AMP tkn)
               (list (force-parsing parse-element)
                     (force-parsing parse-element)
                     (force-parsing parse-element)))
              ((datum? tkn) tkn)
              ((assoc tkn *quotation-tokens*)
               =>
               (lambda (info)
                 (list (second info)
                       (force-parsing parse-element))))
              (else (unread-token lexer tkn) WRONG))))
    (define (parse-flat-el)
      (parse-or-allow
        parse-element
        BAR
        BAR_BAR
        BAR_BAR_BAR))
    (define (parse-list-inside)
      (let ((flat-el+ (kleene-plus parse-flat-el)))
        (if (wrong? flat-el+)
          WRONG
          (begin
            (when (eaten-token? DOT)
                  (set! flat-el+
                    (append flat-el+ (force-parsing parse-element))))
            (repair-flat flat-el+)))))
    (define (repair-flat lst)
      (let ((lst (cons (quote sentinel) lst)))
        (let repair ((elem lst))
          (if (not (pair? (cl-cdr elem)))
            (cdr lst)
            (let ((tmp (second elem)))
              (cond ((eqv? BAR tmp)
                     (set-cdr! elem (list (cddr elem)))
                     (repair (cadr elem)))
                    ((eqv? BAR_BAR tmp)
                     (set-cdr! elem (list (list (cddr elem))))
                     (repair (caadr elem)))
                    ((eqv? BAR_BAR_BAR tmp)
                     (set-cdr! elem (list (list (list (cddr elem)))))
                     (repair (caaadr elem)))
                    (else (repair (cdr elem)))))))))
    (define (parse-flat)
      (let ((flat-el+ (kleene-plus parse-flat-el)))
        (if (wrong? flat-el+)
          WRONG
          (repair-flat flat-el+))))
    (define (parse-list-of-lines)
      (kleene-plus parse-line))
    (true-procedure-body)))
(export parse)
(define setup-parser setup-lexer)
(export setup-parser)
(define (load-smile file)
  (with-input-from-file
    file
    (lambda ()
      (define *parser*
        (setup-parser (current-input-port) #f))
      (let grande-lupe ()
        (let ((parsed (parse *parser*)))
          (unless
            (eqv? parsed EOF)
            (primitive-eval parsed)
            (grande-lupe)))))))
(use-modules
  (smile std)
  (smile internals)
  (ice-9 pretty-print)
  (ice-9 rdelim)
  (ice-9 readline)
  (ice-9 buffered-input))
(define (translate)
  (define *parser*
    (setup-parser (current-input-port) #f))
  (let grande-lupe ()
    (let ((parsed (parse *parser*)))
      (unless
        (eqv? parsed EOF)
        (pretty-print parsed)
        (grande-lupe)))))
(if (null? (cdr (program-arguments)))
  (translate)
  (with-input-from-file
    (cadr (program-arguments))
    translate))
