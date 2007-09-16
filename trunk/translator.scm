#!/usr/bin/guile -s
!#
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
  (ice-9 syncase))
(define-syntax
  loop
  (lambda (x)
    (syntax-case
      x
      (in)
      ((_ function i in lst c1 c2 ...)
       (syntax (function (lambda (i) c1 c2 ...) lst)))
      ((_ function i j in lst1 lst2 c1 c2 ...)
       (syntax
         (function (lambda (i j) c1 c2 ...) lst1 lst2)))
      ((_ function i j k in lst1 lst2 lst3 c1 c2 ...)
       (syntax
         (function
           (lambda (i j k) c1 c2 ...)
           lst1
           lst2
           lst3))))))
(define-macro
  (gensyms a . rest)
  `(let* ,(calc-for i in a (list i (quote (gensym))))
     ,@rest))
(define-macro
  (for . x)
  (append (quote (loop for-each)) x))
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
  (append (quote (loop map)) x))
(define-syntax
  do-times
  (lambda (x)
    (syntax-case
      x
      ()
      ((_ count c1 c2 ...)
       (syntax
         (let req ((n count))
           (when (> n 0) (begin c1 c2 ...) (req (1- n)))))))))
(define (all? pred lst)
  (cond ((null? lst) #t)
        ((pred (first lst)) (all? pred (rest lst)))
        (else #f)))
(define (any? pred lst)
  (cond ((null? lst) #f)
        ((pred (first lst)) (#t))
        (else (any? pred (rest lst)))))
(define-syntax
  when
  (lambda (x)
    (syntax-case
      x
      ()
      ((_ cnd c1 c2 ...)
       (syntax (if cnd (begin c1 c2 ...)))))))
(define-syntax
  unless
  (lambda (x)
    (syntax-case
      x
      ()
      ((_ cnd c1 c2 ...)
       (syntax (if (not cnd) (begin c1 c2 ...)))))))
(define-syntax
  bind
  (lambda (x)
    (syntax-case
      x
      ()
      ((_ name (a) b c ...)
       (syntax (let ((a (car name))) (begin b c ...))))
      ((_ name (a b c ...) d e ...)
       (syntax
         (let ((a (car name)) (tmp (cdr name)))
           (bind tmp (b c ...) d e ...)))))))
(define-syntax
  lambda-bind
  (lambda (x)
    (syntax-case
      x
      ()
      ((_ (a b ...) c d ...)
       (syntax
         (lambda (tmp) (bind tmp (a b ...) c d ...)))))))
(define (output-delimited func delim args)
  (unless
    (null? args)
    (let req ((args args))
      (func (car args))
      (let ((tail (cdr args)))
        (unless (null? tail) (display delim) (req tail))))))
(define*
  (print #:key
         (before "")
         (delim " ")
         (after "
")
         #:rest
         args)
  (display before)
  (let ((tmp (make-q)))
    (let req ((args args))
      (cond ((null? args)
             (output-delimited display delim (q->list tmp)))
            ((member
               (car args)
               '(#:before #:delim #:after)
               eq?)
             (req (cl-c___r dd args)))
            (else (enq! tmp (car args)) (req (cdr args))))))
  (display after))
(define rest cdr)
(define q->list car)
(define q-top q-front)
(define (list->q lst)
  (let ((q (cons lst #f))) (sync-q! q) q))
(define (cl-car x)
  (if (null? x) (quote ()) (car x)))
(define (cl-cdr x)
  (if (null? x) (quote ()) (cdr x)))
(define (symbol-append . lst)
  (string->symbol
    (apply string-append (map symbol->string lst))))
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
    (let req ((level 0) (str ""))
      (if (>= level a)
        (let* ((symbol (string->symbol str))
               (full-name
                 (symbol-append (quote cl-c) symbol (quote r))))
          (enq! result
                `(define-macro
                   ((unquote full-name) __x__)
                   (cl-c___r (unquote symbol) __x__)))))
      (when (< level b)
            (req (1+ level) (string-append str "a"))
            (req (1+ level) (string-append str "d"))))
    (q->list result)))
(define-cl-c___rs 2 5)
(define (range a b)
  (make-stream
    (lambda (state)
      (if (<= state b)
        (cons state (1+ state))
        '()))
    a))
(define (list-range a b)
  (stream->list (range a b)))
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
(define-macro
  (reset-lexer lexer)
  `(set! ,lexer
     (copy-lexer (unquote lexer))))
(define (setup-lexer port interactive)
  (make <lexer>
        #:port
        port
        #:interactive?
        interactive))
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
        (char-set #\space #\newline #\ht #\")))
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
      (or (not (on-empty-line?))
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
            ((char=? chr #\,)
             (let ((tmp (read-char port)))
               (if (char=? tmp #\@)
                 COMMA_AT
                 (begin (unread-char tmp port) COMMA))))
            ((assoc chr *brace-tokens*)
             =>
             (lambda (spec)
               (bind spec
                     (chr token type open)
                     (if open
                       (open-parenthesis type)
                       (close-parenthesis type))
                     token)))
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
        (begin
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
                     ENDLINE)))))))
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
(define (unread-token lexer tkn)
  (q-push! (lexer-token-buffer lexer) tkn))
(define-class <token> () (type))
(define-method
  (initialize (tkn <token>) args)
  (bind args
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
(register-token SINGLE_QUOTE COMMA COMMA_AT)
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
          (let ((tmp (proc)))
            (unless (wrong? tmp) (enq! q tmp) (read-them))))
        (q->list q)))
    (define (kleene-plus proc)
      (let ((lst (kleene-star proc)))
        (if (null? lst) WRONG lst)))
    (define (parse-or-allow parser . allowed)
      (let ((tmp (read-token lexer)))
        (if (member tmp allowed)
          tmp
          (begin (unread-token lexer tmp) (parser)))))
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
(define setup-parser setup-lexer)
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
               (if (eqv? (quote quote) (car current))
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
