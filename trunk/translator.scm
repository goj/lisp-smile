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
  (ice-9 format)
  (ice-9 syncase))
(define-syntax
  var
  (lambda (x)
    (syntax-case
      x
      ()
      ((_ name binding a b ...)
       (syntax (let ((name binding)) a b ...))))))
(define-syntax
  loop
  (lambda (x)
    (syntax-case
      x
      ()
      ((_ name a b ...) (syntax (let name () a b ...))))))
(define-syntax
  for
  (lambda (x)
    (syntax-case
      x
      (in from to)
      ((_ i from a to b c1 c2 ...)
       (syntax
         (let req ((i a))
           (if (<= i b) (begin c1 c2 ... (req (1+ i)))))))
      ((_ i in lst c1 c2 ...)
       (syntax (for-each (lambda (i) c1 c2 ...) lst)))
      ((_ i j in lst1 lst2 c1 c2 ...)
       (syntax
         (for-each (lambda (i j) c1 c2 ...) lst1 lst2)))
      ((_ i j k in lst1 lst2 lst3 c1 c2 ...)
       (syntax
         (for-each
           (lambda (i j k) c1 c2 ...)
           lst1
           lst2
           lst3))))))
(define-syntax
  calc-for
  (lambda (x)
    (syntax-case
      x
      (in from to)
      ((_ i from a to b c1 c2 ...)
       (syntax
         (calc-for i in (list-range a b) c1 c2 ...)))
      ((_ i in lst c1 c2 ...)
       (syntax (map (lambda (i) c1 c2 ...) lst)))
      ((_ i j in lst1 lst2 c1 c2 ...)
       (syntax (map (lambda (i j) c1 c2 ...) lst1 lst2)))
      ((_ i j k in lst1 lst2 lst3 c1 c2 ...)
       (syntax
         (map (lambda (i j k) c1 c2 ...) lst1 lst2 lst3))))))
(define-syntax
  repeat
  (lambda (x)
    (syntax-case
      x
      (times)
      ((_ count times c1 c2 ...)
       (syntax
         (let req ((n count))
           (unless (zero? n) (begin c1 c2 ...) (req (1- n)))))))))
(define (all-fn? pred lst)
  (cond ((null? lst) #t)
        ((pred (first lst)) (all-fn? pred (rest lst)))
        (else #f)))
(define (any-fn? pred lst)
  (cond ((null? lst) #f)
        ((pred (first lst)) (#t))
        (else (any-fn? pred (rest lst)))))
(define-syntax
  all?
  (lambda (x)
    (syntax-case
      x
      (in)
      ((_ i in lst c1 c2 ...)
       (syntax (all-fn? (lambda (i) c1 c2 ...) lst))))))
(define-syntax
  any?
  (lambda (x)
    (syntax-case
      x
      (in)
      ((_ i in lst c1 c2 ...)
       (syntax (all-fn? (lambda (i) c1 c2 ...) lst))))))
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
       (syntax (var a (car name) (begin b c ...))))
      ((_ name (a b c ...) d e ...)
       (syntax
         (let ((a (car name)) (tmp (cdr name)))
           (bind tmp (b c ...) d e ...)))))))
(define (output-delimited func delim args)
  (unless
    (null? args)
    (let req ((args args))
      (func (car args))
      (var tail
           (cdr args)
           (unless (null? tail) (display delim) (req tail))))))
(define (print* . args)
  (output-delimited display " " args))
(define (print . args)
  (apply print* args)
  (newline))
(define rest cdr)
(define (cl-car x)
  (if (null? x) (quote ()) (car x)))
(define (cl-cdr x)
  (if (null? x) (quote ()) (cdr x)))
(define (range a b)
  (make-stream
    (lambda (state)
      (if (<= state b)
        (cons state (1+ state))
        '()))
    a))
(define (list-range a b)
  (var q
       (make-q)
       (let loop ((state a))
         (if (<= state b) (enq! state) (q->list q)))))
(define q->list car)
(define (lexer-error . args)
  (define (str arg) (format #f "~a" arg))
  (raise (list 'lexer-error
               (string-join (map str args) " ")
               (cons (port-line *port*) (port-column *port*)))))
(define *port* *unspecified*)
(define *indentation* *unspecified*)
(define *empty-line* *unspecified*)
(define *interactive* *unspecified*)
(define *braces* *unspecified*)
(define *token-buffer* *unspecified*)
(define (setup-lexer port interactive)
  (set! *port* port)
  (set! *interactive* interactive)
  (reset-lexer))
(define (reset-lexer)
  (set! *indentation* (quote (0)))
  (set! *empty-line* #t)
  (set! *braces* (quote ()))
  (set! *token-buffer* (make-q)))
(define (make-char-set-predicate char-set)
  (lambda (chr) (char-set-contains? char-set chr)))
(define (read-token)
  (define ignored (char-set #\space #\ht))
  (define separators
    (char-set-union
      (apply char-set (map first *brace-tokens*))
      (char-set #\space #\newline #\ht #\")))
  (define (separator? chr)
    (char-set-contains? separators chr))
  (define (read-until-predicate pred result)
    (var chr
         (read-char *port*)
         (if (pred chr)
           (begin
             (unread-char chr *port*)
             (reverse-list->string result))
           (read-until-predicate pred (cons chr result)))))
  (define (string->thing str)
    (cond ((assoc str *token-strings*) => second)
          (else (read (open-input-string str)))))
  (define (open-parenthesis type)
    (set! *braces* (cons type *braces*)))
  (define (close-parenthesis type)
    (cond ((null? *braces*)
           (lexer-error "unexcepted closing brace"))
          ((eq? type (car *braces*))
           (set! *braces* (cdr *braces*)))
          (else (lexer-error "incompatible braces"))))
  (define (on-flat?)
    (or (not *empty-line*) (null? *braces*)))
  (define (read-slash)
    (var chr
         (read-char *port*)
         (cond ((char=? chr #\newline) (eat-whitespace))
               ((char-set-contains? ignored chr) (read-slash))
               (else (unread-char chr *port*) (start-new-line)))))
  (define (read-string)
    (unread-char #\" *port*)
    (read *port*))
  (define (read-real-token chr)
    (set! *empty-line* #f)
    (cond ((char=? chr #\#)
           (unread-char #\# *port*)
           (read *port*))
          ((char=? chr #\") (read-string))
          ((char=? chr #\\) (read-slash))
          ((char=? chr #\') SINGLE_QUOTE)
          ((char=? chr #\,)
           (var tmp
                (read-char *port*)
                (if (char=? tmp #\@)
                  COMMA_AT
                  (begin (unread-char tmp *port*) COMMA))))
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
      *empty-line*
      (enq! *token-buffer* ENDLINE))
    (repeat
      (1- (length *indentation*))
      times
      (enq! *token-buffer* DEDENT))
    (enq! *token-buffer* EOF)
    (set! *indentation* (quote (1)))
    (deq! *token-buffer*))
  (define (start-new-line)
    (if *empty-line*
      (eat-whitespace)
      (begin
        (loop position
              (var chr
                   (read-char *port*)
                   (cond ((eof-object? chr) (do-at-eof))
                         ((and (char=? chr #\newline)
                               (= 0 (port-column *port*))
                               *interactive*)
                          (do-at-eof))
                         ((char-whitespace? chr) (position))
                         (else
                          (set! *empty-line* #t)
                          (unread-char chr *port*)
                          (var col
                               (port-column *port*)
                               (cond ((> col (first *indentation*))
                                      (set! *indentation*
                                        (cons col *indentation*))
                                      (enq! *token-buffer* INDENT))
                                     ((member col *indentation*)
                                      =>
                                      (lambda (whats-left)
                                        (let ((prev-len (length *indentation*))
                                              (curr-len (length whats-left)))
                                          (set! *indentation* whats-left)
                                          (do ((countdown
                                                 (- prev-len curr-len)
                                                 (1- countdown)))
                                              ((= countdown 0))
                                            (enq! *token-buffer* DEDENT)))))
                                     (else
                                      (lexer-error
                                        "bad indentation"
                                        col
                                        *indentation*))))
                          ENDLINE)))))))
  (define (read-part)
    (var chr
         '_
         (loop pre-loop
               (set! chr (read-char *port*))
               (when (char-whitespace? chr) (pre-loop)))
         (let loop ((result (list chr)))
           (set! chr (read-char *port*))
           (if (char-whitespace? chr)
             (reverse-list->string result)
             (loop (cons chr result))))))
  (define (eat-whitespace)
    (define (ignore-comment)
      (do ((tmp #\_ (read-char *port*)))
          ((char=? tmp #\newline)
           (unread-char tmp *port*)
           (eat-whitespace))
        'do-nothing))
    (var chr
         (read-char *port*)
         (if (eof-object? chr)
           (do-at-eof)
           (case chr
             ((#\space #\ht) (eat-whitespace))
             ((#\newline)
              (if (on-flat?) (start-new-line) (eat-whitespace)))
             ((#\;) (ignore-comment))
             ((#\#)
              (var chr2
                   (read-char *port*)
                   (case chr2
                     ((#\!) (ignore-comment))
                     ((#\|)
                      (let read-comment ((depth 1))
                        (var current
                             (read-part)
                             (cond ((string=? current "#|")
                                    (read-comment (1+ depth)))
                                   ((string=? current "|#")
                                    (if (= depth 1)
                                      (eat-whitespace)
                                      (read-comment (1- depth))))
                                   (else (read-comment depth))))))
                     (else
                      (unread-char chr2 *port*)
                      (read-real-token chr)))))
             (else (read-real-token chr))))))
  (if (q-empty? *token-buffer*)
    (eat-whitespace)
    (deq! *token-buffer*)))
(define (unread-token tkn)
  (q-push! *token-buffer* tkn))
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
          (var str
               (symbol->string tkn)
               `(define ,tkn
                  (make <token> (unquote str)))))))
(define (special-token? x) (is-a? x <token>))
(define (datum? x) (not (special-token? x)))
(register-token INDENT DEDENT ENDLINE)
(register-token DOT BAR BAR_BAR BAR_BAR_BAR)
(register-token SINGLE_QUOTE COMMA COMMA_AT)
(register-token OPEN_PAREN CLOSE_PAREN)
(register-token OPEN_BRACKET CLOSE_BRACKET)
(register-token EOF WRONG)
(define (parser-error . args)
  (define (str arg) (format #f "~a" arg))
  (raise (list 'parser-error
               (string-join (map str args) " ")
               (cons (port-line *port*) (port-column *port*))
               (read-token)
               (read-line))))
(define *brace-tokens*
  `((#\( (unquote OPEN_PAREN) ROUND #t)
    (#\) (unquote CLOSE_PAREN) ROUND #f)
    (#\[ (unquote OPEN_BRACKET) SQUARE #t)
    (#\] (unquote CLOSE_BRACKET) SQUARE #f)))
(define *token-strings*
  `(("." (unquote DOT))
    ("|" (unquote BAR))
    ("||" (unquote BAR_BAR))
    ("|||" (unquote BAR_BAR_BAR))
    ("~EOF~" (unquote EOF))))
(define *quotation-tokens*
  (list (list SINGLE_QUOTE (quote quote))
        (list COMMA (quote unquote))
        (list COMMA_AT (quote unquote-splicing))))
(define (parse)
  (define (true-procedure-body)
    (var part
         (parse-or-allow parse-line EOF)
         (if (wrong? part)
           (parser-error "error when parsing line")
           part)))
  (define (wrong? x) (eqv? WRONG x))
  (define (eat-token tkn)
    (var eaten
         (read-token)
         (unless
           (eqv? tkn eaten)
           (parser-error
             "ecepting"
             tkn
             "but"
             eaten
             "encountered"))))
  (define (eaten-token? tkn)
    (var eaten
         (read-token)
         (if (eqv? tkn eaten)
           #t
           (begin (unread-token eaten) #f))))
  (define (eat-and-return food return)
    (eat-token food)
    return)
  (define (kleene-star proc)
    (var q
         (make-q)
         (loop read-them
               (var tmp
                    (proc)
                    (unless (wrong? tmp) (enq! q tmp) (read-them))))
         (q->list q)))
  (define (kleene-plus proc)
    (var lst
         (kleene-star proc)
         (if (null? lst) WRONG lst)))
  (define (parse-or-allow parser . allowed)
    (var tmp
         (read-token)
         (if (member tmp allowed)
           tmp
           (begin (unread-token tmp) (parser)))))
  (define (force-parsing parser)
    (var tmp
         (parser)
         (when (wrong? tmp)
               (parser-error (procedure-name parser) "failed"))
         tmp))
  (define (parse-line)
    (var tkn
         (read-token)
         (cond ((eqv? DOT tkn)
                (eat-and-return ENDLINE (parse-element)))
               ((eqv? ENDLINE tkn)
                (eat-token INDENT)
                (eat-and-return DEDENT (parse-list-of-lines)))
               (else
                (unread-token tkn)
                (var flat
                     (parse-list-inside)
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
    (var tkn
         (read-token)
         (cond ((eqv? OPEN_PAREN tkn)
                (var inside
                     (parse-list-inside)
                     (eat-token CLOSE_PAREN)
                     (if (wrong? inside) (quote ()) inside)))
               ((eqv? OPEN_BRACKET tkn)
                (var inside
                     (parse-list-inside)
                     (eat-token CLOSE_BRACKET)
                     (if (wrong? inside) (quote ()) inside)))
               ((datum? tkn) tkn)
               ((assoc tkn *quotation-tokens*)
                =>
                (lambda (info)
                  (list (second info)
                        (force-parsing parse-element))))
               (else (unread-token tkn) WRONG))))
  (define (parse-flat-el)
    (parse-or-allow
      parse-element
      BAR
      BAR_BAR
      BAR_BAR_BAR))
  (define (parse-list-inside)
    (var flat-el+
         (kleene-plus parse-flat-el)
         (if (wrong? flat-el+)
           WRONG
           (begin
             (when (eaten-token? DOT)
                   (set! flat-el+
                     (append flat-el+ (force-parsing parse-element))))
             (repair-flat flat-el+)))))
  (define (repair-flat lst)
    (var lst
         (cons (quote sentinel) lst)
         (let repair ((elem lst))
           (if (not (pair? (cl-cdr elem)))
             (cdr lst)
             (var tmp
                  (second elem)
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
    (var flat-el+
         (kleene-plus parse-flat-el)
         (if (wrong? flat-el+)
           WRONG
           (repair-flat flat-el+))))
  (define (parse-list-of-lines)
    (kleene-plus parse-line))
  (true-procedure-body))
(define (translate)
  (setup-lexer (current-input-port) #f)
  (loop grande-lupe
        (var parsed
             (parse)
             (unless
               (eqv? parsed EOF)
               (pretty-print parsed)
               (grande-lupe)))))
(if (null? (cdr (program-arguments)))
  (translate)
  (with-input-from-file
    (cadr (program-arguments))
    translate))
