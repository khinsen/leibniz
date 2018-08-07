#lang racket

(provide format-context
         format-signature-declaration
         format-term
         format-label
         format-rule
         format-equation
         format-transformation
         format-asset
         format-asset-reference
         leibniz-input leibniz-input-with-hover
         leibniz-output leibniz-output-with-hover
         leibniz-comment
         plain-text)

(require scribble/core
         scribble/base
         (only-in scribble/html-properties make-css-addition hover-property)
         "./condd.rkt"
         (prefix-in operators: "./operators.rkt")
         (prefix-in terms: "./terms.rkt")
         (prefix-in equations: "./equations.rkt"))

(module+ test
  (require rackunit))

; HTML styles

(define leibniz-css
  (make-css-addition
   #".Leibniz { background-color: #E8E8FF; }\n.LeibnizOutput { background-color: #E0FFE0; }\n.LeibnizComment { background-color: #FFE8E8; }\n"))

(define leibniz-style (style "Leibniz" (list leibniz-css)))
(define leibniz-output-style (style "LeibnizOutput" (list leibniz-css)))
(define leibniz-comment-style (style "LeibnizComment" (list leibniz-css)))

(define (leibniz-input . body)
  (elem #:style leibniz-style
        body))

(define (leibniz-output . body)
  (elem #:style leibniz-output-style
        body))

(define (leibniz-comment . body)
  (elem #:style leibniz-comment-style
        body))

(define (leibniz-input-with-hover text . body)
  (elem #:style (style "Leibniz" (list leibniz-css (hover-property text)))
        body))

(define (leibniz-output-with-hover text . body)
  (elem #:style (style "LeibnizOutput" (list leibniz-css (hover-property text)))
        body))

; Rendering of Leibniz data in terms of Scribble elements

; Rendering of Leibniz data in terms of Scribble elements

(define (format-sort symbol)
  (if symbol
      (symbol->string symbol)
      "<any>"))

(define (format-var name sort)
  (list (italic (symbol->string name))
        ":"
        (format-sort sort)))

(define (format-op-arg var-or-sort-decl)
  (match var-or-sort-decl
    [(list 'sort sort) (format-sort sort)]
    [(list 'var name sort) (format-var name sort)]))

(define (format-subsort-declaration sort1 sort2)
  (list (format-sort sort1)
        " ⊆ "   ; MEDIUM MATHEMATICAL SPACE ; SUBSET OF OR EQUAL TO
        (format-sort sort2)))

(define (op-symbol-and-type op-symbol)
  (define s (symbol->string op-symbol))
  (cond
    [(member s '("[]" "^" "_")) (values s 'special-op)]
    [(string-prefix? s "_") (values (substring s 1) 'infix-op)]
    [else (values s 'prefix-op)]))

(define (op-type op-symbol)
  (define-values (op-str type) (op-symbol-and-type op-symbol))
  type)

(define (format-op-declaration op-symbol arg-decls result-sort)
  (define-values (op-str op-type) (op-symbol-and-type op-symbol))
  (case op-type
    [(prefix-op)
     (list op-str
           (if (zero? (length arg-decls))
               ""
               (list "(" (add-between (map format-op-arg arg-decls) ", ") ")"))
           " : "
           (format-sort result-sort))]
    [(infix-op)
     (list (format-op-arg (first arg-decls))
           " " op-str " "
           (format-op-arg (second arg-decls))
           " : "
           (format-sort result-sort))]
    [(special-op)
     (case op-str
       [("[]")
        (list (format-op-arg (first arg-decls))
              "["
              (add-between (map format-op-arg (rest arg-decls)) ", ")
              "] : "
              (format-sort result-sort))]
       [("_")
        (list (format-op-arg (first arg-decls))
              (subscript (format-op-arg (second arg-decls)))
              " : "
              (format-sort result-sort))]
       [("^")
        (list (format-op-arg (first arg-decls))
              (superscript (format-op-arg (second arg-decls)))
              " : "
              (format-sort result-sort))])]))

(define (format-signature-declaration decl)
  (match decl
    [(list 'sort sort-symbol)
     (format-sort sort-symbol)]
    [(list 'subsort sort-symbol-1 sort-symbol-2)
     (format-subsort-declaration sort-symbol-1 sort-symbol-2)]
    [(list 'op op-symbol arg-sorts result-sort)
     (format-op-declaration op-symbol arg-sorts result-sort)]
    [(list 'var name sort)
     (format-var name sort)]))

(define (term->string term)
  (let ([o (open-output-string)])
    (terms:display-term term o)
    (get-output-string o)))

(define (infix-term? term [neighbor-op #f])
  (define-values (raw-op args) (terms:term.op-and-args term))
  (and raw-op
       (let-values ([(op type) (op-symbol-and-type raw-op)])
         (and (equal? type 'infix-op)
              (not (equal? op neighbor-op))))))

(define (parenthesize term-elem condition)
  (if condition
      (list "(" term-elem ")")
      term-elem))

(define (format-label label [as-prefix? #t])
  (if label
      (list (bold (symbol->string label))
            (if as-prefix?  ": " ""))
      ""))

(define (format-term signature label term)
  (list (format-label label)
        (condd
         [(terms:var? term)                   ; vars: print name in italic
          (italic (symbol->string (terms:var-name term)))]
         #:do (define-values (raw-op args) (terms:term.op-and-args term))
         [(not raw-op)                        ; everything other than an op-term: convert to string
          (term->string term)]
         #:do (define-values (op type) (op-symbol-and-type raw-op))
         [(zero? (length args))               ; no args implies prefix-op
          op]
         [(equal? type 'prefix-op)
          (list op
                "("
                (add-between (for/list ([arg args]) (format-term signature #f arg)) ", ")
                ")")]
         #:do (define arg1 (first args))
         #:do (define f-arg1 (format-term signature #f arg1))
         [(equal? op "[]")
          (list (parenthesize f-arg1 (infix-term? arg1))
                "["
                (add-between (for/list ([arg (rest args)]) (format-term signature #f arg)) ", ")
                "]")]
         #:do (define arg2 (second args))
         #:do (define f-arg2 (format-term signature #f arg2))
         [(equal? type 'infix-op)
          (list (parenthesize f-arg1 (infix-term? arg1 op))
                " " op  " "
                (parenthesize f-arg2 (infix-term? arg2)))]
         [(equal? op "^")
          (list f-arg1 (superscript f-arg2))]
         [(equal? op "_")
          (list f-arg1 (subscript f-arg2))]
         [else (error (format "operator ~a of type ~a" op type))])))

(define (format-rule-or-transformation label rule signature separator)
  (define c-vars (operators:all-vars signature))
  (define proc-rule? (procedure? (equations:rule-replacement rule)))
  (define pattern-elem (format-term signature #f (equations:rule-pattern rule)))
  (define replacement-elem
    (if proc-rule? 
        (italic "<procedure>")
        (format-term signature #f (equations:rule-replacement rule))))
  (define vars (terms:term.vars (equations:rule-pattern rule)))
  (define cond (equations:rule-condition rule))

  (define var-elems (for/list ([var (in-set vars)]
                               #:unless (equal? (hash-ref c-vars
                                                          (terms:var-name var)
                                                          #f)
                                                (terms:var-sort var)))
                      (list "∀ "
                            (italic (symbol->string (terms:var-name var)))
                            " : "
                            (format-sort (terms:var-sort var)))))
  (define clause-elems
    (if cond
        (append var-elems
                (list (list (elem #:style leibniz-style
                                  "if "
                                  (format-term signature #f cond)))))
        var-elems))
  (define clause-prefix
    (if (> (length clause-elems) 2)
        (list (linebreak ) (hspace 2))
        (list (hspace 2))))

  (list (format-label label)
        pattern-elem
        (if proc-rule? " ↣ "  separator)
        replacement-elem
        (append* (map (λ (e) (append clause-prefix e)) clause-elems))))

(define (format-rule label rule signature)
  (format-rule-or-transformation label rule signature " ⇒ "))

(define (format-transformation label transformation signature)
  (format-rule-or-transformation label
                                 (equations:transformation-rule transformation)
                                 signature " → "))

(define (format-equation label equation signature)
  (define vars (set-union (terms:term.vars (equations:equation-left equation))
                          (terms:term.vars (equations:equation-right equation))))
  (define c-vars (operators:all-vars signature))
  (define cond (equations:equation-condition equation))
  (define left-elem (format-term signature #f (equations:equation-left equation)))
  (define right-elem (format-term signature #f (equations:equation-right equation)))
  (define var-elems (for/list ([var (in-set vars)]
                               #:unless (equal? (hash-ref c-vars
                                                          (terms:var-name var)
                                                          #f)
                                                (terms:var-sort var)))
                      (list "∀ "
                            (italic (symbol->string (terms:var-name var)))
                            " : "
                            (format-sort (terms:var-sort var)))))
  (define clause-elems
    (if cond
        (append var-elems
                (list (list (elem #:style leibniz-style
                                  "if "
                                  (format-term signature #f cond)))))
        var-elems))
  (define clause-prefix
    (if (> (length clause-elems) 2)
        (list (linebreak ) (hspace 2))
        (list (hspace 2))))
 
 (list (format-label label)
        left-elem
        " = "
        right-elem
        (append* (map (λ (e) (append clause-prefix e)) clause-elems))))

;;
;; Format assets and asset references
;;
(define (format-asset label asset signature)
  (cond
    [(equations:equation? asset) (format-equation label asset signature)]
    [(equations:rule? asset) (format-rule label asset signature)]
    [(equations:transformation? asset) (format-transformation label asset signature)]
    [(terms:term? asset) (format-term signature label asset)]
    [(hash? asset) "formatting of nested assets not yet implemented"]
    [else (error (format "illegal asset type: ~a" asset))]))

(define (format-asset-reference label asset)
  (define prefix (cond
                   [(equations:equation? asset) "=."]
                   [(equations:rule? asset) "⇒."]
                   [else ""]))
  (list prefix (format-label label #f)))

;;
;; Format a complete context
;;
(define (format-context decls)
  (list (format-sort-declarations (hash-ref decls 'sorts))
        (format-subsort-declarations (hash-ref decls 'subsorts))
        (format-op-declarations (hash-ref decls 'ops))
        (format-var-declarations (hash-ref decls 'vars))
        (format-rule-declarations (hash-ref decls 'rules)
                                  (hash-ref decls 'vars)
                                  (hash-ref decls 'compiled-signature #f))
        (format-asset-declarations (hash-ref decls 'assets)
                                   (hash-ref decls 'vars)
                                   (hash-ref decls 'compiled-signature #f))))

(define (format-sort-declarations sort-decls)
  (define sorts
    (for/list ([s sort-decls])
      (elem #:style leibniz-output-style
            (format-sort s))))
  (if (empty? sorts)
      ""
      (list "Sorts: " (add-between sorts ", ") (linebreak))))

(define (format-subsort-declarations subsort-decls)
  (define subsorts
    (for/list ([sd subsort-decls])
      (elem #:style leibniz-output-style
            (format-subsort-declaration (car sd) (cdr sd)))))
  (if (empty? subsorts)
      ""
      (list "Subsort relations: " (add-between subsorts ", ") (linebreak))))

(define (format-op-declarations op-decls)
  (define ops
    (for/list ([od op-decls])
      (elem #:style leibniz-output-style
            (apply format-op-declaration od))))
  (if (empty? op-decls)
      ""
      (list "Operators:" (linebreak)
            (apply nested
                   (add-between ops (linebreak))
                   #:style 'inset))))

(define (format-var-declarations var-decls)
  (define vars
    (for/list ([(name sort) var-decls])
      (elem #:style leibniz-output-style
            (format-var name sort))))
  (if (hash-empty? var-decls)
      ""
      (list "Vars: "
            (add-between vars ", ")
            (linebreak)
            (linebreak))))

(define (format-rule-declarations rule-decls var-decls signature)
  (define rules
    (for/list ([rd rule-decls])
      (elem #:style leibniz-output-style
            (format-rule/tr-declaration #f rd var-decls signature))))
  (if (empty? rule-decls)
      ""
      (list "Rules:" (linebreak)
            (apply nested
                   (add-between rules (linebreak))
                   #:style 'inset))))

(define (format-asset-declarations asset-decls var-decls signature)
  (define assets
    (for/list ([(label ad) asset-decls])
      (elem #:style leibniz-output-style
            (format-asset-declaration label ad var-decls signature))))
  (if (empty? asset-decls)
      ""
      (list "Assets:" (linebreak)
            (apply nested
                   (add-between assets (linebreak))
                   #:style 'inset))))

(define (format-rule/tr-declaration label decl context-vars signature)
  (match-define `(,type-tag ,vars ,pattern ,replacement ,condition) decl)
  (define pattern-elem (format-decl-term pattern signature))
  (define proc-rule? (procedure? replacement))
  (define replacement-elem
    (if proc-rule?
        (italic "<procedure>")
        (format-decl-term replacement signature)))
  (list* (format-label label)
         pattern-elem
         (cond
           [(equal? type-tag 'transformation) " → "]
           [proc-rule? " ↣ "]
           [else " ⇒ "])
         replacement-elem
         (format-clauses condition vars context-vars signature)))

(define (format-equation-declaration label decl context-vars signature)
  (match-define `(equation ,vars ,left ,right ,condition) decl)
  (define left-elem (format-decl-term left signature))
  (define right-elem (format-decl-term right signature))
  (list* (format-label label)
         left-elem
         " = "
         right-elem
         (format-clauses condition vars context-vars signature)))

(define (format-asset-declaration label decl context-vars signature)
  (match decl
    [`(equation ,vars ,left ,right ,condition)
     (format-equation-declaration label decl context-vars signature)]
    [`(rule ,vars ,pattern ,replacement ,condition)
     (format-rule/tr-declaration label decl context-vars signature)]
    [`(transformation ,vars ,pattern ,replacement ,condition)
     (format-rule/tr-declaration label decl context-vars signature)]
    [`(as-equation ,asset-ref)
     (list (format-label label)
           "=("
           (format-label asset-ref #f)
           ") ")]
    [`(as-rule ,asset-ref,flip?)
     (list (format-label label)
           (if flip? "⇐(" "⇒(")
           (format-label asset-ref #f)
           ") ")]
    [`(substitute ,rule-ref ,asset-ref ,reduce?)
     (list (format-label label)
           (format-label rule-ref #f)
           "("
           (format-label asset-ref #f)
           (if reduce? ") ⇩"  ")"))]
    [`(transform ,tr-ref ,asset-ref ,reduce?)
     (list (format-label label)
           (format-label asset-ref #f)
           " ⁞ "
           (format-label tr-ref #f)
           (if reduce? " ⇩" ""))]
    [`(assets ,assets)
     (add-between
      (for/list ([(label2 value) assets])
        (define nested-label (string->symbol (string-join (list (symbol->string label)
                                                                (symbol->string label2))
                                                          ".")))
        (format-asset-declaration nested-label value context-vars signature))
      (linebreak))]
    [term
     (list (format-label label)
           (format-decl-term term signature))]))

(define (format-clauses condition vars context-vars signature)
  (define var-elems
    (for/list ([(name sort) vars])
      (if (equal? (hash-ref context-vars name #f) sort)
          ""
          (list (linebreak) (hspace 2)
                "∀ "
                (format-var name sort)))))
  (if condition
        (cons (list (linebreak) (hspace 2)
                    "if "
                    (format-decl-term condition signature))
              var-elems)
        var-elems))

(define (format-decl-term decl-term signature)

  (define (infix-decl-term? decl-term [neighbor-op #f])
    (condd
      [(not (equal? (first decl-term) 'term))
       #f]
      #:do (define raw-op (second decl-term))
      #:do (define-values (op type) (op-symbol-and-type raw-op))
      [(equal? op neighbor-op)
       #f]
      [else
       (equal? type 'infix-op)]))

  (match decl-term
    [(list 'term-or-var raw-name)
     (define-values (name _) (op-symbol-and-type raw-name))
     (if (and signature
              (operators:lookup-var signature raw-name))
         (italic name)
         name)]
    [(list 'term raw-op args)
     (define-values (op type) (op-symbol-and-type raw-op))
     (condd
      [(zero? (length args))               ; no args implies prefix-op
       op]
      [(equal? type 'prefix-op)
       (list op
             "("
             (add-between (for/list ([arg args])
                            (format-decl-term arg signature)) ", ")
             ")")]
      #:do (define arg1 (first args))
      #:do (define f-arg1 (format-decl-term arg1 signature))
      [(equal? op "[]")
       (list (parenthesize f-arg1 (infix-decl-term? arg1))
             "["
             (add-between (for/list ([arg (rest args)])
                            (format-decl-term arg signature)) ", ")
             "]")]
      #:do (define arg2 (second args))
      #:do (define f-arg2 (format-decl-term arg2 signature))
      [(equal? type 'infix-op)
       (list (parenthesize f-arg1 (infix-decl-term? arg1 op))
             " " op  " "
             (parenthesize f-arg2 (infix-decl-term? arg2)))]
      [(equal? op "^")
       (list f-arg1 (superscript f-arg2))]
      [(equal? op "_")
       (list f-arg1 (subscript f-arg2))])]
    [(list (or 'integer 'rational 'floating-point) n)
     (term->string n)]
    [_ (error (format "illegal term ~a" decl-term))]))

(define (plain-text body)
  (cond
    [(list? body)
     (apply string-append (map plain-text (flatten body)))]
    [(not (element? body))
     ; replace math spaces by plain spaces
     (string-normalize-spaces body #px"\\p{Zs}+" #:trim? #f)]
    [(equal? 'superscript (element-style body))
     (string-append "^{" (plain-text (element-content body)) "}")]
    [(equal? 'subscript (element-style body))
     (string-append "_{" (plain-text (element-content body)) "}")]
    [(equal? (element-style body) 'newline)
     "\n"]
    [(equal? (element-style body) 'hspace)
     " "]
    [else
     (plain-text (element-content body))]))

(module+ test

  (check-equal? (plain-text
                 (format-op-declaration 'foo (list '(sort bar)) 'baz))
                "foo(bar) : baz")
  (check-equal? (plain-text
                 (format-op-declaration '^ '((sort foo) (sort bar)) 'baz))
                "foo^{bar} : baz")
  (check-equal? (plain-text
                 (format-op-declaration '_ '((sort foo) (sort bar)) 'baz))
                "foo_{bar} : baz")
  (check-equal? (plain-text
                 (format-op-declaration '|[]| '((sort foo) (sort bar)) 'baz))
                "foo[bar] : baz")
  (check-equal? (plain-text
                 (format-var 'X 'foo))
                "X:foo")
  (check-equal? (plain-text (format-signature-declaration '(subsort foo bar)))
                "foo ⊆ bar")
  (check-equal? (plain-text (format-rule/tr-declaration
                             #f
                             (list 'rule (hash) '(term-or-var X) '(term-or-var Y) #f)
                             (hash) #f))
                "X ⇒ Y")
  (check-equal? (plain-text (format-rule/tr-declaration
                             #f
                             (list 'transformation (hash) '(term-or-var X) '(term-or-var Y) #f)
                             (hash) #f))
                "X → Y")
  (check-equal? (plain-text (format-asset-declaration
                             'eq1
                             (list 'equation (hash) '(term-or-var X) '(term-or-var Y) #f)
                             (hash) #f))
                "eq1: X = Y")
  (check-equal? (plain-text (format-asset-declaration
                             'rule1
                             (list 'rule (hash) '(term-or-var X) '(term-or-var Y) #f)
                             (hash) #f))
                "rule1: X ⇒ Y")
  (check-equal? (plain-text (format-asset-declaration
                             'a-sum
                             '(term _+ ((term-or-var X) (term-or-var Y)))
                             (hash) #f))
                "a-sum: X + Y")
  (check-equal? (plain-text (format-asset-declaration
                             'group
                             (list 'assets (hash 'index '(integer 2) 'value '(term-or-var X)))
                             (hash) #f))
                "group.index: 2\ngroup.value: X"))
