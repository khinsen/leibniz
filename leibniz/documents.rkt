#lang racket

(provide empty-document
         add-to-library
         add-context-from-source
         get-asset
         get-context
         make-term make-rule make-transformation make-equation
         make-test
         get-document-sxml get-context-sxml
         write-xml import-xml
         write-signature-graphs
         clean-declarations)

(require (prefix-in sorts: "./sorts.rkt")
         (only-in "./sorts.rkt" sort-graph? empty-sort-graph)
         (prefix-in operators: "./operators.rkt")
         (prefix-in terms: "./terms.rkt")
         (prefix-in equations: "./equations.rkt")
         (prefix-in rewrite: "./rewrite.rkt")
         (prefix-in tools: "./tools.rkt")
         (prefix-in builtins: "./builtin-contexts.rkt")
         "./transformations.rkt"
         "./lightweight-class.rkt"
         racket/hash
         sxml
         threading)

(module+ test
  (require rackunit))

;;
;; Re-raise exceptions with the source location information from the document
;;
(define-struct (exn:fail:leibniz exn:fail) (a-srcloc-list)
  #:property prop:exn:srclocs
  (λ (a-struct)
    (match a-struct
      [(struct exn:fail:leibniz (msg marks a-srcloc-list))
       a-srcloc-list])))

(define ((re-raise-exn loc) e)
  (if loc 
      (raise (make-exn:fail:leibniz
              (exn-message e)
              (current-continuation-marks)
              (for/list ([l loc])
                (apply srcloc l))))
      (raise e)))

;;
;; Compile declarations from a context to internal datas tructures.
;;
(define (get-loc locs decl)
  (cond
    [(procedure? locs)
     (locs decl)]
    [(hash? locs)
     (hash-ref locs decl #f)]
    [else
     #f]))

(define (compile-sort-graph includes sort-decls subsort-decls locs)
  ;; Merge the sort graphs of the included contexts.
  (define after-includes
    (for/fold ([ms sorts:empty-sort-graph])
              ([m/c includes])
      (sorts:merge-sort-graphs ms
                               (operators:signature-sort-graph
                                (hash-ref (cdr m/c) 'compiled-signature)))))
  ;; Process the sort declarations.
  (define after-sorts
    (for/fold ([sorts after-includes])
              ([s sort-decls])
      (with-handlers ([exn:fail? (re-raise-exn (get-loc locs s))])
        (sorts:add-sort sorts s))))
  ;; Process the subsort declarations.
  (for/fold ([sorts after-sorts])
            ([ss subsort-decls])
    (with-handlers ([exn:fail? (re-raise-exn (get-loc locs ss))])
      (sorts:add-subsort-relation sorts (car ss) (cdr ss)))))

(define (compile-signature sorts includes op-decls var-decls locs)
  (define (argsort sort-or-var-decl)
    (match sort-or-var-decl
      [(list 'sort sort-id) sort-id]
      [(list 'var var-name sort-id) sort-id]))
  ;; Merge the signatures of the included contexts.
  (define after-includes
    (for/fold ([msig (operators:empty-signature sorts)])
              ([m/c includes])
      (define csig (hash-ref (cdr m/c) 'compiled-signature))
      (define isig (case (car m/c)
                     [(use) (operators:remove-vars csig)]
                     [(extend) csig]))
      (operators:merge-signatures msig isig sorts)))
  ;; Process the op declarations.
  (define after-ops
    (for/fold ([sig after-includes])
              ([od op-decls])
      (with-handlers ([exn:fail? (re-raise-exn (get-loc locs od))])
        (match-define (list name arity rsort) od)
        (operators:add-op sig name (map argsort arity) rsort
                          #:meta (get-loc locs od)))))
  ;; Process the var declarations.
  (define signature
    (for/fold ([sig after-ops])
              ([(vname vsort) var-decls])
      (with-handlers ([exn:fail? (re-raise-exn (get-loc locs (hash vname vsort)))])
        (operators:add-var sig vname vsort))))
  ;; Check for non-regularity.
  (define non-regular (operators:non-regular-op-example signature))
  (when non-regular
    (match-let ([(list op arity rsorts) non-regular])
      (displayln (format "Warning: operator ~a~a has ambiguous sorts ~a"
                         op arity rsorts))))
  signature)

(define (make-term* signature)
  (letrec ([fn (match-lambda
                 [(list 'term/var name)
                  (terms:make-var-or-term signature name)]
                 [(list 'term op args)
                  (terms:make-term signature op (map fn args))]
                 [(list 'integer n) n]
                 [(list 'rational r) r]
                 [(list 'floating-point fp) fp])])
    fn))

(define (make-pattern* signature local-vars)
  (letrec ([fn (match-lambda
                 [#f
                  #f]
                 [(list 'term/var name)
                  (terms:make-var-or-term signature name local-vars)]
                 [(list 'term op args)
                  (terms:make-term signature op (map fn args))]
                 [(list 'integer n) n]
                 [(list 'rational r) r]
                 [(list 'floating-point fp) fp])])
    fn))

(define (make-rule* signature rule-expr)
  (match rule-expr
    [(list 'rule vars pattern replacement condition)
     (let* ([mp (make-pattern* signature vars)])
       (equations:make-rule signature (mp pattern)
                            (mp condition)
                            (mp replacement)
                            #f #t))]
    [_ #f]))

(define (as-rule* signature value flip?)
  (cond
    [(equations:rule? value)
     value]
    [(equations:equation? value)
     (define-values (left right)
       (if flip?
           (values (equations:equation-right value)
                   (equations:equation-left value))
           (values (equations:equation-left value)
                   (equations:equation-right value))))
     (equations:make-rule signature
                          left
                          (equations:equation-condition value)
                          right
                          #f #t)]
    [else (error (format "cannot convert ~a to a rule" value))]))

(define (compile-rules signature includes rule-decls locs)
  ;; Merge the rule lists of the included contexts.
  (define after-includes
    (for/fold ([mrl equations:empty-rulelist])
              ([m/c includes])
      (equations:merge-rulelists mrl
                                 (hash-ref (cdr m/c) 'compiled-rules)
                                 signature)))
  ;; Process the rule declarations
  (for/fold ([rl after-includes])
            ([rd rule-decls])
    (with-handlers ([exn:fail? (re-raise-exn (get-loc locs rd))])
      (equations:add-rule rl (make-rule* signature rd)))))

(define (make-equation* signature equation-expr)
  (match equation-expr
    [(list 'equation vars left right condition)
     (let ([mp (make-pattern* signature vars)])
       (equations:make-equation signature
                                (mp left)
                                (mp condition)
                                (mp right)))]
    [_ #f]))

(define (as-equation* signature value)
  (cond
    [(equations:equation? value)
     value]
    [(equations:rule? value)
     (equations:make-equation signature
                              (equations:rule-pattern value)
                              (equations:rule-condition value)
                              (equations:rule-replacement value))]
    [else (error (format "cannot convert ~a to an equation" value))]))

(define (combine-assets label asset1 asset2)
  (cond
    [(equal? asset1 asset2)
     asset1]
    [(and (equal? (first asset1) 'assets) (equal? (first asset1) 'assets))
     (list 'assets
           (hash-union (second asset1) (second asset2) #:combine/key combine-assets))]
    [else
     (error (format "Asset label ~a already used for value ~a" label asset1))]))

(define (combine-compiled-assets label asset1 asset2)
  (cond
    [(equal? asset1 asset2)
     asset1]
    [(and (hash? asset1) (hash? asset2))
     (hash-union asset1 asset2 #:combine/key combine-compiled-assets)]
    [(and (box? asset1) (not (unbox asset1)))
     asset2]
    [(and (box? asset2) (not (unbox asset2)))
     asset1]
    [else
     (error (format "Asset label ~a already used for value ~a" label asset1))]))

(define (compile-assets signature includes asset-decls locs)
  (define unevaluated empty)
  ;; Helper function for compiling a single asset.
  (define (compile-asset decl)
    (match decl
      [(list 'rule arg ...)
       (box (make-rule* signature decl))]
      [(list 'equation arg ...)
       (box (make-equation* signature decl))]
      [(or (list 'as-equation label)
           (list 'as-rule label _))
       (let ([no-value (box #f)])
         (set! unevaluated (cons (list no-value decl) unevaluated))
         no-value)]
      [(list 'assets assets)
       (for/hash ([(label value) assets])
         (values label (compile-asset value)))]
      ;; TODO Should test for valid term declarations, rather than suppose
      ;; that it must be a term since it's no other valid declaration.
      [term
       (box ((make-term* signature) decl))]))
  ;; Helper functions for computing dependent assets
  (define (compute-asset assets decl)
    (match decl
      [(list 'as-equation label)
       (and~> (lookup-asset assets label)
              (as-equation* signature _))]
      [(list 'as-rule label flip?)
       (and~> (lookup-asset assets label)
              (as-rule* signature _ flip?))]))
  (define (compute-assets unevaluated)
    (for/fold ([remaining empty])
              ([box+decl unevaluated])
      (match-define (list box decl) box+decl)
      (define value (compute-asset assets decl))
      (if value
          (begin (set-box! box value)
                 remaining)
          (cons (box decl) remaining))))
  (define (compute-assets-iteratively unevaluated)
    (let ([remaining (compute-assets unevaluated)])
      (if (equal? (length remaining) (length unevaluated))
          remaining
          (compute-assets-iteratively remaining))))
  ;; Merge the assets of the included contexts.
  (define after-includes
    (for/fold ([merged-assets (hash)])
              ([m/c includes])
      (hash-union merged-assets (hash-ref (cdr m/c) 'compiled-assets (hash))
                  #:combine/key combine-compiled-assets)))
  ;; Process the asset declarations
  (define assets
    (for/fold ([assets after-includes])
              ([(label ad) asset-decls])
      (with-handlers ([exn:fail? (re-raise-exn (get-loc locs (hash label ad)))])
        (hash-union assets (hash label (compile-asset ad))
                    #:combine/key combine-compiled-assets))))
  ;; Compute unevaluated assets
  (define remaining (compute-assets-iteratively unevaluated))
  (unless (empty? remaining)
    (error (format "~a remaining unevaluated assets" (length remaining))))
  ;; Return compiled assets, guaranteed without unevaluated boxes
  assets)

;;
;; Convert an SXML document to the internal document data structure.
;;
(define (combine-varsets name sort1 sort2)
  (unless (equal? sort1 sort2)
    (error (format "Var ~a of sort ~a redefined with sort ~a"
                   name sort1 sort2)))
  sort1)

(define (sxml->document sxml-document)
  (match-define
    (list '*TOP* (list 'leibniz-document
                       (list 'library document-refs ...)
                       sxml-contexts ...))
    sxml-document)
  (let ([doc-with-library
         (for/fold ([doc empty-document])
                   ([dref document-refs])
           (match-define `(document-ref (@ (id ,name)) ,ref) dref)
           (send doc import-xml name ref))])
    (for/fold ([doc doc-with-library])
              ([sxml-context sxml-contexts])
      (define-values (namespace context-decls) (sxml->context sxml-context))
      (send doc add-context namespace (hash-set context-decls 'locs (λ (x) #f))))))

(define (sxml->context sxml-context)

  (define (sxml->arg sxml-arg)
    (match sxml-arg
      [`(var (@ (id ,var-name) (sort ,sort-name)))
       (list 'var (string->symbol var-name) (string->symbol sort-name))]
      [`(var (@ (sort ,sort-name) (id ,var-name)))
       (list 'var (string->symbol var-name) (string->symbol sort-name))]
      [`(sort (@ (id ,sort-name)))
       (list 'sort (string->symbol sort-name))]))

  (define (sxml->vars sxml-vars)
    (for/fold ([vars (hash)])
              ([v sxml-vars])
      (define new-var
        (match v
          [(or `(var (@ (id ,vn) (sort ,sn)))
               `(var (@ (sort ,sn) (id ,vn))))
           (hash (string->symbol vn) (string->symbol sn))]))
      (hash-union vars new-var #:combine/key combine-varsets)))

  (define (sxml->asset sxml-asset)
    (match sxml-asset
      [`(assets (asset (@ (id ,label)) ,value) ...)
       (list 'assets
             (for/hash ([l label]
                        [v value])
               (values (string->symbol l) (sxml->asset v))))]
      [`(equation (vars ,ev ...)
                  (left ,el)
                  ,ec
                  (right ,er))
       (list 'equation
             (sxml->vars ev)
             (sxml->asset el)
             (sxml->asset er)
             (match ec
               [`(condition)       #f]
               [`(condition ,term) term]))]
      [`(rule (vars ,rv ...)
              (pattern ,rp)
              ,rc
              (replacement ,rr))
       (list 'rule
             (sxml->vars rv)
             (sxml->asset rp)
             (sxml->asset rr)
             (match rc
               [`(condition)       #f]
               [`(condition ,term) term]))]
      [`(term (@ (op ,op-string)) ,args ...)
       (list 'term
             (string->symbol op-string)
             (map sxml->asset args))]
      [`(term-or-var (@ (name ,name-string)))
       (list 'term/var (string->symbol name-string))]
      [`(,number-tag (@ (value ,v)))
       (list number-tag (read (open-input-string v)))]
      [`(as-equation (@ (ref ,asset-ref)))
       (list 'as-equation (string->symbol asset-ref))]
      [`(as-rule (@ (ref ,asset-ref) (flip ,flip?)))
       (list 'as-rule (string->symbol asset-ref) (equal? flip? "true"))]))

  (match-define
    `(context (@ (id, name))
              (includes ,sxml-includes ...)
              (sorts ,sxml-sorts ...)
              (subsorts ,sxml-subsorts ...)
              (vars ,sxml-vars ...)
              (ops ,sxml-ops ...)
              (rules ,sxml-rules ...)
              (assets ,sxml-assets ...))
    sxml-context)
 
  (define includes (for/list ([include sxml-includes])
                     (match include
                       [(list mode include-ref)
                        (cons mode include-ref)])))
  (define sorts (for/set ([sort sxml-sorts])
                  (match sort
                    [`(sort (@ (id ,s)))
                     (string->symbol s)])))
  (define subsorts (for/set ([subsort sxml-subsorts])
                     (match subsort
                       [`(subsort (@ (subsort ,s1) (supersort ,s2)))
                        (cons (string->symbol s1) (string->symbol s2))]
                       [`(subsort (@ (supersort ,s2) (subsort ,s1)))
                        (cons (string->symbol s1) (string->symbol s2))])))
  (define vars (sxml->vars sxml-vars))
  (define ops (for/set ([op sxml-ops])
                (match op
                  [`(op (@ (id ,on))
                        (arity ,oa ...)
                        (sort (@ (id ,os))))
                   (list (string->symbol on)
                         (map sxml->arg oa)
                         (string->symbol os))])))
  (define rules (for/list ([rule sxml-rules])
                  (sxml->asset rule)))
  (define assets (second (sxml->asset (cons 'assets sxml-assets))))

  (values name
          (hash 'includes includes
                'sorts sorts
                'subsorts subsorts
                'vars vars
                'ops ops
                'rules rules
                'assets assets
                'locs (hash))))

;; Decompose an compound asset label into a list of nested simple asset labels

(define (nested-labels compound-label)
  (~> (symbol->string compound-label)
      (string-split ".")
      (map string->symbol _)))

;;
;; A document is a collection of contexts that can refer to each other by name.
;; Each document also keeps a library of other documents to whose contexts
;; its own contexts can refer. The dependency graph is acyclic by construction.
;;
(define-class document

  (field contexts order library library-refs)
  ;; contexts: a hash mapping context names to hashes with keys
  ;;           'includes 'sorts 'ops 'vars 'rules 'assets,
  ;;           values are lists/sets/hashes of the declarations in
  ;;            each category
  ;; order: a list of context names in the inverse order of definition
  ;; library: a hash mapping document names to documents
  ;; library-refs: a hash mapping document names to external references
  ;;               (filenames for now, later maybe DOIs or hashes)

  ;; Add another document to the library.
  (define (add-to-library name library-document external-ref)
    (document contexts order
              (hash-set library name library-document)
              (hash-set library-refs name external-ref)))

  ;; Add a built-in context. Used only for preparing a single document
  ;; called "builtins", which is automatically added to every other
  ;; document's library.
  (define (add-builtin-context name include-decls signature rules)
    (define temp-doc (add-context-from-source name include-decls))
    (define inclusion-decls (send temp-doc get-context name))
    (document (hash-set contexts name
                        (hash 'includes
                              (hash-ref inclusion-decls 'includes)
                              'compiled-signature
                              signature
                              'compiled-rules
                              rules
                              'compiled-assets
                              (hash)))
              (cons name order)
              library
              library-refs))

  ;; Take context declarations parsed by Leibniz' extensions to Scribble
  ;; and convert them to the internal context data structure.
  (define (preprocess-source-declarations source-decls)

    (define (add-loc context decl loc)
      (hash-update context 'locs
                   (λ (ls)
                     (if (and loc (not (hash-has-key? ls decl)))
                         (hash-set ls decl (list loc))
                         ls))))

    (define (add-include context mode cname loc)
      (~> context
          (hash-update 'includes (λ (cnames) (append cnames (list (cons mode cname)))))
          (add-loc cname loc)))

    (define (add-sort context s loc)
      (~> context
          (hash-update 'sorts (λ (ss) (set-add ss s)))
          (add-loc s loc)))

    (define (add-subsort context s1 s2 loc)
      (define new-ss (cons s1 s2))
      (~> context
          (hash-update 'subsorts (λ (ss) (set-add ss new-ss)))
          (add-loc new-ss loc)))

    (define (add-op context name arity rsort loc)
      (define new-op (list name arity rsort))
      (~> context
          (hash-update 'ops (λ (ops) (set-add ops new-op)))
          (add-loc new-op loc)))

    (define (add-var context name sort loc)
      (define new-var (hash name sort))
      (~> context
          (hash-update 'vars (λ (vars)
                               (hash-union vars new-var
                                           #:combine/key combine-varsets)))
          (add-loc new-var loc)))

    (define (group-clauses clauses loc)
      (for/fold ([vars (hash)]
                 [condition #f])
                ([c clauses])
        (match c
          [`(var ,name ,sort)
           (values (hash-union vars (hash name sort)
                               #:combine/key combine-varsets)
                   condition)]
          [term
           (if condition
               (values vars (list 'term '_∧ (list condition term)))
               (values vars term))])))

    (define (add-rule context pattern replacement clauses loc)
      (define-values (vars condition) (group-clauses clauses loc))
      (define new-rule (list 'rule vars pattern replacement condition))
      (~> context
          (hash-update 'rules (λ (rules) (if (member new-rule rules)
                                             rules
                                             (append rules (list new-rule)))))
          (add-loc new-rule loc)))

    (define (preprocess-asset value loc)
      (match value
        [(list 'rule pattern replacement clauses)
         (define-values (vars condition) (group-clauses clauses loc))
         (list 'rule vars pattern replacement condition)]
        [(list 'equation left right clauses)
         (define-values (vars condition) (group-clauses clauses loc))
         (list 'equation vars left right condition)]
        [(list 'assets (list label value) ...)
         (list 'assets (for/hash ([l label]
                                  [v value])
                         (values l (preprocess-asset v loc))))]
        [(list 'as-rule label flip?)
         value]
        [(list 'as-equation label)
         value]
        [term
         term]))

    (define (add-asset context label value loc)
      (define preprocessed-value (preprocess-asset value loc))
      (define nested (nested-labels label))
      (define new-asset (hash (first nested)
                              (for/fold ([v preprocessed-value])
                                        ([l (reverse (rest nested))])
                                (list 'assets (hash l v)))))
      (~> context
          (hash-update 'assets (λ (assets) (hash-union assets new-asset
                                                       #:combine/key combine-assets)))
          (add-loc new-asset loc)))

    (define (add-include-prefix inserted-context cname)
      (define elements (map string-trim (string-split cname "/")))
      (if (equal? (length elements) 1)
          inserted-context
          (let ([prefix (first elements)])
            (hash-update inserted-context 'includes
                         (λ (is)
                           (for/list ([mode/name is])
                             (cons (car mode/name)
                                   (string-append prefix "/" (cdr mode/name)))))))))

    (define (merge context inserted-context loc)
      (define (merge* key v1 v2)
        (case key
          [(includes rules) (remove-duplicates (append v1 v2))]
          [(sorts subsorts ops) (set-union v1 v2)]
          [(vars) (hash-union v1 v2 #:combine/key combine-varsets)]
          [(assets) (hash-union v1 v2 #:combine/key combine-assets)]
          [(locs) (hash-union v1 v2 #:combine (λ (a b) a))]))
      (define (add-loc-prefix context loc)
        (hash-update context 'locs
                     (λ (ls)
                       (for/hash ([(d l) ls])
                         (values d (cons loc l))))))
      (hash-union context (add-loc-prefix inserted-context loc)
                  #:combine/key merge*))

    (~> (for/fold ([context (hash 'includes empty
                                  'sorts (set)
                                  'subsorts (set)
                                  'ops (set)
                                  'vars (hash)
                                  'rules (list)
                                  'assets (hash)
                                  'locs (hash))])
                  ([decl/loc source-decls])
          (match-define (cons decl loc) decl/loc)
          (match decl
            [(list 'use cname)
             (add-include context 'use cname loc)]
            [(list 'extend cname)
             (add-include context 'extend cname loc)]
            [(list 'insert cname tr ...)
             (define (apply-transformations context tr)
               (with-handlers ([exn:fail? (re-raise-exn loc)])
                 (transform-context-declarations context tr)))
             (define insertion
               (~> (get-context cname)
                   (add-include-prefix cname)
                   (apply-transformations tr)
                   clean-declarations))
             (merge context insertion loc)]
            [(list 'sort s)
             (add-sort context s loc)]
            [(list 'subsort s1 s2)
             (~> context
                 (add-sort s1 loc)
                 (add-sort s2 loc)
                 (add-subsort s1 s2 loc))]
            [(list 'op name arity rsort)
             (for/fold ([context (~> context
                                     (add-sort rsort loc)
                                     (add-op name arity rsort loc))])
                       ([arg arity])
               (match arg
                 [(list 'var name sort)
                  (add-var context name sort loc)]
                 [(list 'sort s)
                  context]))]
            [(list 'var name sort)
             (add-var context name sort loc)] 
            [(list 'rule pattern replacement clauses)
             (add-rule context pattern replacement clauses loc)]
            [(list 'asset label value)
             (add-asset context label value loc)]))))

  ;; Add a context defined by a sequence of source declarations.
  (define (add-context-from-source name source-decls)
    (add-context name (preprocess-source-declarations source-decls)))

  ;; Add a context defined by a context data structure.
  (define (add-context name context)
    (define locs (hash-ref context 'locs))
    (define included-contexts
      (for/list ([mode/name (hash-ref context 'includes)])
        (with-handlers ([exn:fail? (re-raise-exn (get-loc locs name))])
          (cons (car mode/name) (get-context (cdr mode/name))))))
    (define sorts (compile-sort-graph included-contexts
                                      (hash-ref context 'sorts)
                                      (hash-ref context 'subsorts)
                                      locs))
    (define signature (compile-signature sorts included-contexts
                                         (hash-ref context 'ops)
                                         (hash-ref context 'vars)
                                         locs))
    (define rules (compile-rules signature included-contexts
                                 (hash-ref context 'rules)
                                 locs))
    (define assets (compile-assets signature included-contexts
                                   (hash-ref context 'assets)
                                   locs))
    (define compiled (hash 'compiled-signature signature
                           'compiled-rules rules
                           'compiled-assets assets))
    (document (hash-set contexts name (hash-union context compiled))
              (cons name order)
              library
              library-refs))

  ;; Retrieve a context by name
  (define (get-context name)
    (define elements (map string-trim (string-split name "/")))
    (case (length elements)
      [(1)
       (unless (hash-has-key? contexts (first elements))
         (error (format "no context named ~a" name)))
       (hash-ref contexts (first elements))]
      [(2)
       (unless (hash-has-key? library (first elements))
         (error (format "no library named ~a" (first elements))))
       (define library-doc (hash-ref library (first elements)))
       (send library-doc get-context (second elements))]
      [else
       (error (format "illegal context specification ~a" name))]))

  ;; Return an SXML representation of the document
  (define (get-document-sxml)
    `(*TOP* (leibniz-document
             (library
              ,@(for/list ([(name ref) library-refs]
                           #:unless (equal? name "builtins"))
                  `(document-ref (@ (id ,name)) ,ref)))
             ,@(for/list ([name (reverse order)])
                 (get-context-sxml name)))))

  ;; Return an SXML representation of a context.
  (define (get-context-sxml name)

    (define (vars->sxml vars)
      (for/list ([(name sort) vars])
        `(var (@ (id ,(symbol->string name))
                 (sort ,(symbol->string sort))))))

    (define (op->sxml op)
      `(op (@ (id ,(symbol->string (first op))))
           (arity ,@(for/list ([sv (second op)])
                      (if (equal? (first sv) 'sort)
                          `(sort (@ (id ,(symbol->string (second sv)))))
                          `(var (@ (id ,(symbol->string (second sv)))
                                   (sort ,(symbol->string (third sv))))))))
           (sort (@ (id ,(symbol->string (third op)))))))

    (define (condition->sxml condition)
      (if (equal? condition #f)
          `(condition)
          `(condition ,(asset->sxml condition))))

    (define (asset->sxml asset)
      (match asset
        [(list 'assets asset-data)
         (assets->sxml asset-data)]
        [(list 'equation vars left right condition)
         `(equation (vars ,@(vars->sxml vars))
                    (left ,(asset->sxml left))
                    ,(condition->sxml condition)
                    (right ,(asset->sxml right)))]
        [(list 'rule vars pattern replacement condition)
         `(rule (vars ,@(vars->sxml vars))
                (pattern ,(asset->sxml pattern))
                ,(condition->sxml condition)
                (replacement ,(asset->sxml replacement)))]
        [(list 'term/var name)
         `(term-or-var (@ (name ,(symbol->string name))))]
        [(list 'term op args)
         `(term (@ (op ,(symbol->string op)))
                ,@(for/list ([arg args])
                    (asset->sxml arg)))]
        [(list (and number-tag (or 'integer 'rational 'floating-point)) x)
         `(,number-tag (@ (value ,(format "~a" x))))]
        [(list 'as-equation asset-ref)
         `(as-equation (@ (ref ,(symbol->string asset-ref))))]
        [(list 'as-rule asset-ref flip?)
         `(as-rule (@ (ref ,(symbol->string asset-ref))
                      (flip ,(if flip? "true" "false"))))]))

    (define (assets->sxml assets)
      `(assets ,@(for/list ([(label asset) assets])
                   `(asset (@ (id ,(symbol->string label))) ,(asset->sxml asset)))))

    (define cdecls (hash-ref contexts name))

    `(context (@ (id ,name))
              (includes ,@(for/list ([mode/name (hash-ref cdecls 'includes)])
                            (list (car mode/name) (cdr mode/name))))
              (sorts ,@(for/list ([s (hash-ref cdecls 'sorts)])
                         `(sort (@ (id ,(symbol->string s))))))
              (subsorts ,@(for/list ([sd (hash-ref cdecls 'subsorts)])
                            `(subsort (@ (subsort ,(symbol->string (car sd)))
                                         (supersort ,(symbol->string (cdr sd)))))))
              (vars ,@(vars->sxml (hash-ref cdecls 'vars)))
              (ops ,@(for/list ([od (hash-ref cdecls 'ops)])
                       (op->sxml od)))
              (rules ,@(for/list ([rd (hash-ref cdecls 'rules)])
                         (asset->sxml rd)))
              ,(assets->sxml (hash-ref cdecls 'assets))))

  ;; Write the document to an XML file.
  (define (write-xml filename)
    (define sxml (get-document-sxml))
    (call-with-output-file filename
      (λ (output-port)
        (srl:sxml->xml sxml output-port))
      #:mode 'text #:exists 'replace))

  ;; Import an SXML document to the library.
  (define (import-sxml document-name sxml-document external-ref)
    (add-to-library document-name
                    (sxml->document sxml-document)
                    external-ref))

  ;; Import an document from an XML file to the library.
  (define (import-xml document-name filename)
    (import-sxml document-name
                 (call-with-input-file filename
                   (λ (input-port)
                     (ssax:xml->sxml input-port empty))
                   #:mode 'text)
                 filename))

  ;; Create internal data structures from source declarations
  ;; for various asset types.

  (define (make-term context-name term-expr loc)
    (define context (get-context context-name))
    (define signature (hash-ref context 'compiled-signature))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      ((make-term* signature) term-expr)))

  (define (make-rule context-name rule-expr loc)
    (define context (get-context context-name))
    (define signature (hash-ref context 'compiled-signature))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (make-rule* signature
                  (first (hash-ref (preprocess-source-declarations
                                    (list (cons rule-expr loc)))
                                   'rules)))))

  (define (make-transformation context-name rule-expr loc)
    (define context (get-context context-name))
    (define signature (hash-ref context 'compiled-signature))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (equations:make-transformation signature
                                     (make-rule* signature
                                                 (first (hash-ref (preprocess-source-declarations
                                                                   (list (cons rule-expr loc)))
                                                                  'rules))))))

  (define (make-equation context-name equation-expr loc)
    (define context (get-context context-name))
    (define signature (hash-ref context 'compiled-signature))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (define ad (hash-ref (preprocess-source-declarations
                            (list (cons (list 'asset 'dummy-label equation-expr) loc)))
                           'assets))
      (unless (equal? (hash-count ad) 1)
        (error "can't happen"))
      (make-equation* signature
                      (first (hash-values ad)))))

  (define (make-test context-name rule-expr loc)
    (define context (get-context context-name))
    (define signature (hash-ref context 'compiled-signature))
    (define rules (hash-ref context 'compiled-rules))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (define rd (hash-ref (preprocess-source-declarations
                            (list (cons rule-expr loc)))
                           'rules))
      (unless (equal? (length rd) 1)
        (error "can't happen"))
      (match (first rd)
        [(list 'rule (hash-table) pattern replacement #f)
         (let* ([mt (make-pattern* signature (hash))]
                [term (mt pattern)]
                [expected (mt replacement)]
                [rterm (rewrite:reduce signature rules term)])
           (list term expected rterm))]
        [_ (error "test may not contain rule clauses")])))

  ;; Make a graphical representation of the signatures of all documents
  ;; (using graphviz).
  (define (write-signature-graphs directory)

    (define (delete-directory-recursive directory)
      (when (directory-exists? directory)
        (for ([item (directory-list directory)])
          (define path (build-path directory item))
          (cond
            [(file-exists? path)
             (delete-file path)]
            [(directory-exists? path)
             (delete-directory-recursive path)]))
        (delete-directory directory)))

    (define (recursive-file-list directory)
      (flatten
       (for/list ([item (directory-list directory)])
         (define path (build-path directory item))
         (cond
           [(file-exists? path)
            path]
           [(directory-exists? path)
            (recursive-file-list path)]))))

    (delete-directory-recursive directory)
    (for ([(name context) (in-hash contexts)])
      (define path (build-path directory name))
      (tools:signature->graphviz path (hash-ref context 'compiled-signature)))

    (define dot (find-executable-path "dot"))
    (unless dot
      (displayln "Warning: dot executable not found"))
    (for ([dot-file (recursive-file-list directory)])
      (with-output-to-file (path-replace-extension dot-file #".png")
        (thunk (system* dot "-Tpng" dot-file)))
      (delete-file dot-file))))

;; Retrieve an asset
(define (lookup-asset assets label)
  (define value
    (for/fold ([a assets])
              ([l (nested-labels label)])
      (hash-ref a l)))
  (if (box? value)
      (unbox value)
      value))

(define (get-asset context label)
  (lookup-asset (hash-ref context 'compiled-assets) label))

;; Utility functions for processing context declarations

(define (clean-declarations cdecls)
  (for/fold ([c cdecls])
            ([key (hash-keys cdecls)]
             #:when (string-prefix? (symbol->string key) "compiled"))
    (hash-remove c key)))

;; A document containing the builtin contexts

(define builtins
  (~> (document (hash) empty (hash) (hash))
      (add-builtin-context "truth"
                   empty
                   builtins:truth-signature
                   builtins:truth-rules)
      (add-builtin-context "integers"
                   (list (cons '(use "truth") #f))
                   builtins:integer-signature
                   builtins:merged-integer-rules)
      (add-builtin-context "rational-numbers"
                   (list (cons '(use "truth") #f))
                   builtins:rational-signature
                   builtins:merged-rational-rules)
      (add-builtin-context "real-numbers"
                   (list (cons '(use "truth") #f))
                   builtins:real-number-signature
                   builtins:merged-real-number-rules)
      (add-builtin-context "IEEE-floating-point"
                   (list (cons '(use "integers") #f))
                   builtins:IEEE-float-signature
                   builtins:merged-IEEE-float-rules)))

;; An empty document has "builtins" in its library, ensuring that
;; it is always available.

(define empty-document
  (~> (document (hash) empty  (hash) (hash))
      (add-to-library "builtins" builtins #f)))

;; Tests

(module+ test
  (define test-document1
    (~> empty-document
        (add-context-from-source
         "test"
         (list (cons '(sort foo) #f)
               (cons '(sort bar) #f)
               (cons '(subsort foo bar) #f)))))
  (check-equal? (~> empty-document
                    (add-context-from-source
                     "test"
                     (list (cons '(subsort foo bar) #f))))
                test-document1)
  (check-equal? (~> empty-document
                    (add-context-from-source
                     "test"
                     (list (cons '(sort foo) #f)
                           (cons '(sort bar) #f)
                           (cons '(subsort foo bar) #f)
                           (cons '(sort bar) #f)
                           (cons '(subsort foo bar) #f)
                           (cons '(sort foo) #f))))
                test-document1)

  (check-equal? (~> empty-document
                    (add-context-from-source
                     "test"
                     (list (cons '(subsort foo bar) #f)
                           (cons '(op a-foo () foo) #f)))
                    (make-term "test" '(term/var a-foo) #f)
                    (terms:term->string))
                "foo:a-foo")

  (check-equal? (~> empty-document
                    (add-context-from-source
                     "test"
                     (list (cons '(subsort foo bar) #f)
                           (cons '(op a-foo () foo) #f)
                           (cons '(op a-foo ((sort bar)) foo) #f)
                           (cons '(op a-bar ((var X foo)) bar) #f)
                           (cons '(rule (term a-foo ((term/var X)))
                                        (term/var a-foo)
                                        ((var X foo))) #f)
                           (cons '(asset eq1
                                         (equation (term/var a-foo)
                                                   (term a-foo
                                                         ((term/var a-foo)))
                                                   ())) #f)
                           (cons '(asset nested.asset (term/var a-foo)) #f)
                           (cons '(asset deeply.nested.asset (term/var a-foo)) #f)))
                    (get-context "test")
                    (clean-declarations))
                (hash 'includes empty
                      'locs (hash)
                      'sorts (set 'foo 'bar)
                      'subsorts (set (cons 'foo 'bar))
                      'ops (set 
                            '(a-foo () foo)
                            '(a-foo ((sort bar)) foo)
                            '(a-bar ((var X foo)) bar))
                      'vars (hash 'X 'foo)
                      'rules (list
                              (list 'rule
                                    (hash 'X 'foo)
                                    '(term a-foo ((term/var X)))
                                    '(term/var a-foo)
                                    #f))
                      'assets (hash 'eq1 (list 'equation
                                               (hash)
                                               '(term/var a-foo)
                                               '(term a-foo ((term/var a-foo)))
                                               #f)
                                    'nested (list 'assets (hash 'asset
                                                                '(term/var a-foo)))
                                    'deeply (list 'assets (hash 'nested
                                                                (list 'assets
                                                                      (hash 'asset
                                                                            '(term/var a-foo))))))))

  (let ([decls (list (cons '(sort foo) #f)
                     (cons '(sort bar) #f)
                     (cons '(var X foo) #f)
                     (cons '(var X foo) #f)
                     (cons '(var X bar) #f))])
    (check-not-exn (thunk
                    (~> empty-document
                        (add-context-from-source "test" (take decls 4)))))
    ;; var name redefined
    (check-exn exn:fail?
               (thunk
                (~> empty-document
                    (add-context-from-source "test" decls)))))

  (let ([decls (list (cons '(subsort foo bar) #f)
                     (cons '(op a-foo () foo) #f)
                     (cons '(op a-foo ((sort bar)) foo) #f)
                     (cons '(op a-bar ((var X foo)) bar) #f)
                     (cons '(asset eq1
                                   (equation (term/var a-foo)
                                             (term a-foo ((term/var a-foo)))
                                             ())) #f)
                     (cons '(asset eq1
                                   (equation (term/var a-foo)
                                             (term a-foo ((term/var a-foo)))
                                             ())) #f)
                     (cons '(asset eq1
                                   (equation (term a-foo ((term/var a-foo)))
                                             (term/var a-foo)
                                             ())) #f))])
    (check-not-exn (thunk
                    (~> empty-document
                        (add-context-from-source "test" (take decls 5)))))
    ;; asset name redefined to the same value
    (check-not-exn (thunk
                    (~> empty-document
                        (add-context-from-source "test" (take decls 6)))))
    ;; asset name redefined to a different value
    (check-exn exn:fail?
               (thunk
                (~> empty-document
                    (add-context-from-source "test" decls)))))

  ;; (check-exn exn:fail?
  ;;            ; non-regular signature
  ;;            (thunk
  ;;             (~> empty-document
  ;;                 (add-context-from-source "test"
  ;;                              (list (cons '(subsort A B) #f)
  ;;                                    (cons '(subsort A C) #f)
  ;;                                    (cons '(op foo ((sort B)) B) #f)
  ;;                                    (cons '(op foo ((sort C)) C) #f))))))

  (define test-document2
    (~> empty-document
        (add-context-from-source
         "test"
         (list (cons '(use "builtins/integers") #f)
               (cons '(subsort foo bar) #f)
               (cons '(op a-foo () foo) #f)
               (cons '(op a-foo ((var X foo)) foo) #f)
               (cons '(rule (term a-foo ((term/var X)))
                            (term/var a-foo)
                            ((var X foo))) #f)
               (cons '(asset a-term (term/var a-foo)) #f)
               (cons '(asset a-rule (rule (term a-foo ((term/var X)))
                                          (term/var a-foo)
                                          ((var X foo)))) #f)
               (cons '(asset eq1
                             (equation (term a-foo ((term/var X)))
                                       (term/var a-foo)
                                       ((var X foo)))) #f)
               (cons '(asset eq2
                             (equation (integer 2)
                                       (integer 3)
                                       ())) #f)
               (cons '(asset more-assets
                             (assets [int1 (integer 2)]
                                     [int2 (integer 3)])) #f)
               (cons '(asset equation-from-rule (as-equation a-rule)) #f)
               (cons '(asset rule-from-equation (as-rule eq1 #f)) #f)))))

  (check-true (~> test-document2
                  (make-test "test"
                             '(rule
                               (term a-foo ((term/var a-foo)))
                               (term/var a-foo)
                               ())
                             #f)
                  ;; check if the last two (out of three) elements are equal
                  rest
                  list->set
                  set-count
                  (equal? 1)))

  (check-equal? (~> test-document2
                    get-document-sxml
                    sxml->document
                    (get-context "test")
                    (hash-remove 'locs))
                (~> (get-context test-document2 "test")
                    (hash-remove 'locs)))

  (check-equal? (~> test-document2
                    (get-context "test")
                    (get-asset 'more-assets.int1))
                2))

;; Tests for module "transformations.rkt"
;; Put here to avoid cyclic dependencies.

(module+ test

  (define a-document
    (~> empty-document
        (add-context-from-source
         "template"
         (list (cons '(subsort SQ Q) #f)
               (cons '(var a Q) #f)
               (cons '(var b SQ) #f)
               (cons '(op foo () SQ) #f)
               (cons '(op + ((sort SQ) (sort SQ)) SQ) #f)
               (cons '(op * ((sort SQ) (sort SQ)) Q) #f)
               (cons '(rule (term + ((term/var b) (term/var x)))
                            (term/var b)
                            ((var x SQ))) #f)
               (cons '(asset eq-asset
                             (equation (term + ((term/var b) (term/var x)))
                                       (term/var b)
                                       ((var x SQ)))) #f)
               (cons '(asset term-asset (term/var foo)) #f)
               (cons '(asset rule-from-eq (as-rule eq-asset #f)) #f)))))

  ;; hide-vars
  (check-equal? (~> a-document
                    (add-context-from-source
                     "test"
                     (list (cons '(insert "template" hide-vars) #f)))
                    (get-context "test"))
                (~> a-document
                    (add-context-from-source
                     "test"
                     (list (cons '(subsort SQ Q) #f)
                           (cons '(op foo () SQ) #f)
                           (cons '(op + ((sort SQ) (sort SQ)) SQ) #f)
                           (cons '(op * ((sort SQ) (sort SQ)) Q) #f)
                           (cons '(rule (term + ((term/var b) (term/var x)))
                                        (term/var b)
                                        ((var a Q) (var b SQ) (var x SQ))) #f)
                           (cons '(asset eq-asset
                                         (equation (term + ((term/var b)
                                                            (term/var x)))
                                                   (term/var b)
                                                   ((var a Q)
                                                    (var b SQ)
                                                    (var x SQ)))) #f)
                           (cons '(asset term-asset (term/var foo)) #f)
                           (cons '(asset rule-from-eq (as-rule eq-asset #f)) #f)))
                    (get-context "test")))

  (check-exn exn:fail?
             (thunk (~> a-document
                        (add-context-from-source
                         "with-var-term"
                         (list (cons '(insert "template") #f)
                               (cons '(asset var-term-asset (term/var b)) #f)))
                        (add-context-from-source
                         "test"
                         (list (cons '(insert "with-var-term" hide-vars) #f))))))

  ;; rename-sort
  (check-equal? (~> a-document
                    (add-context-from-source
                     "test"
                     (list (cons '(insert "template" (rename-sort SQ M)) #f)))
                    (get-context "test"))
                (~> a-document
                    (add-context-from-source
                     "test"
                     (list (cons '(subsort M Q) #f)
                           (cons '(var a Q) #f)
                           (cons '(var b M) #f)
                           (cons '(op foo () M) #f)
                           (cons '(op + ((sort M) (sort M)) M) #f)
                           (cons '(op * ((sort M) (sort M)) Q) #f)
                           (cons '(rule (term + ((term/var b) (term/var x)))
                                        (term/var b)
                                        ((var x M))) #f)
                           (cons '(asset eq-asset
                                         (equation (term + ((term/var b)
                                                            (term/var x)))
                                                   (term/var b)
                                                   ((var x M)))) #f)
                           (cons '(asset term-asset (term/var foo)) #f)
                           (cons '(asset rule-from-eq (as-rule eq-asset #f)) #f)))
                    (get-context "test")))

  ;; real->float
  (define heron
    (~> empty-document
        (add-context-from-source
         "using-rational"
         (list (cons '(use "builtins/real-numbers") #f)
               (cons '(op heron ((var x ℝnn) (var ε ℝp) (var e ℝnn)) ℝnn) #f)
               (cons '(rule (term heron ((term/var x) (term/var ε) (term/var e)))
                            (term/var e)
                            ((term _< ((term abs ((term _- ((term/var x)
                                                            (term ^ ((term/var e)
                                                                     (integer 2)))))))
                                       (term/var ε))))) #f)
               (cons '(rule (term heron ((term/var x) (term/var ε) (term/var e)))
                            (term heron ((term/var x) (term/var ε) 
                                         (term _× ((rational 1/2)
                                                   (term _+ ((term/var e)
                                                             (term _÷ ((term/var x)
                                                                       (term/var e))))))))) 
                            ()) #f)
               (cons '(asset rule-asset
                             (rule (term heron ((term/var x) (term/var ε) (term/var e)))
                                   (term heron ((term/var x) (term/var ε) 
                                                (term _× ((rational 1/2)
                                                          (term _+ ((term/var e)
                                                                    (term _÷ ((term/var x)
                                                                              (term/var e))))))))) 
                                   ())) #f)
               (cons '(asset eq-asset
                             (equation (term heron ((term/var x) (term/var ε) (term/var e)))
                                       (term heron ((term/var x) (term/var ε) 
                                                    (term _× ((rational 1/2)
                                                              (term _+ ((term/var e)
                                                                        (term _÷ ((term/var x)
                                                                                  (term/var e))))))))) 
                                       ())) #f)
               (cons '(asset term-asset
                             (term _× ((rational 1/2)
                                       (term heron ((term/var x) (term/var ε) (term/var e)))))) #f)))
        (add-context-from-source
         "using-float"
         (list (cons '(use "builtins/real-numbers") #f)
               (cons '(use "builtins/IEEE-floating-point") #f)
               (cons '(op heron ((var x FP64) (var ε FP64) (var e FP64)) FP64) #f)
               (cons '(rule (term heron ((term/var x) (term/var ε) (term/var e)))
                            (term/var e)
                            ((term _< ((term abs ((term _- ((term/var x)
                                                            (term ^ ((term/var e)
                                                                     (integer 2)))))))
                                       (term/var ε))))) #f)
               (cons '(rule (term heron ((term/var x) (term/var ε) (term/var e)))
                            (term heron ((term/var x) (term/var ε) 
                                         (term _× ((floating-point 0.5)
                                                   (term _+ ((term/var e)
                                                             (term _÷ ((term/var x)
                                                                       (term/var e))))))))) 
                            ()) #f)
               (cons '(asset rule-asset
                             (rule (term heron ((term/var x) (term/var ε) (term/var e)))
                                   (term heron ((term/var x) (term/var ε) 
                                                (term _× ((floating-point 0.5)
                                                          (term _+ ((term/var e)
                                                                    (term _÷ ((term/var x)
                                                                              (term/var e))))))))) 
                                   ())) #f)
               (cons '(asset eq-asset
                             (equation (term heron ((term/var x) (term/var ε) (term/var e)))
                                       (term heron ((term/var x) (term/var ε) 
                                                    (term _× ((floating-point 0.5)
                                                              (term _+ ((term/var e)
                                                                        (term _÷ ((term/var x)
                                                                                  (term/var e))))))))) 
                                       ())) #f)
               (cons '(asset term-asset
                             (term _× ((floating-point 0.5)
                                       (term heron ((term/var x) (term/var ε) (term/var e)))))) #f)))
        (add-context-from-source
         "converted-to-float"
         (list (cons '(insert "using-rational" (real->float FP64)) #f)))))
  (check-equal? (~> heron
                    (get-context "converted-to-float"))
                (~> heron
                    (get-context "using-float"))))
