#lang racket

(provide empty-document
         add-to-library
         new-context-from-source
         get-context get-context-declarations
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
         (prefix-in contexts: "./contexts.rkt")
         (prefix-in rewrite: "./rewrite.rkt")
         (prefix-in tools: "./tools.rkt")
         (prefix-in builtins: "./builtin-contexts.rkt")
         "./transformations.rkt"
         "./lightweight-class.rkt"
         racket/hash
         sxml
         threading)

(module+ test
  (require rackunit)

  (contexts:define-context test-context1
    (sort foo)
    (sort bar)
    (subsort foo bar))
  
  (contexts:define-context test-context2
    (sort foo)
    (sort bar)
    (sort boolean)
    (subsort foo bar)
    (op a-foo foo)
    (op (a-foo bar) foo)
    (op (a-bar foo) bar)
    (op true boolean)
    (op false boolean)
    (op (_∧ boolean boolean) boolean)
    (op (a-test foo) boolean)
    (op (another-test foo) boolean)
    (var a-var foo)
    (=> #:var (X foo) (a-foo X) a-foo #:if (_∧ (a-test X) (another-test X)))))

;; Re-raise exceptions with the source location information from the document

(define-struct (exn:fail:leibniz exn:fail) (a-srcloc)
  #:property prop:exn:srclocs
  (λ (a-struct)
    (match a-struct
      [(struct exn:fail:leibniz (msg marks a-srcloc))
       (list a-srcloc)])))

(define ((re-raise-exn loc) e)
  (if loc 
      (raise (make-exn:fail:leibniz
              (exn-message e)
              (current-continuation-marks)
              (apply srcloc loc)))
      (raise e)))

;; Make a signature from a sequence of declarations

(define (get-loc locs decl)
  (cond
    [(procedure? locs)
     (locs decl)]
    [(hash? locs)
     (hash-ref locs decl #f)]
    [else
     #f]))

(define (make-sort-graph includes sort-decls subsort-decls locs)
  (define after-includes
    (for/fold ([ms sorts:empty-sort-graph])
              ([m/c includes])
      (sorts:merge-sort-graphs ms (contexts:context-sort-graph (cdr m/c)))))
  (define after-sorts
    (for/fold ([sorts after-includes])
              ([s sort-decls])
      (with-handlers ([exn:fail? (re-raise-exn (get-loc locs s))])
        (sorts:add-sort sorts s))))
  (for/fold ([sorts after-sorts])
            ([ss subsort-decls])
    (with-handlers ([exn:fail? (re-raise-exn (get-loc locs ss))])
      (sorts:add-subsort-relation sorts (car ss) (cdr ss)))))

(define (make-signature sorts includes op-decls var-decls locs)
  (define (argsort sort-or-var-decl)
    (match sort-or-var-decl
      [(list 'sort sort-id) sort-id]
      [(list 'var var-name sort-id) sort-id]))
  (define after-includes
    (for/fold ([msig (operators:empty-signature sorts)])
              ([m/c includes])
      (define csig (contexts:context-signature (cdr m/c)))
      (define isig (case (car m/c)
                     [(use) (operators:remove-vars csig)]
                     [(extend) csig]))
      (operators:merge-signatures msig isig sorts)))
  (define after-ops
    (for/fold ([sig after-includes])
              ([od op-decls])
      (with-handlers ([exn:fail? (re-raise-exn (get-loc locs od))])
        (match-define (list name arity rsort) od)
        (operators:add-op sig name (map argsort arity) rsort
                          #:meta (get-loc locs od)))))
  (define signature
    (for/fold ([sig after-ops])
              ([(vname vsort) var-decls])
      (with-handlers ([exn:fail? (re-raise-exn (get-loc locs (hash vname vsort)))])
        (operators:add-var sig vname vsort))))
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

(define (make-rulelist signature includes rule-decls locs)
  (define after-includes
    (for/fold ([mrl equations:empty-rulelist])
              ([m/c includes])
      (equations:merge-rulelists mrl (contexts:context-rules (cdr m/c)) signature)))
  (for/fold ([rl after-includes])
            ([rd rule-decls])
    (with-handlers ([exn:fail? (re-raise-exn (get-loc locs rd))])
      (equations:add-rule rl (make-rule* signature rd)))))

(define (make-equation* signature equation-expr)
  (match equation-expr
    [(list 'equation label vars left right condition)
     (let ([mp (make-pattern* signature vars)])
       (equations:make-equation signature
                                (mp left)
                                (mp condition)
                                (mp right)
                                label))]
    [_ #f]))

(define (make-assets signature includes asset-decls locs)
  (define (compile-asset decl)
    (match decl
      [(list 'rule arg ...)
       (make-rule* signature decl)]
      [(list 'equation arg ...)
       (make-equation* signature decl)]
      [(hash-table (_ _) ...)
       (for/hash ([(label value) decl])
         (values label (compile-asset value)))]
      [term
       ((make-term* signature) decl)]))
  (define after-includes
    (for/fold ([merged-assets (hash)])
              ([m/c includes])
      (hash-union merged-assets (hash-ref (cdr m/c) 'compiled-assets (hash))
                  #:combine/key combine-assets)))
  (for/fold ([assets after-includes])
            ([(label ad) asset-decls])
    (with-handlers ([exn:fail? (re-raise-exn (get-loc locs (hash label ad)))])
      (hash-union assets (hash label (compile-asset ad)) #:combine/key combine-assets))))

;; Combine varsets checking for name clashes. For use with hash-union.

(define (combine-varsets name sort1 sort2)
  (unless (equal? sort1 sort2)
    (error (format "Var ~a of sort ~a redefined with sort ~a"
                   name sort1 sort2)))
  sort1)

;; Combine assets checking for name clashes. For use with hash-union.

(define (combine-assets label asset1 asset2)
  (unless (equal? asset1 asset2)
    (error (format "Asset label ~a already used for value ~a" label asset1)))
  asset1)

;; Convert SXML to document

(define (sxml->document sxml-document)
  (match-define
    (list '*TOP* (list 'leibniz-document sxml-contexts ...))
    sxml-document)
  (for/fold ([doc empty-document])
            ([sxml-context sxml-contexts])
    (define-values (name context-decls) (sxml->declarations sxml-context))
    (send doc new-context name (hash-set context-decls 'locs (λ (x) #f)))))

(define (sxml->declarations sxml-context)

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

  (define (sxml->term sxml-term)
    (match sxml-term
      [`(term (@ (op ,op-string)) ,args ...)
       (list 'term
             (string->symbol op-string)
             (map sxml->term args))]
      [`(term-or-var (@ (name ,name-string)))
       (list 'term/var (string->symbol name-string))]
      [`(,number-tag (@ (value ,v)))
       (list number-tag (read (open-input-string v)))]))

  (define (sxml->rule sxml-rule)
    (match sxml-rule
      [`(rule (vars ,rv ...)
              (pattern ,rp)
              ,rc
              (replacement ,rr))
       (list 'rule
             (sxml->vars rv)
             (sxml->term rp)
             (sxml->term rr)
             (match rc
               [`(condition)
                #f]
               [`(condition ,term)
                term]))]))

  (define (sxml->equation sxml-eq)
    (match sxml-eq
      [`(equation (@ (id ,elabel))
                  (vars ,ev ...)
                  (left ,el)
                  ,ec
                  (right ,er))
       (list 'equation
             (string->symbol elabel)
             (sxml->vars ev)
             (sxml->term el)
             (sxml->term er)
             (match ec
               [`(condition)
                #f]
               [`(condition ,term) 
                term]))]))

  (define (sxml->assets sxml-assets)
    (match sxml-assets
      [`(assets (asset (@ (id ,label)) ,value) ...)
       (for/hash ([l label]
                  [v value])
         (values (string->symbol l) (sxml->asset v)))]))

  (define (sxml->asset sxml-asset)
    (define (try-term asset)
      (with-handlers ([exn:misc:match? (λ (exn) (try-rule sxml-asset))])
        (sxml->term sxml-asset)))
    (define (try-rule asset)
      (with-handlers ([exn:misc:match? (λ (exn) (try-eq sxml-asset))])
        (sxml->rule sxml-asset)))
    (define (try-eq asset)
      (with-handlers ([exn:misc:match? (λ (exn) (try-assets sxml-asset))])
        (sxml->equation sxml-asset)))
    (define (try-assets asset)
      (with-handlers ([exn:misc:match? (λ (exn) #f)])
        (sxml->assets sxml-asset)))
    (try-term sxml-asset))

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
                   (list (string->symbol on) (map sxml->arg oa) (string->symbol os))])))
  (define rules (for/list ([rule sxml-rules])
                  (sxml->rule rule)))
  (define assets (for/hash ([asset sxml-assets])
                   (match asset
                     [`(asset (@ (id ,alabel))
                              ,value)
                      (values (string->symbol alabel)
                              (sxml->asset value))])))

  (values name
          (hash 'includes includes
                'sorts sorts
                'subsorts subsorts
                'vars vars
                'ops ops
                'rules rules
                'assets assets
                'locs (hash))))

;; Documents track declarations and expressions embedded in a Scribble document

(define-class document

  (field contexts decls order library)
  ;; contexts: a hash mapping context names (strings) to contexts
  ;; decls: a hash mapping context names to hashes with keys
  ;;        'includes 'sorts 'ops 'vars 'rules 'assets,
  ;;        values are lists/sets/hashes of the declarations in each category
  ;; order: a list of context names in the inverse order of definition
  ;; library: a hash mapping document names to documents

  (define (add-to-library name library-document)
    (document contexts decls order
              (hash-set library name library-document)))

  (define (add-context name include-decls context)
    (define temp-doc (new-context-from-source name include-decls))
    (define inclusion-context (send temp-doc get-context name))
    (define inclusion-decls (send temp-doc get-context-declarations name))
    (define full-context
      (contexts:merge-contexts inclusion-context context))
    (document (hash-set contexts name full-context)
              (hash-set decls name
                        (hash 'includes
                              (hash-ref inclusion-decls 'includes)))
              (cons name order)
              library))

  (define (preprocess-declarations context-decls)

    (define (add-loc decls decl loc)
      (hash-update decls 'locs
                   (λ (ls)
                     (if (and loc (not (hash-has-key? ls decl)))
                         (hash-set ls decl loc)
                         ls))))

    (define (add-include decls mode cname loc)
      (~> decls
          (hash-update 'includes (λ (cnames) (append cnames (list (cons mode cname)))))
          (add-loc cname loc)))

    (define (add-sort decls s loc)
      (~> decls
          (hash-update 'sorts (λ (ss) (set-add ss s)))
          (add-loc s loc)))

    (define (add-subsort decls s1 s2 loc)
      (define new-ss (cons s1 s2))
      (~> decls
          (hash-update 'subsorts (λ (ss) (set-add ss new-ss)))
          (add-loc new-ss loc)))

    (define (add-op decls name arity rsort loc)
      (define new-op (list name arity rsort))
      (~> decls
          (hash-update 'ops (λ (ops) (set-add ops new-op)))
          (add-loc new-op loc)))

    (define (add-var decls name sort loc)
      (define new-var (hash name sort))
      (~> decls
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

    (define (add-rule decls pattern replacement clauses loc)
      (define-values (vars condition) (group-clauses clauses loc))
      (define new-rule (list 'rule vars pattern replacement condition))
      (~> decls
          (hash-update 'rules (λ (rules) (if (member new-rule rules)
                                             rules
                                             (append rules (list new-rule)))))
          (add-loc new-rule loc)))

    (define (preprocess-asset value loc)
      (match value
        [(list 'rule pattern replacement clauses)
         (define-values (vars condition) (group-clauses clauses loc))
         (list 'rule vars pattern replacement condition)]
        [(list 'equation label left right clauses)
         (define-values (vars condition) (group-clauses clauses loc))
         (list 'equation label vars left right condition)]
        [(list 'assets (list label value) ...)
         (for/hash ([l label]
                    [v value])
           (values l (preprocess-asset v loc)))]
        [term
         term]))

    (define (add-asset decls label value loc)
      (define new-asset (hash label (preprocess-asset value loc)))
      (~> decls
          (hash-update 'assets (λ (assets) (hash-union assets new-asset
                                                       #:combine/key combine-assets)))
          (add-loc new-asset loc)))

    (define (merge decls1 decls2 loc)
      (define (merge* key v1 v2)
        (case key
          [(includes rules) (remove-duplicates (append v1 v2))]
          [(sorts subsorts ops) (set-union v1 v2)]
          [(vars) (hash-union v1 v2 #:combine/key combine-varsets)]
          [(assets) (hash-union v1 v2 #:combine/key combine-assets)]
          [(locs) (hash-union v1 v2 #:combine (λ (a b) a))]))
      (with-handlers ([exn:fail? (re-raise-exn loc)])
        (hash-union decls1 decls2
                    #:combine/key merge*)))

    (~> (for/fold ([decls (hash 'includes empty
                                'sorts (set)
                                'subsorts (set)
                                'ops (set)
                                'vars (hash)
                                'rules (list)
                                'assets (hash)
                                'locs (hash))])
                  ([decl/loc context-decls])
          (match-define (cons decl loc) decl/loc)
          (match decl
            [(list 'use cname)
             (add-include decls 'use cname loc)]
            [(list 'extend cname)
             (add-include decls 'extend cname loc)]
            [(list 'insert cname tr ...)
             (merge decls
                    (clean-declarations
                     (transform-context-declarations (get-context-declarations cname) tr))
                    loc)]
            [(list 'sort s)
             (add-sort decls s loc)]
            [(list 'subsort s1 s2)
             (~> decls
                 (add-sort s1 loc)
                 (add-sort s2 loc)
                 (add-subsort s1 s2 loc))]
            [(list 'op name arity rsort)
             (for/fold ([decls (~> decls
                                   (add-sort rsort loc)
                                   (add-op name arity rsort loc))])
                       ([arg arity])
               (match arg
                 [(list 'var name sort)
                  (add-var decls name sort loc)]
                 [(list 'sort s)
                  decls]))]
            [(list 'var name sort)
             (add-var decls name sort loc)] 
            [(list 'rule pattern replacement clauses)
             (add-rule decls pattern replacement clauses loc)]
            [(list 'asset label value)
             (add-asset decls label value loc)]))))

  (define (new-context-from-source name context-decls)
    (new-context name (preprocess-declarations context-decls)))

  (define (new-context name cdecls)
    (define locs (hash-ref cdecls 'locs))
    (define included-contexts
      (for/list ([mode/name (hash-ref cdecls 'includes)])
        (with-handlers ([exn:fail? (re-raise-exn (get-loc locs name))])
          (cons (car mode/name) (get-context (cdr mode/name))))))
    (define included-context-decls
      (for/list ([mode/name (hash-ref cdecls 'includes)])
        (with-handlers ([exn:fail? (re-raise-exn (get-loc locs name))])
          (cons (car mode/name) (get-context-declarations (cdr mode/name))))))
    (define sorts (make-sort-graph included-contexts
                                   (hash-ref cdecls 'sorts)
                                   (hash-ref cdecls 'subsorts)
                                   locs))
    (define signature (make-signature sorts included-contexts
                                      (hash-ref cdecls 'ops)
                                      (hash-ref cdecls 'vars)
                                      locs))
    (define rules (make-rulelist signature included-contexts
                                 (hash-ref cdecls 'rules)
                                 locs))
    (define assets (make-assets signature included-context-decls
                                (hash-ref cdecls 'assets)
                                locs))
    (define context (contexts:make-context sorts
                                           signature
                                           rules
                                           equations:empty-equationset))
    (define compiled (hash 'compiled-signature signature
                           'compiled-rules rules
                           'compiled-assets assets))
    (document (hash-set contexts name context)
              (hash-set decls name (hash-union cdecls compiled))
              (cons name order)
              library))

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

  (define (get-context-declarations name)
    (define elements (map string-trim (string-split name "/")))
    (case (length elements)
      [(1)
       (unless (hash-has-key? contexts (first elements))
         (error (format "no context named ~a" name)))
       (hash-ref decls (first elements))]
      [(2)
       (unless (hash-has-key? library (first elements))
         (error (format "no library named ~a" (first elements))))
       (define library-doc (hash-ref library (first elements)))
       (send library-doc get-context-declarations (second elements))]
      [else
       (error (format "illegal context specification ~a" name))]))

  (define (get-document-sxml)
    `(*TOP* (leibniz-document
             ,@(for/list ([name (reverse order)])
                 (get-context-sxml name)))))

  (define (get-context-sxml name)

    (define (term->sxml term)
      (match term
        [(list 'term/var name)
         `(term-or-var (@ (name ,(symbol->string name))))]
        [(list 'term op args)
         `(term (@ (op ,(symbol->string op)))
                ,@(for/list ([arg args])
                    (term->sxml arg)))]
        [(list (and number-tag (or 'integer 'rational 'floating-point)) x)
         `(,number-tag (@ (value ,(format "~a" x))))]))

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
          `(condition ,(term->sxml condition))))

    (define (rule->sxml rule)
      (match-let
          ([(list 'rule vars pattern replacement condition) rule])
        `(rule (vars ,@(vars->sxml vars))
               (pattern ,(term->sxml pattern))
               ,(condition->sxml condition)
               (replacement ,(term->sxml replacement)))))

    (define (equation->sxml eq)
      (match-let
          ([(list 'equation label vars left right condition) eq])
        `(equation (@ (id ,(symbol->string label)))
                   (vars ,@(vars->sxml vars))
                   (left ,(term->sxml left))
                   ,(condition->sxml condition)
                   (right ,(term->sxml right)))))

    (define (asset->sxml asset)
      (define (try-term asset)
        (with-handlers ([exn:misc:match? (λ (exn) (try-rule asset))])
          (term->sxml asset)))
      (define (try-rule asset)
        (with-handlers ([exn:misc:match? (λ (exn) (try-eq asset))])
          (rule->sxml asset)))
      (define (try-eq asset)
        (with-handlers ([exn:misc:match? (λ (exn) (try-assets asset))])
          (equation->sxml asset)))
      (define (try-assets asset)
        (with-handlers ([exn:misc:match? (λ (exn) #f)])
          (assets->sxml asset)))
      (try-term asset))

    (define (assets->sxml assets)
      `(assets ,@(for/list ([(label asset) assets])
                   `(asset (@ (id ,(symbol->string label))) ,(asset->sxml asset)))))

    (define cdecls (hash-ref decls name))

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
                         (rule->sxml rd)))
              ,(assets->sxml (hash-ref cdecls 'assets))))

  (define (write-xml filename)
    (define sxml (get-document-sxml))
    (call-with-output-file filename
      (λ (output-port)
        (srl:sxml->xml sxml output-port))
      #:mode 'text #:exists 'replace))

  (define (import-sxml document-name sxml-document)
    (add-to-library document-name (sxml->document sxml-document)))

  (define (import-xml document-name filename)
    (import-sxml document-name
                 (call-with-input-file filename
                   (λ (input-port)
                     (ssax:xml->sxml input-port empty))
                   #:mode 'text)))

  (define (make-term context-name term-expr loc)
    (define context (get-context context-name))
    (define signature (contexts:context-signature context))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      ((make-term* signature) term-expr)))

  (define (make-rule context-name rule-expr loc)
    (define context (get-context context-name))
    (define signature (contexts:context-signature context))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (make-rule* signature
                  (first (hash-ref (preprocess-declarations
                                    (list (cons rule-expr loc)))
                                   'rules)))))

  (define (make-transformation context-name rule-expr loc)
    (define context (get-context context-name))
    (define signature (contexts:context-signature context))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (equations:make-transformation signature
                                     (make-rule* signature
                                                 (first (hash-ref (preprocess-declarations
                                                                   (list (cons rule-expr loc)))
                                                                  'rules))))))

  (define (make-equation context-name equation-expr loc)
    (define context (get-context context-name))
    (define signature (contexts:context-signature context))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (define ad (hash-ref (preprocess-declarations
                            (list (cons (list 'asset (second equation-expr) equation-expr) loc)))
                           'assets))
      (unless (equal? (hash-count ad) 1)
        (error "can't happen"))
      (make-equation* signature
                      (first (hash-values ad)))))

  (define (make-test context-name rule-expr loc)
    (define context (get-context context-name))
    (define signature (contexts:context-signature context))
    (define rules (contexts:context-rules context))
    (with-handlers ([exn:fail? (re-raise-exn loc)])
      (define rd (hash-ref (preprocess-declarations
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
      (tools:signature->graphviz path context))

    (define dot (find-executable-path "dot"))
    (unless dot
      (displayln "Warning: dot executable not found"))
    (for ([dot-file (recursive-file-list directory)])
      (with-output-to-file (path-replace-extension dot-file #".png")
        (thunk (system* dot "-Tpng" dot-file)))
      (delete-file dot-file))))

;; Utility functions for processing context declarations

(define (clean-declarations cdecls)
  (for/fold ([c cdecls])
            ([key (hash-keys cdecls)]
             #:when (string-prefix? (symbol->string key) "compiled"))
    (hash-remove c key)))

;; A document containing the builtin contexts

(define builtins
  (~> (document (hash) (hash) empty (hash))
      (add-context "truth"
                   empty
                   builtins:truth)
      (add-context "integers"
                   (list (cons '(use "truth") #f))
                   builtins:integers)
      (add-context "rational-numbers"
                   (list (cons '(use "truth") #f))
                   builtins:rational-numbers)
      (add-context "real-numbers"
                   (list (cons '(use "truth") #f))
                   builtins:real-numbers)
      (add-context "IEEE-floating-point"
                   (list (cons '(use "integers") #f))
                   builtins:IEEE-floating-point)))

(define empty-document
  (~> (document (hash) (hash) empty  (hash))
      (add-to-library "builtins" builtins)))

(module+ test
  (check-equal? (~> empty-document
                    (new-context-from-source
                     "test"
                     (list (cons '(sort foo) #f)
                           (cons '(sort bar) #f)
                           (cons '(subsort foo bar) #f)))
                    (get-context "test"))
                test-context1)
  (check-equal? (~> empty-document
                    (new-context-from-source
                     "test"
                     (list (cons '(subsort foo bar) #f)))
                    (get-context "test"))
                test-context1)
  (check-equal? (~> empty-document
                    (new-context-from-source
                     "test"
                     (list (cons '(sort foo) #f)
                           (cons '(sort bar) #f)
                           (cons '(subsort foo bar) #f)
                           (cons '(sort bar) #f)
                           (cons '(subsort foo bar) #f)
                           (cons '(sort foo) #f)))
                    (get-context "test"))
                test-context1)

  (check-equal? (~> empty-document
                    (new-context-from-source
                     "test"
                     (list (cons '(subsort foo bar) #f)
                           (cons '(op a-foo () foo) #f)))
                    (make-term "test" '(term/var a-foo) #f)
                    (terms:term->string))
                "foo:a-foo")

  (check-equal? (~> empty-document
                    (new-context-from-source
                     "test"
                     (list (cons '(subsort foo bar) #f)
                           (cons '(sort boolean) #f)
                           (cons '(op a-foo () foo) #f)
                           (cons '(op a-foo ((sort bar)) foo) #f)
                           (cons '(op a-bar ((sort foo)) bar) #f)
                           (cons '(op true () boolean) #f)
                           (cons '(op false () boolean) #f)
                           (cons '(op _∧ ((sort boolean) (sort boolean)) boolean) #f)
                           (cons '(op a-test ((sort foo)) boolean) #f)
                           (cons '(op another-test ((sort foo)) boolean) #f)
                           (cons '(var a-var foo) #f)
                           (cons '(rule (term a-foo ((term/var X)))
                                        (term/var a-foo)
                                        ((var X foo)
                                         (term a-test ((term/var X)))
                                         (term another-test ((term/var X))))) #f)))
                    (get-context "test"))
                test-context2)

  (check-equal? (~> empty-document
                    (new-context-from-source
                     "test"
                     (list (cons '(subsort foo bar) #f)
                           (cons '(op a-foo () foo) #f)
                           (cons '(op a-foo ((sort bar)) foo) #f)
                           (cons '(op a-bar ((var X foo)) bar) #f)
                           (cons '(rule (term a-foo ((term/var X)))
                                        (term/var a-foo)
                                        ((var X foo))) #f)
                           (cons '(asset eq1
                                         (equation eq1
                                                   (term/var a-foo)
                                                   (term a-foo ((term/var a-foo)))
                                                   ())) #f)))
                    (get-context-declarations "test")
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
                                               'eq1
                                               (hash)
                                               '(term/var a-foo)
                                               '(term a-foo ((term/var a-foo)))
                                               #f))))

  (let ([decls (list (cons '(sort foo) #f)
                     (cons '(sort bar) #f)
                     (cons '(var X foo) #f)
                     (cons '(var X foo) #f)
                     (cons '(var X bar) #f))])
    (check-not-exn (thunk
                    (~> empty-document
                        (new-context-from-source "test" (take decls 4)))))
    ;; var name redefined
    (check-exn exn:fail?
               (thunk
                (~> empty-document
                    (new-context-from-source "test" decls)))))

  (let ([decls (list (cons '(subsort foo bar) #f)
                     (cons '(op a-foo () foo) #f)
                     (cons '(op a-foo ((sort bar)) foo) #f)
                     (cons '(op a-bar ((var X foo)) bar) #f)
                     (cons '(asset eq1
                                   (equation eq1
                                             (term/var a-foo)
                                             (term a-foo ((term/var a-foo)))
                                             ())) #f)
                     (cons '(asset eq1
                                   (equation eq1
                                             (term/var a-foo)
                                             (term a-foo ((term/var a-foo)))
                                             ())) #f)
                     (cons '(asset eq1
                                   (equation eq1
                                             (term a-foo ((term/var a-foo)))
                                             (term/var a-foo)
                                             ())) #f))])
    (check-not-exn (thunk
                    (~> empty-document
                        (new-context-from-source "test" (take decls 5)))))
    ;; asset name redefined to the same value
    (check-not-exn (thunk
                    (~> empty-document
                        (new-context-from-source "test" (take decls 6)))))
    ;; asset name redefined to a different value
    (check-exn exn:fail?
               (thunk
                (~> empty-document
                    (new-context-from-source "test" decls)))))

  ;; (check-exn exn:fail?
  ;;            ; non-regular signature
  ;;            (thunk
  ;;             (~> empty-document
  ;;                 (new-context-from-source "test"
  ;;                              (list (cons '(subsort A B) #f)
  ;;                                    (cons '(subsort A C) #f)
  ;;                                    (cons '(op foo ((sort B)) B) #f)
  ;;                                    (cons '(op foo ((sort C)) C) #f))))))

  (define test-document
    (~> empty-document
        (new-context-from-source
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
                             (equation eq1
                                       (term a-foo ((term/var X)))
                                       (term/var a-foo)
                                       ((var X foo)))) #f)
               (cons '(asset eq2
                             (equation eq2
                                       (integer 2)
                                       (integer 3)
                                       ())) #f)
               (cons '(asset more-assets
                             (assets [int1 (integer 2)]
                                     [int2 (integer 3)])) #f)))))

  (check-true (~> test-document
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

  (check-equal? (~> test-document
                    get-document-sxml
                    sxml->document
                    (get-context "test"))
                (get-context test-document "test"))

  (check-equal? (~> test-document
                    get-document-sxml
                    sxml->document
                    (get-context-declarations "test")
                    (hash-remove 'locs))
                (~> (get-context-declarations test-document "test")
                    (hash-remove 'locs))))

;; Tests for module "transformations.rkt"
;; Put here to avoid cyclic dependencies.

(module+ test

  (define a-document
    (~> empty-document
        (new-context-from-source
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
                             (equation eq
                                       (term + ((term/var b) (term/var x)))
                                       (term/var b)
                                       ((var x SQ)))) #f)
               (cons '(asset term-asset (term/var foo)) #f)))))

  ;; hide-vars
  (check-equal? (~> a-document
                    (new-context-from-source
                     "test"
                     (list (cons '(insert "template" hide-vars) #f)))
                    (get-context-declarations "test"))
                (~> a-document
                    (new-context-from-source
                     "test"
                     (list (cons '(subsort SQ Q) #f)
                           (cons '(op foo () SQ) #f)
                           (cons '(op + ((sort SQ) (sort SQ)) SQ) #f)
                           (cons '(op * ((sort SQ) (sort SQ)) Q) #f)
                           (cons '(rule (term + ((term/var b) (term/var x)))
                                        (term/var b)
                                        ((var a Q) (var b SQ) (var x SQ))) #f)
                           (cons '(asset eq-asset
                                         (equation eq
                                                   (term + ((term/var b) (term/var x)))
                                                   (term/var b)
                                                   ((var a Q) (var b SQ) (var x SQ)))) #f)
                           (cons '(asset term-asset (term/var foo)) #f)))
                    (get-context-declarations "test")))

  (check-exn exn:fail?
             (thunk (~> a-document
                        (new-context-from-source
                         "with-var-term"
                         (list (cons '(insert "template") #f)
                               (cons '(asset var-term-asset (term/var b)) #f)))
                        (new-context-from-source
                         "test"
                         (list (cons '(insert "with-var-term" hide-vars) #f))))))

  ;; rename-sort
  (check-equal? (~> a-document
                    (new-context-from-source
                     "test"
                     (list (cons '(insert "template" (rename-sort SQ M)) #f)))
                    (get-context-declarations "test"))
                (~> a-document
                    (new-context-from-source
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
                                         (equation eq
                                                   (term + ((term/var b) (term/var x)))
                                                   (term/var b)
                                                   ((var x M)))) #f)
                           (cons '(asset term-asset (term/var foo)) #f)))
                    (get-context-declarations "test")))

  ;; real->float
  (define heron
    (~> empty-document
        (new-context-from-source
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
                             (equation label
                                       (term heron ((term/var x) (term/var ε) (term/var e)))
                                       (term heron ((term/var x) (term/var ε) 
                                                    (term _× ((rational 1/2)
                                                              (term _+ ((term/var e)
                                                                        (term _÷ ((term/var x)
                                                                                  (term/var e))))))))) 
                                       ())) #f)
               (cons '(asset term-asset
                             (term _× ((rational 1/2)
                                       (term heron ((term/var x) (term/var ε) (term/var e)))))) #f)))
        (new-context-from-source
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
                             (equation label
                                       (term heron ((term/var x) (term/var ε) (term/var e)))
                                       (term heron ((term/var x) (term/var ε) 
                                                    (term _× ((floating-point 0.5)
                                                              (term _+ ((term/var e)
                                                                        (term _÷ ((term/var x)
                                                                                  (term/var e))))))))) 
                                       ())) #f)
               (cons '(asset term-asset
                             (term _× ((floating-point 0.5)
                                       (term heron ((term/var x) (term/var ε) (term/var e)))))) #f)))
        (new-context-from-source
         "converted-to-float"
         (list (cons '(insert "using-rational" (real->float FP64)) #f)))))
  (check-equal? (~> heron
                    (get-context-declarations "converted-to-float"))
                (~> heron
                    (get-context-declarations "using-float"))))
