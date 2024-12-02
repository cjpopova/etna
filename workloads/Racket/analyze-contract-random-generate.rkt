#lang racket

#|
The following script tries to run contract-exercise on all of non-flat contracts in the current directory
1. Scan over benchmarks modules
2. For each module, call `module->exports` to get the list of values `provide`d as symbols
3. For each symbol, call `dynamic-require` to get the value from the module
4. Call `contract-value` to get the contract on the module
5. exercise
|#


; Print a line of the form "~a,...\n"  where each argument is comm`a-separated
(define-syntax print-analysis
  (syntax-rules ()
    [(_ info ...) (printf "~a,~a,~a,~a\n" info ...)]))

; path-to-module symbol value contract -> void
; print output: abridged-path, symbol, contract, crg-works? 
(define (analyze-contract mod-path sym val ctc)
  (with-handlers ([exn:fail?
                   (λ (e)
                     (if (regexp-match
                          #rx"contract-random-generate: unable to construct any generator for contract.*"
                          (exn-message e))
                         (print-analysis mod-path sym ctc 2)
                         (print-analysis mod-path sym ctc 0)))])
    ; first, check if we can actually create a generator for the function's domains (assuming the input is a function)
    (for-each contract-random-generate (base->-doms/c ctc)) ; raises exception if it cannot create a generator
    (contract-exercise val) ; exercise the function
    (print-analysis mod-path sym ctc 1)))

; path -> path
; Example: ../fully-typed-benchmarks/acquire/typed/something.rkt -> acquire/typed/something.rkt
(define (abridged-path p)
  (define pieces (take-right (explode-path p) 3)) ; get the benchmark-name/base|typed/module-name.rkt in a list
  (build-path (first pieces) (second pieces) (third pieces)))

; path to rkt file -> void
; loop over all exports from the file and call analyze-contract on them
(define (scan-file module-path)
  (define a-path (abridged-path module-path))
  (dynamic-require module-path 0) ; we need require for module->exports to work
  (define-values [val-exports stx-exports] (module->exports module-path))
  (define syms
    (append (if (empty? val-exports) '() (map first (rest (assoc 0 val-exports))))
            (if (empty? stx-exports) '() (map first (rest (assoc 0 stx-exports))))))
  (for* ([sym (in-list syms)] ; loop over results from module->exports
         [val (in-value ; get the value of the export from the module
               (with-handlers ([exn:fail? (λ (e) #false)])
                 (dynamic-require module-path sym)))]
         #:when val
         [ctc (in-value (value-contract val))] ; get the contract from the value
         #:when (and ctc (not (flat-contract? ctc)))) ; skip boring contracts
    (analyze-contract a-path sym val ctc)
    #;(with-handlers ([exn:fail? (λ (e)
                                   (printf "\n~a ~a\n" module-path sym)
                                   (displayln e))])
        (contract-exercise val))))

; String Symbol -> void
; Run analyze-contracts on a given contract
(define (one-test a-path sym)
  (define module-path a-path)
  (dynamic-require module-path 0) ; we need require for module->exports to work
  (define val (with-handlers ([exn:fail? (λ (e) #false)])
                (dynamic-require module-path sym)))
  (printf "Exercising ~a\n" val)
  (contract-exercise #:fuel 100 val))

(one-test "SYSTEMF/src/impl.rkt" 'tshift)
#;(scan-file "SYSTEMF/src/impl.rkt")


#;(for* ([benchmark (directory-list (current-directory) #:build? #t)]
       #:when (equal? 'directory (file-or-directory-type benchmark)))
  (scan-file (build-path benchmark (string->path "src/impl.rkt"))))

