#lang racket/base
(require racket/sandbox)
(provide (all-from-out racket/sandbox))

;; no limits -- the handin server uses per-session limits
(sandbox-memory-limit #f)

;; share these with evaluators
(sandbox-namespace-specs
 (let ([specs (sandbox-namespace-specs)])
   `(,(car specs)
     ,@(cdr specs)
     lang/posn
     ,@(if gui? '(mrlib/cache-image-snip) '()))))

;; local overrides
(require racket/runtime-path)
(define-runtime-path overrides "overridden-collects")
(define-runtime-path global-overrides "/home/c/cs-110/spd/overridden-collects")
(sandbox-override-collection-paths
  (cons overrides (cons global-overrides (sandbox-override-collection-paths))))

;; allow bitmap/url calls through
(sandbox-network-guard (lambda (a b c d)
			 (if (and (eqv? a 'tcp-connect) (eqv? d 'client))
			     (void)
			     (error "Can't access network from sandbox."))))
