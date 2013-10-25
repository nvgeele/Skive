#!racket

;; Code from the Rackjure project

(require (for-syntax racket/base syntax/parse))

(define-syntax (~> stx)
  (syntax-parse stx
		[(_ x)
		 #'x]
		;; When e is a symbol like 'a, that's actually (quote a), so DON'T
		;; thread x into that. We want ((quote a) x), NOT (quote x a).
		[(_ x ((~literal quote) e))
		 #'((quote e) x)]
		[(_ x (e e_1 ...))
		 #'(e x e_1 ...)]
		[(_ x e)
		 #'(~> x (e))]
		[(_ x form form_1 ...)
		 #'(~> (~> x form) form_1 ...)]))
