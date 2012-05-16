(use-modules (system foreign))
(define libmmr (dynamic-link "./libmmr.so"))

(define ffi-test
   (pointer->procedure int
		                            (dynamic-func "ffi_test" libmmr)
					                         (list int int)))
(ffi-test 1 2)
