#lang racket/base

; Pattern matching on binary data in a bytestring

(require #;racket/fixnum racket/contract racket/match racket/require
         soup-lib/parameter srfi/207
         (for-syntax racket/base racket/fixnum racket/match racket/string syntax/parse))
(require (filtered-in (lambda (name)
                        (and (or (string-prefix? name "unsafe-fx")
                                 (string-prefix? name "unsafe-bytes"))
                             (substring name 7))) racket/unsafe/ops))

(module+ test (require rackunit net/ip))

(provide
 binary
 (contract-out
  [byte->nybbles (-> byte? (values byte? byte?))]
  [binary-match-default-endianness (parameter/c (or/c 'big-endian 'little-endian 'native-endian
                                                      'network-order 'host-order)
                                                (or/c 'big-endian 'little-endian 'native-endian))]
 ))

(define-parameter binary-match-default-endianness
  'native-endian
  (lambda (e)
    (case e
      ((network-order big-endian) 'big-endian)
      ((host-order native-endian) 'native-endian)
      ((little-endian) 'little-endian))))

(define (byte->nybbles b)
  (values (fxrshift b 4) (fxand b #x0F)))

(struct binary-match-fail ())

(define (symbol->endianness s)
  (case s
    ((big-endian network-order) #t)
    ((little-endian) #f)
    ((native-endian host-order) (system-big-endian?))))

(begin-for-syntax

  (define-syntax-class endianness
    #:datum-literals (big-endian little-endian native-endian)
    (pattern (~or* big-endian little-endian native-endian)))
  
  (define-syntax-class binary-pattern
    #:datum-literals (bytes zero-padded until-byte until-byte* length-prefixed s8 u8 s16 u16 s32 u32 s64 u64 f32 f64)
    (pattern
      (bytes pat len:exact-positive-integer)
      #:fail-when (and (not (fixnum? (syntax-e #'len))) #'len) "length must be a positive fixnum")

    (pattern
      (zero-padded pat len:exact-positive-integer)
      #:fail-when (and (not (fixnum? (syntax-e #'len))) #'len) "length must be a positive fixnum")

    (pattern
      (until-byte pat delim:exact-nonnegative-integer)
      #:fail-when (and (not (byte? (syntax-e #'delim))) #'delim) "expected a byte? value")
    (pattern
      (until-byte* pat delim:exact-nonnegative-integer)
      #:fail-when (and (not (byte? (syntax-e #'delim))) #'delim) "expected a byte? value")

    (pattern (length-prefixed pat))
    (pattern (length-prefixed pat (~or* u8 u16 u32 u64) endian:endianness))

    (pattern (s8 pat))
    (pattern (u8 pat))

    (pattern (s16 pat))
    (pattern (s16 pat endian:endianness))
    (pattern (u16 pat))
    (pattern (u16 pat endian:endianness))

    (pattern (s32 pat))
    (pattern (s32 pat endian:endianness))
    (pattern (u32 pat))
    (pattern (u32 pat endian:endianness))

    (pattern (s64 pat))
    (pattern (s64 pat endian:endianness))
    (pattern (u64 pat))
    (pattern (u64 pat endian:endianness))

    (pattern (f32 pat))
    (pattern (f32 pat endian:endianness))
    (pattern (f64 pat))
    (pattern (f64 pat endian:endianness))
    )

  (define (extract-sint bs bs-len i len endianness)
    #`(if (fx<= (fx+ #,i #,len) #,bs-len)
          (integer-bytes->integer #,bs #t (symbol->endianness #,endianness) #,i (fx+ #,i #,len))
          (raise (binary-match-fail))))
  
  (define (extract-uint bs bs-len i len endianness)
    #`(if (fx<= (fx+ #,i #,len) #,bs-len)
          (integer-bytes->integer #,bs #f (symbol->endianness #,endianness) #,i (fx+ #,i #,len))
          (raise (binary-match-fail))))
  
  (define (extract-real bs bs-len i len endianness)
    #`(if (fx<= (fx+ #,i #,len) #,bs-len)
          (floating-point-bytes->real #,bs (symbol->endianness #,endianness) #,i (fx+ #,i #,len))
          (raise (binary-match-fail))))

  (define (uXX->size u)
    (case u
      ((u8) 1)
      ((u16) 2)
      ((u32) 4)
      ((u64) 8)))

  (define (symbol->endianness s)
    (case s
      ((big-endian network-order) #t)
      ((little-endian) #f)
      ((native-endian host-order) (system-big-endian?))))

  (define (compile-pattern bs i bs-len pat)
    (match (syntax->datum pat)
      
      ((list 'bytes _ len)
       (with-syntax ([len (caddr (syntax-e pat))])
         #`(if (fx> (fx+ #,i len) #,bs-len)
               (raise (binary-match-fail))
               (let ([old-i #,i])
                 (set! #,i (fx+ #,i len))
                 (subbytes #,bs old-i #,i)))))

      ((list 'zero-padded _ len)
       (with-syntax ([len (caddr (syntax-e pat))])
         #`(if (fx> (fx+ #,i len) #,bs-len)
               (raise (binary-match-fail))
               (let ([z-padded (subbytes #,bs #,i (fx+ #,i len))])
                 (set! #,i (fx+ #,i len))
                 (bytestring-trim-right z-padded (lambda (byte) (fx= byte 0)))))))

      ((list 'until-byte _ delim)
         #`(let ([idx (bytestring-index #,bs (lambda (byte) (fx= byte #,(caddr (syntax-e pat)))) #,i #,bs-len)]
                 [old-i #,i])
             (cond
               (idx
                (set! #,i (fx+ idx 1))
                (subbytes #,bs old-i idx))
               (else
                (raise (binary-match-fail))))))

      ((list 'until-byte* _ delim)
         #`(let ([idx (bytestring-index #,bs (lambda (byte) (fx= byte #,(caddr (syntax-e pat)))) #,i #,bs-len)]
                 [old-i #,i])
             (cond
               (idx
                (set! #,i (fx+ idx 1))
                (subbytes #,bs old-i idx))
               (else
                (set! #,i #,bs-len)
                (subbytes #,bs old-i #,bs-len)))))

      ((list 'length-prefixed _) ; Two byte big-endian length followed by that many bytes
       #`(begin
           (unless (fx>= (fx- #,bs-len #,i) 2) (raise (binary-match-fail)))
           (let* ([len (integer-bytes->integer #,bs #f #t #,i (fx+ #,i 2))]
                  [end-idx (fx+ #,i len 2)])
             (if (fx<= end-idx #,bs-len)
                 (let ([b (subbytes #,bs (fx+ #,i 2) end-idx)])
                   (set! #,i end-idx)
                   b)
                 (raise (binary-match-fail))))))

      ((list 'length-prefixed _ size endian) ; size bytes length followed by that many bytes
       #`(let ([size-len #,(uXX->size (syntax-e (caddr (syntax-e pat))))])
           (unless (fx>= (fx- #,bs-len #,i) size-len) (raise (binary-match-fail)))
           (let* ([len (integer-bytes->integer #,bs #f #,(symbol->endianness (syntax-e (cadddr (syntax-e pat)))) #,i (fx+ #,i size-len))]
                  [end-idx (fx+ #,i len size-len)])
             (if (fx<= end-idx #,bs-len)
                 (let ([b (subbytes #,bs (fx+ #,i size-len) end-idx)])
                   (set! #,i end-idx)
                   b)
                 (raise (binary-match-fail))))))

      ((list 's8 _)
       #`(cond
           ((fx< #,i #,bs-len)
            (set! #,i (fx+ #,i 1))
            (integer-bytes->integer #,bs #t #f (fx- #,i 1) i))
           (else
            (raise (binary-match-fail)))))

      ((list 'u8 _)
       #`(cond
           ((fx< #,i #,bs-len)
            (set! #,i (fx+ #,i 1))
            (bytes-ref #,bs (fx- #,i 1)))
           (else
            (raise (binary-match-fail)))))

      ((list 's16 _)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 2))
           #,(extract-sint bs bs-len #'old-i #'2 #'(binary-match-default-endianness))))
      ((list 's16 _ endian)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 2))
           #,(extract-sint bs bs-len #'old-i #'2 #`(quote #,(caddr (syntax-e pat))))))
      ((list 'u16 id)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 2))
           #,(extract-uint bs bs-len #'old-i #'2 #'(binary-match-default-endianness))))
      ((list 'u16 _ endian)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 2))
           #,(extract-uint bs bs-len #'old-i #'2 #`(quote #,(caddr (syntax-e pat))))))

      ((list 's32 _)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 4))
           #,(extract-sint bs bs-len #'old-i #'4 #'(binary-match-default-endianness))))
      ((list 's32 _ endian)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 4))
           #,(extract-sint bs bs-len #'old-i #'4 #`(quote #,(caddr (syntax-e pat))))))
      ((list 'u32 id)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 4))
           #,(extract-uint bs bs-len #'old-i #'4 #'(binary-match-default-endianness))))
      ((list 'u32 _ endian)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 4))
           #,(extract-uint bs bs-len #'old-i #'4 #`(quote #,(caddr (syntax-e pat))))))

      ((list 's64 _)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 8))
           #,(extract-sint bs bs-len #'old-i #'8 #'(binary-match-default-endianness))))
      ((list 's64 _ endian)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 8))
           #,(extract-sint bs bs-len #'old-i #'8 #`(quote #,(caddr (syntax-e pat))))))
      ((list 'u64 id)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 8))
           #,(extract-uint bs bs-len #'old-i #'8 #'(binary-match-default-endianness))))
      ((list 'u64 _ endian)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 8))
           #,(extract-uint bs bs-len #'old-i #'8 #`(quote #,(caddr (syntax-e pat))))))

      ((list 'f32 _)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 4))
           #,(extract-real bs bs-len #'old-i #'4 #'(binary-match-default-endianness))))
      ((list 'f32 _ endian)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 4))
           #,(extract-real bs bs-len #'old-i #'4 #`(quote #,(caddr (syntax-e pat))))))

      ((list 'f64 _)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 8))
           #,(extract-real bs bs-len #'old-i #'8 #'(binary-match-default-endianness))))
      ((list 'f64 _ endian)
       #`(let ([old-i #,i])
           (set! #,i (fx+ #,i 8))
           #,(extract-real bs bs-len #'old-i #'8 #`(quote #,(caddr (syntax-e pat))))))

      ((list 'rest* _)
       #`(let ([old-i #,i])
           (set! #,i #,bs-len)
           (subbytes #,bs old-i #,bs-len)))
      
      (_ (raise-syntax-error 'binary "unknown pattern" pat))))

  (define (compile-patterns pats)
    (with-syntax ([(bs i bs-len) (generate-temporaries '(bs i bs-len))])
      #`(lambda (bs)
          (with-handlers* ([binary-match-fail? (lambda (e) (void))])
            (let ([bs-len (bytes-length bs)]
                  [i 0])
              (list
               #,@(map (lambda (pat) (compile-pattern #'bs #'i #'bs-len pat)) (syntax->list pats)))))))))

(define-match-expander binary
  (lambda (stx)
    (syntax-parse stx
      #:datum-literals (rest*)
      ((binary pats:binary-pattern ...+ (~optional (rest* rest-pat)))
       #`(? bytes? (app #,(compile-patterns #'(pats ... (~? (~@ (rest* rest-pat))))) (list pats.pat ... (~? (~@ rest-pat)))))))))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.
  (check-equal? (match #"abcd1234" ((binary (bytes a 4) (bytes b 4)) (list a b))) '(#"abcd" #"1234"))
  (check-equal? (match (real->floating-point-bytes 3.14 8) ((binary (f64 a)) a)) 3.14)
  (check-equal? (match #"\xFF\x00" ((binary (u16 a little-endian)) a)) 255)
  (check-equal? (match #"\x00\xFFfilename\0\0\0\0" ((binary (u16 a big-endian) (zero-padded fname 12)) (list a fname))) '(255 #"filename"))
  (check-equal? (match #"foo\0bar" ((binary (until-byte a 0) (until-byte* b 0)) (list a b))) '(#"foo" #"bar"))
  (check-equal? (match #"\0\x11the cat is orange!" ((binary (length-prefixed (app bytes->string/utf-8 str)) (rest* leftover)) (list str leftover)))
                '("the cat is orange" #"!"))
  (check-equal? (match #"\x11\0\0\0the cat is orange!" ((binary (length-prefixed (app bytes->string/utf-8 str) u32 little-endian) (rest* leftover)) (list str leftover)))
                '("the cat is orange" #"!"))

  
  ; IPv4 header
  (define header #"\x45\x00\x00\x3c\x1c\x46\x40\x00\x40\x06\xb1\xe6\xac\x10\x0a\x63\xac\x10\x0a\x0c")
  (check-equal?
   (parameterize ([binary-match-default-endianness 'network-order])
     (match header
       ((binary
         (u8 (app byte->nybbles version header-length)) (u8 service-type) (u16 total-length)
         (u16 identification) (u16 flags+fragment)
         (u8 ttl) (u8 protocol) (u16 checksum)
         (bytes (app make-ip-address source-address) 4)
         (u32 (app (lambda (n) (make-ip-address n 4)) dest-address))
         (rest* options))
        (list version header-length service-type total-length ttl protocol (ip-address->string source-address) (ip-address->string dest-address) options))))
   '(4 5 0 60 #x40 6 "172.16.10.99" "172.16.10.12" #""))
  
  )