

(use-modules (ice-9 rdelim))

(define (string-starts-with? str prefix)
  (define prefix-length (string-length prefix)) 
  (and (>= (string-length  str) prefix-length)
    (string=? (substring str 0 prefix-length) prefix)
    )
  )

(define (hashtable->alist ht)
  (let-values (((ks vs) (hashtable-entries ht)))
    (vector->list (vector-map cons ks vs))))
