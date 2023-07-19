

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



(define (read-file-line-by-line filename)
  (call-with-input-file filename
    (lambda (input-port)
      (let loop ((lines '())
                 (line (read-line input-port)))
        (if (eof-object? line)
            (reverse lines) ; Return the lines in reverse order
            (loop (cons line lines) (read-line input-port)))))))





