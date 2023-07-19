

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

;;add dfaulta code below
(define (parse-genofile-labels line)
  (if (string-starts-with? line "@")
      line
      #f))

(define (parse-genofile-headers line)
  (if (string-starts-with? line "#")
    line
    #f))


(define (read-file-line-by-line filename parse-genofile-function)
  (with-input-from-file filename
    (lambda (input-port)
      (let loop ((lines '())
                 (line (read-line input-port)))
        (if (eof-object? line)
            (reverse lines) ; Return the lines in reverse order
            (loop (cons (parse-genofile-function line) lines) (read-line input-port)))))))



(define (parse-genotype-marker line geno-obj parlist)
  (define marker-row (map string-trim (string-split line "\t")))


  (define geno-table
    `((,(hash-ref geno-obj "mat") . -1)
      (,(hash-ref geno-obj "pat") . 1)
      (,(hash-ref geno-obj "het") . 0)
      (,(hash-ref geno-obj "unk") . "U")))
  (define start-pos (if (hash-ref geno-obj "Mbmap") 4 3))
  (when (> (length parlist) 0)
    (set! start-pos (+ start-pos 2)))

  (define alleles (drop marker-row start-pos))
  (define genotype
    (map (lambda (allele)
           (if (hash-has-key? geno-table allele)
               (hash-ref geno-table allele)
               "U"))
         alleles))
  (when (> (length parlist) 0)
    (set! genotype (cons -1 (cons 1 genotype))))
  (define cm-val
    (with-handlers ((condition? (lambda (_) #f)))
      (let ((cm-column (hash-ref geno-obj "cm_column"))
            (mb-column (hash-ref geno-obj "mb_column")))
        (if (hash-ref geno-obj "Mbmap")
            (string->number mb-column)
            (string->number cm-column)))))
  (list
    (cons "chr" (list-ref marker-row 0))
    (cons "name" (list-ref marker-row 1))
    (cons "cM" cm-val)
    (cons "Mb"
          (if (hash-ref geno-obj "Mbmap")
              (string->number (hash-ref geno-obj "mb_column"))
              #f))
    (cons "genotype" genotype)))

