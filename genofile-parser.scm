
(use-modules (ice-9 ftw)
             (ice-9 rdelim)
             (srfi srfi-1))

(define (string-starts-with? str prefix)
  (define prefix-length (string-length prefix)) 
  (and (>= (string-length  str) prefix-length)
    (string=? (substring str 0 prefix-length) prefix)
    )
  )



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
         (if (assoc allele geno-table)
             (cdr (assoc allele geno-table))
             "U"))
       alleles))

(when (> (length parlist) 0)
  (set! genotype (cons -1 (cons 1 genotype))))

(define cm-val
  (let ((cm-column (hash-ref geno-obj "cm_column"))
        (mb-column (hash-ref geno-obj "mb_column")))
    (let ((result (with-exception-handler
                    (lambda (key args) #f)
                    (lambda ()
                      (if (hash-ref geno-obj "Mbmap")
                          (string->number mb-column)
                          (string->number cm-column))))))
      (if (condition? result)
          #f
          result))))
(list
  (cons "chr" (list-ref marker-row 0))
  (cons "name" (list-ref marker-row 1))
  (cons "cM" cm-val)
  (cons "Mb"
        (if (hash-ref geno-obj "Mbmap")
            (let ((mb-column (hash-ref geno-obj "mb_column")))
              (let ((result (with-exception-handler
                              (lambda (key args) #f)
                              (lambda ()
                                (string->number mb-column)))))
                (if (condition? result)
                    #f
                    result)))
            #f))
  (cons "genotype" genotype))


(define (parse-label line)
  (let* ((label-value (map string-trim (string-split (substring line 1) ":")))
         (label (car label-value))
         (value (cadr label-value)))
    (if (not (member label '("name" "filler" "type" "mat" "pat" "het" "unk")))
        #f
        (if (equal? label "name")
            '(group . value)
            (cons label value)))))

(define (parse-genotype-labels lines)
  (define acceptable-labels '("name" "filler" "type" "mat" "pat" "het" "unk"))
  (define (parse-line line)
    (parse-label line))
  (apply append
         (map (lambda (item)
                (if item (list item) '()))
              (map parse-line lines))))



(define (parse-genotype-file filename parlist)
  "Parse the provided genotype file into a usable Guile data structure."
  (let* ((lines (file->lines filename))
         (lines-without-comments (filter (lambda (line) (not (string-prefix? "#" (string-trim line)))) lines))
         (contents (filter (lambda (line) (not (string=? "" (string-trim line))))) lines-without-comments)
         (labels (parse-genotype-labels (filter (lambda (line) (string-prefix? "@" (string-trim line))) contents)))
         (data-lines (filter (lambda (line) (not (string-prefix? "@" (string-trim line)))) contents))
         (header (parse-genotype-header (car data-lines) parlist))
         (geno-obj (apply hash labels header))
         (markers (map (lambda (line) (parse-genotype-marker line geno-obj parlist)) (cdr data-lines))))
    geno-obj))
