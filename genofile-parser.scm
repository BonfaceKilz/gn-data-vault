
;;util methods
(define-public (string-starts-with? str prefix)
  (define prefix-length (string-length prefix))
  (and (>= (string-length str) prefix-length)
       (string=? (substring str 0 prefix-length) prefix)))
(define-module (genofile-parser)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1))

;; genofile line parsers

(define-public (genofiles-line-parsers lines parser-type)
  (map parser-type lines))

(define-public (parse-genofile-labels line)
  (if (string-starts-with? line "@")
      line
      #f))

(define-public (parse-genofile-headers line)
  (if (string-starts-with? line "#")
      line
      #f))

(define-public (parse-label line)
  (let* ((label-value (map string-trim (string-split (substring line 1 (string-length line)) #\:)))
         (label (car label-value))
         (value (cadr label-value)))
    (if (not (member label '("name" "filler" "type" "mat" "pat" "het" "unk")))
        #f
        (if (equal? label "name")
            (cons "group" value)
            (cons label value)))))


(define (parse-genotype-labels lines)
  (define acceptable-labels '("name" "filler" "type" "mat" "pat" "het" "unk"))
  (define (parse-line line)
    (parse-label line))
  (apply append
         (map (lambda (item)
                (if item (list item) '()))
              (map parse-line lines))))

;; file readers
(define-public (read-file-line-by-line filename)
  (call-with-input-file filename
    (lambda (input-port)
      (let loop ((lines '())
                 (line (read-line input-port)))
        (if (eof-object? line)
            (reverse lines)	   ; Return the lines in reverse order
            (loop (cons line lines) (read-line input-port)))))))

(define-public (read-file-line-by-lines filename parse-genofile-function)
  (with-input-from-file filename
    (lambda (input-port)
      (let loop ((lines '())
                 (line (read-line input-port)))
        (if (eof-object? line)
            (reverse lines) ; Return the lines in reverse order
            (loop (cons (parse-genofile-function line) lines) (read-line input-port)))))))

(define-public (parse-genotype-marker line geno-obj parlist)
  (define marker-row (map string-trim (string-split line #\tab)))

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

  ;;(define cm-val 12.1)  ;;for debug purpose only

  ;;parsing
  (define cm-val
    (catch 'exception
      (lambda ()
	(let ((cm-column (string->number (hash-ref geno-obj "cm_column" "test"))))
          cm-column))
      (lambda (condition)
	(if (and (hash-ref geno-obj "Mbmap") (hash-ref geno-obj "mb_column"))
            (let ((mb-column (string->number (hash-ref geno-obj "mb_column"))))
              mb-column)
            0))))
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
                 (if (not (number? result))
                     result
                     #f)))
             #f))
   (cons "genotype" genotype)))

(define-public (parse-genotype-file filename)
  (let* ((lines (read-file-line-by-line filename))
	 (lines-without-comments (filter (lambda (line) (not (string-prefix? "#" (string-trim line)))) lines)))
    lines))
