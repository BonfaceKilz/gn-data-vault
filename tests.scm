(use-modules (srfi srfi-64)
             (ice-9 hash-table)
             (genofile-parser ))

(test-begin "genofile parser")

(test-assert "String starts with certain character" (string-starts-with? "#alexander" "#"))
(test-assert "String starts with @" (string-starts-with? "@type:BXD" "@"))
(test-assert "not start with a character"(not (string-starts-with? "@name:BXD" "name")))
(test-assert "test for longer string" (string-starts-with? "this-is-a-longer-tester" "this-is-a"))
;;string starts with
;;read-file-line-by-line filename parse-genofile-functio
(test-equal "test file reading "
  (list "this is a test" "second line" "third line" "@name:kabui" "@type:bxd" "#test:name")
  (read-file-line-by-line "name.txt"))


(test-equal "test parser for labels" (list "@this is a test" #f "@third line")
            (genofiles-line-parsers (list "@this is a test" "second line" "@third line")
                                    parse-genofile-labels) )

(test-equal "test parser for headers" (list "#header1:name" #f #f)
	    (genofiles-line-parsers (list "#header1:name" "header2" "@header3")
				    parse-genofile-headers))

(test-equal "Test valid label"
  (parse-label "  name:alex")
  '("group" . "alex"))

(test-equal "Test invalid label"
  (parse-label "  invalid_label  :  SomeValue  ")
  #f)
(test-equal "Test other labels"
  (parse-label " type:foo")
  '("type" . "foo"))

(define my-hash (make-hash-table))

(hash-set! my-hash "mat" 12)
(hash-set! my-hash "A"  1)
(hash-set! my-hash "pat" 3)
(hash-set! my-hash  "T" "dsad")
(hash-set! my-hash  "het" 2)
(hash-set! my-hash "C"  12)
(hash-set! my-hash  "unk" "test")
(hash-set! my-hash  "N"  "q")

(define  expected-results
  `(("chr" . "chr1")
    ("name" . "Marker1")
    ("cM" . #f)
    ("Mb" . #f)
    ("genotype" "U" "U" "U")))

(define results (parse-genotype-marker "chr1\tMarker1\tA\tA\tC\tT"
				       my-hash
				       '()))
(test-equal "Test parsing genofile markers" expected-results results)

(test-equal "test genofile parsing " (parse-genotype-file "name.txt") (list "this is a test" "second line" "third line" "@name:kabui" "@type:bxd" "#test:name"))

(test-end "genofile parser")
