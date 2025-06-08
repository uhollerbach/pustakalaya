;;; -*- mode: scheme; -*-

;;; A small library manager to help me track my books
;;; Copyright 2025, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: 2clause BSD, see file 'LICENSE' for details

(load-library "hash.scm")
(load-library "arg-parse.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (initialize-db fname)
  (if (file-exists? fname)
      (begin (write-string "database file '" fname "' exists already!\n")
	     (exit 1))
      (let ((db (sqlite-open fname 'create))
	    (ins-auth "INSERT INTO author (name1) VALUES (?1)")
	    (ins-ctype "INSERT INTO content_type (name) VALUES (?1)")
	    (ins-btype "INSERT INTO book_type (name) VALUES (?1)")
	    (ins-lang "INSERT INTO language (name) VALUES (?1)")
	    (ins-loc "INSERT INTO location (name) VALUES (?1)"))
	;; main table. all the *_id entries are indices into other tables
	(sqlite-run db #<< HERE_DOC
		    >CREATE TABLE books (
		    >    id INTEGER PRIMARY KEY AUTOINCREMENT,
		    >    title TEXT,
		    >    author1_id INTEGER,
		    >    author2_id INTEGER,
		    >    author3_id INTEGER,
		    >    author4_id INTEGER,
		    >    author5_id INTEGER,
		    >    content_type_id INTEGER,
		    >    book_type_id INTEGER,
		    >    publisher_id INTEGER,
		    >    year INTEGER,
		    >    edition INTEGER,
		    >    language_id INTEGER,
		    >    ISBN INTEGER,
		    >    location_id INTEGER,
		    >    notes TEXT
		    >)
		    >HERE_DOC
		    )

	;; authors table
	(sqlite-run db #<< HERE_DOC
		    >CREATE TABLE author (
		    >    id INTEGER PRIMARY KEY AUTOINCREMENT,
		    >    name1 TEXT,	-- family name
		    >    name2 TEXT,	-- personal name
		    >    name3 TEXT,	-- personal name 2
		    >    aka INTEGER	-- pointer to another entry?
		    >)
		    >HERE_DOC
		    )

	(for-each (lambda (a) (do-pbrc db ins-auth a))
		  '("Anonymous" "Nobody" "et al"))

;; link the 'Anonymous' and 'Nobody' entries?
;; UPDATE author SET aka = (SELECT id FROM author WHERE name1 = 'Anonymous')
;;     WHERE name1 = 'Nobody';

	;; content type table
	(sqlite-run db #<< HERE_DOC
		    >CREATE TABLE content_type (
		    >    id INTEGER PRIMARY KEY AUTOINCREMENT,
		    >    name TEXT
		    >)
		    >HERE_DOC
		    )

	(for-each (lambda (a) (do-pbrc db ins-ctype a))
		  '("science fiction" "fantasy" "computer science"
		    "mathematics" "physics" "cookbook" "bande dessinee"
		    "manga" "origami"))

	;; physical artifact type
	(sqlite-run db #<< HERE_DOC
		    >CREATE TABLE book_type (
		    >    id INTEGER PRIMARY KEY AUTOINCREMENT,
		    >    name TEXT
		    >)
		    >HERE_DOC
		    )

	(for-each (lambda (a) (do-pbrc db ins-btype a))
		  '("paperback" "hardcover" "electronic" "copier"
		    "manuscript" "popup book"))

	;; publisher table
	(sqlite-run db #<< HERE_DOC
		    >CREATE TABLE publisher (
		    >    id INTEGER PRIMARY KEY AUTOINCREMENT,
		    >    name TEXT
		    >)
		    >HERE_DOC
		    )

	;; language table
	(sqlite-run db #<< HERE_DOC
		    >CREATE TABLE language (
		    >    id INTEGER PRIMARY KEY AUTOINCREMENT,
		    >    name TEXT
		    >)
		    >HERE_DOC
		    )

	(for-each (lambda (a) (do-pbrc db ins-lang a))
		  '("English" "German" "French" "Spanish" "Latin"))

	;; add as many more as you like
	;; "Mandarin" "Hindi" "Arabic" "Bengali" "Portuguese" "Russian"
	;; "Indonesian" "Urdu" "Japanese" "Marathi" "Vietnamese"
	;; "Telugu" "Turkish" "Swahili" "Tagalog" "Tamil" "Korean"
	;; "Thai" "Italian" "Gujarati" "Amharic" "Kannada"
	;; "Esperanto" "Dutch" "Danish" "Catalan" "Polish"
	;; "Czech" "Sanskrit" "Hebrew" "Greek" "Aramaic"

	;; location table
	(sqlite-run db #<< HERE_DOC
		    >CREATE TABLE location (
		    >    id INTEGER PRIMARY KEY AUTOINCREMENT,
		    >    name TEXT
		    >)
		    >HERE_DOC
		    )

	(for-each (lambda (a) (do-pbrc db ins-loc a))
		  '("lent out" "destroyed" "given away" "coffee table"
		    "night stand"))
	(sqlite-close db))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Table names don't seem to be suitable for binding via
;;; statement-{prepare,bind,run}, so we have to glue strings
;;; together. That's slightly unsafe in general, but the table
;;; names are a fixed set, so not too bad.

(define (show-table db name)
  (write-string "######## " name " ########\n")
  (for-each (lambda (s) (display s) (newline))
	    (sqlite-run db (string-append "SELECT * FROM " name)))
  (newline))

(define (do-pbrc db stmt . vals)
  (let* ((s1 (sqlite-statement-prepare db stmt))
	 (_1 (apply sqlite-statement-bind s1 vals))
	 (res (sqlite-statement-run s1))
	 (_2 (sqlite-statement-cleanup s1)))
    res))

(define (check-insert db tname chk ins vals)
  (if (list-length>? 1 (apply do-pbrc db chk vals))
      (write-string "Entry '" (apply string-join-by " " vals)
		    "' already exists in table '" tname "'!\n")
      (apply do-pbrc db ins vals)))

(define (add-generic db tname val)
  (let ((chk (string-append
	      "SELECT * FROM " tname " WHERE UPPER(name) = UPPER(?1)"))
	(ins (string-append "INSERT INTO " tname " (name) VALUES ( ?1 )")))
    (check-insert db tname chk ins (list val))))

(defmacro (has-comma? str)
  `(string-find-last-char ,str #\,))

(define (rem-comma str)
  (let ((cix (has-comma? str)))
    (if cix (string-copy str 0 cix) str)))

(define (add-author db val)
  (let* ((t1 "SELECT * FROM author WHERE (UPPER(name1) = UPPER(?1) AND name2 IS NULL and name3 IS NULL)")
	 (t2 "SELECT * FROM author WHERE (UPPER(name1) = UPPER(?1) AND UPPER(name2) = UPPER(?2) and name3 IS NULL)")
	 (t3 "SELECT * FROM author WHERE (UPPER(name1) = UPPER(?1) AND UPPER(name2) = UPPER(?2) and UPPER(name3) = UPPER(?3))")
	 (i1 "INSERT INTO author (name1) VALUES ( ?1 )")
	 (i2 "INSERT INTO author (name1, name2) VALUES ( ?1, ?2 )")
	 (i3 "INSERT INTO author (name1, name2, name3) VALUES ( ?1, ?2, ?3 )")
	 (tname "author")
	 (ns (string-split-by char-whitespace? val))
	 (nns (list-length ns))
	 (cns (case nns
		((1) ns)
		((2) (if (has-comma? (car ns))
			 (cons (rem-comma (car ns)) (cdr ns))
			 (list-reverse ns)))
		((3) (if (has-comma? (car ns))
			 (cons (rem-comma (car ns)) (cdr ns))
			 (cons (caddr ns) (list-head ns 2))))
		(else ()))))
    (case nns
      ((1) (check-insert db tname t1 i1 cns))
      ((2) (check-insert db tname t2 i2 cns))
      ((3) (check-insert db tname t3 i3 cns))
      (else (write-string "Cannot add '" val "' to author table\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-sanitize prompt)
  (let ((e1 (read-line-interactive prompt)))
    (if (string? e1)
	(let ((e2 (string-trim char-whitespace? e1)))
	  (if (string=? e2 "") #f e2))
	#f)))

(define (check-author db prompt)
  (let ((ti "SELECT * FROM author WHERE id = ?1")
	(tn1 #<< HERE_DOC
	     >SELECT * FROM author WHERE (
	     >    UPPER(name1) = UPPER(?1) OR
	     >    UPPER(name2) = UPPER(?1) OR
	     >    UPPER(name3) = UPPER(?1)
	     >)
	     >HERE_DOC
	     )
	(tn2 #<< HERE_DOC
	     >SELECT * FROM author WHERE (
	     >    (UPPER(name1) = UPPER(?1) AND
	     >     UPPER(name2) = UPPER(?2)) OR
	     >    (UPPER(name1) = UPPER(?1) AND
	     >     UPPER(name3) = UPPER(?2)) OR
	     >    (UPPER(name2) = UPPER(?1) AND
	     >     UPPER(name1) = UPPER(?2)) OR
	     >    (UPPER(name2) = UPPER(?1) AND
	     >     UPPER(name3) = UPPER(?2)) OR
	     >    (UPPER(name3) = UPPER(?1) AND
	     >     UPPER(name1) = UPPER(?2)) OR
	     >    (UPPER(name3) = UPPER(?1) AND
	     >     UPPER(name2) = UPPER(?2))
	     >)
	     >HERE_DOC
	     )
	(tn3 #<< HERE_DOC
	     >SELECT * FROM author WHERE (
	     >    (UPPER(name1) = UPPER(?1) AND
	     >     UPPER(name2) = UPPER(?2) AND
	     >     UPPER(name3) = UPPER(?3)) OR
	     >    (UPPER(name1) = UPPER(?1) AND
	     >     UPPER(name3) = UPPER(?2) AND
	     >     UPPER(name2) = UPPER(?3)) OR
	     >    (UPPER(name2) = UPPER(?1) AND
	     >     UPPER(name1) = UPPER(?2) AND
	     >     UPPER(name3) = UPPER(?3)) OR
	     >    (UPPER(name2) = UPPER(?1) AND
	     >     UPPER(name3) = UPPER(?2) AND
	     >     UPPER(name1) = UPPER(?3)) OR
	     >    (UPPER(name3) = UPPER(?1) AND
	     >     UPPER(name1) = UPPER(?2) AND
	     >     UPPER(name2) = UPPER(?3)) OR
	     >    (UPPER(name3) = UPPER(?1) AND
	     >     UPPER(name2) = UPPER(?2) AND
	     >     UPPER(name1) = UPPER(?3))
	     >)
	     >HERE_DOC
	     )
	(val (read-sanitize prompt)))
    (cond ((not val)
	   #f)
	  ((check-numeric val)
	   (let ((id (cdr (do-pbrc db ti val))))
	     (if (null? id)
		 (begin (printf "author %v is unknown\n" val)
			#f)
		 (begin (printf "author %v is %v\n" (caar id) (cdar id))
			(caar id)))))
	  (else
	   (let* ((ns (string-split-by char-whitespace? val))
		  (nns (list-length ns))
		  (cns (case nns
			 ((1) (cdr (apply do-pbrc db tn1 ns)))
			 ((2) (cdr (apply do-pbrc db tn2 ns)))
			 ((3) (cdr (apply do-pbrc db tn3 ns)))
			 (else ()))))
	     (cond ((null? cns)
		    (printf "author %v is unknown\n" val)
		    #f)
		   ((list-length=? 1 cns)
		    (printf "author %v is %v\n" (caar cns) (cdar cns))
		    (caar cns))
		   (else
		    (printf "author %v is ambiguous\n" val)
		    (for-each (lambda (a) (display a) (newline)) cns)
		    #f)))))))

;;; TODO: this is a monadic pattern - rewrite it that way?

(define (read-authors db)
  (let ((c1 (check-author db "Author 1")))
    (if c1
	(let ((c2 (check-author db "Author 2")))
	  (if c2
	      (let ((c3 (check-author db "Author 3")))
		(if c3
		    (let ((c4 (check-author db "Author 4")))
		      (if c4
			  (let ((c5 (check-author db "Author 5")))
			    (if c5
				(list c1 c2 c3 c4 c5)
				(list c1 c2 c3 c4 #f)))
			  (list c1 c2 c3 #f #f)))
		    (list c1 c2 #f #f #f)))
	      (list c1 #f #f #f #f)))
	(begin
	  (write-string "unknown or bad author 1!\n")
	  '(#f #f #f #f #f)))))

(define (check-numeric val)
  (if (and val (regex-match "^[0-9]+$" val)) val #f))

(define (check-isbn val)
  (if val
      (let* ((vcs (filter (lambda (c) (or (char-numeric? c) (char=? c #\X)))
			  (string->char val)))
	     (vl (list-length vcs)))
	(cond ((= 10 vl)
	       (let* ((c0 (char->integer #\0))
		      (ds (map (lambda (c) (i- (char->integer c) c0))
			       (list-head vcs 9)))
		      (sum (foldl1 i+ (map i* ds '(10 9 8 7 6 5 4 3 2))))
		      (cc (let ((cd (modulo (i- 11 (modulo sum 11)) 11)))
			    (if (= cd 10)
				#\X
				(integer->char (i+ c0 cd))))))
		 (char=? cc (list-ref vcs 9))))
	      ((= 13 vl)
	       (let* ((c0 (char->integer #\0))
		      (ds (map (lambda (c) (i- (char->integer c) c0))
			       (list-head vcs 12)))
		      (sum (foldl1 i+ (map i* ds '(1 3 1 3 1 3 1 3 1 3 1 3))))
		      (cc (integer->char
			   (i+ c0 (modulo (i- 10 (modulo sum 10)) 10)))))
		 (char=? cc (list-ref vcs 12))))
	      (else #f)))
      #f))

(define (read-generic db tname)
  (let* ((cmd1 (string-append "SELECT * FROM " tname))
	 (_1 (for-each (lambda (v) (printf "%v\t%v\n" (car v) (cadr v)))
		       (sqlite-run db cmd1)))
	 (entry (read-line-interactive (string-append "Enter " tname))))
    (if (string? entry)
	(let* ((e1 (string-trim char-whitespace? entry))
	       (res1 (do-pbrc db (string-append cmd1 " WHERE id = ?1") e1))
	       (res2 (do-pbrc db (string-append cmd1 " WHERE name = ?1") e1)))
	  (cond ((list-length=? 2 res1) (caadr res1))
		((list-length=? 2 res2) (caadr res2))
		(else (write-string "unknown " tname "!\n")
		      #f)))
	#f)))

(define (add-one-book db)
  (let* ((lam1 (lambda (s) (symbol->string (car s))))
	 (lam2 (lambda (i) (string-append "?" (number->string i))))
	 (title (cons 'title (read-sanitize "Title")))
	 (authors (map cons
		       '(author1_id author2_id author3_id author4_id author5_id)
		       (read-authors db)))
	 (ctype (cons 'content_type_id (read-generic db "content_type")))
	 (btype (cons 'book_type_id (read-generic db "book_type")))
	 (publisher (cons 'publisher_id (read-generic db "publisher")))
	 (year (cons 'year (check-numeric (read-sanitize "Year of publication"))))
	 (edition (cons 'edition (check-numeric (read-sanitize "Edition"))))
	 (language (cons 'language_id (read-generic db "language")))
	 (isbn (cons 'ISBN (check-isbn (read-sanitize "ISBN"))))
	 (location (cons 'location_id (read-generic db "location")))
	 (d1 (list ctype btype publisher year edition language isbn location))
	 (d2 (cons title (list-append authors d1)))
	 (d3 (filter (lambda (val) (cdr val)) d2))
	 (target (string-join-by ", " (map lam1 d3)))
	 (vstr (string-join-by ", " (map lam2 (fromto 1 (list-length d3)))))
	 (sstr (string-append
		"INSERT INTO books (" target ") VALUES (" vstr ")"))
	 (vals (map cdr d3)))
    (display sstr)
    (newline)
    (display vals)
    (newline)
    (let loop ()
      (let* ((a1 (read-line-interactive "Add entry? \\[Y/n]"))
	     (ans (if a1 (string-trim-left char-whitespace? a1) "")))
	(cond ((or (string=? ans "")
		   (char=? #\y (char-downcase (string-ref ans 0))))
	       (apply do-pbrc db sstr vals))
	      ((char=? #\n (char-downcase (string-ref ans 0)))
	       (write-string "Skip entry!\n"))
	      (else (write-string "huh? don't understand '" ans "'\n")
		    (loop)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((fvals (parse-command-line-flags
	       '("-a" strings "add an author")
	       '("-b" flag "add a book")
	       '("-B" strings "add a physical book type")
	       '("-C" strings "add a book content type")
	       '("-l" strings "add a location")
	       '("-L" strings "add a language")
	       '("-p" strings "add a publisher")
	       '("-v" flag "be verbose")
	       '("-I" string "create a new mostly empty database")
	       '("-h" flag "show a very brief help message, then exit")))
       (init-db (hash-table-ref fvals "-I" #f))
       (add-auth (hash-table-ref fvals "-a" ()))
       (add-book (hash-table-ref fvals "-b" #f))
       (add-btype (hash-table-ref fvals "-B" ()))
       (add-ctype (hash-table-ref fvals "-C" ()))
       (add-loc (hash-table-ref fvals "-l" ()))
       (add-lang (hash-table-ref fvals "-L" ()))
       (add-pub (hash-table-ref fvals "-p" ()))
       (verbose (hash-table-ref fvals "-v" #f))
       (do-mods (or (not (null? add-btype))
		    (not (null? add-ctype))
		    (not (null? add-loc))
		    (not (null? add-lang))
		    (not (null? add-pub))
		    (not (null? add-auth))
		    add-book)))
  (when (hash-table-ref fvals "-h" #f)
    (write-string
     command-name " {-a|-b|-B|-C|-l|-L|-p} arg to add a new entry\n")
    (exit 0))
  (when init-db
    (initialize-db init-db)
    (write-string "created new database '" init-db "'\n")
    (set! command-line-arguments (cons init-db command-line-arguments)))
  (let* ((db-file (if (null? command-line-arguments)
		      "pustakalaya.db"
		      (car command-line-arguments)))
	 (db (sqlite-open db-file (if do-mods 'read-write 'read-only))))
    (if do-mods
	(begin
	  (for-each (lambda (v) (add-generic db "book_type" v)) add-btype)
	  (for-each (lambda (v) (add-generic db "content_type" v)) add-ctype)
	  (for-each (lambda (v) (add-generic db "location" v)) add-loc)
	  (for-each (lambda (v) (add-generic db "language" v)) add-lang)
	  (for-each (lambda (v) (add-generic db "publisher" v)) add-pub)
	  (for-each (lambda (a) (add-author db a)) add-auth)
	  (when add-book
	    (let loop ()
	      (add-one-book db)
	      (let* ((m1 (read-line-interactive "Add more books? \\[Y/n]"))
		     (more (if m1 (string-trim-left char-whitespace? m1) "")))
		(cond ((string=? more "")
		       (loop))
		      ((char=? #\n (char-downcase (string-ref more 0)))
		       (write-string "g'bye!\n"))
		      ((char=? #\y (char-downcase (string-ref more 0)))
		       (loop))
		      (else (write-string "huh? don't understand '"
					  more "'\n")))))))
	(if verbose
	    (for-each (lambda (t) (show-table db t))
		      '("language" "book_type" "content_type"
			"publisher" "location" "author"))
	    (begin
	      (unless (null? add-btype) (show-table db "book_type"))
	      (unless (null? add-ctype) (show-table db "content_type"))
	      (unless (null? add-loc) (show-table db "location"))
	      (unless (null? add-lang) (show-table db "language"))
	      (unless (null? add-pub) (show-table db "publisher"))
	      (unless (null? add-auth) (show-table db "author")))))
    (sqlite-close db)))
