;;; -*- mode: scheme; -*-

;;; A small library manager to help me track my books
;;; Copyright 2025, Uwe Hollerbach <uhollerbach@gmail.com>
;;; License: 2clause BSD, see file 'LICENSE' for details

(load-library "hash.scm")
(load-library "arg-parse.scm")

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
		    >);
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

	;; add as many more as you like
	(for-each (lambda (a) (do-pbrc db ins-lang a))
		  '("English" "Mandarin" "Hindi" "Spanish" "Arabic"
		    "French" "Bengali" "Portuguese" "Russian" "Indonesian"
		    "Urdu" "German" "Japanese" "Marathi" "Vietnamese"
		    "Telugu" "Turkish" "Swahili" "Tagalog" "Tamil" "Korean"
		    "Thai" "Italian" "Gujarati" "Amharic" "Kannada"
		    "Esperanto" "Dutch" "Danish" "Catalan" "Polish"
		    "Czech" "Sanskrit" "Hebrew" "Latin" "Greek" "Aramaic"))

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

;;; Table names don't seem to be suitable for binding via
;;; statement-{prepare,bind,run}, so we have to glue strings together.
;;; That's slightly unsafe in general, but here the table names
;;; are a fixed set, so not too bad.

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

(define (add-one-book db)
  (let* ((l1 (read-line-interactive "line1"))
	 (l2 (read-line-interactive "line2"))
	 (l3 (read-line-interactive "line3")))
    (printf "current input is:\nline1: >>>%s<<<\nline2: >>>%s<<<\nline3: >>>%s<<<\n" l1 l2 l3)
    (let ((acc (read-line-interactive "accept? (Yes/edit/no)")))
      (printf "YEN is >>>%v<<<\n" acc))))

;;;	 (more (if m1 (string-trim-left char-whitespace? m1) "")))
;;;    (cond ((string=? more "")
;;;	   (loop))
;;;	  ((char=? #\n (char-downcase (string-ref more 0)))
;;;	   (write-string "g'bye!\n"))
;;;	  ((char=? #\y (char-downcase (string-ref more 0)))
;;;	   (loop))
;;;	  (else (write-string "huh? don't understand '"
;;;			      more "'\n")))))))

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
