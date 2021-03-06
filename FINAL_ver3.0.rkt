#lang racket
;Citation:
; our database of sentiword is taken from https://hlt-nlp.fbk.eu/technologies/sentiwords for sentiment database
; our database of dictionary is taken from https://raw.githubusercontent.com/eneko/data-repository/master/data/words.txt
; The topic model is taken from Philip's Assignment 8
; We get help from Yutaro on writing word cloud procedure. 

(require csc151)
(require plot)
(require data/gvector)
(require (prefix-in htdp: 2htdp/image))
(require bestfit)
(require (file "C:\\Users\\ma_ph\\Documents\\Grinnell\\151Final\\Projecttopics.rkt")) ;Please change this to appropriate location when running
(provide list->img)
(provide wordpos)
(provide scbhistogram)
(provide scblineplot)

;_____________________________________General_____________________________________


(define dictionarylink "C:\\Users\\ma_ph\\Documents\\Grinnell\\151Final\\dictionaryd.txt") ;Please change this to appropriate location when running
(define filenames "C:\\Users\\ma_ph\\Documents\\Grinnell\\151Final\\filenames.txt") ;Please change this to appropriate location when running
(define sentimentdata "C:\\Users\\ma_ph\\Documents\\Grinnell\\151Final\\SentiWords_1.1.txt") ;Please change this to appropriate location when running

;;; Procedure:
;;;   not-zero?
;;; Parameters:
;;;   val, a number
;;; Purpose:
;;;  Returns true if number is zero, false otherwise.
;;; Produces:
;;;   bool, a boolean
(define not-zero?
  (lambda (val)
    (not (zero? val))))


;;; Procedure:
;;;   consec-values
;;; Parameters:
;;;  vec, a vector
;;; Purpose:
;;;  Returns a list containing the elements of vec at indices (pos-1) and pos
;;; Produces:
;;;   lst, a list

(define consec-values
  (lambda (vec pos)
    (map
     (lambda (index)
       (* 1.0 (vector-ref vec index)))
     (list (- pos 1) pos))))

;;; Procedure:
;;;   binary-search
;;; Parameters:
;;;   vec, a vector to search
;;;   get-key, a procedure of one parameter that, given a data item,
;;;     returns the key of a data item
;;;   less-equal?, a binary predicate that tells us whether or not
;;;     one key is less than or equal to another
;;;   key, a key we're looking for
;;; Purpose:
;;;   Search vec for a value whose key matches key.
;;; Produces:
;;;   match, a number.
;;; Preconditions:
;;;   * The vector is "sorted".  That is,
;;;     (less-equal? (get-key (vector-ref vec i))
;;;                  (get-key (vector-ref vec (+ i 1))))
;;;     holds for all reasonable i.
;;;   * The get-key procedure can be applied to all values in the vector.
;;;   * The less-equal? procedure can be applied to all pairs of keys
;;;     in the vector (and to the supplied key).
;;;   * The less-equal? procedure is transitive.  That is, if
;;;     (less-equal? a b) and (less-equal? b c) then it must
;;;     be that (less-equal? a c).
;;;   * If two values are equal, then each may precede the other.
;;;   * Similarly, if two values may each precede the other, then
;;;     the two values are equal.
;;; Postconditions:
;;;   * If vector contains no element whose key matches key, match is -1.
;;;   * If vec contains an element whose key equals key, match is the
;;;     index of one such value.  That is, key is 
;;;       (get-key (vector-ref vec match))
(define binary-search
  (lambda (gvec less-equal? key)
    (let search-portion ([lower-bound 0]
                         [upper-bound (- (gvector-count gvec) 1)])
      (if (> lower-bound upper-bound)
          -1
          (let* ([point-of-division (quotient (+ lower-bound upper-bound) 2)]
                 [sep-elt-key (gvector-ref gvec point-of-division)])
            (cond
              [(and (less-equal? key sep-elt-key)
                    (less-equal? sep-elt-key key))
               (gvector-ref gvec point-of-division)]
              [(less-equal? key sep-elt-key)
               (search-portion lower-bound (- point-of-division 1))]
              [else
               (search-portion (+ point-of-division 1) upper-bound)]))))))

;;; Procedure:
;;;   file->paragraphs
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Extract all of the paragraphs from a file.
;;; Produces:
;;;   paragraphs, a list of strings
(define file->paragraphs
  (lambda (fname)
    (regexp-split #px"\n\n+"
                  (file->string fname))))



;______________________________________Cleaning Up_________________________________

;;; Procedure:
;;;   gvector-cons
;;; Parameters:
;;;   gv, a gvector
;;;   val, a scheme value
;;; Purpose:
;;;   append the value to gv
;;; Produces:
;;;   gvf, a gvector
(define gvector-cons
  (lambda (gv val)
    (gvector-add! gv val)
    gv))


;;; Procedure:
;;;   stringlist->hash
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   turn a list of strings to a hash table with 26 english characters as key which indicates the starting letter of the string. 
;;; Produces:
;;;   word-hash, a hashtable
(define stringlist->hash
  (lambda (lst)
    (let* ([word-hash (make-hash)]
           [dummy (for-each (section hash-set! word-hash <> (gvector)) (map integer->char (range 97 123)))])
      (for-each
       (lambda (str)
         (when (and (< 96 (char->integer (char-downcase (string-ref str 0)))) (> 123 (char->integer (char-downcase (string-ref str 0)))))
           (hash-set! word-hash (char-downcase (string-ref str 0)) (gvector-cons (hash-ref word-hash (string-ref (string-downcase str) 0)) str))))
       lst)
      word-hash)))


;;;; Identifier:
;;;;   dictionary
;;;; Type:
;;;;   strings
;;;; Content:
;;;;   Standard English Dictionary (containing all words)
(define dictionary (string-split (list-ref (file->paragraphs  dictionarylink) 0) "\n"))


;;;; Identifier:
;;;;   dictionary-hash
;;;; Type:
;;;;   hashtable
;;;; Content:
;;;;   Standard English Dictionary (containing all words) in hashtable form
(define dictionary-hash (stringlist->hash dictionary))


;;; Procedure:
;;;   best-replacement
;;; Parameters:
;;;   str, a string
;;;   wgvec, a gvector
;;; Purpose:
;;;   find the best replacement of the str
;;; Produces:
;;;   strbest, a string
(define best-replacement
  (lambda (str wgvec)
    (if (string? (binary-search (hash-ref dictionary-hash (char-downcase (string-ref str 0))) string-ci<=? str))
        str
        "")))


;;; Procedure:
;;;   hamming-distance
;;; Parameters:
;;;   str1, a string
;;;   str2, a string
;;; Purpose:
;;;   find the hamming distance between two string
;;; Produces:
;;;   distance, a number
(define hamming-distance
  (lambda (str1 str2)
    (let* ([s1 (list->vector (string->list str1))]
           [s2 (list->vector (string->list str2))]
           [l1 (vector-length s1)]
           [l2 (vector-length s2)])
      (let kernel ([counter 0]
                   [pos 0]
                   )
        (if (or (= pos l1) (= pos l2))
            (+ counter (- l1 pos) (- l2 pos))
            (if (equal? (vector-ref s1 pos) (vector-ref s2 pos))
                (kernel counter (+ 1 pos))
                (kernel (+ 1 counter) (+ 1 pos))))))))

;;; Procedure:
;;;   correct
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   correct the str
;;; Produces:
;;;   corrected, a string
(define correct
  (lambda (str)
    (let ([limit (quotient (string-length str) 2)])
      (let kernel ([charno 0]
                   [current (best-replacement str (hash-ref dictionary-hash (char-downcase (string-ref str 0))))])
        (if (or (> charno 1) (= (hamming-distance str current) 0))
            current
            (let ([replacement (best-replacement str (hash-ref dictionary-hash (list-ref most-freq-first-char charno)))])
              (if (< (hamming-distance str replacement) (hamming-distance str current))
                  (kernel (+ 1 charno) replacement)
                  (kernel (+ 1 charno) current))))))))


;;;; Identifier:
;;;;   most-freq-first-char
;;;; Type:
;;;;   list
;;;; Content:
;;;;   a list of most frequent first character
(define most-freq-first-char (list #\t #\a #\o #\s #\i #\w #\c #\b #\p #\h))


;;; Procedure:
;;;   correct-wordlist
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   correct a list of words
;;; Produces:
;;;   correctedlst, a list
(define correct-wordlist
  (lambda (lst)
    (map correct lst)))

;;; Procedure:
;;;   word?
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   check if str is a word
;;; Produces:
;;;   word?, a boolean
(define word?
  (lambda (str)
    (and
     (not (empty? (regexp-match* #px"[A-Za-z]+" str)))
     (equal? (car (regexp-match* #px"[A-Za-z]+" str)) str))))

;;; Procedure:
;;;   string-append-with-space
;;; Parameters:
;;;   str1, a string
;;;   str2, a string
;;; Purpose:
;;;   append two strings
;;; Produces:
;;;   strc, a string that is combination of str1 and str2
(define string-append-with-space
  (lambda (str1 str2)
    (string-append str1 " " str2)))

;;; Procedure:
;;;   cleanup-string (*final procedure)
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   clean up the str to be readable
;;; Produces:
;;;   strclean, a string
(define cleanup-string
  (lambda (str)
    (let ([strc (regexp-replace* #px"» " (regexp-replace* #px"\n" (regexp-replace* #px".\n" (string-downcase str) "") " ") "")])
      (if (or (number? (string->number strc)) (zero? (string-length strc)) (empty? (string-split strc)))
          (void)
          (let ([word-vec (make-vector (string-length strc))]
                [word-lst (regexp-match*  #px"[a-zA-Z]+|[,.;?\"]+|[\n]+" strc)])
            (if (empty? word-lst)
                ""
                (reduce string-append-with-space (map (lambda (str) (if (word? str) (correct str) str)) word-lst))))))))



;_______________________________Topic Modeling___________________________________

;;; Procedure:
;;;   letter
;;; Parameters:
;;;   n, a non-negative integer
;;; Purpose:
;;;   Convert a number to a letter (such as for the name of a topic)
;;; Produces:
;;;   let, a string
(define letter
  (lambda (n)
    (string (integer->char (+ (char->integer #\A) n)))))

;;; Procedure:
;;;   randomly-assign-topics
;;; Parameters:
;;;   words, a list of strings
;;;   n, an integer between 0 and 25 inclusive
;;; Purpose:
;;;   Takes a list of words and creates a hash table that associates each word with one of n different topics
;;; Produces:
;;;   hash, a hashtable
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   Numbers of key in hash = (length words)
;;;   each elements of words is randomly assigned to a letter
;;;   if n = 3, then each element can be assigned to A, B, or C


(define randomly-assign-topics
  (lambda (words n)
    (randomly-assign-topics-kernel words n (make-hash) 0)))

;;; Procedure:
;;;   randomly-assign-topics-kernel
;;; Parameters:
;;;   words, a list of strings
;;;   n, an integer between 0 and 25 inclusive
;;;   hash, a hashtable
;;;   i, an integer
;;; Purpose:
;;;   Takes a list of words and creates a hash table that associates each word with one of n different topics
;;; Produces:
;;;   hash2, a hashtable

(define randomly-assign-topics-kernel
  (lambda (words n hash i)
    (cond
      [(= i (vector-length words))
       hash]
      [else
       (hash-set! hash (vector-ref words i) (letter (random n)))
       (randomly-assign-topics-kernel words n hash (+ i 1))])))

;;; Procedure:
;;;   cleanup
;;; Parameters:
;;;   str, a nonempty string
;;; Purpose:
;;;   takes a string, extracts the words, removes any of the less interesting words, and converts everything to lowercase
;;; Produces:
;;;   text, a list of strings
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (length text) <= (string-length str)
;;;   all elements in text appear in str. 

;b
(define cleanup
  (lambda (str)
    (filter unnec? (regexp-match* #px"[a-z']+"  (string-downcase str)))))

(define unnec?
  (lambda (str)
    (not (contains? (vector "a" "an" "the" "of" "and" "that" "this" "those" "these" "yet" "he" "him" "his" "she" "her" "hers" "if" "its" "it" "could" "would" "as" "by" "get" "got" "yes" "no" "at" "on" "or" "in" "with" "without" "be" "was" "were" "then" "were" "however" "but" "oh" "if" "is" "are" "himself" "herself" "ll" "s" "my" "when" "why" "what" "had" "have" "has" "me" "did" "nor" "too" "one" "over" "why" "you" "for" "t" "to" "re" "so" "ve" "do" "am" "not" "some" "i" "up" "down" "which" "very" "their" "there" "as" "else" "m" "just" "let" "into" "four" "much" "still" "next" "rather" "bit" "ever" "first" "all" "upon" "xi" "p" "r" "pm" "ri" "o" "l" "e" "c" "out" "vii" "ii" "i" "iii" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "m" "n" "q" "r" "t" "u" "v" "w" "x" "y" "z") str))))



;c

;;;Extracted from Assignment 5:


;;; Procedure:
;;;   remove-duplicates
;;; Parameters:
;;;   list-of-words, a list of strings 
;;; Purpose:
;;;   removes all duplicates from a list of words
;;; Produces:
;;;   lst, a list 
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   lst should contain no duplicate words
;;;   (length lst) <= (length list-of-words)


(define remove-duplicates
  (lambda (words)
    (let ([l (vector-length words)])
      (let kernel ([pos 1]
                   [valid-indices (list 0)])
        (if (= 0 l)
            (vector)
            (if (= pos l)
                (list->vector (map (section vector-ref words <>) valid-indices))
                (if (equal? (vector-ref words pos) (vector-ref words (- pos 1)))
                    (kernel (+ 1 pos) valid-indices)
                    (kernel (+ 1 pos) (cons pos valid-indices)))))))))

;;; Procedure:
;;;   unique-words
;;; Parameters:
;;;   list-of-texts, a list of lists
;;; Purpose:
;;;   takes a list of texts and provides a list of all of the words in the list of texts, with each word appearing once. 
;;; Produces:
;;;   lst, a list 
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (length lst) <= (reduce + (map length list-of texts))

(define unique-words
  (lambda (list-of-texts)
    (remove-duplicates (list->vector (reduce append list-of-texts)))))

;d
;;; Procedure:
;;;   text-probs
;;; Parameters:
;;;   text, a list
;;;   topics, a hashtable
;;; Purpose:
;;;   takes as input a list of words and the type of hash table produced by randomly-assign-topics, and produces a hash table that correctly maps each topic name to the percentage of words in the list that fall into that topic. 
;;; Produces:
;;;   hash, a hashtable 
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (hash-has-key? hash [random value in topics]) should be true

(define text-probs
  (lambda (text topics)
    (text-probs-kernel text topics (make-hash) 0)))

;;; Procedure:
;;;   text-probs-kernel
;;; Parameters:
;;;   text, a list
;;;   topics, a hashtable
;;;   hash, a hashtable
;;; Purpose:
;;;   takes as input a list of words and the type of hash table produced by randomly-assign-topics, and produces a hash table that correctly maps each topic name to the percentage of words in the list that fall into that topic. 
;;; Produces:
;;;   hash2, a hashtable

(define text-probs-kernel
  (lambda (text topics hash i)
    (if (= i (vector-length text))
        hash
        (cond
          [(hash-has-key? topics (vector-ref text i))
           (if (hash-has-key? hash (hash-ref topics (vector-ref text i)))
               (hash-set! hash (hash-ref topics (vector-ref text i)) (+ (/ 1 (vector-length text)) (hash-ref hash (hash-ref topics (vector-ref text i)))))
               (hash-set! hash (hash-ref topics (vector-ref text i)) (/ 1 (vector-length text))))
           (text-probs-kernel text topics hash (+ i 1))]
          [else
           (text-probs-kernel text topics hash (+ i 1))]))))
;e

;;; Procedure:
;;;   word-appearances-helper
;;; Parameters:
;;;   word, a string
;;;   texts, a list of lists of strings
;;; Purpose:
;;;   takes a word and a list of texts and produces list of the number of times the word appears in each text
;;; Produces:
;;;   nums, a list of integer


(define word-appearances-helper
  (lambda (word lst)
    (letrec ([kernel (lambda (count-so-far remaining)
                       (if (null? remaining)
                           count-so-far
                           (if (equal? word (car remaining))
                               (kernel (+ 1 count-so-far) (cdr remaining))
                               (kernel count-so-far (cdr remaining))
                               )))])
      (kernel 0 lst))))

;;; Procedure:
;;;   word-appearances
;;; Parameters:
;;;   word, a string
;;;   texts, a list of lists of strings
;;; Purpose:
;;;   takes a word and a list of texts and produces list of the number of times the word appears in each text
;;; Produces:
;;;   nums, a list of integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (length nums) = (length texts)

(define word-appearances
  (lambda (word texts)
    (letrec ([kernel (lambda (so-far remaining)
                       (if (null? remaining)
                           so-far
                           (kernel (append so-far (list (word-appearances-helper word (car remaining)))) (cdr remaining))))])
      (kernel null texts))))

;;; Procedure:
;;;   word-percentages
;;; Parameters:
;;;   word, a string
;;;   texts, a list of lists of strings
;;; Purpose:
;;;   returns the fraction of the total number of appearances of the word in each text
;;; Produces:
;;;   lst-of-percentages, a list of numbers
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (length nums) = (length texts)

(define word-percentages
  (lambda (word texts)
    (let ([list-of-appearances (word-appearances word texts)])
      (letrec ([kernal (lambda (i lst-of-percentages)
                         (if (= i (length texts))
                             lst-of-percentages
                             (kernal (+ i 1) (append lst-of-percentages (list (/ (list-ref list-of-appearances i) (reduce + list-of-appearances)))))))])
        (kernal 0 null)))))

(define tw-percentage
  (lambda (word texts)
    (let ([list-of-appearances (word-appearances word texts)])
      (letrec ([kernal (lambda (i lst-of-percentages)
                         (if (= i (length texts))
                             lst-of-percentages
                             (kernal (+ i 1) (append lst-of-percentages (list (/ (list-ref list-of-appearances i) (length (list-ref texts i))))))))])
        (kernal 0 null)))))
;f
;;; Procedure:
;;;   word-probs-helper
;;; Parameters:
;;;   word-percent, a number
;;;   list-of-probs, a list of values return by text-probs
;;; Purpose:
;;;   generate a probability that a particular word belongs in one topic
;;; Produces:
;;;   num, a number

(define word-probs-helper
  (lambda (word-percents list-of-probs topic-letter)
    (letrec ([kernal (lambda (i so-far)
                       (if (equal? i (length list-of-probs))
                           so-far
                           (if (hash-has-key? (list-ref list-of-probs i) topic-letter)
                               (kernal (+ 1 i) (+ (* (hash-ref (list-ref list-of-probs i) topic-letter) (list-ref word-percents i)) so-far))
                               (kernal (+ 1 i) so-far))))])
      (kernal 0 0))))

(define word-probs-helper2
  (lambda (word-percents list-of-probs topic-letters)
    (letrec ([kernal (lambda (j so-far)
                       (if (= j (vector-length topic-letters))
                           so-far
                           (cond
                             [(= j 0)
                              (kernal (+ j 1) (append (append (list (vector-ref topic-letters j)) (list (word-probs-helper word-percents list-of-probs (vector-ref topic-letters j)))) so-far))]
                             [(< j 2)
                              (kernal (+ j 1) (list (append (list (vector-ref topic-letters j)) (list (word-probs-helper word-percents list-of-probs (vector-ref topic-letters j)))) so-far))]
                             [else
                              (kernal (+ j 1) (cons (append (list (vector-ref topic-letters j)) (list (word-probs-helper word-percents list-of-probs (vector-ref topic-letters j)))) so-far))])))])
      (kernal 0 null))))

;;; Procedure:
;;;   word-probs
;;; Parameters:
;;;   word-percents, a list of numbers
;;;   list-of-probs, a list of values return by text-probs
;;;   topic-letters, a list of string
;;; Purpose:
;;;   generate a list of probabilities that a particular word belongs in each of the different topics
;;; Produces:
;;;   lst, a list of lists
;;; Preconditions:
;;;   word-percents is generated by (word-percentages)
;;;   (length word-percents) = (length list-of-text-probs)
;;; Postconditions:
;;;   (length lst) = (length word-percents)
;;;   for i, (reduce + (list-ref (list-ref lst i) 1)) = 1

(define word-probs
  (lambda (word-percents list-of-probs topic-letters)
    (word-probs-helper2 word-percents list-of-probs topic-letters)))
                                   
;g

;;; Procedure:
;;;   biased-select
;;; Parameters:
;;;   lst, a non-empty list of value/probability lists
;;; Purpose:
;;;   Select one of the elements in the list, choosing
;;;   the element according to probability.  (This is
;;;   called "biased selection" in the literature.)
;;; Produces:
;;;   value, a value
;;; Preconditions:
;;;   * Each element of lst has the form (val prob).
;;;   * Each probability is a real number.
;;;     That is (all (o real? cadr) lst)
;;;   * Each probability is between 0 and 1, inclusive.
;;;   * The sum of all the probabilities is 1.
;;;     That is, (reduce + (map cadr lst)) = 1.
;;; Postconditions:
;;;   * value is one of the values in the list.  That is
;;;     (member? value (map car lst)).
;;;   * It is difficult to predict which value we get.
;;;   * Suppose the list is of the form ((val1 prob1)
;;;     (val2 prob2) ... (valn probn)).  Over a long
;;;     series of calls, we'll see val1 about prob1
;;;     of the time, val2 about prob2 of the time, and so
;;;     on and so forth.
(define biased-select
  (lambda (lst)
    (let kernel ([r (random)]
                 [remaining lst])
      (let* ([entry (car remaining)]
             [value (car entry)]
             [prob (cadr entry)])
        (cond
          [(null? (cdr remaining))
           value]
          [(< r prob)
           value]
          [else
           (kernel (- r prob)
                   (cdr remaining))])))))


;;; Procedure:
;;;   select-topic
;;; Parameters:
;;;   word-percents, a list of numbers
;;;   list-of-text-probs, a list of values return by text-probs
;;;   topic-letters, a list of string
;;; Purpose:
;;;   probabilistically selects a topic for a word given the word percentages and the list of text probabilities
;;; Produces:
;;;   str, a string representing a topic
;;; Preconditions:
;;;   word-percents is generated by (word-percentages)
;;;   (length word-percents) = (length list-of-text-probs)
;;; Postconditions:
;;;   str is an element of topic-letters

(define select-topics
  (lambda (word-percents list-of-probs topic-letters)
    (biased-select (word-probs word-percents list-of-probs topic-letters))))
    
;;; Procedure:
;;;   update-probs!
;;; Parameters:
;;;   text, a list of strings
;;;   probs, a hashtable
;;;   word, a string
;;;   oldtopic, a string
;;;   newtopic, a string
;;; Purpose:
;;;   updates the probabilities for a text based on the change of a word from an old topic to a new topic
;;; Produces:
;;;   [no additional]
;;; Preconditions:
;;;   both oldtopic and newtopic are keys of probs
;;;   word is an element of text
;;; Postconditions:
;;;   (hash-ref probs newtopic) increases
;;;   (hash-ref probs oldtopic) decreases

(define update-probs!
  (lambda (text probs word oldtopic newtopic)
    (hash-set! probs oldtopic (- (hash-ref probs oldtopic) (car (tw-percentage word (list text)))))
    (if (hash-has-key? probs newtopic)
        (hash-set! probs newtopic (+ (hash-ref probs newtopic) (car (tw-percentage word (list text)))))
        (hash-set! probs newtopic (car (tw-percentage word (list text)))))))

;;; Procedure:
;;;   random-elt
;;; Parameters:
;;;   lst, a non-empty list 
;;; Purpose:
;;;   Unpredictably pick an element of lst.
;;; Produces:
;;;   val, a value
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   * val is an element of lst.
;;;   * If lst contains more than one element, it is difficult to predict 
;;;     which element val is.
(define random-elt
  (lambda (vec)
    (vector-ref vec (random (vector-length vec)))))


;;; Procedure:
;;;   improve-model!
;;; Parameters:
;;;   topics, a hashtable
;;;   list-of-texts, a list of strings
;;;   list-of-probs, a hashtable
;;; Purpose:
;;;   randomly selects a word and reassigns it to a new topic and updates the list of probabilities
;;; Produces:
;;;   [no additional]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (hash-ref probs newtopic) increases
;;;   (hash-ref probs oldtopic) decreases

(define improve-model!
  (lambda (topics list-of-texts list-of-probs)
    (let* ([word (random-elt (list->vector (reduce append list-of-texts)))]
           [oldtopic (hash-ref topics word)]
           [newtopic (select-topics (word-percentages word list-of-texts) list-of-probs (remove-duplicates (list->vector (filter string? (reduce append (map append (map list (map car (reduce append (map hash->list list-of-probs)))) (map list (map cdr (reduce append (map hash->list list-of-probs))))))))))])
      (hash-set! topics word newtopic)
      (letrec ([kernal (lambda (i)
                         (if (= i (length list-of-texts))
                             (void)
                             (cond
                               [(contains? (list->vector (list-ref list-of-texts i)) word)
                                (update-probs!
                                 (list-ref list-of-texts i)
                                 (list-ref list-of-probs i)
                                 word
                                 oldtopic
                                 newtopic)
                                (kernal (+ i 1))]
                               [else
                                (kernal (+ i 1))])))])
        (kernal 0)))))


;;; Procedure:
;;;   contains?
;;; Parameters:
;;;   lst, a list
;;;   val, a scheme value
;;; Purpose:
;;;   check if val appears in lst
;;; Produces:
;;;   b, a boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   return #t if val appears in lst
;;;   return #f otherwise

(define contains?
  (lambda (vec val)
    (let ([l (vector-length vec)])
      (let kernel ([pos 0])
        (if (= l pos)
            #f
            (if (equal? val (vector-ref vec pos))
                #t
                (kernel (+ pos 1))))))))
                 
        
 



;;; Procedure:
;;;   display-topics
;;; Parameters:
;;;   topics, a hashtable
;;; Purpose:
;;;   put topics table in a human-readable form
;;; Produces:
;;;   lst, a list of lists
;;; Preconditions:
;;;   topics should be produced by (randomly-assign-topics)
;;; Postconditions:
;;;   [no additional]

(define display-topics
  (lambda (topics)
    (display (display-topics-helper2 topics))))

;;; Procedure:
;;;   display-topics-helper2
;;; Parameters:
;;;   topics, a hashtable
;;; Purpose:
;;;   put topics table in a human-readable form
;;; Produces:
;;;   lst, a list of lists

(define display-topics-helper2
  (lambda (topics)
    (let ([cato (remove-duplicates (hash-values topics))])
      (letrec ([kernal (lambda (i so-far)
                         (if (= i (length cato))
                             so-far
                             (kernal (+ i 1) (append so-far (display-topics-helper topics (list-ref cato i))))))])
        (kernal 0 null)))))

;;; Procedure:
;;;   display-topics-helper
;;; Parameters:
;;;   topics, a hashtable
;;; Purpose:
;;;   recurse for each cato
;;; Produces:
;;;   lst, a list

(define display-topics-helper
  (lambda (topics cato)
    (let ([topiclst (hash->list topics)])
      (letrec ([kernal (lambda (j so-far)
                         (if (= j (length topiclst))
                             so-far
                             (if (equal? cato (cdr (list-ref topiclst j)))
                                 (kernal (+ j 1) (append so-far (list (car (list-ref topiclst j)))))
                                 (kernal (+ j 1) so-far))))])
        (kernal 0 (list (string-append "\n" cato ":" )))))))


;;; Procedure:
;;;   topic-model
;;; Parameters:
;;;   list-of-strings, a list
;;;   num-topics, a number
;;;   num-iterations, a number
;;; Purpose:
;;;   builds and returns a topic model (hash table) for the list of strings
;;; Produces:
;;;   model, a hashtable
;;; Preconditions:
;;;   topics should be produced by (randomly-assign-topics)
;;; Postconditions:
;;;   [no additional]

(define topic-model
  (lambda (list-of-strings num-topics num-iterations)
    (let* ([list-of-texts (map cleanup list-of-strings)]
           [uniques (unique-words list-of-texts)]
           [topics (randomly-assign-topics uniques num-topics)]
           [hash-of-probs (map (section text-probs <> topics) (map (o remove-duplicates list->vector) list-of-texts))])
      (letrec ([kernal (lambda (i)
                         (cond
                           [(= i num-iterations)
                            topics]
                           [else
                            (improve-model! topics (map (o vector->list remove-duplicates list->vector) list-of-texts) hash-of-probs)
                            (kernal (+ 1 i))]))])
        (kernal 0)))))


;___________________________Extracting S&B Data__________________________________________


;;; Procedure:
;;;   scbfiles-yearlink
;;; Parameters:
;;;   year, an integer
;;; Purpose:
;;;   Find all the links of the newspaper in a given year
;;; Produces:
;;;   linklst, a list
(define scbfiles-yearlink
  (lambda (year)
    (let ([filelst (string-split (list-ref (file->paragraphs filenames) 0) "\n")])
      (let kernel ([pos 0]
                   [so-far null])
        (if (= pos (length filelst))
            so-far
            (if (regexp-match? (number->string year) (list-ref filelst pos))
                (kernel (+ 1 pos) (append so-far (list (list-ref filelst pos))))
                (kernel (+ 1 pos) so-far)))))))


;;; Procedure:
;;;   filenames-in-month
;;; Parameters:
;;;   year, an integer
;;;   month, an integer
;;; Purpose:
;;;   get the file link in the given month of given year
;;; Produces:
;;;   linklst, a list

(define filenames-in-month
  (lambda (year month)
    (regexp-match* (pregexp (string-append "usiagrc_scb_" (number->string year) "_" (number->string month) "[^\n]+txt")) (file->string filenames))))

;;; Procedure:
;;;   scbfiles-year
;;; Parameters:
;;;   year, an integer
;;; Purpose:
;;;   Find all the data of scb files in a given year
;;; Produces:
;;;   datalst, a list
(define scbfiles-year
  (lambda (year)
    (map file->paragraphs (map (section string-append "/home/rebelsky/public_html/SandB/" <>) (scbfiles-yearlink year)))))


;;; Procedure:
;;;   pad-month-wzero
;;; Parameters:
;;;   month, an integer
;;; Purpose:
;;;   Return a two-character string containing the month by apppropriately adding zeros ("08" "11").
;;; Produces:
;;;   str, a string

(define pad-month-wzero
  (lambda (month)
    (if (= (string-length (number->string month)) 1)
        (string-append "0" (number->string month))
        (number->string month))))

;;; Procedure:
;;;   filenames-in-year-month
;;; Parameters:
;;;   year, an integer
;;;   month, an integer
;;; Purpose:
;;;   Find all the filenames in a particular year and month
;;; Produces:
;;;   datalst, a list of strings

(define filenames-in-year-month
  (lambda (year month)
    (regexp-match* (pregexp (string-append "usiagrc_scb_" (number->string year) "_" (pad-month-wzero month) "[^\n]+txt")) (file->string filenames))))

;;; Procedure:
;;;   files-in-year-month
;;; Parameters:
;;;   year, an integer
;;;   month, an integer
;;; Purpose:
;;;   Find all the data of scb files in a given year and month
;;; Produces:
;;;   datalst, a list of strings

(define files-in-year-month
  (lambda (year month)
    (map file->string (map (section string-append "/home/rebelsky/public_html/SandB/" <>) (filenames-in-year-month year month)))))


;_______________________________Analyzing Sentiments__________________________

;;; Identifier:
;;;   sentiword
;;; Type:
;;;   vector
;;; Content:
;;;   Sentiment database, provided by HLP
(define sentiword
  (list->vector (filter (lambda (str) (not (regexp-match? #px"_" str))) (drop (string-split (list-ref (file->paragraphs sentimentdata) 0) "\n") 449))))



;;; Procedure:
;;;   binary-search-sentiment
;;; Parameters:
;;;   vec, a vector to search
;;;   key, a key we're looking for
;;; Purpose:
;;;   Search vec for a value whose key matches key.
;;; Produces:
;;;   match, a number or a list of numbers
(define binary-search-sentiment
  (lambda (vec key)
    (let ([get-key (lambda (str) (first (string-split str "#")))]
          [less-equal? string-ci<=?])
      (let search-portion ([lower-bound 0]
                           [upper-bound (- (vector-length vec) 1)])
        (if (> lower-bound upper-bound)
            -1
            (let* ([point-of-division (quotient (+ lower-bound upper-bound) 2)]
                   [separating-element (vector-ref vec point-of-division)]
                   [sep-elt-key (get-key separating-element)])
              (cond
                [(and (less-equal? key sep-elt-key)
                      (less-equal? sep-elt-key key))
                 (cond
                   [(or (zero? point-of-division) (= point-of-division (- (vector-length vec) 1)))
                    point-of-division]
                   [else
                    (define so-far (list point-of-division))
                    (let ([off 0])
                      (letrec ([kernel (lambda (i so-far off)
                                         (if (not (equal? key (get-key (vector-ref vec (+ i 1)))))
                                             so-far
                                             (kernel (+ i 1) (append so-far (list (+ i 1))) (+ 1 off))))]
                               [kernel2 (lambda (i so-far)
                                          (if (not (equal? key (get-key (vector-ref vec (- i 1)))))
                                              so-far
                                              (kernel2 (- i 1) (append so-far (list (- i 1))))))])
                        (kernel point-of-division so-far 0)
                        (kernel2 (- point-of-division off) so-far)))])]
                                         
                [(less-equal? key sep-elt-key)
                 (search-portion lower-bound (- point-of-division 1))]
                [else
                 (search-portion (+ point-of-division 1) upper-bound)])))))))

;;; Procedure:
;;;   get-score
;;; Parameters:
;;;   input, position of the word 
;;; Purpose:
;;;   find the score in that position
;;; Produces:
;;;   score, a number
(define get-score
  (lambda (input)
    (if (and (not (list? input)) (= input -1))
        0
        (let ([kernel (lambda (int)
                        (string->number (last (string-split (vector-ref sentiword int) "\t"))))])
          (if (number? input)
              (kernel input)
              (/ (reduce + (map get-score input)) (length input)))))))

;;; Procedure:
;;;   get-total
;;; Parameters:
;;;   topic, a topic
;;;   topics, a hashtable of topics generated by topic-model
;;; Purpose:
;;;   Find all the scores for the word in topic from topics and add them all together. 
;;; Produces:
;;;   total, a number

(define get-total
  (lambda (topic topics)
    (if (empty? (display-topics-helper topics topic))
        0
        (reduce + (map get-score (map (section binary-search-sentiment sentiword <>) (cdr (display-topics-helper topics topic))))))))

;;; Procedure:
;;;   cleanup-void
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   filter out all the void in a list
;;; Produces:
;;;   lstwovoid, a list
(define cleanup-void
  (lambda (lst)
    (filter (lambda (str) (not (void? str))) lst)))

;;; Procedure:
;;;   score-over-years
;;; Parameters:
;;;   word, a string
;;; Purpose:
;;;   produce a list of scores representing the topic in each year from 1961-1970 in scb articles
;;; Produces:
;;;   scorelst, a list
(define score-over-years
  (lambda (word)
    (let kernel ([year 1961]
                 [so-far null])
      (if (= year 1971)
          so-far
          (let* ([topics (topic-model (cleanup-void (map cleanup-string (list-ref (scbfiles-year year) 0))) 10 1000)]
                 [topic? (hash-has-key? topics word)])
            (if topic?
                (kernel (+ 1 year) (append so-far (list (get-total (hash-ref topics word) topics))))
                (kernel (+ 1 year) (append so-far (list 0)))))))))

(define score-over-years2
  (lambda (word year1 year2)
    (let kernel ([year year1]
                 [month 1]
                 [so-far null])
      (if (= year year2)
          so-far
          (let* ([topics (vector-ref (list-ref topic-modeled (- year 1961)) (- month 1))]
                 [topic? (hash-has-key? topics word)])
            (if (= month 12)
                (if topic?
                    (kernel (+ 1 year) 1 (append so-far (list (get-total (hash-ref topics word) topics))))
                    (kernel (+ 1 year) 1 (append so-far (list 0))))
                (if topic?
                    (kernel year (+ 1 month) (append so-far (list (get-total (hash-ref topics word) topics))))
                    (kernel year (+ 1 month) (append so-far (list 0))))))))))

;__________________________________Graphing_________________________________________________


;;; Procedure:
;;;   make-xs
;;; Parameters:
;;;   year1, an integer
;;;   year2, an integer
;;; Purpose:
;;;   Make a list of float values starting from year1 and going up to year2 with a step of 1/12
;;; Produces:
;;;   numlst, a list

(define make-xs
  (lambda (year1 year2)
    (reduce append
            (map
             (lambda (year)
               (cdr (range year (+ 1 year) (/ 1.0 12))))
             (range year1 year2)))))

(define make-xs2
  (lambda (year1 year2)
    (reduce append
            (map
             (lambda (year)
               (let ([nonzero (length (filter (section notemptymonth? year <>) (range 1 13)))])
                 (cdr (range year (+ 1 year) (/ 1.0 nonzero)))))
             (range year1 year2)))))


;;; Procedure:
;;;   yearly-scores
;;; Parameters:
;;;   scores, a list of numbers
;;; Purpose:
;;;  Finds the average of the non-zero sentiment scores from scores which contains data an whole integer number of years.
;;; Produces:
;;;   datalst, a list of numbers

(define yearly-scores
  (lambda (scores)
    (let kernel ([so-far null]
                 [remscores scores])
      (if (null? remscores)
          so-far
          (kernel
           (cons (/ (reduce + (take remscores 12)) 12) so-far)
           (drop remscores 12))))))


;;; Procedure:
;;;   months
;;; Parameters:
;;;   year, an integer
;;; Purpose:
;;;  Creates a list of strings of the form "<YEAR>-<MONTH#>"
;;; Produces:
;;;   months, a list of strings

(define months
  (lambda (year)
    (map (section string-append "'" (substring(number->string year) 2) <>) (list "-01" "-02" "-03" "-04 " "-05" "-06" "-07" "-08" "-09" "-10" "-11" "-12"))))

;;; Procedure:
;;;   histogram
;;; Parameters:
;;;  xs, a list of numbers
;;;  ys, a list of numbers
;;;  type, an integer 
;;;  size, a number
;;; Purpose:
;;;   Creates a histogram with xs as the x-values and ys as the y-values
;;; Produces:
;;;   hist, a plot
;;; Preconditions:
;;;   * type is 1 if the user requires a histogram by month, otherwise it is 2 (histogram by year).
;;;   * size is a number indicating the number of x values and decides the scale for x-axis
;;; Postconditions:
;;;   [no additional]

(define histogram
  (lambda (xs ys type size)
    (parameterize ([plot-width  (floor (* 650 size))]
                   [plot-height   500]
                   [plot-tick-size 10])
      (let ([label (if (= type 1) "Month" "Year")])
        (plot (discrete-histogram (map (section list <> <>) xs ys) #:y-min -2)                           
              #:title (string-append "Topic scores by " label)
              #:x-label label
              #:y-label "Sentiment Score")))))

;;; Procedure:
;;;   scbhistogram
;;; Parameters:
;;;  word, a string
;;;  year1, an integer
;;;  year2, an integer
;;;  type, an integer
;;; Purpose:
;;;   Creates a histogram showing sentiment score of language associated with word used in S&B from year1 to year2 (both included). 
;;; Produces:
;;;   hist, a plot
;;; Preconditions:
;;;   * type is 1 if the user requires a histogram by month, otherwise it is 2 (histogram by year).
;;;   * year2 >= year1
;;; Postconditions:
;;;   [no additional]

(define scbhistogram
  (lambda (word year1 year2 type)
    (let ([scores (score-over-years2 word year1 year2)])
      (if (= type 1)
          (histogram (reduce append (map months (range year1 year2))) scores 1 (/ (* 12 (+ 1 (- year2 year1))) 15)) 
          (histogram (range year1 year2) (yearly-scores scores) 2 (/ (+ 1 (- year2 year1)) 10))))))



;;; Procedure:
;;;   make-function
;;; Parameters:
;;;   xs, a vector of numbers 
;;;   ys, a vector of numbers
;;;   x, a number
;;; Purpose:
;;;  Returns a corresponding y value for x by through linearization
;;; Produces:
;;;   y, a number
;;; Preconditions:
;;;  * xs is sorted in increasing order
;;;  * (vector-length xs) = (vector-length ys)
;;;  Postconditions:
;;;   * If xs = {x_1,x_2,...,x_n} such that x_i < x <= x_{i+1}, then y = y_i + (x-x_i)(y_{i+1}-y_i)/(x_{i+1}-x_i).

(define make-function
  (lambda (xs ys x)
    (let kernel ([pos 0])
      (if (> pos (vector-length xs))
          #f
          (if (>= (vector-ref xs pos) x)
              ((linear-fit (consec-values xs pos) (consec-values ys pos)) x)
              (kernel (+ 1 pos)))))))


;;; Procedure:
;;;   line-plot
;;; Parameters:
;;;   xs, a vector of numbers 
;;;   ys, a vector of numbers
;;;   x1, a number
;;;   x2, a number
;;; Purpose:
;;;  Generates a line-plot with xs as x-values, ys as y-values and x ranging from x1 to x2.
;;; Produces:
;;;   lineplot, a plot


(define line-plot
  (lambda (xs ys x1 x2)
    (parameterize ([plot-width  1000]
                   [plot-height   1000])
      (plot (function (section make-function xs ys <>) x1 x2 #:label "Sentiment Score" #:y-min -2)
             #:x-label "Years"
              #:y-label "Sentiment Score"))))


;;; Procedure:
;;;   scblineplot
;;; Parameters:
;;;   word, a string
;;;   year1, an integer
;;;   year2,  an integer
;;; Purpose:
;;;  Creates a line plot with sentiment score of language used associated with word in S&B from year1 to year2 (both included). 
;;; Produces:
;;;   lineplot, a plot
;;; Preconditions:
;;;  * year2 >= year1
;;;  Postconditions:
;;;   [No additional]


(define scblineplot
  (lambda (word year1 year2)
    (let ([scores (score-over-years2 word year1 year2)])
      (line-plot (list->vector (make-xs2 year1 year2)) (list->vector (filter not-zero? scores)) year1 (- year2 (/ 1 12))))))
                               

;__________________Word Cloud___________________________

;;; Procedure:
;;;   get-words
;;; Parameters:
;;;   topics, a hashtable
;;;   topic, a string
;;; Purpose:
;;;   get all the words related to the topic
;;; Produces:
;;;   wordlst, a list
(define get-words
  (lambda (topics topic)
    (cdr (display-topics-helper topics (hash-ref topics topic)))))


;;; Procedure:
;;;   topics-year-hash
;;; Parameters:
;;;   word, a string
;;; Purpose:
;;;   creates a hashtable with year as a key and list of all the words related to word in that year as value
;;; Produces:
;;;   wordhash, a hashtable
(define topics-year-hash
  (lambda (word year1 year2)
    (let kernel ([year year1]
                 [month 1]
                 [so-far (make-hash)])
      (if (= year year2)
          so-far
          (let* ([topics (vector-ref (list-ref topic-modeled (- year 1961)) (- month 1))]
                 [topic? (hash-has-key? topics word)])
            (cond
              [topic?
               (hash-set! so-far year (append (if (hash-has-key? so-far year) (hash-ref so-far year) null) (get-words topics word)))
               (if (= month 12)
                   (kernel (+ year 1) 1 so-far)
                   (kernel year (+ month 1) so-far))
               ]
              [else
               (if (= month 12)
                   (kernel (+ year 1) 1 so-far)
                   (kernel year (+ month 1) so-far))]))))))
                

;;; Procedure:
;;;   word-cloud-single-freq
;;; Parameters:
;;;   wordhash, a hashtable
;;;   wordex, word we want 
;;; Purpose:
;;;   find the number of times wordex appear in wordhash
;;; Produces:
;;;   freq, a number
(define word-cloud-single-freq
  (lambda (wordhash wordex year1 year2)
    (let kernel ([year year1]
                 [freq 0])
      (if (= year year2)
          freq
          (cond
            [(not (hash-has-key? wordhash year))
             (kernel (+ 1 year) freq)]
            [(contains? (list->vector (hash-ref wordhash year)) wordex)
             (hash-set! wordhash year (remove wordex (hash-ref wordhash year)))
             (kernel (+ 1 year) (+ 1 freq))]
            [else
             (kernel (+ 1 year) freq)])))))


;;; Procedure:
;;;   hash-clear!
;;; Parameters:
;;;   hash, a hashtable
;;; Purpose:
;;;   clear all unnecessary keys
;;; Produces:
;;;   hashcleared, a hashtable
(define hash-clear
  (lambda (hash)
    (let* ([keys (hash-keys hash)]
           [unnec (filter (lambda (x) (not (unnec? x))) keys)])
      (letrec ([kernel (lambda (i hash)
                         (if (= i (length unnec))
                             hash
                             (kernel (+ i 1) (hash-remove hash (list-ref unnec i)))))])
        (kernel 0 hash)))))


;;; Procedure:
;;;   word-cloud-allfreq
;;; Parameters:
;;;   word, a string
;;; Purpose:
;;;   Find the frequency of each word associated with word*
;;; Produces:
;;;   freqlst, a list
(define word-cloud-allfreq
  (lambda (word year1 year2)
    (let ([wordhash (hash-clear (topics-year-hash word year1 year2))])
      (let reyear ([year year1]
                   [freqlst null])
        (cond
          [(= year year2)
           freqlst]
          [(not (hash-has-key? wordhash year))
           (reyear (+ 1 year) freqlst)]
          [else
           (reyear (+ 1 year)
                   (append freqlst
                           (let reword ([pos 0]
                                        [freqlstperyear (make-hash)])
                             (cond
                               [(>= pos (length (hash-ref wordhash year)))
                                (hash->list freqlstperyear)]
                               [else
                                (hash-set! freqlstperyear (list-ref (hash-ref wordhash year) pos) (word-cloud-single-freq wordhash (list-ref (hash-ref wordhash year) pos) year1 year2))
                                (reword (+ 1 pos) freqlstperyear)]))))])))))


;;; Procedure:
;;;   word-cloud-single
;;; Parameters:
;;;   word, a string
;;;   sentiment, a number
;;;   freq, an int
;;; Purpose:
;;;   Create an artistic word of the word* (color represents sentiment and size represents frequency)
;;; Produces:
;;;   image


(define word-cloud-single
  (lambda (word sentiment freq)
    (htdp:text word (* 10 freq) (htdp:make-color (- 155 (inexact->exact (ceiling (* 100 sentiment)))) (+ 155 (inexact->exact (ceiling (* 100 sentiment)))) (random 255) 255))))

;;; Procedure:
;;;   pair-list->hash
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   turn a list of pairs to a hashtable
;;; Produces:
;;;   hash, a hashtable

(define pair-list->hash
  (lambda (lst)
    (make-hash (map (lambda (x) (cons (car x) (cdr x))) lst)))) 
  
;;; Procedure:
;;;   n-most-polarized
;;; Parameters:
;;;   lst, a list
;;;   n, a number
;;; Purpose:
;;;   find the first n elements in the lst that is the most polarized
;;; Produces:
;;;   lst-most-polarized, a list

(define n-most-polarized
  (lambda (lst n)
    (let ([abs>=? (lambda (x y) (>= (abs (cdr x)) (abs (cdr y))))])
      (take (sort lst abs>=?) n))))

;;; Procedure:
;;;   word-cloud
;;; Parameters:
;;;   word, a string
;;; Purpose:
;;;   Create a word cloud of all the word associated with word* (color represents sentiment (darker is more negative) and size represents frequency)
;;; Produces:
;;;   images
(define word-cloud
  (lambda (word year1 year2)
    (let* ([word-freq (word-cloud-allfreq word year1 year2)]
           [word-sent (n-most-polarized (map cons (map car word-freq) (map get-score (map (section binary-search-sentiment sentiword <>) (map car word-freq)))) 100)])
      (map append (map list (map word-cloud-single (map car (remove-duplicates-pair-list word-sent)) (map cdr (remove-duplicates-pair-list word-sent)) (map (section hash-ref (pair-list->hash (remove-duplicates-pair-list word-freq)) <>) (map car (remove-duplicates-pair-list word-sent))))) (map list (map (section hash-ref (pair-list->hash (remove-duplicates-pair-list word-freq)) <>) (map car (remove-duplicates-pair-list word-sent))))))))

;;; Procedure:
;;;   notemptymonth?
;;; Parameters:
;;;   year, a number
;;;   month, a number
;;; Purpose:
;;;   check in the newspaper database to see if there's any article in the given month and year
;;; Produces:
;;;   notempty, a boolean

(define notemptymonth?
  (lambda (year month)
    (not (empty? (files-in-year-month year month)))))

;;; Procedure:
;;;   xy
;;; Citation: Thank you Yutaro
;;; Parameters:
;;;   img, an image
;;;   x, a number
;;;   y, a number
;;; Purpose:
;;;   put img in given x,y coordinates
;;; Produces:
;;;   imgxy, an image

(define (xy img x y)
  (htdp:beside (htdp:rectangle x 0 'solid "red")
          (htdp:above (htdp:rectangle 0 y 'solid "red")
                 img)))

;;; Procedure:
;;;   remove-duplicates-pair-list
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   remove the duplicates in a list of pairs
;;; Produces:
;;;   lst-no-duplicate, a list

      
(define remove-duplicates-pair-list
  (lambda (lst)
    (let ([srtlst (sort lst (lambda (pair1 pair2) (string-ci<=? (car pair1) (car pair2))))])
      (let kernel ([so-far (cons (car srtlst) null)]
                   [lst2 (cdr srtlst)])
        (if (null? lst2)
            so-far
            (if (equal? (car (car lst2)) (car (car so-far)))
                (kernel so-far (cdr lst2))
                (kernel (cons (car lst2) so-far) (cdr lst2))))))))

;;;; Identifier:
;;;;   model
;;;; Type:
;;;;   list of lists of hashtables
;;;; Content:
;;;;   all topic modeled of given scb database

;(define model
;  (let kernel ([year 1961]
;               [so-far (gvector)])
;    (cond
;      [(= year 1971)
;       so-far]
;      [else
;       (gvector-add!
;        so-far
;        (list->vector (map
;                       (lambda (month)
;                         (if (notemptymonth? year month)
;                             (topic-model (cleanup-void (map cleanup-string (files-in-year-month year month))) 10 1000)
;                             (make-hash)))
;                      (range 1 13))))
;       (kernel (+ 1 year) so-far)])))


                    
       
;;; Procedure:
;;;   colliding?
;;; Parameters:
;;;   word1, an image
;;;   word2, an image
;;; Purpose:
;;;   check if two images collide with each other
;;; Produces:
;;;   collision!, a boolean
        
               
;(define colliding?
;  (lambda (word1 word2)
;    (let ([w1 (list-ref word1 1)]
;          [w2 (list-ref word2 1)]
;          [h1 (list-ref word1 2)]
;          [h2 (list-ref word2 2)]
;          [x1 (list-ref word1 3)]
;          [x2 (list-ref word2 3)]
;          [y1 (list-ref word1 4)]
;          [y2 (list-ref word2 4)])
;    (or
;     (and (> (+ x1 (/ w1 2)) (- x2 (/ w2 2))) (< (- x1 (/ w1 2)) (- x2 (/ w2 2))) (> (+ y1 (/ h1 2)) (- y2 (/ h2 2))) (< (- y1 (/ h1 2)) (- y2 (/ h2 2))))
;     (and (> (+ x1 (/ w1 2)) (- x2 (/ w2 2))) (< (- x1 (/ w1 2)) (- x2 (/ w2 2))) (> (+ y1 (/ h1 2)) (- y2 (/ h2 2))) (< (- y1 (/ h1 2)) (+ y2 (/ h2 2))))
;     (and (> (+ x1 (/ w1 2)) (- x2 (/ w2 2))) (< (- x1 (/ w1 2)) (- x2 (/ w2 2))) (> (+ y1 (/ h1 2)) (+ y2 (/ h2 2))) (< (- y1 (/ h1 2)) (+ y2 (/ h2 2))))
;     (and (> (+ x1 (/ w1 2)) (- x2 (/ w2 2))) (< (- x1 (/ w1 2)) (+ x2 (/ w2 2))) (> (+ y1 (/ h1 2)) (+ y2 (/ h2 2))) (< (- y1 (/ h1 2)) (+ y2 (/ h2 2))))
;     (and (> (+ x1 (/ w1 2)) (+ x2 (/ w2 2))) (< (- x1 (/ w1 2)) (+ x2 (/ w2 2))) (> (+ y1 (/ h1 2)) (+ y2 (/ h2 2))) (< (- y1 (/ h1 2)) (+ y2 (/ h2 2))))
;     (and (> (+ x1 (/ w1 2)) (+ x2 (/ w2 2))) (< (- x1 (/ w1 2)) (+ x2 (/ w2 2))) (> (+ y1 (/ h1 2)) (- y2 (/ h2 2))) (< (- y1 (/ h1 2)) (+ y2 (/ h2 2))))
;     (and (> (+ x1 (/ w1 2)) (+ x2 (/ w2 2))) (< (- x1 (/ w1 2)) (+ x2 (/ w2 2))) (> (+ y1 (/ h1 2)) (- y2 (/ h2 2))) (< (- y1 (/ h1 2)) (- y2 (/ h2 2))))
;     (and (> (+ x1 (/ w1 2)) (- x2 (/ w2 2))) (< (- x1 (/ w1 2)) (+ x2 (/ w2 2))) (> (+ y1 (/ h1 2)) (- y2 (/ h2 2))) (< (- y1 (/ h1 2)) (- y2 (/ h2 2))))
;     (and (> (+ x1 (/ w1 2)) (- x2 (/ w2 2))) (< (- x1 (/ w1 2)) (+ x2 (/ w2 2))) (> (+ y1 (/ h1 2)) (- y2 (/ h2 2))) (< (- y1 (/ h1 2)) (+ y2 (/ h2 2))))))))

(define colliding?
  (lambda (word1 word2)
    (let ([w1 (list-ref word1 1)]
          [w2 (list-ref word2 1)]
          [h1 (list-ref word1 2)]
          [h2 (list-ref word2 2)]
          [x1 (list-ref word1 3)]
          [x2 (list-ref word2 3)]
          [y1 (list-ref word1 4)]
          [y2 (list-ref word2 4)])
      (and (< x1 (+ x2 w2)) (> (+ x1 w1) x2) (< y1 (+ y2 h2)) (> (+ y1 h1) y2)))))


;;; Procedure:
;;;   wordpos
;;; Parameters:
;;;   word, a string
;;;   year1, starting year
;;;   year2, ending year
;;; Purpose:
;;;   returns a list of lists of images with their position
;;; Produces:
;;;   lst-no-duplicate, a list

(define wordpos-kernel
  (lambda (word year1 year2 size)
    (let* ([wordcloud-freq (word-cloud word year1 year2)]
           [wordcloud-sorted (map car (sort wordcloud-freq (lambda (lst1 lst2) (> (cadr lst1) (cadr lst2))) ))]
          [cloudlength (length wordcloud-sorted)]
          [widths-sorted (map htdp:image-width wordcloud-sorted)]
          [heights-sorted (map htdp:image-height wordcloud-sorted)]
          [words (map append (map list wordcloud-sorted) (map list widths-sorted) (map list heights-sorted))])
      (let kernel ([pos 1]
                   [sofar (list (append (list-ref words 0) (list (random size)) (list (random size))))]
                   [x (random size)]
                   [y (random size)])
        (cond
          [(= pos cloudlength)
            sofar]
          [(reduce (lambda (x y) (or x y)) (map (section colliding? (append (list-ref words pos) (list x y)) <>) sofar))
           (kernel pos sofar (random size) (random size))]
          [else
           (kernel (+ 1 pos) (cons (append (list-ref words pos) (list x y)) sofar) (random size) (random size))])))))

(define wordpos
  (lambda (word year1 year2)
    (let* ([size (cond
                   [(> (- year2 year1) 8)
                    575]
                   [(> (- year2 year1) 7)
                    550]
                   [(> (- year2 year1) 5)
                    500]
                   [(> (- year2 year1) 3)
                    450]
                   [(> (- year2 year1) 2)
                    400]
                   [else
                    300])]
           [wordlst (wordpos-kernel word year1 year2 size)]
           [words (map car wordlst)]
           [pos (map (section drop <> 3) wordlst)]
           [wordpos (map append (map list words) pos)])
      wordpos)))

;;; Procedure:
;;;   list->img
;;; Parameters:
;;;   li, a list produced by wordpos
;;; Purpose:
;;;   plot the list into an image
;;; Produces:
;;;   wordcloud, an image
;;; Citation:
;;;   Thanks, Yutaro

(define list->img
  (λ(li)
    (apply
     htdp:overlay/align
     (cons
      'left
      (cons
       'top
       (map
        (λ(elem)
          (apply xy elem)
          )
        li))));;img x y 
    ))
