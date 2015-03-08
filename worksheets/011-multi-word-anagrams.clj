;; gorilla-repl.fileformat = 1

;; **
;;; # Multi-word anagrams
;;; 
;;; **This part of the tutorial is aimed at more experencied Clojure programmers; an algorithm is described and it is left as an exercise for you to implement the algorithm.**
;;; 
;;; We use some functions from [org.clojure/math.combinatorics](https://github.com/clojure/math.combinatorics) to solve multi-word anagrams, so start with a slightly revised namespace declaration (that adds the combinatorics functions we'll need):
;; **

;; @@
(ns anagrams.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :refer [cartesian-product combinations]])
  (:import [org.apache.commons.lang3 StringUtils]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ...and define the functions from part two:
;; **

;; @@
(def alphabetic? (set (seq "abcdefghijklmnopqrstuvwxyz")))

(defn strip-accents
  "Strip accents from a string."
  [word]
  (StringUtils/stripAccents word))

(defn word->key
  "Compute the anagram key for a word."
  [word]
  (->> (strip-accents word)
       (str/lower-case)
       (filter alphabetic?)
       (sort)
       (str/join)))

(defn add-word-to-dictionary
  "Add a single word to the dictionary."
  [dict word]
  (update-in dict [(word->key word)] conj word))

(defn build-dictionary
  "Build a dictionary from the given list of words"
  [words]
  (reduce add-word-to-dictionary {} words))

(defn read-wordlist
  "Read words from the specified file or resource."
  [source]
  (str/split-lines (slurp source)))

(def dict (build-dictionary (read-wordlist (io/resource "words"))))

(def anagrams (comp dict word->key))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/anagrams</span>","value":"#'anagrams.core/anagrams"}
;; <=

;; **
;;; # The Goal
;;; 
;;; Your goal is to write a new function that will be called like:
;;; 
;;; <pre>
;;;     (multi-word-anagrams "letitbe" [3 2 2])
;;; </pre>
;;; 
;;; The first argument is the letters for the anagram, and the second argument the lengths of the words we are looking for. I suggest you add a pre-condition to check that the arguments are consistent:
;; **

;; @@
(defn multi-word-anagrams
  [letters word-lengths]
  {:pre [(= (count letters) (reduce + word-lengths))]}
  ;; Your code goes here
  nil)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/multi-word-anagrams</span>","value":"#'anagrams.core/multi-word-anagrams"}
;; <=

;; **
;;; # The Algorithm
;;; 
;;; We split the problem into two parts: first, find all partitions of the letters into the specified word-lengths, such that every part of earh partition appears in the anagram dictionary; once we have the partitions, compute the cartesian product of the anagrams of each part of the partition.
;;; 
;;; While we could use `clojure.math.combinatorics/partitions` to comupte all partitons and filter the results, this approach would be terribly inefficient.
;;; 
;;; ## Partitioning the letters
;;; 
;;; This is amenable to a recursive solution. For example, to compute `(partition-letters "letitbe" [3 2 2])` we would remove 3 letters from `letitbe` and then call `partition-letters` on the remainder, with word-lengths `[2 2]`. If there are no solutions to the sub-problem, discard that choice of letters, otherwise we have found a solution. Do this for all choices of 3 letters that can be anagrammed and we have all solutions.
;;; 
;;; We can use `clojure.math.combinatorics/combinations` to give us all selections of `n` letters from a collection:
;;; 
;; **

;; @@
(map str/join (combinations "letitbe" 3))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;let&quot;</span>","value":"\"let\""},{"type":"html","content":"<span class='clj-string'>&quot;lei&quot;</span>","value":"\"lei\""},{"type":"html","content":"<span class='clj-string'>&quot;let&quot;</span>","value":"\"let\""},{"type":"html","content":"<span class='clj-string'>&quot;leb&quot;</span>","value":"\"leb\""},{"type":"html","content":"<span class='clj-string'>&quot;lee&quot;</span>","value":"\"lee\""},{"type":"html","content":"<span class='clj-string'>&quot;lti&quot;</span>","value":"\"lti\""},{"type":"html","content":"<span class='clj-string'>&quot;ltt&quot;</span>","value":"\"ltt\""},{"type":"html","content":"<span class='clj-string'>&quot;ltb&quot;</span>","value":"\"ltb\""},{"type":"html","content":"<span class='clj-string'>&quot;lte&quot;</span>","value":"\"lte\""},{"type":"html","content":"<span class='clj-string'>&quot;lit&quot;</span>","value":"\"lit\""},{"type":"html","content":"<span class='clj-string'>&quot;lib&quot;</span>","value":"\"lib\""},{"type":"html","content":"<span class='clj-string'>&quot;lie&quot;</span>","value":"\"lie\""},{"type":"html","content":"<span class='clj-string'>&quot;ltb&quot;</span>","value":"\"ltb\""},{"type":"html","content":"<span class='clj-string'>&quot;lte&quot;</span>","value":"\"lte\""},{"type":"html","content":"<span class='clj-string'>&quot;lbe&quot;</span>","value":"\"lbe\""},{"type":"html","content":"<span class='clj-string'>&quot;eti&quot;</span>","value":"\"eti\""},{"type":"html","content":"<span class='clj-string'>&quot;ett&quot;</span>","value":"\"ett\""},{"type":"html","content":"<span class='clj-string'>&quot;etb&quot;</span>","value":"\"etb\""},{"type":"html","content":"<span class='clj-string'>&quot;ete&quot;</span>","value":"\"ete\""},{"type":"html","content":"<span class='clj-string'>&quot;eit&quot;</span>","value":"\"eit\""},{"type":"html","content":"<span class='clj-string'>&quot;eib&quot;</span>","value":"\"eib\""},{"type":"html","content":"<span class='clj-string'>&quot;eie&quot;</span>","value":"\"eie\""},{"type":"html","content":"<span class='clj-string'>&quot;etb&quot;</span>","value":"\"etb\""},{"type":"html","content":"<span class='clj-string'>&quot;ete&quot;</span>","value":"\"ete\""},{"type":"html","content":"<span class='clj-string'>&quot;ebe&quot;</span>","value":"\"ebe\""},{"type":"html","content":"<span class='clj-string'>&quot;tit&quot;</span>","value":"\"tit\""},{"type":"html","content":"<span class='clj-string'>&quot;tib&quot;</span>","value":"\"tib\""},{"type":"html","content":"<span class='clj-string'>&quot;tie&quot;</span>","value":"\"tie\""},{"type":"html","content":"<span class='clj-string'>&quot;ttb&quot;</span>","value":"\"ttb\""},{"type":"html","content":"<span class='clj-string'>&quot;tte&quot;</span>","value":"\"tte\""},{"type":"html","content":"<span class='clj-string'>&quot;tbe&quot;</span>","value":"\"tbe\""},{"type":"html","content":"<span class='clj-string'>&quot;itb&quot;</span>","value":"\"itb\""},{"type":"html","content":"<span class='clj-string'>&quot;ite&quot;</span>","value":"\"ite\""},{"type":"html","content":"<span class='clj-string'>&quot;ibe&quot;</span>","value":"\"ibe\""},{"type":"html","content":"<span class='clj-string'>&quot;tbe&quot;</span>","value":"\"tbe\""}],"value":"(\"let\" \"lei\" \"let\" \"leb\" \"lee\" \"lti\" \"ltt\" \"ltb\" \"lte\" \"lit\" \"lib\" \"lie\" \"ltb\" \"lte\" \"lbe\" \"eti\" \"ett\" \"etb\" \"ete\" \"eit\" \"eib\" \"eie\" \"etb\" \"ete\" \"ebe\" \"tit\" \"tib\" \"tie\" \"ttb\" \"tte\" \"tbe\" \"itb\" \"ite\" \"ibe\" \"tbe\")"}
;; <=

;; **
;;; But note that, because the input string contains duplicate letters, we get some duplicates in the output. Furthermore, two strings with letters in different orders will produce the same anagrams, so we'll want to trim down this list. I suggest you start by implementing a helper functon, `distinct-by`, to return just those combinations that have distinct values of `word->key`.
;;; 
;;; ## Building the solutions
;;; 
;;; Once we have a partition of the input letters, building the solutions is relatively straightforward. For example, one partition of `"fightline"` into `[5 4]` is `["ghint" "efil"]`, and we have:
;; **

;; @@
(anagrams "ghint")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;thing&quot;</span>","value":"\"thing\""},{"type":"html","content":"<span class='clj-string'>&quot;night&quot;</span>","value":"\"night\""}],"value":"(\"thing\" \"night\")"}
;; <=

;; @@
(anagrams "efil")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;life&quot;</span>","value":"\"life\""},{"type":"html","content":"<span class='clj-string'>&quot;lief&quot;</span>","value":"\"lief\""},{"type":"html","content":"<span class='clj-string'>&quot;file&quot;</span>","value":"\"file\""},{"type":"html","content":"<span class='clj-string'>&quot;Leif&quot;</span>","value":"\"Leif\""}],"value":"(\"life\" \"lief\" \"file\" \"Leif\")"}
;; <=

;; **
;;; We can simply use `clojure.math.combinatorics/cartesian-product` to combine these two results:
;; **

;; @@
(clojure.pprint/pprint (cartesian-product (anagrams "ghint") (anagrams "efil")))
;; @@
;; ->
;;; ((&quot;thing&quot; &quot;life&quot;)
;;;  (&quot;thing&quot; &quot;lief&quot;)
;;;  (&quot;thing&quot; &quot;file&quot;)
;;;  (&quot;thing&quot; &quot;Leif&quot;)
;;;  (&quot;night&quot; &quot;life&quot;)
;;;  (&quot;night&quot; &quot;lief&quot;)
;;;  (&quot;night&quot; &quot;file&quot;)
;;;  (&quot;night&quot; &quot;Leif&quot;))
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; 
;; **
