;; gorilla-repl.fileformat = 1

;; **
;;; # Anagrams
;;; 
;;; **anagram** *noun* a word, phrase, or sentence formed by another by rearranging its letters. For example, *angel* is an angram of *glean*. 
;; **

;; **
;;; # A few words on namespaces
;;; 
;;; Clojure functions are usually grouped together into *namespaces*. This allows us to split up our programs into more manageable chunks, and allows two libraries to define a function with the same name without trampling on each other. The core Clojure functions we saw in the previous part of the tutorial are all in the `clojure.core` namespace, and are automatically available to us without qualification. You also saw that we used the `require` function to make the `clojure.repl/doc` function available as `doc`:
;;; <pre>
;;;     (require '[clojure.repl :refer [doc]])
;;; </pre>
;;; 
;;; The `ns` macro defines the namespace for the functions that follow. We can also use the `ns` macro to `require` the libraries that we are going to use in our functions. In this section, we are going to be doing some file I/O and some string manipulation, so we load the necessary libraries in the `ns` declaration:
;; **

;; @@
(ns anagrams.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; The `:as` keyword adds an alias for the namespace that allows us to access functions in the namespace with a short prefix. For example, we can refer to the `clojure.java.io/file` function as `io/file`, and `clojure.string/join` as `str/join`.
;;; 
;;; # Solving anagrams
;;; 
;;; In this part of the tutorial, we are going to write a function that, given a word, will return all words that are anagrams of that word. We start with a list of valid words and use that to build an anagram dictonary that will allow us very quickly to look up anagrams of a given word.
;;; 
;;; We do this by sorting the letters of each word into alphabetical order and using that as a key in a Clojure map. The value in the map is a list of all words that sort to the same key. For example, given the word list:
;; **

;; @@
(def words ["ant" "tan" "tap" "apt" "pat"])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/words</span>","value":"#'anagrams.core/words"}
;; <=

;; **
;;; ...the dictionary would look like:
;; **

;; @@
(def dict {"ant" ["ant" "tan"], "apt" ["apt" "tap" "pat"]})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/dict</span>","value":"#'anagrams.core/dict"}
;; <=

;; **
;;; We could then find all anagrams of `"tap"` by computing its key and looking it up in the dictionary:
;; **

;; @@
(dict "apt")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;apt&quot;</span>","value":"\"apt\""},{"type":"html","content":"<span class='clj-string'>&quot;tap&quot;</span>","value":"\"tap\""},{"type":"html","content":"<span class='clj-string'>&quot;pat&quot;</span>","value":"\"pat\""}],"value":"[\"apt\" \"tap\" \"pat\"]"}
;; <=

;; **
;;; We start by writing a function to compute the key for a given word. It should turn the word into a sequence of characters, sort them into alphabetical order, then join them back together into a string.
;; **

;; @@
(defn word->key
  [word]
  (str/join (sort (seq word))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/word-&gt;key</span>","value":"#'anagrams.core/word->key"}
;; <=

;; **
;;; Here we're seeing for the first time that we have to read Clojure code from the inside out: first we create the sequence of characters: `(seq word)`; then we sort it: `(sort (seq word))`; finally we join it back together: `(str/join (sort (seq word)))`. Let's test our function:
;; **

;; @@
(word->key "tan")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;ant&quot;</span>","value":"\"ant\""}
;; <=

;; **
;;; # Introducing `reduce`
;;; 
;;; Next we need a function that will build a dictionary from our list of words. Whenever we need to *accumulate* some result (in this case, our dictionary) from a list, we should reach for the `reduce` function.
;; **

;; @@
(reduce + 0 [1 2 3 4])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>10</span>","value":"10"}
;; <=

;; **
;;; `reduce` takes an accumulator function, an optional starting value, and a list of values to accumulate. The accumulator function takes two arguments: the current value of the accumulator and the next value to accumulate. When the list of values is exhausted, the accumulator is returned. If we write our own version of `+` that prints its arguments, we can see what's going on in the `reduce` above.
;; **

;; @@
(defn my-plus
  [accum v]
  (println (str "accumulator: " accum ", next value: " v))
  (+ accum v))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/my-plus</span>","value":"#'anagrams.core/my-plus"}
;; <=

;; @@
(reduce my-plus 0 [1 2 3 4])
;; @@
;; ->
;;; accumulator: 0, next value: 1
;;; accumulator: 1, next value: 2
;;; accumulator: 3, next value: 3
;;; accumulator: 6, next value: 4
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-long'>10</span>","value":"10"}
;; <=

;; **
;;; The accumulator function for our dictionary has to take the current value of the dictionary (a map) and the next word to accumulate. It should compute the key for that word, and add the word to the list at that key in the map. We'll use Clojure's `update-in` function to add a word to the dictionary. Take a look at the documentation for `update-in`:
;; **

;; @@
(clojure.repl/doc update-in)
;; @@
;; ->
;;; -------------------------
;;; clojure.core/update-in
;;; ([m [k &amp; ks] f &amp; args])
;;;   &#x27;Updates&#x27; a value in a nested associative structure, where ks is a
;;;   sequence of keys and f is a function that will take the old value
;;;   and any supplied args and return the new value, and returns a new
;;;   nested structure.  If any levels do not exist, hash-maps will be
;;;   created.
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Let's see how this works. We'll start with a simple map:
;; **

;; @@
(def m {:a 1 :b 2 :c 3})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/m</span>","value":"#'anagrams.core/m"}
;; <=

;; **
;;; ...and increment the value at `:b`:
;; **

;; @@
(update-in m [:b] inc)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:c</span>","value":":c"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[:c 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:b</span>","value":":b"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[:b 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:a</span>","value":":a"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[:a 1]"}],"value":"{:c 3, :b 3, :a 1}"}
;; <=

;; **
;;; We have to pass the key to update as a vector because, although here we're working with a simple map, `update-in` can also operate on nested data structures:
;; **

;; @@
(def cities {"London" {:population 7200000},
             "Birmingham" {:population 992000},
             "Leeds" {:population 720000},
             "Glasgow" {:population 560000}})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/cities</span>","value":"#'anagrams.core/cities"}
;; <=

;; **
;;; If somebody in London has a baby, we can write:
;; **

;; @@
(update-in cities ["London" :population] inc)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;Birmingham&quot;</span>","value":"\"Birmingham\""},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:population</span>","value":":population"},{"type":"html","content":"<span class='clj-long'>992000</span>","value":"992000"}],"value":"[:population 992000]"}],"value":"{:population 992000}"}],"value":"[\"Birmingham\" {:population 992000}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;Glasgow&quot;</span>","value":"\"Glasgow\""},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:population</span>","value":":population"},{"type":"html","content":"<span class='clj-long'>560000</span>","value":"560000"}],"value":"[:population 560000]"}],"value":"{:population 560000}"}],"value":"[\"Glasgow\" {:population 560000}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;Leeds&quot;</span>","value":"\"Leeds\""},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:population</span>","value":":population"},{"type":"html","content":"<span class='clj-long'>720000</span>","value":"720000"}],"value":"[:population 720000]"}],"value":"{:population 720000}"}],"value":"[\"Leeds\" {:population 720000}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;London&quot;</span>","value":"\"London\""},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:population</span>","value":":population"},{"type":"html","content":"<span class='clj-long'>7200001</span>","value":"7200001"}],"value":"[:population 7200001]"}],"value":"{:population 7200001}"}],"value":"[\"London\" {:population 7200001}]"}],"value":"{\"Birmingham\" {:population 992000}, \"Glasgow\" {:population 560000}, \"Leeds\" {:population 720000}, \"London\" {:population 7200001}}"}
;; <=

;; **
;;; ...and if they have twins:
;; **

;; @@
(update-in cities ["London" :population] + 2)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;Birmingham&quot;</span>","value":"\"Birmingham\""},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:population</span>","value":":population"},{"type":"html","content":"<span class='clj-long'>992000</span>","value":"992000"}],"value":"[:population 992000]"}],"value":"{:population 992000}"}],"value":"[\"Birmingham\" {:population 992000}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;Glasgow&quot;</span>","value":"\"Glasgow\""},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:population</span>","value":":population"},{"type":"html","content":"<span class='clj-long'>560000</span>","value":"560000"}],"value":"[:population 560000]"}],"value":"{:population 560000}"}],"value":"[\"Glasgow\" {:population 560000}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;Leeds&quot;</span>","value":"\"Leeds\""},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:population</span>","value":":population"},{"type":"html","content":"<span class='clj-long'>720000</span>","value":"720000"}],"value":"[:population 720000]"}],"value":"{:population 720000}"}],"value":"[\"Leeds\" {:population 720000}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;London&quot;</span>","value":"\"London\""},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:population</span>","value":":population"},{"type":"html","content":"<span class='clj-long'>7200002</span>","value":"7200002"}],"value":"[:population 7200002]"}],"value":"{:population 7200002}"}],"value":"[\"London\" {:population 7200002}]"}],"value":"{\"Birmingham\" {:population 992000}, \"Glasgow\" {:population 560000}, \"Leeds\" {:population 720000}, \"London\" {:population 7200002}}"}
;; <=

;; **
;;; If we recall that we can use `conj` to add an element to a list, we can write the accumulator function for our dictionary:
;; **

;; @@
(defn add-word-to-dictionary
  "Add a single word to the dictionary."
  [dict word]
  (update-in dict [(word->key word)] conj word))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/add-word-to-dictionary</span>","value":"#'anagrams.core/add-word-to-dictionary"}
;; <=

;; **
;;; We start with an empty map and generate a dictionary from our list of words:
;; **

;; @@
(reduce add-word-to-dictionary {} words)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;apt&quot;</span>","value":"\"apt\""},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;pat&quot;</span>","value":"\"pat\""},{"type":"html","content":"<span class='clj-string'>&quot;apt&quot;</span>","value":"\"apt\""},{"type":"html","content":"<span class='clj-string'>&quot;tap&quot;</span>","value":"\"tap\""}],"value":"(\"pat\" \"apt\" \"tap\")"}],"value":"[\"apt\" (\"pat\" \"apt\" \"tap\")]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;ant&quot;</span>","value":"\"ant\""},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;tan&quot;</span>","value":"\"tan\""},{"type":"html","content":"<span class='clj-string'>&quot;ant&quot;</span>","value":"\"ant\""}],"value":"(\"tan\" \"ant\")"}],"value":"[\"ant\" (\"tan\" \"ant\")]"}],"value":"{\"apt\" (\"pat\" \"apt\" \"tap\"), \"ant\" (\"tan\" \"ant\")}"}
;; <=

;; **
;;; Let's encapsualte this in a `build-dictionary` function:
;; **

;; @@
(defn build-dictionary
  "Build a dictionary from the given list of words"
  [words]
  (reduce add-word-to-dictionary {} words))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/build-dictionary</span>","value":"#'anagrams.core/build-dictionary"}
;; <=

;; @@
(def dict (build-dictionary words))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/dict</span>","value":"#'anagrams.core/dict"}
;; <=

;; **
;;; # Reading files
;;; 
;;; We won't be able to solve many crossword clues with our current dictionary - it only has 5 words! But we've included a big word list in the `resources/` directory. We can use the `io/resource` function to get our hands on this file:
;; **

;; @@
(io/resource "words")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;URL file:/home/ray/Workspace/anagrams/resources/words&gt;</span>","value":"#<URL file:/home/ray/Workspace/anagrams/resources/words>"}
;; <=

;; **
;;; This file has one word per line. To get a list of words, we simply `slurp` the file into memory and split it into lines:
;; **

;; @@
(def words (str/split-lines (slurp (io/resource "words"))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/words</span>","value":"#'anagrams.core/words"}
;; <=

;; @@
(count words)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>99156</span>","value":"99156"}
;; <=

;; **
;;; That's more like it!
;;; 
;;; # Sanitizing data
;;; 
;;; Let's take a look at the first few words:
;; **

;; @@
(take 10 words)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;A&quot;</span>","value":"\"A\""},{"type":"html","content":"<span class='clj-string'>&quot;A&#x27;s&quot;</span>","value":"\"A's\""},{"type":"html","content":"<span class='clj-string'>&quot;AA&#x27;s&quot;</span>","value":"\"AA's\""},{"type":"html","content":"<span class='clj-string'>&quot;AB&#x27;s&quot;</span>","value":"\"AB's\""},{"type":"html","content":"<span class='clj-string'>&quot;ABM&#x27;s&quot;</span>","value":"\"ABM's\""},{"type":"html","content":"<span class='clj-string'>&quot;AC&#x27;s&quot;</span>","value":"\"AC's\""},{"type":"html","content":"<span class='clj-string'>&quot;ACTH&#x27;s&quot;</span>","value":"\"ACTH's\""},{"type":"html","content":"<span class='clj-string'>&quot;AI&#x27;s&quot;</span>","value":"\"AI's\""},{"type":"html","content":"<span class='clj-string'>&quot;AIDS&#x27;s&quot;</span>","value":"\"AIDS's\""},{"type":"html","content":"<span class='clj-string'>&quot;AM&#x27;s&quot;</span>","value":"\"AM's\""}],"value":"(\"A\" \"A's\" \"AA's\" \"AB's\" \"ABM's\" \"AC's\" \"ACTH's\" \"AI's\" \"AIDS's\" \"AM's\")"}
;; <=

;; **
;;; Ah, we can see that our `word->key` function might have to do a bit more work. We'll have to convert the words to lower-case and remove those apostrophes. We'll also have to deal with accented characters:
;; **

;; @@
(nth words 63793)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;métier&quot;</span>","value":"\"métier\""}
;; <=

;; **
;;; The Apache Commons StringUtils class has a static method to remove diacritics from a string. We've included this dependency in our `project.clj` so it's straightforward to import this Java class and call the `stripAccents` method:
;; **

;; @@
(import '[org.apache.commons.lang3 StringUtils])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-class'>org.apache.commons.lang3.StringUtils</span>","value":"org.apache.commons.lang3.StringUtils"}
;; <=

;; **
;;; 
;; **

;; @@
(defn strip-accents
  [word]
  (StringUtils/stripAccents word))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/strip-accents</span>","value":"#'anagrams.core/strip-accents"}
;; <=

;; @@
(strip-accents (nth words 63793))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;metier&quot;</span>","value":"\"metier\""}
;; <=

;; **
;;; Once we've stripped accents from the word, we can convert it to lower-case and remove any non-alphabetic charaters from the key. Recall that we can use a Clojure set as a predicate function:
;; **

;; @@
(def alphabetic? (set (seq "abcdefghijklmnopqrstuvwxyz")))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/alphabetic?</span>","value":"#'anagrams.core/alphabetic?"}
;; <=

;; @@
(alphabetic? \a)

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"}
;; <=

;; @@
(alphabetic? \`)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Back to our definition of `word->key`:
;; **

;; @@
(defn word->key
  [word]
  (str/join (sort (seq word))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/word-&gt;key</span>","value":"#'anagrams.core/word->key"}
;; <=

;; **
;;; We want to strip accents, convert it to lower-case, and remove non-alphabetic characters before we sort and join:
;; **

;; @@
(defn word->key
  [word]
  (str/join (sort (filter alphabetic? (seq (str/lower-case (strip-accents word)))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/word-&gt;key</span>","value":"#'anagrams.core/word->key"}
;; <=

;; **
;;; Some people find it difficult to read these functions inside-out, so Clojure provides some macros that let us write this sort of thing more like a pipeline:
;; **

;; @@
(defn word->key
  [word]
  (->> (strip-accents word)
       (str/lower-case)
       (seq)
       (filter alphabetic?)
       (sort)
       (str/join)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/word-&gt;key</span>","value":"#'anagrams.core/word->key"}
;; <=

;; **
;;; `->>` is called the *thread last* macro; it takes the result of one step in the pipeline and passes it as the **last** argument to the next step. (There's also the *thread first* macro, `->`, which passes each result as the **first** argument to the next step.) Macros are a powerful feature of Clojure that allow us to manipulate the code before it is compiled. This is possible because Clojure is *homoiconic* - a Clojure program is a Clojure data structure that we can manipulate programmatically. The `->>` macro rewrites our code to the nested form before it is compiled:
;; **

;; @@
(macroexpand-1 '(->> (strip-accents "ANT")
                     (str/lower-case)
                     (seq)
                     (filter alphabetic?) 
                     (sort) 
                     (str/join)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>str/join</span>","value":"str/join"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>sort</span>","value":"sort"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>filter</span>","value":"filter"},{"type":"html","content":"<span class='clj-symbol'>alphabetic?</span>","value":"alphabetic?"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>seq</span>","value":"seq"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>str/lower-case</span>","value":"str/lower-case"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>strip-accents</span>","value":"strip-accents"},{"type":"html","content":"<span class='clj-string'>&quot;ANT&quot;</span>","value":"\"ANT\""}],"value":"(strip-accents \"ANT\")"}],"value":"(str/lower-case (strip-accents \"ANT\"))"}],"value":"(seq (str/lower-case (strip-accents \"ANT\")))"}],"value":"(filter alphabetic? (seq (str/lower-case (strip-accents \"ANT\"))))"}],"value":"(sort (filter alphabetic? (seq (str/lower-case (strip-accents \"ANT\")))))"}],"value":"(str/join (sort (filter alphabetic? (seq (str/lower-case (strip-accents \"ANT\"))))))"}
;; <=

;; **
;;; There is one further simplification we can make: functions that expect a sequence of things will automatically convert a string into a sequence of characters, so we can drop the explicit call to `seq`:
;; **

;; @@
(defn word->key
  [word]
  (->> (strip-accents word)
       (str/lower-case)
       (filter alphabetic?)
       (sort)
       (str/join)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/word-&gt;key</span>","value":"#'anagrams.core/word->key"}
;; <=

;; **
;;; Let's see if it works:
;; **

;; @@
(take 10 (map word->key words))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;a&quot;</span>","value":"\"a\""},{"type":"html","content":"<span class='clj-string'>&quot;as&quot;</span>","value":"\"as\""},{"type":"html","content":"<span class='clj-string'>&quot;aas&quot;</span>","value":"\"aas\""},{"type":"html","content":"<span class='clj-string'>&quot;abs&quot;</span>","value":"\"abs\""},{"type":"html","content":"<span class='clj-string'>&quot;abms&quot;</span>","value":"\"abms\""},{"type":"html","content":"<span class='clj-string'>&quot;acs&quot;</span>","value":"\"acs\""},{"type":"html","content":"<span class='clj-string'>&quot;achst&quot;</span>","value":"\"achst\""},{"type":"html","content":"<span class='clj-string'>&quot;ais&quot;</span>","value":"\"ais\""},{"type":"html","content":"<span class='clj-string'>&quot;adiss&quot;</span>","value":"\"adiss\""},{"type":"html","content":"<span class='clj-string'>&quot;ams&quot;</span>","value":"\"ams\""}],"value":"(\"a\" \"as\" \"aas\" \"abs\" \"abms\" \"acs\" \"achst\" \"ais\" \"adiss\" \"ams\")"}
;; <=

;; @@
(word->key (nth words 63793))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;eeimrt&quot;</span>","value":"\"eeimrt\""}
;; <=

;; **
;;; # Putting it all together
;;; 
;;; We can now generate a dictionary from the large wordlist (this may take a few seconds):
;; **

;; @@
(def dict (build-dictionary words))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/dict</span>","value":"#'anagrams.core/dict"}
;; <=

;; **
;;; ...and we can combine this with the `words->key` function to solve anagrams:
;; **

;; @@
(def anagrams (comp dict word->key))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;anagrams.core/anagrams</span>","value":"#'anagrams.core/anagrams"}
;; <=

;; @@
(anagrams "west")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;wets&quot;</span>","value":"\"wets\""},{"type":"html","content":"<span class='clj-string'>&quot;wet&#x27;s&quot;</span>","value":"\"wet's\""},{"type":"html","content":"<span class='clj-string'>&quot;west&quot;</span>","value":"\"west\""},{"type":"html","content":"<span class='clj-string'>&quot;stew&quot;</span>","value":"\"stew\""},{"type":"html","content":"<span class='clj-string'>&quot;West&quot;</span>","value":"\"West\""}],"value":"(\"wets\" \"wet's\" \"west\" \"stew\" \"West\")"}
;; <=

;; @@
(anagrams "merit")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;timer&quot;</span>","value":"\"timer\""},{"type":"html","content":"<span class='clj-string'>&quot;remit&quot;</span>","value":"\"remit\""},{"type":"html","content":"<span class='clj-string'>&quot;mitre&quot;</span>","value":"\"mitre\""},{"type":"html","content":"<span class='clj-string'>&quot;merit&quot;</span>","value":"\"merit\""}],"value":"(\"timer\" \"remit\" \"mitre\" \"merit\")"}
;; <=

;; **
;;; Our complete namespace would look something like:
;; **

;; @@
(ns anagrams.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [org.apache.commons.lang3 StringUtils]))


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
;;; # Where to go from here
;;; 
;;; If you've enjoyed the tutorial so far, you can load the next worksheet `worksheets/011-multi-word-anagrams.clj` and learn how to solve anagrams whose solution consists of more than one word, or take a look at `src/cam_clj/anagrams.clj` for a complete solution with a command-line interface.
;;; 
;;; Other good resources for learning Clojure include:
;;; * [Clojure for the Brave and True](http://www.braveclojure.com/)
;;; * [Clojure from the Ground Up](https://aphyr.com/posts/301-clojure-from-the-ground-up-welcome)
;;; * [4Clojure](https://www.4clojure.com/)
;; **
