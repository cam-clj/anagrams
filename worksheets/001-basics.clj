;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit alt+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns scenic-dew)
;; @@

;; **
;;; # Clojure Basics
;;; 
;;; It's customary in progaramming language tutorials to start with *hello world*. In Clojure, this looks like:
;; **

;; @@
(println "Hello World")
;; @@
;; ->
;;; Hello World
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Clojure programs consist of *lists*, which start with an opening parenthesis `(` and end with a closing parenthesis `)`. Our hello world program is a list whose first element is the symbol `println` and second element the string `"Hello World"`. In Clojure, the first element of a list is interpreted as a function and the rest of the elements the function arguments. When Clojure evaluates a list, it calls the function with the given arguments. What we see above when we run the program is the output from `println` and the return value `nil`.
;;; 
;;; As well as strings, Clojure has simple data types such as long, double, and boolean:
;; **

;; @@
"one"
2
3.0
true
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; **
;;; We are using Clojure on the JVM, where these are all familiar Java objects. The `class` function will tell us what we are working with:
;; **

;; @@
(class "one")
(class 2)
(class 3.0)
(class true)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-class'>java.lang.Boolean</span>","value":"java.lang.Boolean"}
;; <=

;; **
;;; We use the `require` function to load external libraries. Let's load the `doc` function so we can read documentation for core Clojure functions in the REPL:
;; **

;; @@
(require '[clojure.repl :refer [doc]])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(doc class)
;; @@
;; ->
;;; -------------------------
;;; clojure.core/class
;;; ([x])
;;;   Returns the Class of x
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; # Collections
;;; 
;;; As well as basic data types like strings and numbers, Clojure comes with built-in collection types such as lists (which we have already seen), vectors, maps and sets.
;; **

;; @@
(list 1 2 3)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"(1 2 3)"}
;; <=

;; @@
(vector 1 2 3)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[1 2 3]"}
;; <=

;; @@
(hash-set 1 2 3)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-set'>#{</span>","close":"<span class='clj-set'>}</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"#{1 3 2}"}
;; <=

;; @@
(hash-map :a 1 :b 2 :c 3)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:c</span>","value":":c"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[:c 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:b</span>","value":":b"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[:b 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:a</span>","value":":a"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[:a 1]"}],"value":"{:c 3, :b 2, :a 1}"}
;; <=

;; **
;;; It' s idiomatic in Clojure to use keywords (`:a`, `:b`, `:c` above) as keys in maps, but we could just as well use strings: 
;; **

;; @@
(hash-map "a" 1 "b" 2 "c" 3)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;a&quot;</span>","value":"\"a\""},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[\"a\" 1]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;b&quot;</span>","value":"\"b\""},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[\"b\" 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;c&quot;</span>","value":"\"c\""},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[\"c\" 3]"}],"value":"{\"a\" 1, \"b\" 2, \"c\" 3}"}
;; <=

;; **
;;; A Clojure collection can contain elements of differing data types:
;; **

;; @@
(vector 1 "two" 3.0)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-string'>&quot;two&quot;</span>","value":"\"two\""},{"type":"html","content":"<span class='clj-double'>3.0</span>","value":"3.0"}],"value":"[1 \"two\" 3.0]"}
;; <=

;; **
;;; We use vectors, sets and maps a lot in our Clojure programs, so Clojure helpfully provides a short-hand notation for them (which, incidentally, is just how the REPL prints the data structures):
;; **

;; @@
{:a 1 :b 2 :c 3}
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:c</span>","value":":c"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[:c 3]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:b</span>","value":":b"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[:b 2]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:a</span>","value":":a"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[:a 1]"}],"value":"{:c 3, :b 2, :a 1}"}
;; <=

;; @@
[1 2 3]
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[1 2 3]"}
;; <=

;; @@
#{1 2 3}
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-set'>#{</span>","close":"<span class='clj-set'>}</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"#{1 3 2}"}
;; <=

;; **
;;; # Giving things names
;;; 
;;; In order to refer to things in our programs, we have to give them names. We do this with the `def` (define) function:
;; **

;; @@
(def x 1)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/x</span>","value":"#'user/x"}
;; <=

;; @@
x
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}
;; <=

;; **
;;; One thing about Clojure that makes it different from other programming languages is that its data types are *immutable* - once we've defined `x` as 1 it will always be 1. If we increment it:
;; **

;; @@
(inc x)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}
;; <=

;; **
;;; ...we get back the value 2, but `x` is still 1:
;; **

;; @@
x
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}
;; <=

;; **
;;; Clojure's collections are also immutable:
;; **

;; @@
(def xs [1 2 3])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/xs</span>","value":"#'user/xs"}
;; <=

;; @@
(conj xs 4)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[1 2 3 4]"}
;; <=

;; @@
xs
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[1 2 3]"}
;; <=

;; **
;;; Clojure uses some clever tricks to minimize the memory usage of its immutable data structures. In the example above, the vector `[1 2 3 4]` is just the vector `[1 2 3]` with an element added to the end, and as the vector `[1 2 3]` is immutable, there's no need to make a copy of it when constructing the vector `[1 2 3 4]`. Clojure takes care of all of this for us and, for the most part, we never have to worry about it.
;;; 
;;; # Defining functions
;;; 
;;; The `fn` function creates a function. The first argument to `fn` is the argument vector, and the remaining arguments the function body. The return value of the function is the value returned by the last expression in the body.
;; **

;; @@
(fn [x] (* x 3))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;user$eval6004$fn__6005 user$eval6004$fn__6005@32cf8dd6&gt;</span>","value":"#<user$eval6004$fn__6005 user$eval6004$fn__6005@32cf8dd6>"}
;; <=

;; **
;;; This funciton takes the single argument `x` and multiplies it by 3. The value printed above is just how the REPL prints an anonymous function (of course our function is anonymous - we haven't given it a name!). We can call our function by evaulating a list with our function as the first element:
;;; 
;; **

;; @@
((fn [x] (* 3 x)) 2
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}
;; <=

;; **
;;; We'll see more anonymous functions as we go on, but if we're going to call a function more than once we should give it a name:
;; **

;; @@
(def triple (fn [x] (* x 3)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/triple</span>","value":"#'user/triple"}
;; <=

;; @@
(triple 2)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}
;; <=

;; **
;;; As programming (particularly functional programming) is all about writing functions, Clojure provides a handy macro called `defn` that combines `def` and `fn` for us:
;; **

;; @@
(defn triple [x] (* x 3))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/triple</span>","value":"#'user/triple"}
;; <=

;; **
;;; Using `defn`, we can also add a document string to our function:
;; **

;; @@
(defn triple
  "Multiplies its argument by 3"
  [x]
  (* x 3))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/triple</span>","value":"#'user/triple"}
;; <=

;; **
;;; ...and we can read the doc string using `doc`:
;; **

;; @@
(doc triple)
;; @@
;; ->
;;; -------------------------
;;; user/triple
;;; ([x])
;;;   Multiplies its argument by 3
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Functions in Clojure are first-class citizens. This mean they can be passed as arguments to other functions. The `map` function takes a function and a collection, and returns the result of applying the function to each element in the collection.
;; **

;; @@
(map triple [1 2 3])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"}],"value":"(3 6 9)"}
;; <=

;; **
;;; Note that, although we passed in a vector, `map` has returned a list. Lists and vectors in Clojure have slightly different properties, and if we need a vector back we should use `mapv` rather than `map`:
;; **

;; @@
(mapv triple [1 2 3])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"}],"value":"[3 6 9]"}
;; <=

;; **
;;; We can also *compose* two functions to make a new one. This function first calls `triple` on its argument, then calls `inc` on the result:
;; **

;; @@
(def triple-and-inc (comp inc triple))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/triple-and-inc</span>","value":"#'user/triple-and-inc"}
;; <=

;; @@
(map triple-and-inc [1 2 3])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"}],"value":"(4 7 10)"}
;; <=

;; **
;;; # It's a bit like magic
;;; 
;;; Some things you might not expect also behave as functions in Clojure.
;;; 
;;; A map is a function that looks up a key in itself.
;; **

;; @@
(def population-of {"London" 7200000, "Birmingham" 992000, "Leeds" 720000, "Glasgow" 560000})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/population-of</span>","value":"#'user/population-of"}
;; <=

;; **
;;; This lets us write things like:
;; **

;; @@
(population-of "Glasgow")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>560000</span>","value":"560000"}
;; <=

;; **
;;; A keyword is a function that looks itself up in a map. This lets us represent records as maps, with keywords as accessors:
;; **

;; @@
(def cambridge {:name "Cambridge" :city-status 1951 :governing-body "Cambridge City Council" :population 122700})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/cambridge</span>","value":"#'user/cambridge"}
;; <=

;; @@
(:city-status cambridge)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>1951</span>","value":"1951"}
;; <=

;; **
;;; A vector is a function that looks up an element at the specified index:
;; **

;; @@
(["frog" "bat" "squirrel"] 1)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;bat&quot;</span>","value":"\"bat\""}
;; <=

;; **
;;; A set is a function that returns the element (if it's in the set), otherwise `nil`:
;; **

;; @@
(def vowel? #{\a \A \e \E \i \I \o \O \u \U})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/vowel?</span>","value":"#'user/vowel?"}
;; <=

;; @@
(vowel? \a)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"}
;; <=

;; @@
(vowel? \x)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Note that we're using `vowel?` as a predicte. In Clojure, everything apart from `nil` and `false` is `true`, so we can use a set as a predicate function as long as it does not contain `nil` or `false`.
;;; 
;;; Having defined `vowel?`, we can define `consonant?` as its complement:
;; **

;; @@
(def consonant? (complement vowel?))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/consonant?</span>","value":"#'user/consonant?"}
;; <=

;; @@
(consonant? "x")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; **
;;; This ability to build more complex functions from simple ones makes Clojure a very expressive language.
;;; 
;;; # Clojure's core sequence library
;;; 
;;; *It is better to have 100 functions operate on one data structure than 10 functions on 10 data structures.*
;;; 
;;; All of Clojure's collection types implement a common interface `clojure.lang.ISeq`, and the core Clojure library contains lots of functions that operate on this abstraction. Here are just a few of them:
;; **

;; @@
(def xs [9 8 7 6])
(first xs)
(second xs)
(rest xs)
(last xs)
(take 2 xs)
(drop 2 xs)
(reverse xs)
(sort xs)
(filter even? xs)
(remove even? xs)
        
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"}],"value":"(9 7)"}
;; <=

;; **
;;; If we define `ys` as a list rather than a vector, we can still use all of these core functions.
;;; 
;;; 
;; **

;; @@
(def ys (list 9 8 7 6))
(reverse ys)
;; etc.
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"}],"value":"(6 7 8 9)"}
;; <=

;; **
;;; We mentioned earlier that there are some differences between vectors and lists. One of these is where new elements get inserted. For lists, new elements are inserted on the front, while for vectors new elements are added to the end:
;; **

;; @@
(conj xs 1)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[9 8 7 6 1]"}
;; <=

;; @@
(conj ys 1)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}],"value":"(1 9 8 7 6)"}
;; <=

;; **
;;; We can also treat a string as a sequence of characters:
;; **

;; @@
(seq "Cambridge")
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>(\\C \\a \\m \\b \\r \\i \\d \\g \\e)</span>","value":"(\\C \\a \\m \\b \\r \\i \\d \\g \\e)"}
;; <=

;; @@
(filter vowel? "Cambridge")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\i</span>","value":"\\i"},{"type":"html","content":"<span class='clj-char'>\\e</span>","value":"\\e"}],"value":"(\\a \\i \\e)"}
;; <=

;; @@
(remove vowel? "Cambridge")
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\C</span>","value":"\\C"},{"type":"html","content":"<span class='clj-char'>\\m</span>","value":"\\m"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\r</span>","value":"\\r"},{"type":"html","content":"<span class='clj-char'>\\d</span>","value":"\\d"},{"type":"html","content":"<span class='clj-char'>\\g</span>","value":"\\g"}],"value":"(\\C \\m \\b \\r \\d \\g)"}
;; <=

;; **
;;; 
;;; 
;;; 
;; **

;; **
;;; # Exercises
;;; 
;;; 
;;; 0001: The [Clojure Cheat Sheet](http://jafingerhut.github.io/cheatsheet/clojuredocs/cheatsheet-tiptip-no-cdocs-summary.html) is a handy reference to the core Clojure library. Open up the cheat sheet in another tab. Using the cheat sheet, find the function that adds new key/value to a map, and add `:d => 4` to the map below.
;; **

;; @@
(def m {:a 1 :b 2 :c 3})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;user/m</span>","value":"#'user/m"}
;; <=

;; **
;;; 0010: Using the function you found above, change the value of `:a` in the map `m` to 4. (Remember that this does not actually modify `m`, but returns a new map with the change applied.)
;; **

;; @@

;; @@

;; **
;;; 0011: Again using the cheat sheet, find a function to remove a key/value from a map. Remove `:c` from the map `m`.
;; **

;; @@

;; @@

;; **
;;; 0100: Define a set, `s`, contanining the elements 1,2,3,4,5
;; **

;; **
;;; 
;; **

;; @@

;; @@

;; **
;;; 0101: Using the `conj` function, add the element 6 to the set.
;; **

;; @@

;; @@

;; **
;;; 0110: What happens if you add the element 5 to `s`?
;; **

;; @@

;; @@

;; **
;;; 0111: What happens if you use the `#{}` short-cut to create a set containing the elements 1,2,3,4,5,5 ?
;; **

;; @@

;; @@

;; **
;;; 1000: Remove the element 3 from the set `s`.
;; **

;; @@

;; @@

;; **
;;; 1001: Write a function, `short-word?`, that takes a single string argument, `s`, and returns `true` if the length of the string is less than or equal to 3, otherwise false.
;; **

;; @@

;; @@

;; **
;;; 1011: Define a string, `s`, with the value "The quick brown fox jumps over the lazy dog"
;; **

;; @@

;; @@

;; **
;;; 1100: Using `require` and `:as`, load the `clojure.string` library and alias it to `str`.
;; **

;; @@

;; @@

;; **
;;; 1101: Using a function from the `clojure.string` library, build a vector, `v`, containing the individual words from the string `s`.
;; **

;; @@

;; @@

;; **
;;; 1110: Using the `short-word?` function you defined earlier, count the number of short words in the vector `v`.
;; **

;; @@

;; @@

;; **
;;; 1111: Write a function, `count-short-words`, that takes a string and returns the number of short words found in the string. Modify this function to return the number of *distinct* short words in the string (the same word with different capitalization should not be considered distinct; in our example above, "The" and "the" should be considered as equivalent).
;; **

;; @@

;; @@

;; **
;;; **You know enough now to make a start on our anagram solver. When you're ready, open the worksheet '010-anagrams.clj'.**
;; **
