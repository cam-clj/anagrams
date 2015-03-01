(ns cam-clj.anagrams
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :refer [cartesian-product combinations]])
  (:import [org.apache.commons.lang3 StringUtils])
  (:gen-class))

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

(defn distinct-by
  [f xs]
  (loop [result [] seen #{} xs xs]
    (if (empty? xs)
      result
      (let [x (first xs)
            v (f x)]
        (if (seen v)
          (recur result seen (rest xs))
          (recur (conj result x) (conj seen v) (rest xs)))))))

(defn remove-first
  "Remove the first occurrence of c from xs; return nil if xs does not contain c"
  [xs c]
  (loop [xs (seq xs) ys []]
    (when-let [x (first xs)]
      (if (= x c)
        (concat ys (rest xs))
        (recur (rest xs) (conj ys x))))))

(defn remove-chars
  [letters to-remove]
  (str/join (reduce remove-first letters to-remove)))

(defn partition-letters
  [letters word-lengths]
  {:pre [(= (count letters) (reduce + word-lengths))]}
  (if (= (count word-lengths) 1)
    (if (anagrams letters)
      (list (list letters))
      nil)
    (let [n            (first word-lengths)
          candidates-n (filter anagrams (distinct-by word->key (map str/join (combinations letters n))))]
      (reduce concat (for [candidate candidates-n
                           :let [remaining-letters (remove-chars letters candidate)
                                 remaining-solutions (partition-letters remaining-letters (rest word-lengths))]
                           :when (not (empty? remaining-solutions))]
                       (map (fn [solution] (conj solution candidate)) remaining-solutions))))))

(defn expand-anagrams
  [parts]
  (apply cartesian-product (map anagrams parts)))

(defn multi-word-anagrams
  [letters & word-lengths]
  {:pre [(= (count letters) (reduce + word-lengths))]}
  (mapcat expand-anagrams
          (partition-letters letters word-lengths)))

(defn print-solutions
  [xs]
  (doseq [x xs]
    (println (str/join " " x))))

(def solve-anagram (comp print-solutions multi-word-anagrams))

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn -main [letters & word-lengths]
  (if (empty? word-lengths)
    (solve-anagram letters (count letters))
    (apply solve-anagram letters (map parse-int word-lengths))))
