(ns alphabet-cipher.coder
  (:require [clojure.string :as str]))

;; Our known alphabet
(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn string-seq
  "Returns a function which on each call returns the next character in the
  provided string. It will loop around to first character after the last one
  got returned"
  [string]
  (let [calls (atom 0)]
    (fn []
      (let [x (nth string @calls)]
        (swap! calls #(mod (inc %) (.length string)))
        x))))

(defn rotate
  "Takes a the first character of the string and places it at the end, until
  the string starts with start"
  [string start]
  (let [s (repeatedly (string-seq string))
        n (.indexOf string start)]
    (str/join (take (.length string) (drop n s)))))

(defn rotate-until
  "Rotates the string (via rotate) until sub is the n-th character in the string"
  [string sub n]
  (loop [s string]
    (if (= n (.indexOf s sub))
      s
      (recur (rotate s (str (nth s 1)))))))

(defn encode [keyword message]
  (let [ks (repeatedly (string-seq keyword))]
    (str/join
     (map (fn [k m]
            (let [col (.indexOf alphabet (str k)) ;; Column in the alphabet
                  row (rotate alphabet (str m))]  ;; Matching "rotated" row
              (nth row col)))                     ;; Intersection
          ks message))))

(defn decode [keyword message]
  (let [ks (repeatedly (string-seq keyword))]
    (str/join
     (map (fn [k m]
            (let [col (.indexOf alphabet (str k))          ;; Column in alphabet
                  row (rotate-until alphabet (str m) col)] ;; Row at intersection
              (first row)))                                ;; First char or row
          ks message))))

(defn- repeating-keyword
  "Return the repeating keyword used by the cipher and message"
  [cipher message]
  (str/join
   (map (fn [c m]
          (let [row (rotate alphabet (str m)) ;; Matching "rotated" row
                col (.indexOf row (str c))]   ;; Intersection
            (nth alphabet col)))              ;; Column in alphabet
        cipher message)))

(defn decipher [cipher message]
  (let [raw (repeating-keyword cipher message)]
    ;; Partition the repeating keyword and check if we have two parts that
    ;; match, if so we found the keyword, otherwise partition at a bigger
    ;; size and try again
    (loop [n 1]
      (let [keywords (partition n raw)]
        (if (= (first keywords) (second keywords))
          (str/join (first keywords))
          (recur (inc n)))))))

