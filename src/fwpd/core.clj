;; In ns below, notice that "gen-class" was removed
(ns fwpd.core
  ;; We haven't gone over require but we will.
  (:require [clojure.string :as s]))

(def filename "suspects.csv")

;; Later on we're going to be converting each row in the CSV into a
;; map, like {:name "Edward Cullen" :glitter-index 10}.
;; Since CSV can't store Clojure keywords, we need to associate the
;; textual header from the CSV with the correct keyword.
(def headers->keywords {"Name" :name
                        "Glitter Index" :glitter-index})

(def keywords->headers {:name "Name"
                        :glitter-index "Glitter Index"})

(defn str->int
  [str]
  (Integer. str))

;; CSV is all text, but we're storing numeric data. We want to convert
;; it back to actual numbers.
(def conversions {:name identity
                  :glitter-index str->int})

(defn parse
  "Convert a csv into rows of columns"
  [string]
  (map #(s/split % #",")
       (s/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (let [;; headers becomes the seq (:name :glitter-index)
        headers (map #(get headers->keywords %) (first rows))
        ;; unmapped-rows becomes the seq
        ;; (["Edward Cullen" "10"] ["Bella Swan" "0"] ...)
        unmapped-rows (rest rows)]
    ;; Now let's return a seq of {:name "X" :glitter-index 10}
    (map (fn [unmapped-row]
           ;; We're going to use map to associate each header with its
           ;; column. Since map returns a seq, we use "into" to convert
           ;; it into a map.
           (into {}
                 ;; notice we're passing multiple collections to map
                 (map (fn [header column]
                        ;; associate the header with the converted column
                        [header ((get conversions header) column)])
                      headers
                      unmapped-row)))
         unmapped-rows)))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn names
  [records]
  (map #(:name %) records))

(defn load-names
  "Loads the CSV file"
  []
  (mapify (parse (slurp filename))))

(defn validate
  "Validate that :name and :glitter-index are proper values"
  [validators record]
  (let [name-validate (get validators :name)
        glitter-validate (get validators :glitter-index)]
    (if
        (and (name-validate record) (glitter-validate record))
      true
      false)))

(defn validate-name
  "Ensures that a record has a name"
  [record]
  (let [name (get record :name)]
    (and name (instance? String name) (not (clojure.string/blank? name)))))

(defn validate-glitter
  "Ensures that a record has a glitter-index"
  [record]
  (let [glitter (get record :glitter-index)]
    (and glitter (instance? Long glitter) (or (zero? glitter) (pos? glitter)))))

(def validators {:name validate-name
                 :glitter-index validate-glitter})

(defn prepend
  "Add suspect to list of names"
  [new records]
  (when (validate validators new)
    (conj records new)))

(defn unmapify
  "Turn a seq of maps into a CSV string"
  [records]
  (str
   (s/join "," (vals keywords->headers))
   "\n"
   (s/join "\n" (map #(s/join "," (vals %)) records))))
