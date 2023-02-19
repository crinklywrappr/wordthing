(ns wordthing.core
  (:require [hickory.core :as hickory]
            [hickory.select :as sel]
            [clj-http.client :as client]
            [clojure.java.io :as io]
            [clojure.string :as sg]
            [clojure.data.json :as json]
            [camel-snake-kebab.core :as csk]
            [clojure.edn :as edn]))

(def word-files
  ["words-01-02-hundred.html"
   "words-03-04-hundred.html"
   "words-05-06-hundred.html"
   "words-07-08-hundred.html"
   "words-09-10-hundred.html"
   "words-11-12-hundred.html"
   "words-13-14-hundred.html"
   "words-15-16-hundred.html"
   "words-17-18-hundred.html"
   "words-19-20-hundred.html"
   "words-21-22-hundred.html"
   "words-23-24-hundred.html"
   "words-25-26-hundred.html"
   "words-27-28-hundred.html"
   "words-29-30-hundred.html"])

(def dictionary-route "https://api.dictionaryapi.dev/api/v2/entries/en/")

(def patterns
  [["verb" "noun" "adjective" "noun"]
   ["adjective" "noun" "verb" "noun"]])

(defn drop-junk [s]
  (sg/replace s #"\r|\n|\t" ""))

(defn get-words [resource]
  (mapv
   (comp drop-junk first :content)
   (sel/select
    (sel/tag :li)
    (hickory/as-hickory
     (hickory/parse
      (slurp
       (io/resource resource)))))))

(defn all-words []
  (mapcat get-words word-files))

(defn get-parts [definition]
  (map :part-of-speech (:meanings definition)))

(defn define [word]
  (json/read-str
   (:body (client/get (str dictionary-route word)))
   :key-fn csk/->kebab-case-keyword))

(defn classify [definition]
  (set (mapcat get-parts definition)))

(defn parse-line [line]
  (let [[word classifications] (sg/split line #" " 2)]
    [word (edn/read-string classifications)]))

(defn group-word [m [word classifications]]
  (if (seq classifications)
    (reduce (fn [a b] (update a b conj word)) m classifications)
    (update m "" conj word)))

(defn group-words []
  (with-open [r (io/reader (io/resource "words.txt"))]
    (->> r line-seq (mapv parse-line)
         (reduce group-word {}))))

(defn random-xkcd-passphrase [groups]
  (let [pattern (rand-nth patterns)]
    (mapv #(rand-nth (get groups %)) pattern)))
