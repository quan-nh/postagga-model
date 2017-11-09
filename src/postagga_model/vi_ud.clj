(ns postagga-model.vi-ud
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [postagga.tagger :as tagger]
            [postagga.trainer :as trainer]))

(defn- sent-parser [sent]
  (->> sent
       (map #(str/split % #"\t"))
       (mapv (fn [[_ w _ tag]] [w tag]))))

(defn- corpus [file]
  (->> (slurp file)
       (str/split-lines)
       (partition-by str/blank?)
       (map #(drop 2 %))
       (filter seq)
       (mapv sent-parser)))

(def model (trainer/train (corpus (io/resource "corpora/vi-ud-train.conllu"))))
;(spit "models/vi_ud_model.edn" model)

;; check percentage correct of this model
(defn- compare-result [a b]
  (let [c (map = a b)]
    [(count (filter true? c))
     (count (filter false? c))
     (count c)]))

(def percentage-correct
  (let [result (for [corpus (corpus (io/resource "corpora/vi-ud-dev.conllu"))]
                 (let [sent (mapv first corpus)
                       tags (mapv second corpus)]
                   (compare-result tags (tagger/viterbi model sent))))
        [r w t] (apply map + result)]
    (double (/ r t))))
;=> 0.792513461872503
