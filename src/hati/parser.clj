(ns hati.parser
  (:require [rewrite-clj.parser :as p]
            [rewrite-clj.node :as node]
            [rewrite-clj.zip :as z]
            [clojure.zip :as zip]
            [clojure.string :as str]))

(defn- drop-ws [coll]
  (drop-while node/whitespace-or-comment? coll))

(defn- defn-list? [n]
  (and (= :list (node/tag n))
       (->> n node/children drop-ws first node/string (= "defn"))))

(defn- string-node? [n]
  (and (not (node/whitespace-or-comment? n))
       (or (= :multi-line (node/tag n))
           (string? (node/sexpr n)))))

(defn- docstring? [loc]
  (and (string-node? (z/node loc))
       (-> loc z/up z/node defn-list?)))

(defn- newline? [loc]
  (-> loc z/node node/tag (= :newline)))

(defn- top-level? [loc]
  (-> loc zip/up zip/up nil?))

(defn- strip-comment [s]
  (str/replace s #"^;+\s?" ""))


(defn extract-comments [code-string]
  (let [comments (transient [])
        zz       (z/of-string code-string)]
    (loop [loc zz]
      (when-not (z/end? loc)
        (let [n         (z/node loc)
              node-type (cond (docstring? loc)        :docstring
                              (and (top-level? loc)
                                   (node/comment? n)) :top-level-comment
                              (node/comment? n)       :inner-comment
                              (newline? loc)          :newline
                              :else                   :other)]
          (when-not (= node-type :other)
            (conj!
             comments
             (merge (meta n)
                    (when (= node-type :docstring)
                      {:docstring-of ""})
                    {:tag       (node/tag n)
                     :type      node-type
                     :top-level (top-level? loc)
                     :string    (if (= node-type :docstring)
                                  (read-string (node/string n))
                                  (strip-comment (node/string n)))})))
          (recur (zip/next loc)))))
    (persistent! comments)))

(comment
  (extract-comments (slurp "test-resources/code.clj")))
