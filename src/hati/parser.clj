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


(defn- node-type [loc]
  (let [n (z/node loc)]
   (cond (docstring? loc)        :docstring
         (and (top-level? loc)
              (node/comment? n)) :top-level-comment
         (node/comment? n)       :inner-comment
         (newline? loc)          :newline
         :else                   :code)))


(defn- strip-comment [s]
  (str/replace s #"^;+\s?" ""))


(defn- comment-info [loc]
  (let [n         (z/node loc)
        node-type (node-type loc)]
   (merge (meta n)
          (when (= node-type :docstring)
            {:docstring-of ""})
          {:tag       (node/tag n)
           :type      node-type
           :top-level (top-level? loc)
           :string    (if (= node-type :docstring)
                        (read-string (node/string n))
                        (strip-comment (node/string n)))})))


(defn sieve [code-string]
  (let [prose (transient [])
        code  (transient [])
        zz    (z/of-string code-string)]
    (loop [loc zz]
      (when-not (z/end? loc)
        (if-not (= :code (node-type loc))
          (conj! prose (comment-info loc))
          (when (top-level? loc)
            (conj! code (zip/node loc)))) ;;TODO strip inline comments
        (recur (zip/next loc))))
    {:prose (persistent! prose)
     :code  (persistent! code)}))

(comment
  (sieve (slurp "test-resources/code.clj")))
