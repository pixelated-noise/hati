(ns hati.parser
  (:require [rewrite-clj.parser :as p]
            [rewrite-clj.node :as node]
            [rewrite-clj.zip :as z]
            [clojure.zip :as zip]
            [clojure.string :as str]))

(defn- drop-ws [coll]
  (drop-while node/whitespace-or-comment? coll))

(defn- dead-space? [node]
  (or (node/whitespace-or-comment? node)
      (node/linebreak? node)))

(defn- left-skip-dead-space [loc]
  (first (drop-while (comp dead-space? zip/node)
                     (iterate zip/left (zip/left loc)))))

(defn- left-skip-ws
  "Does not skip newlines"
  [loc]
  (first (drop-while #(when % (-> % zip/node node/tag #{:comma :whitespace}))
                     (iterate zip/left (zip/left loc)))))


(defn- defn-list? [n]
  (and (= :list (node/tag n))
       (->> n node/children drop-ws first node/string (= "defn"))))

(defn- def-list? [n]
  (and (= :list (node/tag n))
       (->> n node/children drop-ws first node/string (= "def"))))

(defn- ns-list? [n]
  (and (= :list (node/tag n))
       (->> n node/children drop-ws first node/string (= "ns"))))

(defn- string-node? [n]
  (and (not (node/whitespace-or-comment? n))
       (or (= :multi-line (node/tag n))
           (string? (node/sexpr n)))))

(defn- fn-docstring? [loc]
  (and (string-node? (z/node loc))
       (-> loc z/up z/node defn-list?)))

(defn- def-docstring? [loc]
  (and (string-node? (z/node loc))
       (-> loc z/up z/node def-list?)))

(defn- ns-docstring? [loc]
  (and (string-node? (z/node loc))
       (-> loc z/up z/node ns-list?)))

(defn- newline? [loc]
  (-> loc z/node node/tag (= :newline)))

(defn- top-level? [loc]
  (-> loc zip/up zip/up nil?))

(defn- sexp-comment? [loc]
  ;; it's a comment
  (and (some-> loc zip/node node/comment?)
       ;; ...and if you skip all whitespace to its left, you don't find a newline nor a comment
       (let [left-node (some-> loc left-skip-ws zip/node)]
         (and (not (node/linebreak? left-node))
              (not (node/comment? left-node))))))


(defn- node-type [loc]
  (let [n (z/node loc)]
    (cond (fn-docstring? loc)     :fn-docstring
          (def-docstring? loc)    :def-docstring
          (ns-docstring? loc)     :ns-docstring
          (and (top-level? loc)
               (node/comment? n)) :top-level-comment
          (sexp-comment? loc)     :sexp-comment
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
             {:docstring-of (-> loc left-skip-dead-space zip/node node/string)})
           {:tag       (node/tag n)
            :type      node-type
            :top-level (top-level? loc)
            :string    (if (#{:fn-docstring :def-docstring :ns-docstring} node-type)
                         (read-string (node/string n))
                         (strip-comment (node/string n)))})))


(defn agg-prose [{row-a    :row
                  string-a :string
                  :as      a}
                 {string-b  :string
                  end-row-b :end-row
                  end-col-b :end-col}]
  (merge a
         {:row     row-a
          :end-row end-row-b
          :end-col end-col-b
          :string  (str string-a string-b)}))

(defn consecutive-prose? [a b]
  (and
   (or (= (:type a) (:type b))
       (and (= :sexp-comment (:type a))
            (= :inner-comment (:type b))))
   (= (:col a) (:col b))
   (= (:end-row a) (:row b))))

(defn agg-consecutive-prose [coll]
  (reduce (fn [agg x]
            (if (consecutive-prose? (last agg) x)
              (conj (vec (butlast agg))
                    (agg-prose (last agg) x))
              (conj agg x)))
          [] coll))


(defn sieve [code-string]
  (let [prose (transient [])
        code  (transient [])]
    (loop [loc (z/of-string code-string)]
      (when-not (z/end? loc)
        (if-not (= :code (node-type loc))
          (conj! prose (comment-info loc))
          (when (top-level? loc)
            (conj! code (zip/node loc)))) ;;TODO strip inline comments
        (recur (zip/next loc))))
    {:prose (agg-consecutive-prose (persistent! prose))
     :code  (persistent! code)}))

(comment
  (sieve (slurp "test-resources/code.clj")))
