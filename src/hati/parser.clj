(ns hati.parser
  (:require [rewrite-clj.parser :as p]
            [rewrite-clj.node :as node]
            [rewrite-clj.node.protocols :as node.protocols]
            [rewrite-clj.zip :as z]
            [rewrite-clj.zip.base :as zip.base]
            [clojure.zip :as zip]
            [clojure.string :as str]))

;; A node of zero length

(defrecord NoneNode []
  node.protocols/Node
  (tag [_] :none)
  (printable-only? [_] true)
  (sexpr [_] (throw (UnsupportedOperationException.)))
  (length [_] 0)
  (string [_] "")

  Object
  (toString [this] ""))


(defn- drop-ws
  "Drop all whitespace or comments from a collection of nodes."
  [coll]
  (drop-while node/whitespace-or-comment? coll))

(defn- dead-space?
  "Check if a node is whitespace, comment or linebreak."
  [node]
  (or (node/whitespace-or-comment? node)
      (node/linebreak? node)))

(defn- left-skip-dead-space
  "Like zip/left, but skipping all dead space (whitespaces, comments,
  linebreaks)."
  [loc]
  (first (drop-while (comp dead-space? zip/node)
                     (iterate zip/left (zip/left loc)))))

(defn- left-skip-ws
  "Like zip/left, but skipping commas and whitespaces. Does not skip
  newlines."
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

(defn- blank-line?
  "A whitespace location with newlines or nils on either side."
  [loc]
  (when (-> loc zip/node node/whitespace?)
    (let [left  (zip/left loc)
          right (zip/right loc)]
      (and (or (nil? left) (-> left zip/node node/linebreak?))
           (or (nil? right) (-> right zip/node node/linebreak?))))))

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

(defn- sexp-comment?
  "A sexp comment is a comment that starts after a sexp, on the same
  line. The theory is that it is providing a commentary on the sexp."
  [loc]
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


(defn- strip-comment-syntax [s]
  (str/replace s #"^;+\s?" ""))


(defn- prose-info
  "Construct a map about the prose pointed to by the zipper location."
  [loc]
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
                         (strip-comment-syntax (node/string n)))})))

;; This section is about aggregating consecutive prose maps into one
;; for easier processing.

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


(defn- newlines-only
  "Convert a node's string into a newlines node depending on how many
  newlines the original string contained (if any). This is necessary
  because some rewrite-clj includes newline characters into comment
  nodes instead of making separate newline nodes."
  [node]
  (let [c (-> node node/string (str/replace #"[^\n]" "") count)]
    (if (zero? c)
      (NoneNode.)
      (node/newlines c))))

(defn- strip-prose
  "Given a node, strip all its non-code and non-newline nodes. The
  newline characters of the stripped nodes are maintained (using
  `newlines-only`) to avoid breaking formatting."
  [node]
  (let [orig-meta (meta node)]
    (loop [loc (z/of-string (node/string node))] ;;TODO pretty wasteful
      (if (zip/end? loc)
        (zip/root loc)
        (if (#{:code :newline} (node-type loc)) (-> loc zip/next recur)
            (-> loc (zip/edit newlines-only) zip/next recur))))))


(defn- remove-blank-lines
  [loc]
  (loop [loc (zip.base/edn loc)]
    (cond (zip/end? loc)    (zip/root loc)
          (blank-line? loc) (-> loc zip/remove zip/next recur)
          :else             (-> loc zip/next recur))))


(defn sieve [code-string]
  (let [prose (transient [])
        code  (transient [])]
    (loop [loc (z/of-string code-string)]
      (when-not (z/end? loc)
        (let [tt (node-type loc)]
          (cond (= :newline tt)
                (do
                  (conj! prose (prose-info loc))
                  ;;(conj! code (prose-info loc))
                  )

                (and (= :code tt)
                     (top-level? loc))
                (conj! code (remove-blank-lines (strip-prose (zip/node loc))))

                :else
                (conj! prose (prose-info loc))))
        (recur (zip/next loc))))
    {:prose (agg-consecutive-prose (persistent! prose))
     :code  (persistent! code)}))

(comment
  (sieve (slurp "test-resources/code.clj")))
