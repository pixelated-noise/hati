(ns hati.core
  (:require [rewrite-clj.parser :as p]
            [rewrite-clj.node :as node]
            [rewrite-clj.zip :as z]
            [clojure.zip :as zip]
            [clojure.string :as str]))


(def form (p/parse-string-all (str "(defn my-function \"Multiplies \nby 3\" [a]\n"
                                   "  (* a 3))")))
;;(println (node/string form))

(def form2 (p/parse-string-all (slurp "/Volumes/work/bsq/vittle-diagnostics/src/monitor/pipeline.clj")))
;;(println (node/string form2))

;;(def zz (z/of-string (slurp "/Volumes/work/bsq/vittle-diagnostics/src/monitor/pipeline.clj")))
(def zz (z/of-string (slurp "test-resources/code.clj")))

(comment
  (loop [loc zz]
    (when-not (z/end? loc)
      (let [n (z/node loc)]
        ;;(println n)
        (when (node/comment? n)
          (print (node/string n)))
        (recur (zip/next loc)))))
  )

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

(defn- strip-comment [s]
  (str/replace s #"^;+\s*" ""))


(defn extract-comments [code-string]
  (let [comments (transient [])
        zz       (z/of-string code-string)]
    (loop [loc zz]
      (when-not (z/end? loc)
        (let [n         (z/node loc)
              node-type (cond (docstring? loc)  :docstring
                              (node/comment? n) :comment
                              (newline? loc)    :newline
                              :else             :other)]
          (when-not (= node-type :other)
            (conj!
             comments
             (merge (meta n)
                    (when (= node-type :docstring)
                      {:docstring-of ""})
                    {:tag          (node/tag n)
                     :type         node-type
                     :string       (if (= node-type :docstring)
                                     (read-string (node/string n))
                                     (strip-comment (node/string n)))})))
          (recur (zip/next loc)))))
    (persistent! comments)))

(comment
  (extract-comments (slurp "test-resources/code.clj")))

(comment
  (do
    (def zz (z/of-string (slurp "test-resources/code.clj")))
    (loop [loc zz]
      (when-not (z/end? loc)
        (let [n (z/node loc)]
          (prn n)
          (recur (zip/next loc))))))
  )

(comment
  (do
    (def zz (z/of-string (slurp "test-resources/code.clj")))
    (loop [loc zz]
      (when-not (z/end? loc)
        (let [n (z/node loc)]
          ;;(prn n 'SEXPR (node/sexpr n))
          ;;(prn (node/tag n) '--> (node/string n))
          ;;(when (defn-list? n) (prn 'DEFN!))
          (when (or (docstring? loc)
                    (node/comment? n)
                    (newline? loc))
            (prn (merge (meta n)
                        {:tag          (node/tag n)
                         :docstring-of ""
                         :string       (if (= :multi-line (node/tag n))
                                         (read-string (node/string n))
                                         (node/string n))}))
            ;; (if (= :multi-line (node/tag n))
            ;;   (println (read-string (node/string n)))
            ;;   (println (node/string n)))
            )
          (recur (zip/next loc))))))
  )
