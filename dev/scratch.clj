(ns scratch
  (:require [hati.parser :as parser]
            [hati.markdown :as md]
            [hati.html :as html]
            [clojure.string :as str]))

;;(println (node/string form))

;;(def form2 (p/parse-string-all (slurp "/Volumes/work/bsq/vittle-diagnostics/src/monitor/pipeline.clj")))
;;(println (node/string form2))

;;(def zz (z/of-string (slurp "/Volumes/work/bsq/vittle-diagnostics/src/monitor/pipeline.clj")))
;;(def zz (z/of-string (slurp "test-resources/code.clj")))

(comment
  (loop [loc zz]
    (when-not (z/end? loc)
      (let [n (z/node loc)]
        ;;(println n)
        (when (node/comment? n)
          (print (node/string n)))
        (recur (zip/next loc)))))
  )

(comment
  (do
    (def zz (z/of-string (slurp "test-resources/code.clj")))
    (loop [loc zz]
      (when-not (z/end? loc)
        (let [n (z/node loc)]1
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
          (when (or (docstring? loc)1
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

(comment
  (do
   (def p (parser/extract-comments (slurp "test-resources/code.clj")))1
   (print (html/highlight-clojure (md/->html (md/parse (str/join (map :string p)))))))
  )
