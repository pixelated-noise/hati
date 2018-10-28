(ns scratch
  (:require [hati.parser :as parser]
            [hati.markdown :as md]
            [hati.html :as html]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

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
  (pp/pprint (:prose (parser/sieve (slurp "test-resources/code.clj"))))

  (do
   (def p (parser/sieve (slurp "test-resources/code.clj")))
   (print (html/highlight-clojure (md/->html (md/parse (str/join (map :string p)))))))
  )
