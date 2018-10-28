;;comments before ns macro

(ns resources.code)


;; # Top-level header 1

;; ## Top-level header 2
;; appearing on *multiple* lines

(def pi "Pi, at least some of it" 3.14)


;; ```clojure
;; (fn1 foo bar
;;      baz adlrin)
;; ```

(
 defn fn1
  "This is the main function used for running a pipeline. It returns a
  \"run report\" which contains information about which stages were
  ran, the start and end time of each stage, and the outcome of each
  stage, be it an actual value or an exception. This function also
  passes the run report to the mailer function that generates run or
  exception reports."
 ([x]
  ;; *stand-alone* inner comment
  (+ x x)
  (let [x 10]
    ;; let's do some printing but also make this comment artificially
    ;; long so that it ends up on multiple lines
    (println 20)
    (println 20) ;; Inner *line* comment that gets too long and takes
                 ;; up extra lines making part of it look like it's a
                 ;; stand-alone comment
    (println 20)
    (println 20))))

(defn fn2
  "This is the main function used for running a pipeline."
  ([x]
   (+ x x)))


"This is a stray string value"
