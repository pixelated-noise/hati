(ns resources.code)


;; = Top-level comment

;; == Top-level comment 2
;; appearing on multiple lines

(
 defn fn1
  "This is the main function used for running a pipeline. It returns a
  \"run report\" which contains information about which stages were
  ran, the start and end time of each stage, and the outcome of each
  stage, be it an actual value or an exception. This function also
  passes the run report to the mailer function that generates run or
  exception reports."
 ([x]
  ;; function inline-comment
  (+ x x)))

(defn fn2
  "This is the main function used for running a pipeline."
  ([x]
   (+ x x)))


"This is a stray string value"
