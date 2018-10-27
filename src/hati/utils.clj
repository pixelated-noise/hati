(ns hati.utils)

(defprotocol HatiComparable
  (at-least [super sub]))

(extend-protocol HatiComparable
  nil
  (at-least [_ _] false)

  Object
  (at-least [super sub] (= super sub))

  clojure.lang.IPersistentVector
  (at-least [super sub]
    (every? true? (map at-least super sub)))

  clojure.lang.IPersistentList
  (at-least [super sub]
    (every? true? (map at-least super sub)))

  clojure.lang.IPersistentMap
  (at-least [super sub]
    (every? true?
     (for [[k v] sub]
       (at-least (super k) v)))))
