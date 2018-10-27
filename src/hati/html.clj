(ns hati.html
  (:require [glow.core :as glow]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.string :as str]
            [hati.utils :refer [at-least]])
  (:import [java.io ByteArrayInputStream]))

(defn- clojure-block? [el]
  (at-least el
            {:tag     :pre
             :content [{:tag   :code
                        :attrs {:class "language-clojure"}}]}))

(defn highlight-clojure [html]
  (let [zipper (-> (str "<html>" html "</html>")
                   .getBytes
                   ByteArrayInputStream.
                   xml/parse
                   zip/xml-zip)
        hi     (loop [loc zipper]
                 (if (zip/end? loc)
                   (zip/root loc)
                   (if (-> loc zip/node clojure-block?)
                     (-> loc (zip/edit update :content
                                       (fn [lines]
                                         [(glow/highlight-html
                                           (str/join "\n" lines))])) zip/next recur)
                     (-> loc zip/next recur))))]
    (-> (with-out-str (xml/emit-element hi))
        (str/replace "<html>\n" "")
        (str/replace "</html>" ""))))
