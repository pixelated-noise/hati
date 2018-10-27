(ns hati.markdown
  (:import [com.vladsch.flexmark.util.options MutableDataSet]
           [com.vladsch.flexmark.parser Parser]
           [com.vladsch.flexmark.html HtmlRenderer]))

;; https://github.com/vsch/flexmark-java/wiki/Attributes-Extension

(defn parse [s]
  (let [options (MutableDataSet.)
        parser  (-> (Parser/builder options) .build)]
    (.parse parser s)))

(defn ->html [doc]
  (let [options  (MutableDataSet.)
        renderer (-> (HtmlRenderer/builder options) .build)]
    (.render renderer doc)))
