(ns hati.markdown
  (:import [com.vladsch.flexmark.util.options MutableDataSet]
           [com.vladsch.flexmark.parser Parser]
           [com.vladsch.flexmark.html HtmlRenderer]))

(defn render [s]
  (let [options  (MutableDataSet.)
        parser   (-> (Parser/builder options) .build)
        renderer (-> (HtmlRenderer/builder options) .build)
        doc      (.parse parser s)]
    (.render renderer doc)))
