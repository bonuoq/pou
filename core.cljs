(ns pou.core
  (:require [goog.dom :as gdom]
            [klipse.plugin :as klp]
            [klipse.utils :as klu]
            [klipse.klipse-editors :as kleds]
            [applied-science.js-interop :as j]
            [cljs.reader :refer [read-string]]))

(def url-params (or (klu/url-parameters) {}))

(defn append-editor [{:keys [mode attrs snippet klipsettings external-libs] 
                      :or {mode "eval-clojure" klipsettings {} external-libs ["https://bonuoq.github.io"]}}]
  (let [data-external-libs (apply str (interpose "," external-libs))
        div (gdom/createDom 
             "div" 
             (clj->js (assoc attrs :data-external-libs data-external-libs)) 
             (gdom/createTextNode (str snippet)))
        idx @klp/snippet-counter
        label (gdom/createTextNode (str "[" idx "] mode: " mode))]
    (gdom/insertSiblingAfter div js/klipse-container.nextSibling)
    (gdom/insertSiblingAfter label js/klipse-container.nextSibling)
    (klp/klipsify div klipsettings mode)))

(defn addp [snippet & {:keys [mode attrs snippet klipsettings] :as editor-settings}] 
  (append-editor (assoc (apply hash-map editor-settings) :snippet snippet)))
                                                            
(defn call-in-editor [k method & args]
  (j/apply (@kleds/editors k) method (clj->js args)))

(defn call-in-result [k method & args]
  (j/apply (@kleds/result-elements k) method (clj->js args)))

(defn set-code [k value] (call-in-editor k :setValue value))

(defn get-result [k] (call-in-result k :getValue))

(defn on-res-change [k callback]
  (let [cb-handler (fn [r] (callback (.getValue r)))]
    (call-in-result k :on "change" cb-handler)
    cb-handler))

(defn off-res-change [k handler] (call-in-result k :off "change" handler))

(defn res-watch [k cb]
  (cb (get-result k))
  (on-res-change k cb))

(defn res-reset! [k resp-atom]
  (res-watch k #(reset! resp-atom %)))

(defn res-swap! [k resp-atom f & args] 
  (res-watch k #(reset! resp-atom (apply f % args))))

(defn trigger-eval [k] 
  (set-code k (call-in-editor k :getValue)))

(defn fetch-url [url callback]
  (-> (str url) js/fetch
    (.then #(callback %))))
    
(defn fetch-gist [id file callback]
  (-> (str "https://api.github.com/gists/" id)
    (fetch-url 
     #(-> (.json %)
        (.then
          (fn [json]
            (callback (-> (js->clj json :keywordize-keys true) :files ((keyword file)) :content))))))))

(defn append-gist [{:keys [id file mode attrs klipsettings append-code] 
                    :or {mode "eval-clojure" append-code ""} 
                    :as editor}]
  (fetch-gist id file #(addp (str % append-code) editor)))

(defn process-url-params [& param-procs]
  (doseq [pp (partition 2 param-procs)]
    (when-let [p (url-params (first pp))]
      ((second pp) (js/atob p)))))

(def decode64 #(js/atob %))
(def parse64 #(read-string (decode64 %)))
(def flatten64 #(flatten (into [] (parse64 %))))
