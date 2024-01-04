(ns pou.core
  (:require [goog.dom :as gdom]
            [klipse.plugin :as klp]
            [klipse.utils :as klu]
            [klipse.klipse-editors :as kleds]
            [applied-science.js-interop :as j]))

(def url-params (or (klu/url-parameters) {}))

(defn append-editor [& {:keys [mode attrs snippet klipsettings] :or {mode "eval-clojure" klipsettings {}} :as editor-map}]
  (let [editor (update-in editor-map [:attrs :data-external-libs] "https://bonuoq.github.io")
        div (gdom/createDom "div" (clj->js (editor _attrs)) (gdom/createTextNode (str snippet)))
        idx @klp/snippet-counter
        label (gdom/createTextNode (str "[" idx "] mode: " mode))]
    (gdom/insertSiblingAfter div js/klipse-container.nextSibling)
    (gdom/insertSiblingAfter label js/klipse-container.nextSibling)
    (klp/klipsify div klipsettings mode)))
                                                            
(defn call-in [k method & args]
  (j/apply (@kleds/editors k) method (clj->js args)))

(defn set-code [k value] call-in k :setValue value)

(defn get-resp [k] (j/call (@kleds/result-elements k) :getValue))

(defn fetch-url-text [url callback]
  (-> (str url) js/fetch
    (.then (fn [r] (.. r text (then callback))))))

(defn eval-url [url & {:keys [editor append-code clear-code] :or {editor 0 append-code ""}}]
  (fetch-url-text url #(set-code editor (str % "\n" append-code
                                             (when clear-code (str "\n(pou.user/set-code " editor " \"" clear-code "\")"))))))
    
(defn fetch-gist [id file callback]
  (-> (str "https://api.github.com/gists/" id) js/fetch
    (.then (fn [r]
       (.then (.json r)
              (fn [json]
                (callback (-> (js->clj json :keywordize-keys true) :files ((keyword file)) :content))))))))
    
(defn eval-gist [& {:keys [id file editor append-code clear-code] :or {editor 0 append-code ""}}]
  (fetch-gist id file #(set-code editor (str % "\n" append-code
                                             (when clear-code (str "\n(pou.user/set-code " editor " \"" clear-code "\")"))))))

(defn eval-gists [f & r]
  (eval-gist :id (:id f) :file (:file f) :editor (:editor f)
             :append-code (if (not (empty? (rest r))) 
                            `(apply eval-gists ~r) 
                            `(apply eval-gist (mapcat seq ~(first r))))))

