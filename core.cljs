(ns pou.core
  (:require [goog.dom :as gdom]
            [klipse.plugin :as klp]
            [applied-science.js-interop :as j]))
    
(def pou (atom {:params (or (js/klipse.utils.url-parameters) {})
                :editors {:main {:idx 0
                                 :mode "eval-clojure"
                                 :cm #(aget js/klipse-editors 0)
                                 :res #(aget js/klipse-results 0)}}}))

(defn append-editor [& {:keys [mode attrs snippet klipsettings] :or {mode "eval-clojure" klipsettings {}} :as varkeyargs}]
  (let [div (gdom/createDom "div" (clj->js attrs) (gdom/createTextNode (str snippet)))]
    (gdom/insertSiblingAfter div js/klipse-container)
    (klp/klipsify div klipsettings mode))
  (let [c (count (@pou :editors))]
    (swap! pou assoc-in 
           [:editors (or (:id attrs) c)]
           (merge varkeyargs {:idx c
                              :mode mode
                              :cm #(aget js/klipse-editors c)
                              :res #(aget js/klipse-results c)}))))
                                                            
(defn cm [k method & args]
  (j/apply ((-> @pou :editors (get k) :cm)) method (clj->js args)))

(defn fetch-url-text [url callback]
  (-> (str url) js/fetch
    (.then (fn [r] (.. r text (then callback))))))

(defn eval-url [url & {:keys [editor append-code clear-code] :or {editor :main append-code ""}}]
  (fetch-url-text url #(cm editor :setValue (str % "\n" append-code
                                                 (when clear-code (str "\n(pou.user/cm " editor " :setValue \"" clear-code "\")"))))))
    
(defn fetch-gist [id file callback]
  (-> (str "https://api.github.com/gists/" id) js/fetch
    (.then (fn [r]
       (.then (.json r)
              (fn [json]
                (callback (-> (js->clj json :keywordize-keys true) :files ((keyword file)) :content))))))))
    
(defn eval-gist [& {:keys [id file editor append-code clear-code] :or {editor :main append-code ""}}]
  (fetch-gist id file #(cm editor :setValue (str % "\n" append-code
                                                 (when clear-code (str "\n(pou.user/cm " editor " :setValue \"" clear-code "\")"))))))

(defn eval-gists [f & r] 
  (eval-gist :id (:id f) :file (:file f) :append-code (if (not (empty? (rest r))) `(apply eval-gists ~r) 
                                                                                  `(apply eval-gist (mapcat seq ~(first r))))))

(def ns-info (list (symbol (str "ns " *ns*)) (map #(list (first %) (-> % second meta :arglists)) (eval `(ns-publics '~(symbol (str *ns*)))))))
