(ns pou.core
  (:require [goog.dom :as gdom]
            [klipse.plugin :as klp]
            [klipse.utils :as klu]
            [klipse.klipse-editors :as kleds]
            [applied-science.js-interop :as j]
            [cljs.reader :refer [read-string]]))

(def url-params (or (klu/url-parameters) {}))

(defn process-url-params [& param-procs]
  (doseq [pp (partition 2 param-procs)]
    (when-let [p (url-params (first pp))]
      ((second pp) p))))

(def decode64 #(js/atob %))
(def parse64 #(read-string (decode64 %)))
(def flatten64 #(flatten (into [] (parse64 %))))

(defn- h? [d] (.-hidden d))
(defn- th [d h?] (-> d .-hidden (set! h?)))
(defn toggle-hidden 
  ([div-id hidden?] (th (gdom/getElement div-id) hidden?))
  ([div-id] (let [d (gdom/getElement div-id)] (th d (not (h? d))))))

(set! js/toggleHidden (partial toggle-hidden))

(def ui (atom {:editors {}
               :external-libs ["https://bonuoq.github.io"]
               :append-fn #(str "Not defined, cannot append:" %)}))

(defn reg-editor [k editor]
  (swap! ui assoc-in [:editors] k editor))
(defn reg-append-fn [append-fn]
  (swap! ui assoc :append-fn append-fn))

(defn append-editor-base [{:keys [id mode attrs snippet klipsettings] :as editor}]
  (let [k @klp/snippet-counter
        id (or id (str "pou-klipse-" k))
        div (gdom/createDom "div" 
                            (clj->js (assoc attrs :id id)) 
                            (gdom/createTextNode (str snippet)))
        title (gdom/createTextNode (str "# " k ", id: " id ", mode: " mode))]
    (gdom/insertSiblingAfter div js/klipse-container.nextSibling)
    (gdom/insertSiblingAfter title js/klipse-container.nextSibling)
    (reg-editor k editor)
    (klp/klipsify div klipsettings mode)))

(reg-append-fn append-editor-base)

(defn append-editor [{:keys [mode attrs klipsettings external-libs] :as editor}]
  (let [data-external-libs (->> external-libs 
                             (into (:external-libs @ui)) 
                             (interpose ",") 
                             (apply str))]
    ((:append-fn @ui) (merge editor {:attrs (merge attrs {:data-external-libs data-external-libs})
                                     :mode (or mode "eval-clojure")
                                     :klipsettings (or klipsettings {})}))))

(defn addp [snippet & {:keys [mode attrs klipsettings external-libs] :as editor-settings}] 
  (append-editor (assoc editor-settings :snippet snippet)))

(set! js/appendSnippet #(addp %1 :mode %2))
                                                            
(defn call-in-editor [k method & args]
  (j/apply (@kleds/editors k) method (clj->js args)))

(defn call-in-result [k method & args]
  (j/apply (@kleds/result-elements k) method (clj->js args)))

(defn set-code [k value] (call-in-editor k :setValue value))

(defn get-code [k] (call-in-editor k :getValue))

(defn get-result [k] (call-in-result k :getValue))

(defn on-code-change [k callback]
  (let [cb-handler (fn [cm] (callback (.getValue cm)))]
    (call-in-editor k :on "change" cb-handler)
    cb-handler))

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

(defn read-edn [url callback]
  (-> (str url)
    (fetch-url
     #(-> (.text %)
        (.then 
         (fn [edn] 
           (callback (cljs.reader/read-string edn))))))))

(defn load-module [module]
  (-> (str "https://bonuoq.github.io/pou/modules/" module ".edn")
    (read-edn
     #(append-editor %))))

(defn load-modules [& modules]
  (doseq [module modules] (load-module module)))

(defn load-ui [ui]
  (load-module (str "ui/" ui)))
    
(defn fetch-gist [id file callback]
  (-> (str "https://api.github.com/gists/" id)
    (fetch-url 
     #(-> (.json %)
        (.then
          (fn [json]
            (callback (-> (js->clj json :keywordize-keys true) :files ((keyword file)) :content))))))))

(defn append-gist [{:keys [id file mode attrs klipsettings append-code] 
                    :or {append-code ""} 
                    :as editor}]
  (fetch-gist id file #(addp (str % append-code) editor)))

(defn append-gists [& gists]
  (doseq [gist gists] (append-gist gist)))
