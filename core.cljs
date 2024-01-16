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
  (swap! ui assoc-in [:editors k] editor))
(defn reg-append-fn [append-fn]
  (swap! ui assoc :append-fn append-fn))

(defn append-editor-base [{:keys [id kl mode attrs snippet klipsettings]
                           :or {klipsettings {}}
                           :as editor}]
  (let [div (gdom/createDom "div" 
                            (clj->js (assoc attrs :id id)) 
                            (gdom/createTextNode (str snippet)))
        title (gdom/createTextNode (str "#" kl ", id: " id ", mode: " mode)]
    (gdom/insertSiblingAfter div js/klipse-container.nextSibling)
    (gdom/insertSiblingAfter title js/klipse-container.nextSibling)
    (klp/klipsify div klipsettings mode)))

(reg-append-fn append-editor-base)

(defn append-editor [{:keys [id mode attrs external-libs]
                      :or {mode "eval-clojure"}
                      :as editor}]
  (let [kl @klp/snippet-counter
        id (or id (:id attrs) (str "pou-" kl))
        data-external-libs (->> external-libs 
                             (into (conj (:external-libs @ui) 
                                         (:data-external-libs attrs)))
                             (interpose ",") 
                             (apply str))
        new-editor (merge editor {:id id :kl kl :mode mode
                                  :attrs (merge attrs {:data-external-libs data-external-libs})})]
    ((:append-fn @ui) new-editor)
    (reg-editor id new-editor)))

(defn addp [snippet & {:keys [mode attrs klipsettings external-libs] :as editor-settings}] 
  (append-editor (assoc editor-settings :snippet snippet)))

(set! js/appendSnippet #(append-editor (js->clj %)))

(def get-kl #(if (number? %) % (-> ui :editors (get %) :kl)))
                                                            
(defn call-in-editor [k method & args]
  (let [kl (get-kl k)]
    (j/apply (@kleds/editors k) method (clj->js args))))

(defn call-in-result [k method & args]
  (let [kl (get-kl k)]
    (j/apply (@kleds/result-elements k) method (clj->js args))))

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
