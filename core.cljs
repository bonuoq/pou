(ns pou.core
  (:require [goog.dom :as gdom]
            [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [klipse.plugin :as klp]
            [klipse.utils :as klu]
            [klipse.common.registry :as klreg]
            [klipse.klipse-editors :as kleds]
            [applied-science.js-interop :as j]
            [cljs.reader :refer [read-string]]))

; UTILS

(defonce url-params (or (klu/url-parameters) {}))

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

(defn loaded! [] (toggle-hidden "loading" true))
(defn loading! [] (toggle-hidden "loading" false))

; BASE STATE

(def pou (atom {:editors {}
                :id-kls {}
                :mode-options (into (sorted-set) (keys @klreg/mode-options))
                :mode-selectors (clojure.set/map-invert @klreg/selector->mode)
                :klipse-settings (js->clj js/klipse-settings)
                :external-libs {"eval-clojure" ["https://bonuoq.github.io"]}
                :uis {}}))

(add-watch klreg/mode-options :re-frame-reg-mode-options 
           #(swap! pou assoc :mode-options (keys %4)))
(add-watch klreg/selector->mode :re-frame-reg-mode-selectors 
           #(swap! pou assoc :mode-selectors (clojure.set/map-invert %4)))


(defn reg-editor [{:keys [id kl] :as editor}]
  (swap! pou assoc-in [:editors kl] editor)
  (swap! pou assoc-in [:id-kls id] kl))

(defn reg-ui [ui-keyword {:keys [append-fn klipsify?] :as ui}]
  (let [div-uis (gdom/getElement "uis")
        div-new-ui (gdom/createDom "div" (clj->js {:class "pou-ui" :id ui-keyword}))]
    (swap! pou assoc-in [:uis ui-keyword] ui)
    (.appendChild div-uis div-new-ui)))

; EDITOR FUNCTIONS

(def get-kl #(if (number? %) % (-> @pou :id-kls %)))
                                                            
(defn call-in-editor [k method & args]
  (j/apply (@kleds/editors (get-kl k)) method (clj->js args)))

(defn call-in-result [k method & args]
  (j/apply (@kleds/result-elements (get-kl k)) method (clj->js args)))

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

; BASE UI

(defn append-editor-base [{:keys [id kl intro mode attrs snippet hidden?]
                           :or {klipsettings {}}
                           :as editor}]
  (let [base (gdom/getElement "base")
        klipse (gdom/createDom "div" (clj->js attrs) (str snippet))
        text (gdom/createDom "p" "pou-intro" (str "#" kl "> " (or 
                                                               intro
                                                               (str "id: " id ", mode: " mode))))
        wrapper (gdom/createDom "div" (clj->js {:id id
                                                :class "pou-wrapper" 
                                                :style {:display (if hidden? "none" "block")}})
                                text klipse)]
    (.appendChild base wrapper)))

(reg-ui :base {:append-fn append-editor-base
               :klipsify? true})

(defn mode->class [mode]
  (->> (get (:mode-selectors @pou) mode)
    (get (:klipse-settings @pou))
    rest
    (apply str)))

(defn when-klipse-ready [callback]
  (let [observer (js/MutationObserver. 
                  (fn [mutations o]
                    (let [id (-> mutations (aget 0) .-addedNodes (aget 0) .-id)]
                      (when (= id "klipse-ready")
                        (.disconnect o)
                        (callback)))))]
    (. observer observe js/document.body #js {:childList true})))

(defn klipsify! [on-ready]
  (when on-ready 
    (when-klipse-ready on-ready))
  (go 
   (<! (klp/init-clj (:klipse-settings @pou)))))
                           
(defn append [editors & {:keys [ui klipsify? on-mounted on-ready] 
                         :or {ui :base 
                              klipsify? (-> @pou :uis ui :klipsify?)}}]
  (dotimes [n (count editors)]
   (let [scope-ui ui
         {:keys [id mode attrs external-libs ui]
          :or {mode "eval-clojure" ui scope-ui}
          :as editor} (get editors n)
         kl (+ @klp/snippet-counter n)
         id (or id (:id attrs) (str "pou-" kl))
         data-external-libs (->> external-libs
                              (into (-> @pou :external-libs (get mode)))
                              (cons (:data-external-libs attrs))
                              (filter some?)
                              distinct
                              (interpose ",")
                              (apply str)
                              not-empty)
         new-editor (merge editor {:id id :kl kl :mode mode
                                   :attrs (merge attrs 
                                                 {:id id :class (mode->class mode)}
                                                 (when data-external-libs 
                                                   {:data-external-libs data-external-libs}))})]
     (reg-editor new-editor)
     (let [append-fn (-> @pou :uis ui :append-fn)]
       (append-fn new-editor))))
  (when klipsify? 
    (go
     (<! (klipsify! on-ready))
     (when on-mounted (on-mounted))
     (call-in-editor (dec @klp/snippet-counter) :focus))))

(defn aed [snippet & {:keys [mode attrs klipsettings external-libs] :as editor-settings}] 
  (append [(assoc editor-settings :snippet snippet)]))

; LOAD & EXPORT FNS

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

(defn append-gist [{:keys [id file]}]
  (fetch-gist id file #(append (cljs.reader/read-string (str %)))))

(defn editors-array []
  (let [array (-> @pou :editors vals)]
    (->> array
      (map #(assoc % :snippet (get-code (:kl %))))
      (map #(dissoc % :kl)))))

(defn read-edn [url callback]
  (-> (str url)
    (fetch-url
     #(-> (.text %)
        (.then 
         (fn [edn] 
           (callback (cljs.reader/read-string edn))))))))

(defn load-module [module & {:keys [on-ready]}]
  (read-edn
   (str "https://bonuoq.github.io/pou/modules/" module ".edn")
   #(append [%] :on-ready on-ready)))

(defn load-module-chain [chain]
  (load-module (first chain) :on-ready #(load-module-chain (rest chain))))

(defn load-modules [& modules]
  (go
   (doseq [m modules]
     (if (coll? m)
       (load-module-chain m)
       (<p! (load-module m))))))

(defn load-ui [ui]
  (loading!)
  (load-module (str "ui/" ui) :on-ready #(loaded!)))

(defn init! []
  (process-url-params :u #(load-ui %)
                      :o #(append (parse64 %))
                      :p #(aed (decode64 %))
                      :d #(append [(parse64 %)])
                      :n #(apply load-modules-async (flatten64 %)))
  (when-not (:u url-params) (loaded!)))

