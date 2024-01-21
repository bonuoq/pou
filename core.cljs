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

(def base (atom {:editors {}
                 :id-kls {}
                 :mode-options (into (sorted-set) (keys @klreg/mode-options))
                 :mode-selectors (clojure.set/map-invert @klreg/selector->mode)
                 :klipse-settings (js->clj js/klipse-settings)
                 :external-libs {"eval-clojure" ["https://bonuoq.github.io"]}
                 :append-fn #(str "Not defined, cannot append:" %)
                 :auto-klipsify true}))

(add-watch klreg/mode-options :re-frame-reg-mode-options 
           #(swap! base assoc :mode-options (keys %4)))
(add-watch klreg/selector->mode :re-frame-reg-mode-selectors 
           #(swap! base assoc :mode-selectors (clojure.set/map-invert %4)))


(defn reg-editor [{:keys [id kl] :as editor}]
  (swap! base assoc-in [:editors kl] editor)
  (swap! base assoc-in [:id-kls id] kl))

(defn reg-append-fn [append-fn]
  (swap! base assoc :append-fn append-fn))

; EDITOR FUNCTIONS

(def get-kl #(if (number? %) % (-> @base :id-kls %)))
                                                            
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

(reg-append-fn append-editor-base)

(defn mode->class [mode]
  (->> (get (:mode-selectors @base) mode)
    (get (:klipse-settings @base))
    rest
    (apply str)))

(defn wait-for-eval [callback]
  (let [observer (js/MutationObserver. 
                  (fn [mutations observer]
                    (doseq [m mutations
                            :let [nodes (.-addedNodes m)]]
                      (doseq [n nodes]
                        (when (= (.-id node) "klipse-ready")
                          (on-ready)
                          (.disconnect observer))))))]
    (. observer observe js/document.body #js {:childList true})))

(defn klipsify! [on-ready]
  (go 
   (<! (klp/init-clj (:klipse-settings @base)))
   (when on-ready (wait-for-eval on-ready))))
     
                           
(defn append [editors & {:keys [klipsify? on-mounted on-ready] 
                         :or {klipsify? (:auto-klipsify @base)}}]
  (dotimes [n (count editors)]
   (let [{:keys [id mode attrs external-libs]
          :or {mode "eval-clojure" klipsify? true}
          :as editor} (get editors n)
         kl (+ @klp/snippet-counter n)
         id (or id (:id attrs) (str "pou-" kl))
         data-external-libs (->> external-libs
                              (into (-> @base :external-libs (get mode)))
                              (cons (:data-external-libs attrs))
                              (filter some?)
                              distinct
                              (interpose ",")
                              (apply str)
                              not-empty)
         new-editor (merge editor {:id id :kl kl :mode mode
                                   :attrs (when data-external-libs
                                            (merge attrs {:id id :class (mode->class mode)
                                                          :data-external-libs data-external-libs}))})]
     (reg-editor new-editor)
     ((:append-fn @base) new-editor)))
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
  (let [array (-> @base :editors vals)]
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

(defn load-module [module & {:keys on-ready}]
  (read-edn
   (str "https://bonuoq.github.io/pou/modules/" module ".edn")
   #(append [% :on-ready on-ready])))

(defn load-modules-async [& modules]
  (go
   (doseq [m modules] 
     (<p! (load-module m)))))

(defn load-ui [ui]
  (loading!)
  (load-module (str "ui/" ui) :on-ready loaded!))

; INIT
        
(process-url-params :u #(load-ui %)
                    :o #(append (parse64 %))
                    :p #(aed (decode64 %))
                    :d #(append [(parse64 %)])
                    :n #(apply load-modules-async (flatten64 %)))

(when-not (:u url-params) (loaded!))

