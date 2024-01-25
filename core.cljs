(ns pou.core
  (:require [goog.dom :as gdom]
            [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [cljs-http.client :as http]
            [klipse.plugin :as klp]
            [klipse.utils :as klu]
            [klipse.common.registry :as klreg]
            [klipse.klipse-editors :as kleds]
            [applied-science.js-interop :as j]
            [cljs.reader :refer [read-string]]
            [klipse.ui.editors.editor :as kl-ed]
            [klipse-clj.repl :as kl-repl]))

; UTILS

(def url-params #(or (klu/url-parameters) {}))

(defn process-url-params [& param-procs]
  (let [params (url-params)]
    (doseq [pp (partition 2 param-procs)]
      (when-let [p (params (first pp))]
        ((second pp) p)))))

(def decode64 #(js/atob %))
(def parse64 #(read-string (decode64 %)))
(def flatten64 #(flatten (into [] (parse64 %))))

(defn- h? [d] (.-hidden d))
(defn- th [d h?] (-> d .-hidden (set! h?)))
(defn toggle-hidden 
  ([div-id hidden?] (th (gdom/getElement div-id) hidden?))
  ([div-id] (let [d (gdom/getElement div-id)] (th d (not (h? d))))))

(defn loaded! [] 
  (toggle-hidden "pou-app" false)
  (toggle-hidden "loading" true))
(defn loading! [] 
  (toggle-hidden "loading" false)
  (toggle-hidden "pou-app" true))

; BASE STATE

(def pou (atom {:editors {}
                :id-kls {}
                :mode-options (into (sorted-set) (keys @klreg/mode-options))
                :mode-selectors (clojure.set/map-invert @klreg/selector->mode)
                :klipse-settings (js->clj js/klipse-settings)
                :external-libs {"eval-clojure" ["https://bonuoq.github.io"]}
                :uis {}
                :modules []}))

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
    (if (= ui-keyword :base)
      (. div-uis prepend div-new-ui)
      (. div-uis appendChild div-new-ui))))

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

(defn peval-str [s] (set-code 0 s))

(defn res-reset! [k resp-atom]
  (res-watch k #(reset! resp-atom %)))

(defn res-swap! [k resp-atom f & args] 
  (res-watch k #(reset! resp-atom (apply f % args))))

(defn trigger-eval [k] 
  (set-code k (call-in-editor k :getValue)))

; BASE UI

(defn- append-editor-base [{:keys [id kl intro mode attrs snippet hidden?]
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

(defn- when-klipse-ready [callback]
  (let [observer (js/MutationObserver. 
                  (fn [mutations o]
                    (let [elm (-> mutations (aget 0) .-addedNodes (aget 0))]
                      (when (= (. elm -id) "klipse-ready")
                        (.disconnect o)
                        (.remove elm)
                        (when callback (callback))))))]
    (. observer observe js/document.body #js {:childList true})))

(defn- show-hint! [cm completions]
  (let [hint-fn (partial kl-ed/list-completions completions)]
        (js/setTimeout
         (fn []
           (.showHint cm (clj->js {:hint (partial kl-ed/list-completions completions)
                                   :completeSingle true}))))))

(defn set-info! [inner-html]
  (-> "pou-info" gdom/getElement .-innerHTML (set! inner-html)))

(defn- get-token-str [cm] (-> cm (.getTokenAt (.getCursor cm)) (aget "string")))

(defn- autocomp-refer! [cm]
  (let [token-str (get-token-str cm)
        completions (map (fn [{:keys [kl id]}]
                           (clj->js {:displayText (str kl " #" id)
                                     :text (case (first token-str)
                                             "&" (get-code kl)
                                             "$" (get-result kl))}))
                         #(select-keys % [:kl :id]) (-> @pou :editors vals))]
    (show-hint! cm completions)))

(defn show-completions! [cm hint? info?]
  (let [token-str (get-token-str cm)
        pre-ns (re-find #".+?\/" token-str)
        completions-no-pre-ns (kl-repl/get-completions token-str)
        completions (if pre-ns (mapv (partial str pre-ns) completions-no-pre-ns) completions-no-pre-ns)]
    (when hint? 
      (show-hint! cm completions))
    (when info? 
      (set-info! (apply str (mapv #(str "<span 
                                        id='" % "'
                                        class='pou-completion'>" 
                                        % "</span>&nbsp;") (rest completions)))))))

(defn- over-cm-extra-keys! [cm key-map] ; override Klipse's and set POU specific  
  (apply j/assoc! (. cm getOption "extraKeys") key-map))

(defn insert-code [k code & {:keys [rel-cursor from to] :or {rel-cursor 0}}]
  (let [cm (@kleds/editors (get-kl k))
        cursor (.getCursor cm)
        from (or from (if (< 0 rel-cursor) 
                        (j/update-in! cursor [:ch] + rel-cursor) 
                        cursor))
        to (or to (if (> 0 rel-cursor) 
                    (j/update-in! cursor [:ch] + rel-cursor) 
                    cursor))]
  (.replaceRange cm code from to)))
  
(defn- cm-reg! [kl]
  (let [{:keys [mode hints?]} (-> @pou :editors (get kl))
        cm (@kleds/editors kl)]
    (when (= mode "eval-clojure")
      (over-cm-extra-keys!
       {:Tab #(show-completions! % true false)
        :Alt-Space #(peval-str (str "(doc " (get-token-str %) ")"))
        :Cmd-. #(autocomp-refer! %)})
      (. cm on "cursorActivity" #(show-completions! cm hints? (not hints?)))
      (. cm on "keyHandled" ; instead Handle extra keys merging (p/call-in-editor 1 :getOption "extraKeys")
         (fn [_ key-handled] (js/console.log (str "CodeMirror #" kl " keyHandled: " key-handled)))))))

(defn klipsify! [on-mounted on-ready] 
  (when-klipse-ready on-ready)
  (let [first-kl @klp/snippet-counter]
    (go 
     (<! (klp/init-clj (:klipse-settings @pou)))
     (when on-mounted
       (on-mounted))
     (let [last-kl (dec @klp/snippet-counter)]
       (doall (map cm-reg! (range first-kl (inc last-kl))))
       (call-in-editor last-kl :focus)))))
                           
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
  (when klipsify? (klipsify! on-mounted on-ready)))

(defn aed [snippet & {:keys [mode attrs klipsettings external-libs on-mounted on-ready] :as editor-settings}] 
  (append [(assoc editor-settings :snippet snippet)] :on-mounted on-mounted :on-ready on-ready))

(set! js/pouAppendMode (fn [mode] (aed "" :mode mode)))

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
  (fetch-gist id file #(append (read-string (str %)))))

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
           (callback (read-string edn))))))))

(defn load-module [module & {:keys [on-ready]}]
  (read-edn
   (str "https://bonuoq.github.io/pou/modules/" module ".edn")
   #(when %
      (append [%] :on-ready (fn []
                              (swap! pou update-in [:modules] conj (str module))
                              (when on-ready (on-ready)))))))

(defn module-loaded? [module]
  (-> @pou :modules (get (str module)) some?))

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
  (process-url-params :ui #(load-ui %)
                      :editor-base #(append [(parse64 %)])
                      :editors-base #(append (parse64 %))
                      :p #(aed (decode64 %))
                      :module #(load-module %)
                      :modules #(apply load-modules (parse64 %))
                      :code #(load-module 'github))
  (when-not (:ui (url-params)) (loaded!)))
