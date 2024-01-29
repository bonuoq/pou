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

(defn toggle-hidden 
  ([query-selector hidden?] 
   (j/assoc! (js/document.querySelector query-selector) :hidden hidden?))
  ([query-selector] 
   (j/update! (js/document.querySelector query-selector) :hidden not)))

(defn loaded! [] 
  (toggle-hidden "#pou-app" false)
  (toggle-hidden "#loading" true))
(defn loading! [] 
  (toggle-hidden "#loading" false)
  (toggle-hidden "#pou-app" true))

; BASE STATE

(def pou (atom {:editors {}
                :id-kls {}
                :mode-options (into (sorted-set) (keys @klreg/mode-options))
                :mode-selectors (clojure.set/map-invert @klreg/selector->mode)
                :klipse-settings (js->clj js/klipse-settings)
                :external-libs {"eval-clojure" ["https://bonuoq.github.io"]}
                :uis {}
                :modules []}))

(add-watch klreg/mode-options :reg-mode-options 
           #(swap! pou assoc :mode-options (keys %4)))
(add-watch klreg/selector->mode :reg-mode-selectors
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

(def get-id #(if (number? %) (-> @pou :editors (get %) :id) %))

(defn get-cm [id & {:keys [n] :or {n 0}}]
  (-> (str "#" id " .CodeMirror") js/document.querySelectorAll (aget n) .-CodeMirror))
                                                            
(defn call-in-editor [k method & args]
  (j/apply 
   (or (@kleds/editors (get-kl k))
       (get-cm (get-id k)))
   method (clj->js args)))

(defn call-in-result [k method & args]
  (j/apply 
   (or (@kleds/result-elements (get-kl k))
       (get-cm (get-id k) 1))
   method (clj->js args)))

(defn set-code [k value] (call-in-editor k :setValue (str value)))

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

(defn res-unwatch [k handler] (off-res-change k handler))

(defn res-reset! [k resp-atom]
  (res-watch k #(reset! resp-atom %)))

(defn res-swap! [k resp-atom f & args] 
  (res-watch k #(reset! resp-atom (apply f % args))))

(defn eval-fn [k] (partial (aget (call-in-editor (get-kl k) :getOption "extraKeys") "Cmd-Enter")))

(defn eval-editor [k] ((eval-fn k)))

(defn set-code-eval [k code]
  (do
    (set-code k code)
    (eval-editor k)))

(defn eval-callback [k code callback] 
  (do
    (res-watch k callback)
    (set-code-eval k code)))

(defn peval-str [s] (set-code-eval 0 s))

; DOM & AJAX HELPERS

(defn sel-parent [selector] 
  (last (re-find #"(.*) " selector)))
(defn sel-child [selector] 
  (or (last (re-find #" (.*)" selector)) selector))
(defn sel-child-tag [selector] 
  (not-empty (re-find #"[^\.#]*" (sel-child selector))))
(defn sel-child-id [selector]
  (when-let [id-find (re-find #"#[^\.]*" (sel-child selector))]
    (subs id-find 1)))
(defn sel-child-class [selector]
  (when-let [class-find (re-find #"\.[^#]*" (sel-child selector))]
    (clojure.string/replace (subs class-find 1) "." " ")))

(defn dom-select [selector-or-element]
  (if (string? selector-or-element)
    (js/document.querySelector selector-or-element)
    selector-or-element))
(defn dom-all [selector]
  (js/document.querySelectorAll selector))

(defn dom-string [s]
  (.createContextualFragment (js/document.createRange) s))
(defn dom-any [any]
  (condp apply [any]
    string? (dom-string any)    
    vector? (str "parse-hiccup! " any)
    any))

(defn dom-create [selector {:as attrs} content]
  (let [tag (sel-child-tag selector)
        as (merge {:id (sel-child-id selector) :class (sel-child-class selector)} attrs)
        children (map dom-any content)]
    (apply gdom/createDom tag (clj->js as) (clj->js children))))

(defn dom [selector & {:keys [attrs parent content map-siblings replace? separator]}]
  (let [p (dom-select (or (sel-parent selector) parent))
        sibling-fn #(dom-create selector (merge attrs (second %)) (into (first %) content))
        elms (if map-siblings
               (->> map-siblings
                 (map sibling-fn)
                 ((if separator 
                    (partial interpose separator)
                    identity)))
               [(dom-create selector attrs content)])]
    (if p
      (if replace?
        (j/apply p :replaceChildren (clj->js elms))
        (j/apply p :append (clj->js elms)))
      elms)))

; (defn fn-apply [function & {:keys [parent-element parent-selector child-tag attrs]})

(defn request [path & {:keys [callback selected-keys read? pre-path options]}] ; return channel?
  (go
   (let [{:keys [status body]} (<! 
                                (http/get (str pre-path path) 
                                          (merge {:with-credentials? false} options)))
         content (if read? (read-string body) body)
         res (if (= status 200)
               (if (not-empty selected-keys)
                 (if (vector? content)
                   (mapv #(select-keys % selected-keys) content)
                   (select-keys content selected-keys))
                 content)
               {:error status :msg body})]
     (when callback (callback res))
     res)))

; BASE UI

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

(defn- get-token-str [cm] (-> cm (.getTokenAt (.getCursor cm)) (aget "string")))

(defn- autocomp-refer! [cm]
  (let [token-str (get-token-str cm)
        pre (when (= \" (first token-str)) \")
        token (if pre (rest token-str) token-str) 
        completions 
        (clj->js (into [nil]
                       (mapv
                        (fn [{:keys [kl id]}]
                          (clj->js {:displayText (str kl " #" id)
                                    :text (str pre (case (first token)
                                                     \. (str kl)
                                                     \# (str "#" id)
                                                     \$ (do
                                                          (eval-editor kl)
                                                          (get-result kl))
                                                     \& (get-code kl)
                                                     \% (get-result kl)))}))
                        (-> @pou :editors vals))))]
    (show-hint! cm completions)))

(defn show-completions! [cm hint? info?]
  (let [token-str (get-token-str cm)
        pre-ns (re-find #".+?\/" token-str)
        completions-no-pre-ns (kl-repl/get-completions token-str)
        completions (if pre-ns (mapv (partial str pre-ns) completions-no-pre-ns) completions-no-pre-ns)]
    (when hint? 
      (show-hint! cm completions))
    (when info?
      (dom "#pou-info a.pou-completion"
           :map-siblings 
           (map
            (fn [c] [(str c) {:onclick #(peval-str (str "(doc " c ")"))}])
            (take 20 (rest completions)))
           :replace? true :attrs {:href "#"}))))

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

(defn- token-doc [cm] (peval-str (str "(doc " (get-token-str cm) ")")))
  
(defn- cm-reg! [kl]
  (let [{:keys [id mode hints?]} (-> @pou :editors (get kl))
        cm (or (@kleds/editors kl) (get-cm id))]
    (j/assoc! (. cm getOption "extraKeys")
              :Cmd-. #(autocomp-refer! %))
    #_(. cm on "keyHandled"
       (fn [_ key-handled] (js/console.log (str "CodeMirror #" kl " keyHandled: " key-handled))))
    (when (= mode "eval-clojure")
      (j/assoc! (. cm getOption "extraKeys")
                :Tab #(show-completions! % true false)
                :Alt-Space #(token-doc %))
      (. cm on "cursorActivity" #(show-completions! cm hints? (not hints?))))))

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

(defn- append-editor-base [{:keys [id kl description mode attrs kl-attrs snippet] :as editor}]
  (let [base (gdom/getElement "base")
        klipse (gdom/createDom "div" (clj->js kl-attrs) (str snippet))
        text (gdom/createDom "p" "pou-intro" (str kl "> " (or 
                                                           description
                                                           (str "#" id ", mode: " mode))))
        wrapper (gdom/createDom "div" (clj->js (assoc attrs :id id :data-kl kl :data-pou editor))
                                text klipse)]
    (.appendChild base wrapper)))

(reg-ui :base {:append-fn append-editor-base
               :klipsify? true})

(def default-keys 
  {:ui :base
   :mode "eval-clojure"
   :attrs {:class "pou-wrapper"}})

(defn append [editors & {:keys [provide override klipsify? on-mounted on-ready]}]
  (dotimes [n (count editors)]
    (let [{:keys [id from-gist] :as specific} (get editors n)
          {:keys [ui mode attrs kl-attrs external-libs eval-time loop? preamble editor-type]
           :as editor} (merge default-keys provide specific override)
          kl (+ @klp/snippet-counter n)
          id (or id (:id attrs) (str "pou-" kl))
          data-external-libs (->> external-libs
                              (into (-> @pou :external-libs (get mode)))
                              (cons (:data-external-libs kl-attrs))
                              (filter some?)
                              distinct
                              (interpose ",")
                              (apply str)
                              not-empty)
          new-editor (assoc editor 
                            :kl kl :id id 
                            :kl-attrs (merge kl-attrs
                                            {:class (mode->class mode)}
                                            (when data-external-libs 
                                              {:data-external-libs data-external-libs})
                                            (when from-gist
                                              {:data-gist-id from-gist})
                                            (when eval-time
                                              (if loop?
                                                {:data-loop-msec eval-time}
                                                {:data-eval-idle-msec eval-time}))
                                            (when preamble
                                              {:data-preamble preamble})
                                            (when editor-type
                                              {:data-editor-type editor-type})))]
      (js/console.log #js["New POU Editor" (clj->js new-editor)])
      (reg-editor new-editor)
      (let [append-fn (-> @pou :uis ui :append-fn)]
        (append-fn new-editor))))
  (let [ui (some :ui [provide default-keys])
        klipsify? (some-> @pou :uis ui :klipsify?)]
    (when klipsify? (klipsify! on-mounted on-ready))))

(defn aed [snippet & {:keys [mode attrs klipsettings external-libs on-mounted on-ready] :as editor-settings}] 
  (append [(assoc editor-settings :snippet snippet)] :on-mounted on-mounted :on-ready on-ready))

; LOAD & EXPORT FNS

(defn editors-array []
  (let [array (-> @pou :editors vals)]
    (->> array
      (map #(assoc % :snippet (get-code (:kl %))))
      (map #(dissoc % :kl)))))

; defn snapshot!

(defn load-module [module & {:keys [on-ready pre-path post-path]}]
  (request (str pre-path module post-path) :read? true
           :callback 
           #(append [%] 
                    :on-ready
                    (fn []
                      (swap! pou update-in [:modules] conj (str module))
                      (when on-ready (on-ready))))))

(defn module-loaded? [module]
  (-> @pou :modules (get (str module)) some?))

(defn load-module-chain [chain & {:keys [pre-path post-path]}] ; add last on-ready
  (load-module (first chain) :pre-path pre-path :post-path post-path
               :on-ready #(load-module-chain (rest chain))))

(defn load-modules [& modules & {:keys [pre-path post-path]}] ; add last on-ready
  (go
   (doseq [m modules]
     (if (coll? m)
       (load-module-chain m :pre-path pre-path :post-path post-path)
       (<! (load-module m))))))

(defn load-ui [ui & {:keys [on-ready pre-path post-path] :or {on-ready (fn[] nil)} :as opts}]
  (loading!)
  (load-module ui (flatten 
                   (seq 
                    (assoc opts :on-ready (fn [] (loaded!) (on-ready)))))))

(defn filext-filter [file-extension files & {:keys [file-key] 
                                             :or {file-key identity}}]
  (filter #(re-find (re-pattern (str "^(.*)." file-extension)) (file-key %)) files))
       
; INITIALIZATION

(defn init! []
  (process-url-params :ui #(load-ui %)
                      :editor-base #(append [(parse64 %)])
                      :editors-base #(append (parse64 %))
                      :p #(aed (decode64 %))
                      :module #(load-module %)
                      :modules #(apply load-modules (parse64 %))
                      :code #(load-module "modules/github.edn"))
  
  (request "https://api.github.com/repos/bonuoq/pou/contents/modules" 
           :selected-keys [:name :download_url]
           :callback (fn [entries] 
                       (doseq [url (map :download_url (filext-filter ".edn" entries :file-key :name))] ; instead of doseq should map async request
                         (request url :read? true :selected-keys [:description]
                                  :callback #(dom "option"
                                              :map-siblings {(:description %) {:value url}}
                                              :parent "select.load-module")))))
  
  (request "https://api.github.com/repos/bonuoq/pou/contents/modules/ui"
           :selected-keys [:name :download_url]
           :callback (fn [entries] 
                       (doseq [url (map :download_url (filext-filter ".edn" entries :file-key :name))] ; instead of doseq should map async request
                         (request url :read? true :selected-keys [:description]
                                  :callback #(dom "option"
                                              :map-siblings {(:description %) {:value url}}
                                              :parent "select.load-ui")))))
  
  (when-not (:ui (url-params)) (loaded!)))
