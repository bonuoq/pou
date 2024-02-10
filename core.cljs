(ns pou.core
  (:require [goog.dom :as gdom]
            [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [cljs.reader :refer [read-string]]
            [clojure.data :as d]
            [cljs-http.client :as http]
            [klipse.ui.editors.editor :as kl-ed]
            [klipse-clj.repl :as kl-repl]
            [klipse.plugin :as klp]
            [klipse.utils :as klu]
            [klipse.common.registry :as klreg]
            [klipse.klipse-editors :as kleds]
            [applied-science.js-interop :as j]
            [editscript.core :as e]))

; UTILS

(def url-params #(or (klu/url-parameters) {}))

(defn process-url-params [& param-procs]
  (let [params (url-params)]
    (doseq [pp (partition 2 param-procs)]
      (when-let [p ((first pp) params)]
        ((second pp) p)))))

(def decode64 #(js/atob %))
(def parse64 #(read-string (decode64 %)))
(def flatten64 #(flatten (into [] (parse64 %))))

; BASE STATE & watches

(def pou (atom 
          {:editors {}
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

; Flexible STATE CHANGES (recursive ediscript diffs and patches)

(defn nod [prev-nod diff-path upd-fn & args]
  (let [upd (apply upd-fn prev-nod args)
        diff-upd (assoc-in upd diff-path (e/diff upd prev-nod))]
    (assoc-in diff-upd diff-path (e/diff diff-upd prev-nod))))

(defn bon [prev-nod n-iter patch-path diff-path]
  (->> (update-in prev-nod (butlast patch-path) dissoc (last patch-path))
    (iterate #(nod % diff-path e/patch (get-in prev-nod patch-path)))
    (take (inc n-iter))))

(defn bonth [prev-nod nth-iter patch-path diff-path]
  (last (bon prev-nod nth-iter patch-path diff-path)))

(defn nod! [upd-path diff-path upd-fn & args]
  (get-in 
   (apply swap! pou update-in upd-path nod diff-path upd-fn args)
   upd-path))

(defn pou! [path diff-k upd-fn & args] 
  (apply nod! path [:uoq diff-k] upd-fn args))

(defn uoq [path n patch-k diff-k]
  (bon (get-in @pou path) n [:uoq patch-k] [:uoq diff-k]))

(defn uoq! [path n patch-k diff-k]
  (get-in
   (swap! pou update-in path bonth n [:uoq patch-k] [:uoq diff-k])
   path))

(defn drp
  ([path upd-fn & args]
   (apply nod (get-in @pou path) [:uoq :drp] upd-fn args))
  ([path n]
   (when (pos? n)
     (uoq path n :drw :drp)))
  ([path] (drp path 0)))

(defn drps
  ([path n sel-keys]
   (when (pos? n)
     (map select-keys (drp path n) (repeat sel-keys))))
  ([path n]
   (drps (butlast path) n [(last path)]))
  ([path]
   (drps path 0)))

(defn drw
  ([path upd-fn & args]
   (apply nod (get-in @pou path) [:uoq :drw] upd-fn args))
  ([path n]
   (when (pos? n)
     (uoq path n :drp :drw)))
  ([path] (drw path 0)))

(defn drws 
  ([path n sel-keys]
   (when (pos? n) 
     (map select-keys (drw path n) (repeat sel-keys))))
  ([path n]
   (drws (butlast path) n [(last path)]))
  ([path]
   (drws path 0)))

(defn drp!
  ([path upd-fn & args]
   (apply nod! path [:uoq :drp] upd-fn args))
  ([path n]
   (when (pos? n) (uoq! path n :drw :drp))))

(defn drps!
  ([path n sel-keys]
   (select-keys (drp! path n) sel-keys)))
  
(defn drw!
  ([path upd-fn & args]
   (apply nod! path [:uoq :drw] upd-fn args))
  ([path n]
   (when (pos? n) (uoq! path n :drp :drw))))

(defn drws!
  ([path n sel-keys]
   (select-keys (drw! path n) sel-keys)))

; REGISTER FUNCTIONS

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

(defn get-cm [k & {:keys [n] :or {n 0}}]
  (-> (str "#" (get-id k) " .CodeMirror") js/document.querySelectorAll (aget n) .-CodeMirror))
                                                            
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

(defn off-code-change [k handler] (call-in-editor k :off "change" handler))

(defn on-code-change [k callback & {:keys [one-shot]}]
  (let [cb-handler (fn [cm] (callback (.getValue cm)))]
    (call-in-editor k :on "change" 
                    (fn [cm]
                      (when one-shot
                        (js/setTimeout #(off-code-change k cb-handler)))
                      (cb-handler cm)))
    cb-handler))

(defn off-res-change [k handler] (call-in-result k :off "change" handler))

(defn on-res-change [k callback & {:keys [one-shot]}]
  (let [cb-handler (fn [r] (callback (.getValue r)))]
    (call-in-result k :on "change"
                    (fn [cm]
                      (when one-shot 
                        (js/setTimeout #(off-res-change k cb-handler)))
                      (cb-handler cm)))
    cb-handler))

(defn res-watch [k cb & {:keys [one-shot]}]
  (cb (get-result k))
  (on-res-change k cb :one-shot one-shot))

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

(defn eval-callback
  ([k callback]
   (res-watch k callback :one-shot true)
   (eval-editor k))
  ([k code callback]
   (res-watch k callback :one-shot true)
   (set-code-eval k code)))

(defn peval-str [s] (set-code-eval 0 s))

(defn drw-editor! [k n]
  (let [code (:code (drw! [:editors (get-kl k)] n))]
    (set-code-eval k code)
    code))

(defn drp-editor! [k n]
  (let [code (:code (drp! [:editors (get-kl k)] n))]
    (set-code-eval k code)
    code))

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
(defn dom-nodelist [selector]
  (js/document.querySelectorAll selector))
(defn dom-vec [selector]
  (-> selector dom-nodelist js/Array.from js->clj))

(defn toggle-hidden! 
  ([selector hidden?]
   (doseq [e (dom-vec selector)]
     (j/assoc! e :hidden hidden?)))
  ([selector] 
   (doseq [e (dom-vec selector)]
     (j/update! e :hidden not))))

(defn loaded! []
  (toggle-hidden! "div#pou-app" false)
  (toggle-hidden! "div#loading" true))
(defn loading! [] 
  (toggle-hidden! "div#loading" false)
  (toggle-hidden! "div#pou-app" true))

(defn dom-string [s]
  (.createContextualFragment (js/document.createRange) s))

(declare dom-any)

(defn dom-create [selector {:as attrs} content]
  (let [tag (sel-child-tag selector)
        as (merge {:id (sel-child-id selector) :class (sel-child-class selector)} attrs)
        children (map dom-any content)]
    (apply gdom/createDom tag (clj->js as) (clj->js children))))

(defn dom-any [any]
  (condp apply [any]
    string? (dom-string any)
    coll? (if (map? any)
            (let [{:keys [type tag attrs content selector]
                   :or {selector (subs (str tag) 1)}} any] ; hickory style map but you can omit :type when {:type :element} & you can provide {:selector "tag.css#selector"} or :tag with selector as keyword {:tag :tag.class#id})
              (case type
                :document (map dom-any content)
                :element (dom-create selector attrs content)
                nil (dom-create selector attrs content)
                (dom-string any))) ; provide a default way to represent clj maps as HTML
            (map dom-any any))
    any))

(defn dom [selector & {:keys [attrs parent content map-siblings replace? separator]}]
  (let [p (dom-select (or (sel-parent selector) parent))
        sibling-fn #(dom-create selector (merge attrs (second %)) (first %) #_(into (first %) content)) ;removed
        elms (if map-siblings
               (into ;added
                (->> map-siblings
                 (map sibling-fn)
                 ((if separator 
                    (partial interpose separator)
                    identity)))
                content) ;added
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
                   (mapv select-keys content (repeat selected-keys))
                   (select-keys content selected-keys))
                 content)
               {:error status :msg body})]
     (when callback (callback res))
     res)))

; BASE UI

(defn- mode->class [mode]
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
                                    :text (if (= \$ (first token))
                                                 kl
                                                 (str pre 
                                                      (case (first token)
                                                        \. (str kl)
                                                        \# (str "#" id)
                                                        \& (get-code kl)
                                                        \% (get-result kl))))
                                    :hint (when (= \$ (first token))
                                            (fn [cm _ data]
                                              (let [cursor (. cm getCursor)
                                                    token (. cm getTokenAt cursor)
                                                    token-start (js/CodeMirror.Pos (.-line cursor) (.-start token))
                                                    kl (. data -text)] 
                                                (eval-callback kl #(. cm replaceRange % token-start cursor)))))}))
                        (-> @pou :editors vals))))]
    (show-hint! cm completions)))

(defn- show-completions! [cm hint? info?]
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

(defn- insert-code [k code & {:keys [rel-cursor from to] :or {rel-cursor 0}}]
  (let [cm (@kleds/editors (get-kl k))
        cursor (.getCursor cm)
        from (or from (if (< 0 rel-cursor) 
                        (j/update-in! cursor [:ch] + rel-cursor) 
                        cursor))
        to (or to (if (> 0 rel-cursor) 
                    (j/update-in! cursor [:ch] + rel-cursor) 
                    cursor))]
  (.replaceRange cm code (clj->js from) (clj->js to))))

(defn- token-doc [cm] (peval-str (str "(doc " (get-token-str cm) ")")))

(defn- drp-code! [cm]
  (drp! [:editors (.-kl cm)] assoc :code (.getValue cm))
  js/CodeMirror.Pass)
  
(defn- cm-reg! [kl]
  (let [{:keys [id mode hints?]} (-> @pou :editors (get kl))
        cm (or (@kleds/editors kl) (get-cm id))]
    (j/assoc! cm :kl kl :id id)
    (j/assoc! (. cm getOption "extraKeys")
              :Cmd-. #(autocomp-refer! %))
    (. cm addKeyMap
       #js {:Cmd-Enter #(drp-code! %)                        
            :Ctrl-Enter #(drp-code! %)})
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

(defn- append-editor-base [{:keys [id kl description mode attrs kl-attrs code] :as editor}]
  (let [base (gdom/getElement "base")
        klipse (gdom/createDom "div" (clj->js kl-attrs) (str code))
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
          id (or id (:id attrs) (gensym "pou"))
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

(defn aed [& {:keys [code mode id from-gist attrs klipsettings external-libs on-mounted on-ready] :as editor-settings}] 
  (append [editor-settings] :on-mounted on-mounted :on-ready on-ready))

; LOAD & EXPORT FNS

(defn editors-array 
  ([& sel-keys] 
   (for [e (-> @pou :editors vals)
         :let [ex (dissoc e :kl)]]
     (select-keys ex sel-keys)))
  ([] (editors-array [:code :mode :id :from-gist :external-libs 
                      :eval-time :loop? :preamble :editor-type])))

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

(defn load-ui [ui & {:keys [on-ready pre-path post-path] :as opts}]
  (loading!)
  (toggle-hidden! "div#uis .pou-ui")
  (load-module ui :pre-path pre-path :post-path post-path
               :on-ready (fn [] 
                           (when on-ready (on-ready))
                           (loaded!))))

(defn filext-filter [file-extension files & {:keys [file-key] 
                                             :or {file-key identity}}]
  (filter #(re-find (re-pattern (str "^(.*)." file-extension)) (file-key %)) files))
       
; INITIALIZATION

(defn init! []
  (process-url-params :ui #(load-ui %)
                      :editor-base #(append [(parse64 %)])
                      :editors-base #(append (parse64 %))
                      :p #(aed :code (decode64 %))
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
