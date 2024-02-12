(ns pou.modules.faust
  (:require [goog.dom :as gdom]
            [klipse.common.registry :as klr]
            [klipse.klipse-editors :as kleds]))

(def trusted-url js/goog.html.legacyconversions.trustedResourceUrlFromString)
(def js-safe-load js/goog.net.jsloader.safeLoad)

(def loaded (atom false))

(def lib-url "https://23trastos.github.io/faust-web-component")

(defn init 
  ([] (init lib-url))
  ([cdn-url]
   (-> (str cdn-url "/dist/faust-web-component.js") trusted-url js-safe-load 
     (.addCallback #(reset! loaded true)))))

(defn place-in [element & {:keys [code mode]} ]
  (let [component (. js/document createElement (if (= mode :widget) "faust-widget" "faust-editor"))]
    (. component appendChild (. js/document createComment code))
    (. element replaceChildren component)))

(defn get-comp [id] 
  (some-> id gdom/getElement (.querySelector "faust-editor, faust-widget")))

(defn get-faust-code [id]
   (some-> (get-comp id) .-shadowRoot (.querySelector ".cm-content") .-cmView .-view .-state .-doc .toString))

(defn get-faust-node [id] 
  (. (get-comp id) -faustNode))
           
(defn set-param [id param-path value]
  (when-let [component (get-comp id)]
    (.. component -faustNode (setParamValue param-path value))
    (.. component -faustUI (paramChangeByDSP param-path value))))

(defn action [id action]
  (some-> (get-comp id) .-shadowRoot (.getElementById (clj->js action)) .click))

(defn on-action [id action handler]
  (some-> (get-comp id) .-shadowRoot (.getElemenyById (cljs->js action)) 
    (.addEventListener "click" handler)))

(defn eval-faust [mode exp {:keys [container container-id] :as kwargs}]
  (if @loaded
    (try
      (place-in container :code (str exp) :mode mode)
      (when (= mode :editor) 
        (js/console.log (some->> container-id last int (get @kleds/editors))))
      (catch :default e
        (set! (. container -innerHTML) (str e))))
    (js/setTimeout #(eval-faust exp kwargs) 500)))

(def widget-opts {:editor-in-mode "text/html"
                  :editor-out-mode "text"
                  :default-editor "html"
                  :eval-fn (partial eval-faust :widget)
                  :no-result true
                  :min-eval-idle-msec 1000
                  :comment-str "//"})

(klr/register-mode "faust-widget" "selector_faust_widget" widget-opts)

(def editor-opts (assoc widget-opts :eval-fn (partial eval-faust :editor)))

(klr/register-mode "faust-editor" "selector_faust_editor" editor-opts)

(when-not @loaded (init))
