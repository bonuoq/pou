(ns pou.modules.faust
  (:require [goog.dom :as gdom]
            [klipse.common.registry :as klr]))

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

(defn get-comp [id-or-element]
  (when-let [elm (if (string? id-or-element) (gdom/getElement id-or-element) id-or-element)]
    (. elm querySelector "faust-editor, faust-widget")))

(defn get-faust-code [id-or-element]
   (some-> (get-comp id-or-element) .-shadowRoot (.querySelector ".cm-content") .-cmView .-view .-state .-doc .toString))

(defn get-faust-node [id-or-element] 
  (. (get-comp id-or-element) -faustNode))
           
(defn set-param [id-or-element param-path value]
  (when-let [component (get-comp id-or-element)]
    (.. component -faustNode (setParamValue param-path value))
    (.. component -faustUI (paramChangeByDSP param-path value))))

(defn action [id-or-element action]
  (some-> (get-comp id-or-element) .-shadowRoot (.getElementById (clj->js action)) .click))

(defn on-action [id-or-element action handler]
  (some-> (get-comp id-or-element) .-shadowRoot (.getElementById (clj->js action)) 
    (.addEventListener "click" handler)))

(defn eval-faust [mode exp {:keys [container] :as kwargs}]
  (if @loaded
    (try
      (place-in container :code (str exp) :mode mode)
      (when (= mode :editor)
        (when-let [elm (-> container .-parentElement (.querySelector ".CodeMirror"))]
          (set! (.-hidden elm) true)
          (on-action elm :run #(-> elm .-CodeMirror (.setValue (get-faust-code elm))))))
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
