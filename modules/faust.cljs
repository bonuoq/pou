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

(defn place-in [element & {:keys [snippet mode]} ]
  (let [component (. js/document createElement (if (= mode :widget) "faust-widget" "faust-editor"))]
    (. component appendChild (. js/document createComment snippet))
    (. element replaceChildren component)))

(defn get-comp [idx] 
  (some-> idx gdom/getElement .querySelector "faust-editor, faust-widget"))

(defn get-faust-code [idx] 
  (or 
   (some-> (get-comp idx) .-shadowRoot (.querySelector ".cm-content") .-innerText)
   (some-> (get-comp idx) .-lastChild .-data)))

(defn get-faust-node [idx] 
  (. (get-comp idx) -faustNode))
           
(defn set-param [idx param-path value]
  (when-let [component (get-comp idx)]
    (.. component -faustNode (setParamValue param-path value))
    (.. component -faustUI (paramChangeByDSP param-path value))))

(defn action [idx action]
  (some-> (get-comp idx) .-shadowRoot (.getElementById (clj->js action)) .click))

(defn eval-faust [exp {:keys [container-id] :as kwargs}]
  (if @loaded
    (let [container (js/document.getElementById container-id)]
      (try
        (js/console.log (str "FAUST eval: " exp))
        (place-in container :snippet (str exp) :mode widget)
        (catch :default e
          (set! (. container -innerHTML) (str e)))))
    (js/setTimeout #(eval-faust exp kwargs) 500)))

(def widget-opts {:editor-in-mode "text/html"
                  :editor-out-mode "text"
                  :default-editor "html"
                  :eval-fn eval-faust
                  :no-result true
                  :min-eval-idle-msec 1000
                  :comment-str "//"})

(klr/register-mode "faust-widget" "selector_faust" widget-opts)
(init)
