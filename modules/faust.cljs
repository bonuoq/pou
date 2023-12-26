(ns pou.modules.faust)

(def trusted-url js/goog.html.legacyconversions.trustedResourceUrlFromString)
(def js-safe-load js/goog.net.jsloader.safeLoad)

(def loaded (atom false))

(def lib-url "https://23trastos.github.io/faust-web-component")

(defn init 
  ([] (init lib-url))
  ([cdn-url]
   (-> (str cdn-url "/dist/faust-web-component.js") trusted-url js-safe-load 
     (.addCallback #(reset! loaded true)))))

(defn place-in [element snippet editor?]
  (let [component (. js/document createElement (if editor? "faust-editor" "faust-widget"))]
    (. component appendChild (. js/document createComment snippet))
    (. element replaceChildren component)))
           
(defn set-param [parent param-path value]
  (when-let [component (. parent querySelector "faust-editor, faust-widget")]
    (.. component -faustNode (setParamValue param-path value))
    (.. component -faustUI (paramChangeByDSP param-path value))))

(defn power-toggle [parent]
  (.. parent -firstChild -shadowRoot (getElementById "power") click))

(defn eval-faust [exp {:keys [container-id]}]
  (if @loaded
    (let [container (js/document.getElementById container-id)]
      (try
        (js/console.log (str "FAUST eval: " exp))
        (place-in container (str exp) false)
        (catch :default e
          (set! (. container -innerHTML) (str e)))))
    (init)))

(def widget-opts {:editor-in-mode "text/html"
                  :editor-out-mode "text"
                  :default-editor "html"
                  :eval-fn eval-faust
                  :no-result true
                  :min-eval-idle-msec 1000
                  :comment-str "//"})

(js/klipse.common.registry.register-mode "faust-widget" "selector_faust" widget-opts)
