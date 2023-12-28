(ns pou.modules.guido
  (:require [klipse.common.registry :as klr]
            [applied-science.js-interop :as j]))

(def trusted-url js/goog.html.legacyconversions.trustedResourceUrlFromString)
(def js-safe-load js/goog.net.jsloader.safeLoad)

(def lib-url "https://unpkg.com/@grame/guidolib")

(def module (atom nil))
(def adapters (atom {}))

(defn load-adapters [& guido-adapters]
  (let [guido-module @module]
    (doseq [i guido-adapters]
      (case i
        :factory (swap! adapters assoc :factory (new (. guido-module -GUIDOFactoryAdapter)))
        (str "there is no such " i " adapter")))))


(defn init [& {:keys [guidolib-url with-adapters]
               :or {guidolib-url lib-url}}]
  (-> (js/FontFace. "Guido2"
                    (str "url(" guidolib-url "/guido2-webfont/guido2-webfont.woff2)")
                    #js{:style "normal" :weight "normal"})
    .load
    (.then
     (fn [font]
       (js/document.fonts.add font)
       (-> (str guidolib-url "/libGUIDOEngine.js") trusted-url js-safe-load
         (.addCallback 
          #(-> 
             (js/GuidoModule)
             (.then
              (fn [guido-module]
                (reset! module guido-module)
                (let [guido-engine (new (. guido-module -GuidoEngineAdapter))]
                  (.init guido-engine)
                  (swap! adapters assoc :engine guido-engine))
                (apply load-adapters with-adapters))))))))))

(defn call [adapter method & args]
  (if-let [ga (adapter @adapters)]
    (j/apply ga method (clj->js args))
    (str "ERR: Guido " adapter " adapter not loaded or does not exist.")))

(defn ar->svg [ar]
  (let [e (:engine @adapters)
        gr (.ar2gr e ar)
        svg (.gr2SVG e gr 1 true 0)]
    (.freeAR e ar)
    (.freeGR e gr)
    svg))

(defn gmn->svg [string]
  (let [e (:engine @adapters)
        parser (.openParser e)
        ar (.string2AR e parser string)]
    (.closeParser e parser)
    (ar->svg ar)))

(defn factory-seq [& factory-cmd-seq]
  (map (fn [& args] (apply call :factory (flatten args))) factory-cmd-seq))

(defn render-guido [exp {:keys [container-id]}]
  (if @module
    (let [conta-inner #(-> container-id js/document.getElementById .-innerHTML (set! %))]
      (try
        (conta-inner (gmn->svg (str exp)))
        (catch :default e
          (conta-inner (str e)))))
    (init)))

(def opts {:editor-in-mode "text/html"
           :editor-out-mode "text"
           :default-editor "html"
           :eval-fn render-guido
           :no-result true
           :min-eval-idle-msec 1000
           :comment-str "%"})

(klr/register-mode "guido-svg" "selector_guido" opts)
