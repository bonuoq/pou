(ns pou.modules.audio
  (:require [cljs-bach.synthesis :as syn]))

(defonce synctx (syn/audio-context))

(defn faust-node [id] 
  (-> (str "klipse-container-" idx) gdom/getElement 
    (.querySelector "faust-editor, faust-widget") 
    .-faustNode))

(defn provide-faust-ctx [id] 
  (reset! faust-ctx (.-context (faust-node id))))

(defn actx
  ([] synctx)
  ([faust-id] (.-context (faust-node faust-id))))

(def actime #(syn/current-time (actx %)))

(defn play! [notes]
  (doseq [{:keys [instrument time duration] :as note} notes]
    (let [at (+ time (actime))
          synth (syn/connect (instrument note) syn/destination)]
      (synth (actx) at duration))))

(defn get-param [node param] 
  (-> node .-parameters (.get param)))

(defn get-faust-param [id param] 
  (get-param (faust-node id) param))

(defn plug-faust-param [id param input at duration]
  (syn/plug (get-faust-param id param) input (actx id) at duration))

(defn play-faust!
  ([id events]
   (doseq [{:keys [param time value pitch duration] :as event} events]
     (plug-faust-param id param (or pitch value) (+ time (actime id) duration))))
  ([events]
   (doseq [{:keys [id param time value pitch duration] :as event} events]
     (plug-faust-param id param (or pitch value) (+ time (actime id) duration)))))
