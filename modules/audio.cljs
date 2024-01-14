(ns pou.modules.audio
  (:require [cljs-bach.synthesis :as syn]))

(defonce synctx (syn/audio-context))

(def faust-ctx (atom nil))

(defn provide-faust-ctx [id] 
  (reset! faust-ctx (.-context (faust-node id))))

(def actx (or @faust-ctx synctx))

(defn play! [notes]
  (doseq [{:keys [instrument time duration] :as note} notes]
    (let [at (+ time (syn/current-time actx))
          synth (syn/connect (instrument note) syn/destination)]
      (synth actx at duration))))

(defn faust-node [id] 
  (-> (str "klipse-container-" idx) gdom/getElement 
    (.querySelector "faust-editor, faust-widget") 
    .-faustNode))

(defn get-param [node param] 
  (-> node .-parameters (.get param)))

(defn get-faust-param [id param] 
  (get-param (faust-node id) param))

(defn plug-param [node param input at duration] 
  (syn/plug (get-param node param) input (or (.-context node) actx) at duration))

(defn plug-faust-param [id param input at duration]
  (plug-param (faust-node id) param input at duration))

(defn play-faust!
  ([id events]
   (let [node (faust-node id)]
     (doseq [{:keys [param time value pitch duration] :as event} events]
       (let [at (+ time (syn/current-time actx))]
         (plug-param node param (or pitch value) at duration)))))
  ([events]
   (doseq [{:keys [id param time value pitch duration] :as event} events]
     (let [node (faust-node id)
           at (+ time (syn/current-time actx))]
       (plug-param node param (or pitch value) at duration)))))
