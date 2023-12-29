(ns pou.core-frame
  (:require [goog.dom :as gdom]
            [klipse.plugin :as klp]
            [re-frame.db :refer [app-db]]
            [reagent.core :as r]
            [applied-science.js-interop :as j]
            [clojure.core.async :as async :refer [<! >!]]))

(defn pou [& ks] (eval (conj ks deref `app-db `some->)))
    
(rf/reg-event-db
 :initialize
 (fn [_ _]  
   {:params (or (js/klipse.utils.url-parameters) {})
    :editors {}}))

(rf/reg-event-db
 :editor-mounted
 (fn [db [_ editor]]
   (update-in db [:editors] merge editor)))

(rf/reg-sub
 :editors
 (fn [db _]
   (:editors db)))

(rf/reg-sub
 :editor-comp
 (fn [db [_ idx]]
   (get-in db [:editors idx :comp])))

(defn create-editor [{:keys [mode attrs snippet klipsettings] :or {mode "eval-clojure" klipsettings {}}}]
  (r/create-class
    {:component-did-mount
     (fn [comp]
      (async/go
       (<! (klp/klipsify (r/dom-node comp) klipsettings mode))
       (let [c (dec @klp/snippet-counter)]
         (rf/dispatch [:editor-mounted
                       {c {:comp comp
                           :mode mode
                           :cm #(aget js/klipse-editors c)
                           :res #(aget js/klipse-results c)}}]))))                                
     :reagent-render
     (fn []
       [:div attrs (str snippet)])}))

(defn pou-re-frame []
  (let [editors @(rf/subscribe [:editors])]
    [:div#pou-editors
     (for [e editors]
       @(rf/subscribe [:editor-comp (-> e key)]))]))

  
