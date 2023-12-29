(ns pou.core-frame
  (:require [goog.dom :as gdom]
            [klipse.plugin :as klp]
            [re-frame.db :refer [app-db]]
            [applied-science.js-interop :as j]))

(defn pou [& ks] (eval (conj ks deref `app-db `->)))
    
(rf/reg-event-db
 :initialize
 (fn [_ _]  
   {:params (or (js/klipse.utils.url-parameters) {})
    :editors {:main {:idx 0
                     :mode "eval-clojure"
                     :cm #(aget js/klipse-editors 0)
                     :res #(aget js/klipse-results 0)}}}))

(rf/reg-event-db
 :editor
 (fn [db [_ editor]]
   (update-in db [:editors] merge editor)))

(rf/reg-sub
 :editors
 (fn [db _]
   (:editors db)))
