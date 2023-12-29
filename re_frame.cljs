(ns pou.re-frame
  (:require [goog.dom :as gdom]
            [klipse.plugin :as klp]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [re-frame.db :refer [app-db]]
            [re-frame.core :as rf]
            [pou.core :as p]
            [applied-science.js-interop :as j]))

(defn pou [& ks] (eval (conj ks deref `app-db `some->)))
    
(rf/reg-event-db
 :initialize
 (fn [_ _]  
   {:params (or (js/klipse.utils.url-parameters) {})
    :editors {}}))

(rf/reg-event-db
 :reg-editor-comp
 (fn [db [_ editor]]
   (let [c @klp/snippet-counter]
     (update-in db [:editors] merge {c editor}))))

(rf/reg-sub
 :editors
 (fn [db _]
   (:editors db)))

(rf/reg-sub
 :editor-comp
 (fn [db [_ idx]]
   (get-in db [:editors idx])))

(defn editor-comp [{:keys [mode attrs snippet klipsettings] :or {klipsettings {}}}]
  (r/create-class
    {:component-did-mount
     (fn [this]
       (klp/klipsify (rdom/dom-node this) klipsettings mode))               
     :reagent-render
     (fn []
       [:div.pou-editor attrs (str snippet)])}))

(defn append-editor [& {:keys [mode] :or {mode "eval-clojure"} :as editor-map}]
  (let [editor (merge editor-map {:mode mode})]
    (p/reg-editor editor)
    (rf/dispatch [:reg-editor-comp [editor-comp editor]])))

(defn pou-re-frame []
  (let [editors @(rf/subscribe [:editors])]
    [:div#pou-editors
     (for [e editors]
       @(rf/subscribe [:editor-comp (-> e key)]))]))

(rdom/render [pou-re-frame] (gdom/getElement "app"))

  
