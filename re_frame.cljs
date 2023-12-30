(ns pou.re-frame
  (:require [goog.dom :as gdom]
            [klipse.plugin :as klp]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [re-frame.db :refer [app-db]]
            [re-frame.core :as rf]
            [pou.core :as p]
            [applied-science.js-interop :as j]))

(rf/reg-sub
 :editors
 (fn [db _]
   (:editors db)))

(rf/reg-sub
 :editor-comp
 (fn [db [_ idx]]
   (get-in db [:editors idx])))

(defn editor-comp [{:keys [mode attrs snippet klipsettings] :or {klipsettings {}}}]
  (let [s (r/atom {:visible? true})]
    (r/create-class
      {:component-did-mount
       (fn [this]
         (klp/klipsify (. (rdom/dom-node this) querySelector ".pou-editor") klipsettings mode))               
       :reagent-render
       (fn []
         (let [visible? (:visible? @s)]
           [:div.pou-toolbar {}
            [:button.toggle-min
             {:on-click #(swap! s update :visible? not)}
             (if visible? "-" "+")]]
           [:div.pou-wrapper 
            {:style {:display (if visible? "block" "none")}}
            [:div.pou-editor attrs (str snippet)]]))})))

(defn append-editor [& {:keys [mode] :or {mode "eval-clojure"} :as editor-map}]
  (let [editor (assoc editor-map :mode mode)
        idx @klp/snippet-counter]
    (p/reg-editor editor)
    (rf/dispatch [:reg-editor-comp {idx [editor-comp editor]}])))

(defn pou-re-frame []
  (let [editors @(rf/subscribe [:editors])]
    [:div#pou-editors
     (for [e editors]
       (let [idx (key e)]
         ^{:key idx} @(rf/subscribe [:editor-comp idx])))]))

(rf/reg-event-db
 :reg-editor-comp
 (fn [db [_ editor]]
   (update-in db [:editors] conj editor)))

(rf/reg-event-db
 :initialize
 (fn [_ _]  
   {:params (or (js/klipse.utils.url-parameters) {})
    :editors {}}))

(rdom/render [pou-re-frame] (gdom/getElement "app"))
(rf/dispatch [:initialize])
