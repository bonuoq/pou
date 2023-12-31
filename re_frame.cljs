(ns pou.re-frame
  (:require [goog.dom :as gdom]
            [klipse.plugin :as klp]
            [klipse.common.registry :as kreg]
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

(rf/reg-sub
 :mode-options
 (fn [db _]
   (:mode-options db)))

(defn editor-comp [{:keys [mode attrs snippet klipsettings] :or {klipsettings {}}}]
  (let [s (r/atom {:visible? true})]
    (r/create-class
      {:component-did-mount
       (fn [this]
         (klp/klipsify (. (rdom/dom-node this) querySelector ".pou-klipse") klipsettings mode))               
       :reagent-render
       (fn []
         (let [visible? (:visible? @s)]
           [:div.pou-wrapper
            [:div.pou-toolbar
             [:button.toggle-min
              {:on-click #(swap! s update :visible? not)}
              (if visible? "<" ">")]]
            [:div.pou-editor
             {:style {:display (if visible? "block" "none")}}
             [:div.pou-klipse attrs (str snippet)]]]))})))

(defn append-editor [& {:keys [mode] :or {mode "eval-clojure"} :as editor-map}]
  (let [idx @klp/snippet-counter
        editor (assoc editor-map :mode mode)]
    (p/reg-editor editor)
    (rf/dispatch [:reg-editor-comp {idx [editor-comp editor]}])))

(defn pou-re-frame []
  (let [s (r/atom {:selected-mode "transpile-clojurescript"})]
    [:div#pou-app
     (for [e @(rf/subscribe [:editors])]
       (let [idx (key e)]
         ^{:key idx} @(rf/subscribe [:editor-comp idx])))
     [:button#append-editor
      {:on-click #(append-editor :mode (:selected-mode @s))}
      "+"]
     [:select#editor-modes
      {:on-change #(swap! s update :selected-mode (.. % -target -value))}
      (for [k @(rf/subscribe [:mode-options])]
        ^{:key k} [:option {:value k} k])]]))

(rf/reg-event-db
 :reg-editor-comp
 (fn [db [_ editor]]
   (update-in db [:editors] conj editor)))

(rf/reg-event-db
 :reg-mode-options
 (fn [db [_ mode-options]]
   (update-in db [:mode-options] into mode-options)))

(rf/reg-event-db
 :initialize
 (fn [_ _]  
   {:params (or (js/klipse.utils.url-parameters) {})
    :editors {}
    :mode-options (into #{} (keys @kreg/mode-options))}))

(add-watch kreg/mode-options :re-frame-reg #(rf/dispatch [:reg-mode-options (keys %4)]))
(rdom/render [pou-re-frame] (gdom/getElement "app"))
(rf/dispatch [:initialize])
