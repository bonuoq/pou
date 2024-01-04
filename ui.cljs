(ns pou.ui
  (:require [goog.dom :as gdom]
            [klipse.plugin :as klp]
            [klipse.common.registry :as klreg]
            [reagent.core :as r]
            [reagent.dom :as rdom]
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

(rf/reg-sub
 :sel-mode
 (fn [db _]
   (:sel-mode db)))

(defn editor-comp [{:keys [mode attrs idx snippet klipsettings] :or {klipsettings {}}}]
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
              (if visible? "<" ">")]
             (str  "[" idx "] mode: " mode)]
            [:div.pou-editor
             {:style {:display (if visible? "block" "none")}}
             [:div.pou-klipse attrs (str snippet)]]]))})))

(defn append-editor [& {:keys [mode] :or {mode "eval-clojure"} :as editor-map}]
  (let [idx @klp/snippet-counter]
    (rf/dispatch [:reg-editor-comp {idx [editor-comp (assoc editor-map :mode mode :idx idx)]}])))

(defn select-mode-comp [mode-options]
  (let [sel-change #(rf/dispatch [:sel-mode-change (.. % -target -value)])]
    (r/create-class
     {:component-did-mount #(sel-change (rdom/dom-node %))
      :reagent-render
      (fn []
        [:select#editor-modes
         {:on-change sel-change}
         (for [k options]
           ^{:key k} [:option {:value k} k])])})))

(defn pou-re-frame []
    [:div#pou-app
     (for [e @(rf/subscribe [:editors])]
       (let [idx (key e)]
         ^{:key idx} @(rf/subscribe [:editor-comp idx])))
     [:button#append-clj
      {:on-click #(append-editor :mode "eval-clojure")}
       "+eval-clojure"}]
     [:button#append-editor
      {:on-click #(append-editor :mode @(rf/subscribe :sel-mode))}
      "+"]
     [select-mode-comp @(rf/subscribe [:mode-options])]))

(rf/reg-event-db
 :reg-editor-comp
 (fn [db [_ editor]]
   (update-in db [:editors] conj editor)))

(rf/reg-event-db
 :reg-mode-options
 (fn [db [_ mode-options]]
   (update-in db [:mode-options] into mode-options)))

(rf/reg-event-db
 :sel-mode-change
 (fn [db [_ sel-mode]]
   (assoc db :sel-mode sel-mode)))

(rf/reg-event-db
 :initialize
 (fn [_ _]  
   {:params (or (js/klipse.utils.url-parameters) {})
    :editors {}
    :mode-options (into (sorted-set) (keys @klreg/mode-options))}))

(add-watch klreg/mode-options :re-frame-reg #(rf/dispatch [:reg-mode-options (keys %4)]))
(rf/dispatch [:initialize])
(rdom/render [pou-re-frame] (gdom/getElement "app"))
