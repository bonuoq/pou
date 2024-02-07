(ns pou.modules.ui.re-frame
  (:require [goog.dom :as gdom]
            [cljs.reader :refer [read-string]]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [re-frame.core :as rf]
            [pou.core :as p]))

; REG SUBS

(rf/reg-sub
 :kl
 (fn [db [_ id]]
   (-> db :editors (get id) :kl)))

(rf/reg-sub
 :editors
 (fn [db _]
   (:editors db)))

(rf/reg-sub
 :ids
 (fn [db _]
   (-> db :editors keys)))

(rf/reg-sub
 :hidden?
 (fn [db [_ id]]
   (-> db :editors (get id) :hidden?)))

(rf/reg-sub
 :mode-options
 #(:mode-options @p/pou))

(rf/reg-sub
 :snapshot
 (fn [db _]
   (->> (:editors db)
     (map #(assoc (val %) :code (p/get-code (:kl (val %))))) 
     (map #(dissoc % :kl))
     (mapv #(assoc % :description (-> (:id %) 
                                    gdom/getElement 
                                    (.querySelector ".pou-intro") 
                                    .-textContent))))))

; REG EVENTS

(rf/reg-event-db
 :hide
 (fn [db [_ id hidden?]]
   (assoc-in db [:editors id :hidden?] hidden?)))

(rf/reg-event-db
 :show-hide
 (fn [db [_ id]]
   (update-in db [:editors id :hidden?] not)))

(rf/reg-event-db
 :reg-editor
 (fn [db [_ editor]]
   (update-in db [:editors] conj editor)))

(rf/reg-event-db
 :ready
 (fn [db [_ editor-comp]]
   (update-in db [:ready] conj editor-comp)))

(rf/reg-event-db
 :discard-editor
 (fn [db [_ id]]
   (let [discarded (assoc-in db [:trash id] (-> db :editors (get id)))]
     (update-in discarded [:editors] dissoc id))))

(rf/reg-event-db
 :recover-editor
 (fn [db [_ id]]
   (let [recovered (assoc-in db [:editors id] (-> db :trash (get id)))]
     (update-in recovered [:trash] dissoc id))))

(rf/reg-event-db
 :drw-editor
 (fn [db [_ id]]
   (assoc-in db [:editors id :code] (p/drw-editor! id 1))))

(rf/reg-event-db
 :drp-editor
 (fn [db [_ id]]
   (assoc-in db [:editors id :code] (p/drp-editor! id 1))))

(rf/reg-event-db
 :initialize
 (fn [_ _] 
   {:editors {} :ready {}}))

; ACTIONS AND HELPER FNS

(defn- append-editor-re-frame [{:keys [id] :as editor}]
  (rf/dispatch [:reg-editor {id editor}]))

(defn append [editors & args]
  (apply p/append editors :provide {:ui :re-frame} args))

(defn snapshot [] (rf/subscribe [:snapshot]))

;;;;; TODO : ASYNC
(defn load-snapshot [snapshot discard-old?]
  (when discard-old? 
    (for [i @(rf/subscribe [:ids])]
      (rf/dispatch [:discard-editor i])))
  (append snapshot))

; COMPONENTS

(defn- editor-comp [{:keys [kl id description mode attrs kl-attrs code]}]
  (r/create-class
   {:component-did-mount 
    (fn [this]
      (p/klipsify! nil #(rf/dispatch [:ready {id this}])))
    :reagent-render
    (fn []
      (let [hidden? @(rf/subscribe [:hidden? id])]
        [:div.pou-wrapper (assoc attrs :id id)
         [:div.re-frame-intro
          [:button.toggle-min
           {:on-click #(rf/dispatch [:show-hide id])}
           (if hidden? "<" ">")] " "
          [:span.pou-intro.pou-editable {:contentEditable true}
           (or description (str "#" id ", mode: " mode))]]
         [:div.pou-editor
          {:style {:display (if hidden? "none" "block")}}
          [:div.re-frame-toolbar
           [:button.drw-editor
           {:on-click #(rf/dispatch [:drw-editor id])}
           "<-eval"]
           [:button.drp-editor
            {:on-click #(rf/dispatch [:drp-editor id])}
            "eval->"]]
          [:div.pou-klipse kl-attrs (str code)]]]))}))

(defn- select-comp [value-atom options-atom]
  (fn []
    [:select
     {:on-change #(reset! value-atom (.. % -target -value))}
     [:option {:value nil :disabled true :selected true} "select mode"]
     (for [k @options-atom]
       ^{:key k} [:option {:value k} k])]))

(defn- pou-re-frame []
  (let [sel-mode (r/atom nil)
        from-gist (r/atom nil)
        ext-libs (r/atom nil)]
    (fn []
      [:div.pou-re-frame-ui
       (for [e @(rf/subscribe [:editors])]
         ^{:key (key e)} [editor-comp (val e)])
       [:div.pou-toolbar
        [:button.append
         {:on-click #(append [{}])}
         "+"] " | "
        [:button
         {:on-click (fn [_]
                      (append [{:mode (or @sel-mode (first @(rf/subscribe [:mode-options])))
                                :external-libs (read-string (str "[" @ext-libs "]"))
                                :attrs {:data-gist-id @from-gist}}])
                      (reset! from-gist nil)
                      (reset! ext-libs nil))}
         "+"]
        [select-comp sel-mode (rf/subscribe [:mode-options])] " "
        [:input {:type "text"
                 :placeholder "user/id [gist]"
                 :value @from-gist
                 :on-change #(reset! from-gist (.. % -target -value))}] " "
        [:input {:type "text"
                 :placeholder "https://a.b https://... [ext-libs]"
                 :value @ext-libs
                 :on-change #(reset! ext-libs (.. % -target -value))}]]])))

; INITIALIZE

(rf/dispatch [:initialize])
(let [ui-div (p/reg-ui :re-frame {:append-fn append-editor-re-frame
                                  :klipsify? false})]
  (rdom/render [pou-re-frame] (gdom/getElement ui-div)))
