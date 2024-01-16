(ns pou.modules.ui.re-frame
  (:require [goog.dom :as gdom]
            [klipse.plugin :as klp]
            [klipse.common.registry :as klreg]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [re-frame.core :as rf]
            [pou.core :as p]
            [applied-science.js-interop :as j]))

; REG SUBS

(rf/reg-sub
 :kl
 (fn [db [_ id]]
   (-> db :editors (get id) :kl)))

(rf/reg-sub
 :uid
 (fn [db [_ id]]
   (if-let [n (-> db :uids (get id))]
     (str id "-" n)
     id)))

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
 :snapshot
 (fn [db _]
   (mapv #(dissoc (val %) :kl) (:editors db))))

(rf/reg-sub
 :mode-options
 (fn [db _]
   (:mode-options db)))

; REG EVENTS

(rf/reg-event-db
 :new-uid
 (fn [db [_ id]]
   (update-in db [:uids id] inc)))

(rf/reg-event-db
 :hidden?
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
 :update-snippet
 (fn [db [_ id]]
   (assoc-in db [:editors id :snippet] (p/get-code (-> db :editors (get id) :kl)))))

(rf/reg-event-db
 :reg-mode-options
 (fn [db [_ mode-options]]
   (update-in db [:mode-options] into mode-options)))

(rf/reg-event-db
 :initialize
 (fn [_ _]
   (add-watch klreg/mode-options :re-frame-reg #(rf/dispatch [:reg-mode-options (keys %4)]))
   {:editors {}
    :mode-options (into (sorted-set) (keys @klreg/mode-options))}))

; COMPONENTS

(defn- editor [{:keys [mode attrs kl id snippet]}]
  (let [hidden? (rf/subscribe [:hidden? id])]
    (fn []
      [:div.pou-wrapper
       [:div.pou-toolbar
        [:button.toggle-min
         {:on-click #(rf/dispatch [:show-hide id])}
         (if @hidden? ">" "<")]
        (str "#" kl ", id: " id ", mode: " mode)]
       [:div.pou-editor
        {:style {:display (if @hidden? "none" "block")}}
        [:div.pou-klipse (assoc attrs :id id) (str snippet)]]])))

(defn editor-comp [{:keys [mode klipsettings] :as editor-settings}]
  (r/create-class
    {:component-did-mount
     (fn [this]
       (klp/klipsify (. (rdom/dom-node this) querySelector ".pou-klipse") klipsettings mode))               
     :reagent-render 
     (editor editor-settings)}))

(defn append-editor [{:keys [id] :as editor}]
  (let [uid @(rf/subscribe [:uid id])]
    (rf/dispatch [:reg-editor {uid (assoc editor :id uid)}])
    (rf/dispatch [:new-uid id])
    (when-not (= id uid) 
      (rf/dispatch [:new-uid uid]))))

(p/reg-append-fn append-editor)

(defn snapshot []
  (doseq [id @(rf/subscribe [:ids])]
    (rf/dispatch [:update-snippet id]))
  (rf/subscribe [:snapshot]))

(defn load-snapshot [snapshot discard-old?]
  (map append-editor snapshot))

(defn select-mode-comp [value-atom mode-options-atom]
  (fn []
    [:select
     {:on-change #(reset! value-atom (.. % -target -value))}
     (for [k @mode-options-atom]
       ^{:key k} [:option {:value k} k])]))

(defn pou-re-frame []
  (let [sel-mode (r/atom nil)
        from-gist (r/atom nil)
        ext-libs (r/atom nil)]
    (fn []
      [:div#pou-app
       (for [e @(rf/subscribe [:editors])]
         ^{:key (key e)} [editor-comp (val e)])
       [:button
        {:on-click #(p/append-editor {})}
         "+eval-clojure"] " | "
       [:button
        {:on-click (fn [_]
                     (p/append-editor {:mode @sel-mode 
                                       :external-libs @ext-libs
                                       :attrs {:data-gist-id @from-gist}})
                     (reset! from-gist nil)
                     (reset! ext-libs nil))}
        "+"]
       [select-mode-comp sel-mode (rf/subscribe [:mode-options])] " "
       [:label "from-gist: "
        [:input {:type "text"
                 :placeholder "user/id"
                 :value @from-gist
                 :on-change #(reset! from-gist (.. % -target -value))}]] " "
       [:label "external-libs: "
        [:input {:type "text"
                 :value @ext-libs
                 :on-change #(reset! ext-libs (.. % -target -value))}]]])))

; INITIALIZE

(set! js/pou.core.append-editor append-editor)
(rf/dispatch [:initialize])
(rdom/render [pou-re-frame] (gdom/getElement "app"))
