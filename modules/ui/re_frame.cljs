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
   (->> (:editors db)
     (mapv #(dissoc (val %) :kl))
     (mapv #(dissoc % :klipsify?)))))

(rf/reg-sub
 :mode-options
 (fn [db _]
   (:mode-options db)))

(rf/reg-sub
 :mode-selectors
 (fn [db [_ mode]]
   (-> db :mode-selectors mode)))

(rf/reg-sub
 :klipse-settings
 (fn [db _]
   (:klipse-settings db)))

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
 :reg-klipse-settings
 (fn [db [_ klipse-settings]]
   (update-in db [:klipse-settings] into klipse-settings)))

(rf/reg-event-db
 :reg-mode-selectors
 (fn [db [_ mode-selectors]]
   (update-in db [:mode-selectors] into mode-selectors)))

(rf/reg-event-db
 :initialize
 (fn [_ _]
   (add-watch klreg/mode-options :re-frame-reg-mode-options 
              #(rf/dispatch [:reg-mode-options (keys %4)]))
   (add-watch klreg/selector->mode :re-frame-reg-mode-selectors 
              #(rf/dispatch [:reg-mode-selectors (clojure.set/map-invert %4)]))
   {:editors {}
    :mode-options (into (sorted-set) (keys @klreg/mode-options))
    :mode-selectors (clojure.set/map-invert @klreg/selector->mode)
    :klipse-settings (js->clj js/klipse-settings)}))

; ACTIONS AND HELPER FNS

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

;;;;; TODO : ASYNC
(defn load-snapshot [snapshot discard-old?]
  (when discard-old? 
    (for [i @(rf/subscribe [:ids])]
      (rf/dispatch [:discard-editor i])))
  (p/load-editors-async snapshot))

; COMPONENTS

(defn- mode->class [mode]
  (get @(rf/subscribe [:klipse-settings]) @(rf/subscribe [:mode-selectors mode])))

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
        [:div.pou-klipse (assoc attrs :id id :class (mode->class mode)) (str snippet)]]])))

(defn editor-comp [{:keys [mode klipsettings] :as editor-settings}]
  (r/create-class
    {:component-did-mount
     (fn [this]
       (klp/klipsify (. (rdom/dom-node this) querySelector ".pou-klipse") klipsettings mode))
     :reagent-render 
     (editor editor-settings)}))

(defn select-comp [value-atom options-atom]
  (fn []
    [:select
     {:on-change #(reset! value-atom (.. % -target -value))}
     (for [k @options-atom]
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
        {:on-click #(p/append-editor {:klipsify? false})}
         "+eval-clojure"] " | "
       [:button
        {:on-click (fn [_]
                     (p/append-editor {:mode (or @sel-mode (first @(rf/subscribe [:mode-options])))
                                       :external-libs @ext-libs
                                       :attrs {:data-gist-id @from-gist}
                                       :klipsify? false})
                     (reset! from-gist nil)
                     (reset! ext-libs nil))}
        "+"]
       [select-comp sel-mode (rf/subscribe [:mode-options])] " "
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

(rf/dispatch [:initialize])
(rdom/render [pou-re-frame] (gdom/getElement "ui"))
