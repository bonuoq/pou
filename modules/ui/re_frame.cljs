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
   (str id (-> db :uids (get id)))))

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

(defn- editor [{:keys [mode attrs kl id snippet]
                :or {mode "eval-clojure"}}]
  (let [hidden? (rf/subscribe [:hidden? id])]
    (fn []
      [:div.pou-wrapper
       [:div.pou-toolbar
        [:button.toggle-min
         {:on-click #(rf/dispatch [:show-hide id])}
         (if @hidden? ">" "<")]
        (str "[" id "] mode: " mode " #klipse-" kl)]
       [:div.pou-editor
        {:style {:display (if @hidden? "none" "block")}}
        [:div.pou-klipse (assoc attrs :id id) (str snippet)]]])))

(defn editor-comp [{:keys [mode klipsettings] 
                    :or {mode "eval-clojure" klipsettings {}}
                    :as editor-settings}]
  (r/create-class
    {:component-did-mount
     (fn [this]
       (klp/klipsify (. (rdom/dom-node this) querySelector ".pou-klipse") klipsettings mode))               
     :reagent-render 
     (editor editor-settings)}))

(defn append-editor [{:keys [id mode attrs snippet klipsettings hidden?] 
                      :or {id "pou" mode "eval-clojure"} :as editor-map}]
  (let [kl @klp/snippet-counter
        uid @(rf/subscribe [:uid id])]
    (rf/dispatch [:reg-editor {uid (assoc editor-map :mode mode :kl kl :id uid)}])
    (rf/dispatch [:new-uid id])
    (when-not (= id uid) 
      (rf/dispatch [:new-uid uid]))))

(defn snapshot []
  (doseq [id @(rf/subscribe [:ids])]
    (rf/dispatch [:update-snippet id]))
  (rf/subscribe [:snapshot]))

(defn load-snapshot [snapshot]
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
        ext-libs (r/atom "https://bonuoq.github.io")]
    (fn []
      [:div#pou-app
       (for [e @(rf/subscribe [:editors])]
         ^{:key (key e)} [editor-comp (val e)])
       [:button
        {:on-click #(append-editor {:attrs {:data-external-libs "https://bonuoq.github.io"}})}
         "+eval-clojure"] " | "
       [:button
        {:on-click (fn [_]
                     (append-editor {:mode @sel-mode 
                                     :attrs {:data-gist-id @from-gist
                                             :data-external-libs @ext-libs}})
                     (reset! from-gist nil)
                     (reset! ext-libs "https://bonuoq.github.io"))}
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
