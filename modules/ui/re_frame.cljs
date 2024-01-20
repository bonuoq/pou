(ns pou.modules.ui.re-frame
  (:require [goog.dom :as gdom]
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
 :mode-options
 #(:mode-options @base))

(rf/reg-sub
 :snapshot
 (fn [db _]
   (->> (:editors db)
     (map #(assoc (val %) :snippet (p/get-code (:kl (val %))))) 
     (map #(dissoc % :kl))
     (mapv #(assoc % :intro (-> (:id %) 
                             gdom/getElement 
                             (.querySelector ".pou-intro") 
                             .-textContent))))))

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
 :change-intro
 (fn [db [_ id intro]]
   (assoc-in db [:editors id :intro] intro)))

(rf/reg-event-db
 :initialize
 (fn [_ _] 
   {:editors {}}))

; ACTIONS AND HELPER FNS

(defn append-editor [{:keys [id] :as editor}]
  (let [uid @(rf/subscribe [:uid id])]
    (rf/dispatch [:reg-editor {uid (assoc editor :id uid)}])
    (rf/dispatch [:new-uid id])
    (when-not (= id uid) 
      (rf/dispatch [:new-uid uid]))))

(p/reg-append-fn append-editor)
(swap! base assoc :auto-klipsify false)

(defn snapshot [] (rf/subscribe [:snapshot]))

;;;;; TODO : ASYNC
(defn load-snapshot [snapshot discard-old?]
  (when discard-old? 
    (for [i @(rf/subscribe [:ids])]
      (rf/dispatch [:discard-editor i])))
  (p/append snapshot))

; COMPONENTS

(defn editor-comp [{:keys [kl id intro mode attrs id snippet]}]
  (r/create-class
   {:component-did-mount #(p/klipsify!)
    :reagent-render
    (fn []
      (let [hidden? @(rf/subscribe [:hidden? id])]
        [:div.pou-wrapper {:id id}
         [:div.pou-toolbar
          (str "#" kl)
          [:button.toggle-min
           {:on-click #(rf/dispatch [:show-hide id])}
           (if hidden? "<" ">")]
          [:p.pou-intro {:contentEditable true}
           (or intro (str "id: " id ", mode: " mode))]]
         [:div.pou-editor
          {:style {:display (if hidden? "none" "block")}}
          [:div.pou-klipse attrs (str snippet)]]]))}))

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
        {:on-click #(p/append [{}])}
         "+eval-clojure"] " | "
       [:button
        {:on-click (fn [_]
                     (p/append [{:mode (or @sel-mode (first @(rf/subscribe [:mode-options])))
                                 :external-libs @ext-libs
                                 :attrs {:data-gist-id @from-gist}}])
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
(p/loaded!)
