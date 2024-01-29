(ns pou.modules.github
  (:require [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [cljs-http.client :as http]
            [goog.dom :as gdom]
            [pou.core :as p :refer [pou]]))

(defn- token []
  (some-> @pou :github :access_token))

(def loaded? #(some? (token)))

(defn login! [& {:keys [client-id scope redirect-param-str] 
                 :or {client-id "ecde871676236cae5c25" scope "gist"}}]
  (js/window.open
   (str "https://github.com/login/oauth/authorize?"
        "client_id=" client-id "&"
        "scope=" scope
        (when redirect-param-str
          (str "&redirect_uri="
               "https://bonuoq.github.io/pou?" redirect-param-str)))
   "github" "popup,width=480,height=600,left=100,top=100"))

(set! js/githubLogin login!)

(defn request [api-path & {:keys [callback selected-keys]}]
  (go
   (let [{:keys [status body]} (<! (http/get (str "https://api.github.com/" api-path)
                                             {:with-credentials? false
                                              :oauth-token (token)}))
         res (if (= status 200)
               (if (not-empty selected-keys)
                 (if (vector? body)
                   (mapv #(select-keys % selected-keys) body)
                   (select-keys body selected-keys))
                 body)
               {:error status :message (str "GitHub API Request Error (status=" status "): " body)})]
     (when callback (callback res))
     res)))

(defn- update-div! [inner-html]
  (-> "pou-github" gdom/getElement .-innerHTML 
    (set! inner-html)))

(defn- update-user! [{:keys [login avatar_url]}]
  (update-div! (str "<span class='gh-login'><img class='gh-avatar' src='" avatar_url "'>" login "</span>"))
  (swap! pou update-in [:github] merge {:user login :avatar avatar_url}))

(defn update-gists! []
  (request "gists" 
           :selected-keys [:id :description :files]
           :callback (fn [gists]
                       (swap! pou assoc-in [:github :gists] gists))))
  
(defn- logged! [auth-res]
  (when (:access_token auth-res)
    (swap! pou assoc :github auth-res)
    (request "user" :callback update-user!)
    (update-gists!)))

(defn auth! [code]
  (.replaceState js/history {} "" "/pou")
  (go
   (let [{:keys [status body]}
         (<! (http/post (str "https://cors-anywhere.herokuapp.com/" ; for development purposes
                             "https://github.com/login/oauth/access_token")
                        {:with-credentials? false
                         :headers {"Accept" "application/json"}
                         :json-params {:client_id "ecde871676236cae5c25"
                                       :client_secret "38d46c164985bf82f9b617f7d0cd95633026ac48"
                                       :code code
                                       :redirect_uri "https://bonuoq.github.io/pou/"}}))]
     (js/console.log (str "GitHub Auth response (" status "): " body))
     (if (= status 200)
       (logged! body)
       (println (str "Github API Authorization Error (status=" status "): " body))))))

; side-fx

(-> "pou-extensions" gdom/getElement .-innerHTML 
  (set! "<div id='pou-github' class='pou-extension'><button class='gh-login' onclick='githubLogin()'>GitHub connect</button></div>"))

(p/process-url-params :code #(auth! %))
