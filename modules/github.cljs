(ns pou.modules.github
  (:require [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [cljs-http.client :as http]
            [goog.dom :as gdom]
            [pou.core :refer [pou]]))

(defn token []
  (some-> @pou :github :access_token))

(def loaded? #(some? (token)))

(defn login! []
  (set! js/window.location "https://github.com/login/oauth/authorize?client_id=ecde871676236cae5c25"))

(set! js/githubLogin login!)

(defn request [api-path & opt-sel-keys]
  (go
   (let [{:keys [status body]} (<! (http/get (str "https://api.github.com/" api-path)
                                             {:with-credentials? false
                                              :oauth-token (token)}))]
     (if (= status 200)
       (if (not-empty opt-sel-keys)
         (select-keys body opt-sel-keys)
         body)
       (println (str "Github API Request Error (status=" status "): " body))))))

(defn update-div [inner-html]
  (-> "pou-github" gdom/getElement .-innerHTML 
    (set! inner-html)))

(defn auth [code]
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
     (swap! pou assoc :github (js->clj body :keywordize-keys true))
     (let [{:keys [login avatar_url]}
           (<! (request "user" :login :avatar_url))]
       (update-div (str "<span class='gh-login'><img class='gh-avatar' src='" avatar_url "'>" login "</span>"))))))

(-> "pou-extensions" gdom/getElement .-innerHTML 
  (set! "<div id='pou-github' class='pou-extension'><button class='gh-login' onclick='githubLogin()'>Github Login</button></div>"))
