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
                                              :oauth-token (github/token)}))]
     (if (= status 200)
       (if (not-empty opt-sel-keys)
         (select-keys body opt-sel-keys)
         body)
       (println (str "Github API Request Error (status=" status "): " body))))))

(defn auth [code]
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
     (swap! pou assoc :github (js->clj body :keywordize-keys true)))))

(-> "top-bar" gdom/getElement .-innerHTML (set! "<button onclick='githubLogin()'>Github Login</button>"))
