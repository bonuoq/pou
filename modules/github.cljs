(ns pou.modules.github
  (:require [cljs.core.async :refer [<!] :refer-macros [go]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [cljs-http.client :as http]
            [pou.core :refer [pou]]))

(defn login! []
  (set! js/window.location "https://github.com/login/oauth/authorize?client_id=ecde871676236cae5c25"))

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
     (js/console.log (str "Github Login: " [status body]))
     (swap! pou assoc :github (js->clj body :keywordize-keys true)))))