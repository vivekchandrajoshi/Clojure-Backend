(ns cpe.model.comment
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! <! promise-chan chan]]
            [cljs-time.core :as t]
            [cljs-time.coerce :as tc]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.mongodb :as db]
            [cpe.entity :refer [from-db to-db attr-key-db schema-db]]
            [cpe.custommongodbfn :as c]
            [cpe.mail :as mail]
            ))

(defn create-comment-by-plant-id
  [data claims]
  (log/trace "create comment by plantId: claims:" (pr-str claims) ", data:" (pr-str data))
  (let [ret-chan (promise-chan)]
    (go
      (let [{:keys [err msg result]} (<! (db/insert-entity :comment
                                           (-> data
                                             ;(dissoc :last-read :date-modified :modified-by)
                                             (assoc :comments []
                                                    :created-by (:id claims)
                                                    :date-created (js/Date.)
                                                    :last-post (js/Date.)
                                                    :last-read [{:read-by (claims :id)
                                                                 :read-on (js/Date.)}]))))
            user-id (->> ((<! (db/find-entities :user
                                #js{:plantId (:plant-id data),
                                    :emailAlert true}
                                {:project #js{:plantId 1}})) :result)

                      (mapv (fn [user]
                              (if-not(= (claims :id) (user :id))
                                (user :id)
                                ))))
            plant-name ((<! (db/find-one :plant
                              #js{:_id (:plant-id data)}
                              {:project #js{:name 1}}
                             )) :result)
            ]
        (if err
          (put! ret-chan {:err err :msg msg})
          (do
            (if-not (empty? (remove nil? user-id))
               (mail/send-mail
                (mail/mail-template (:name claims)
                                     (remove nil? user-id) "comment"
                                     (plant-name :name) (data :subject) (data :subject))))
            (put! ret-chan {:result result}))
          )))
    ret-chan))

(defn get-comment-by-plant-id
  [query]
  (log/trace "get comment by plantId: query: " query)
  (c/get-comment-by-plant-id :comment query))

(defn get-comments-with-reply
  [query]
  (log/trace "get comments with reply by plantId: query: " query)
  (c/get-comments-with-reply :comment query))

(defn update-comment
  [data claims]
  (log/trace "update comment by commentId: claims:" claims "comment" (pr-str data))
  (c/update-comment :comment (-> data
                               (dissoc :date-created :created-by :plant-id :last-post :last-read :comments :id)
                               (assoc data
                                      :date-modified (js/Date.)
                                      :modified-by (:id claims))) (:id data)))

(defn get-reply-by-comment-id
  [query claims]
  (log/trace "get" (pr-str claims query))
  (let [ret-chan (promise-chan)]
    (go
      (let [update-read (<! (c/update-read :comment
                              {:read-on (js/Date.)
                               :read-by (claims :id)} (query :id)))
            dbData       (if ((js->clj update-read :keywordize-keys true) :result)
                           (<! (c/get-reply-by-comment-id :comment query))
                           )
            commentsData (get-in dbData [:result :comments] [])]
        (put! ret-chan {:result commentsData})
        ))
    ret-chan))

(defn get-reply-by-time
  [query]
  (log/trace "get comment" (pr-str query))
  (c/get-reply-by-time query))


(defn get-comment-by-comment-id
  [query claims]
  (log/trace "get-comment-by-comment-id: " (pr-str query))
  (c/get-comment-by-comment-id :comment query))


(defn update-read
  [data claims]
  (log/trace "update comment read by commentId: claims:" claims "comment" (pr-str data))
  (c/update-read :comment {:read-by (claims :id)
                           :read-on (js/Date.)} (:id data)))

(defn update-include-in-report
  [data claims]
  (log/trace "update comment include-in-report by commentId: claims:" claims "Include in report:" (pr-str data))
  (let [permanently? (contains? data :include-in-report-permanently)
        include? (contains? data :include-in-report)
        save-data (cond-> {}
                    permanently? (assoc :include-in-report-permanently (:include-in-report-permanently data))
                    include? (assoc :include-in-report (:include-in-report data)))]    
    (c/update-comment :comment save-data (:id data))))

(defn update-include-in-report-multi
  [data claims]
  (log/trace "update comment include-in-report-multi: claims:" claims "Include in report multi:" (pr-str data))
  (let [entity-key :comment
        query (clj->js (mapv (fn [{:keys [id include-in-report]}]
                               #js{:updateMany #js{:filter #js{:_id id}
                                                   :update #js{:$set #js{:includeInReport include-in-report}}
                                                   :upsert true}})                    
                             (:include-comments-in-report data)))]
    (db/update-many-entities entity-key data query)))

(defn add-reply
  [data claims]
  (log/trace "Add comment by commentId: comment" (pr-str data))
  (let [ret-chan (promise-chan)]
    (go
      (let [id      (:id data)
            comment (:comment data)
            user-id (->> ((<! (db/find-entities :user
                                #js{:plantId (:plant-id data),
                                    :emailAlert true}
                                {:project #js{:plantId 1}})) :result)

                      (mapv (fn [user]
                              (if-not(= (claims :id) (user :id))
                                (user :id)
                                ))))
            plant-name ((<! (db/find-one :plant
                              #js{:_id (:plant-id data)}
                              {:project #js{:name 1}}
                              )) :result)
            comment-subject ((<! (db/find-one :comment
                              #js{:_id  (data :id)}
                              {:project #js{:subject 1}}
                              )) :result)


            last-post (<! (db/update-comment :comment
                            #js{:$set #js{:lastPost (js/Date.)}}
                            #js{:_id  (data :id)}))
            {:keys [err msg result]} (<! (c/update-read :comment
                                           {:read-on (js/Date.)
                                            :read-by (claims :id)} id))]
        (if err
          (put! ret-chan {:err err :msg msg})
          (let [db-chan (db/add-reply :comment (assoc {} :comment comment
                                                         :date-created (js/Date.)
                                                         :created-by (:id claims)) id)
                {:keys [err msg result]} (<! db-chan)]
            (if err
              (put! ret-chan {:err err :msg msg})
              (do
                (if-not (empty? (remove nil? user-id))
                  (mail/send-mail
                  (mail/mail-template (:name claims)
                    (remove nil? user-id) "reply" (plant-name :name)
                    (comment-subject :subject) (data :comment))
                    ))
                (put! ret-chan {:result result}))
              )))))
    ret-chan))
