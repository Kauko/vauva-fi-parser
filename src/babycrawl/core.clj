(ns babycrawl.core
  (:gen-class)
  (:require [hickory.core :as hickory]
            [hickory.select :as select]
            [clj-http.client :as client]
            [clojure.string :as string]))

(def start-page "https://www.vauva.fi/keskustelu/3569600/muuttopaivakirjat")

(defn replace-links [comment]
  (keep
    (fn [comment-part]
      (if (string? comment-part)
        comment-part
        (when-let [link (get-in comment-part [:attrs :href])]
          link)))
    comment))

(defn- parse-username [individual-comment]
  (->> individual-comment
       (select/select (select/descendant
                        (select/and (select/tag :div)
                                    (select/class "top"))
                        (select/and (select/tag :span)
                                    (select/class "username"))))
       (mapcat :content)
       (apply str)))

(defn- parse-timestamp [individual-comment]
  (->> individual-comment
       (select/select (select/descendant
                        (select/and (select/tag :div)
                                    (select/class "top"))
                        (select/and (select/tag :div)
                                    (select/class "field-name-post-date"))
                        (select/and (select/tag :div)
                                    (select/class "field-items"))
                        (select/and (select/tag :div)
                                    (select/class "field-item"))))
       (mapcat :content)
       (apply str)))

(defn- parse-downvotes [individual-comment]
  (->> individual-comment
       (select/select (select/descendant
                        (select/and (select/tag :div)
                                    (select/class "bottom"))
                        (select/and (select/tag :div)
                                    (select/class "rate-widget"))
                        (select/and (select/tag :li)
                                    (select/class "last"))
                        (select/and (select/tag :span)
                                    (select/class "rate-voting-count"))
                        (select/tag :span)))
       (mapcat :content)
       (apply str)))

(defn- parse-upvotes [individual-comment]
  (->> individual-comment
       (select/select (select/descendant
                        (select/and (select/tag :div)
                                    (select/class "bottom"))
                        (select/and (select/tag :div)
                                    (select/class "rate-widget"))
                        (select/and (select/tag :li)
                                    (select/class "first"))
                        (select/and (select/tag :span)
                                    (select/class "rate-voting-count"))
                        (select/tag :span)))
       (mapcat :content)
       (apply str)))

(defn- parse-header [individual-comment]
  (str
    (parse-username individual-comment)
    " " (parse-timestamp individual-comment)
    " | Ylös " (parse-upvotes individual-comment)
    " | Alas " (parse-downvotes individual-comment)
    "\n\n"
    ))

(defn- parse-quote-contents [quote-contents]
  (->>
    (for [quote-part quote-contents]
      (cond
        (string? quote-part)
        quote-part

        (= :p (:tag quote-part))
        (when-let [c (:content quote-part)]
          (apply str (replace-links c)))

        (= :a (:tag quote-part))
        (get-in quote-part [:attrs :href])

        :else
        nil))
    (remove nil?)
    (apply str)
    (string/trim)))

(defn- parse-quotes [individual-comment]
  (->> (for [quote-box (select/select (select/descendant
                                        (select/and (select/tag :div)
                                                    (select/class "middle"))
                                        (select/and (select/tag :div)
                                                    (select/class "field-items"))
                                        (select/child
                                          (select/and (select/tag :div)
                                                      (select/class "field-item"))
                                          (select/tag :blockquote)))
                                      individual-comment)]
         (let [main-quote (first
                            (select/select
                              (select/and (select/tag :div)
                                          (select/class "quote-content-main-wrapper"))
                              quote-box))]
           (conj
             (vec
               (for [previous-quote (select/select
                                      (select/and (select/tag :div)
                                                  (select/class "quote-content-wrapper"))
                                      quote-box)]
                 (parse-quote-contents (:content previous-quote))))
             (parse-quote-contents (:content main-quote)))))
       (map-indexed
         (fn [box-i parsed-quote-box]
           (str "Quote #" (inc box-i) ":\n"
                (apply str
                       (->> parsed-quote-box
                            (map string/trim)
                            (remove empty?)
                            (map-indexed
                              (fn [quote-i parsed-quote]
                                (str (inc quote-i) ": \"" parsed-quote "\"\n")))))
                "\n")))
       (apply str)))

(defn- parse-text [individual-comment]
  (->> (for [comment-part (select/select (select/child
                                           (select/descendant
                                             (select/and (select/tag :div)
                                                         (select/class "middle"))
                                             (select/and (select/tag :div)
                                                         (select/class "field-items")))
                                           (select/tag :div)
                                           (select/not (select/tag :blockquote)))
                                         individual-comment)]
         (cond
           (string? comment-part)
           comment-part

           (= :p (:tag comment-part))
           (when-let [c (:content comment-part)]
             (apply str (replace-links c)))

           (= :a (:tag comment-part))
           (get-in comment-part [:attrs :href])

           :else
           nil))
       (remove nil?)
       (apply str)
       (string/trim)))

(defn parse-individual-comment [individual-comment]
  (str
    (parse-header individual-comment)
    (parse-quotes individual-comment)
    (parse-text individual-comment)
    "\n\n--------\n"))

(defn individual-comments-from-page [page]
  (select/select
    (select/and (select/tag :div)
                (select/class "sanoma-comment"))
    page))

(defn- get-page-from-url [url]
  (let [{:keys [status body]} (client/get url)]
    (cond (= 200 status)
          (-> body hickory/parse hickory/as-hickory)

          :else
          (let [s (rand-int 61)]
            (do (println status ":: We're being throttled? Sleeping for" s "s")
                (Thread/sleep (* s 1000))
                (get-page-from-url url))))))

(defn parse-page-at-url [url]
  (->> (get-page-from-url url)
       individual-comments-from-page
       (keep parse-individual-comment)
       (apply str)))

(defn get-pages! [base-url]
  (loop [n 1
         comments-from-all-pages [(parse-page-at-url base-url)]]
    (let [comments-from-page (parse-page-at-url (str base-url "?page=" n))
          comments-from-previous-page (last comments-from-all-pages)]
      (if (not= comments-from-page comments-from-previous-page)
        (do (println "Processed page" (inc n))
            #_(Thread/sleep (* (rand-int 5) 1000))
            (recur (inc n)
                   (vec (conj comments-from-all-pages comments-from-page))))
        (do (println "Collected" n "pages.")
            comments-from-all-pages)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Collecting comments")
  (->> (get-pages! start-page)
       (map-indexed
         (fn [i page-str]
           [(str "\n\n***** SIVU " (inc i) " *****\n\n")
            page-str]))
       (mapcat identity)
       (apply str)
       (spit "muuttopaivakirjat_lyhyempi.txt"))
  (println "Done!"))
