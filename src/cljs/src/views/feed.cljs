;; cljs/src/views/feed.cljs: UI implementation for feed management.
;; Copyright 2011-2013, Vixu.com, F.M. (Filip) de Waard <fmw@vixu.com>.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;; http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns vix.views.feed
  (:use-macros [vix.crossover.macros :only [defhandler]])
  (:require [vix.document :as document]
            [vix.util :as util]
            [vix.ui :as ui]
            [clojure.set :as set]
            [clojure.string :as string]
            [domina :as domina]
            [domina.events :as events])
  (:use [domina.css :only [sel]]))

(def error-messages
  {:default-slug-has-invalid-chars-error
   (str "Slugs can only contain '/', '-', '.', alphanumeric characters "
        "and tokens (e.g. {day} and {document-title}).")
   :default-slug-has-consecutive-dashes-or-slashes-error
   "Slugs shouldn't contain any consecutive '-' or '/' characters."
   :default-slug-initial-slash-required-error
   "The default slug needs to start with a '/'."
   :default-slug-requires-document-title-error
   "The default slug needs to include a {document-title} token."
   :default-slug-has-invalid-tokens-error
   "The following slug tokens aren't recognized: "
   :default-slug-has-unbalanced-braces-error
   "The default slug contains unbalanced '{' and '}' characters."
   :feed-name-required-error
   "The feed name value is required."
   :feed-title-required-error
   "The feed title value is required."
   :feed-name-has-invalid-characters-error
   "The feed name value can only contain '-' and alphanumeric characters."
   :feed-name-only-allows-dashes-in-body-error
   "The feed name needs to start and end with an alphanumeric character."
   :feed-name-has-consecutive-dashes-error
   "The feed name shouldn't contain any consecutive '-' characters."
   :could-not-save-feed-error
   "Something went wrong while trying to save this feed."
   :feed-update-conflict-error
   (str "This feed has been updated after this page was opened. "
        "Please refresh the page to edit the most recent version.")
   :feed-already-exists-error
   "The provided feed already exists. Please use a different name."})

(defn get-invalid-tokens
  "Takes all tokens (e.g. {year}) from the provided default slug
   format string and returns any tokens that aren't in a predefined
   list of acceptable tokens (i.e. {language}, {day}, {year}, {month},
   {document-title}, {feed-name} and {ext})."
  [slug]
  (set/difference (set (re-seq #"\{[^\}]{0,}}" slug))
                  #{"{language}"
                    "{day}"
                    "{month}"
                    "{year}"
                    "{document-title}"
                    "{feed-name}"
                    "{ext}"}))

(defn has-unbalanced-braces?
  "Returns true if all braces are balanced in the provided default
   slug value (i.e. /{title} passes, /{title does not). "
  [slug]
  (let [braces (re-seq #"[\{\}]" slug)]
    (if (odd? (count braces))
      true
      (not-empty (filter #(not (= ["{" "}"] %)) (partition 2 braces))))))

(defn validate-default-slug-format
  "Validates the provided slug string and returns :pass if valid, or,
   if invalid, a map with a keyword for the error mapped to its :error
   key and the applicable human-readable error message mapped to
   its :message key.

   Slugs must:
   - start with a slash (:default-slug-initial-slash-required-error),
   - not contain any invalid characters (i.e. [^-a-zA-Z0-9{}.];
     :default-slug-has-invalid-chars-error),
   - contain a {document-title} token
     (:default-slug-requires-document-title-error),
   - have a balanced number of braces (see the has-unbalanced-braces fn;
     :default-slug-has-unbalanced-braces-error),
   - not have any invalid tokens (see the get-invalid-tokens fn;
     :default-slug-has-invalid-tokens-error),
   - not have consecutive slashes or dashes
     (:default-slug-has-consecutive-dashes-or-slashes-error)."
  [slug]
  (let [invalid-tokens (get-invalid-tokens slug)]
    (cond
     (not (= (first slug) "/"))
     {:error :default-slug-initial-slash-required-error
      :message (:default-slug-initial-slash-required-error error-messages)}
     (re-find #"[^/\-a-zA-Z0-9\{\}\.]" slug)
     {:error :default-slug-has-invalid-chars-error
      :message (:default-slug-has-invalid-chars-error error-messages)}
     (not (re-find #"\{document-title\}" slug))
     {:error :default-slug-requires-document-title-error
      :message (:default-slug-requires-document-title-error error-messages)}
     (has-unbalanced-braces? slug)
     {:error :default-slug-has-unbalanced-braces-error
      :message (:default-slug-has-unbalanced-braces-error error-messages)}
     (not-empty invalid-tokens)
     {:error :default-slug-has-invalid-tokens-error
      :message (str (:default-slug-has-invalid-tokens-error error-messages)
                    (apply str (interpose ", " invalid-tokens)))}
     (util/has-consecutive-dashes-or-slashes? slug)
     {:error :default-slug-has-consecutive-dashes-or-slashes-error
      :message (:default-slug-has-consecutive-dashes-or-slashes-error
                error-messages)}
     :else
     :pass)))

(defn validate-feed-name
  "Validates the feed name. Returns :pass if valid, or, if invalid,
   a map with the keyword for the particular error mapped to its
   :error key and the applicable human-readable error message mapped
   to its :message key.

   Feed names must not:
   - be blank (:feed-name-required-error),
   - contain invalid characters (i.e. not [-a-zA-Z0-9]+ ;
     :feed-name-has-invalid-characters-error),
   - start or end with a dash character
     (:feed-name-only-allows-dashes-in-body-error),
   - have any consecutive dashes (:feed-name-has-consecutive-dashes-error)."
  [feed-name]
  (cond
   (string/blank? feed-name)
   {:error :feed-name-required-error
    :message (:feed-name-required-error error-messages)}
   (not (re-matches #"[\-a-zA-Z0-9]+" feed-name))
   {:error :feed-name-has-invalid-characters-error
    :message (:feed-name-has-invalid-characters-error error-messages)}
   (or (= (first feed-name) "-") (= (last feed-name) "-"))
   {:error :feed-name-only-allows-dashes-in-body-error
    :message (:feed-name-only-allows-dashes-in-body-error error-messages)}
   (util/has-consecutive-dashes-or-slashes? feed-name)
   {:error :feed-name-has-consecutive-dashes-error
    :message (:feed-name-has-consecutive-dashes-error error-messages)}
   :else
   :pass))

(defn group-feeds-by-language
  "Converts a sequence of feed maps to a map with full language names
   as string key values and a sequence of feeds for that language as
   values."
  [feeds]
  (let [languages (map :language-full feeds)]
    (zipmap languages
            (map (fn [language]
                   (filter #(= language (:language-full %)) feeds))
                 languages))))

(defn list-feeds-for-language
  "Accepts a vector with the full language string as the first value
   and the feeds for that language as the second value. Returns a map
   that can be passed as an argument to the ui/show! function to
   display the segment for the particular language in the feed UI."
  [[language feeds]]
  {:snippet :feed/.feed-list-for-language-container
   :transformations
   [{:selector (str "//div[@class='"
                    "feed-list-for-language-container"
                    "'][last()]/h4[1]")
     :text language}]
   :children
   (map
    (fn [{:keys [title language name]}]
      (let [parent-xpath
            "(//table[@class='feed-list'])[last()]/tbody[last()]"]
        {:parent parent-xpath
         :snippet :feed/.feed-list-row
         :transformations
         [{:selector (str parent-xpath "/tr[last()]/td[1]/a[1]")
           :text title
           :attrs {:href (str "/admin/" language  "/" name "/overview")}}
          {:selector (str parent-xpath "/tr[last()]/td[2]/a[1]")
           :attrs {:href (str "/admin/edit-feed/" language "/" name)}}
          {:selector (str parent-xpath "/tr[last()]/td[3]/a[1]")
           :attrs {:id (str "delete_link:"  language ":" name)}}]}))
    feeds)})

(defhandler delete-feed-callback
  "Deletes the given feed."
  [evt]
  (let [[_ language feed-name] (string/split (.-id (events/target evt)) ":")]
    (document/delete-feed-shortcut language
                                   feed-name
                                   list-feeds!)))

(defhandler delete-document-callback
  "Deletes the given document."
  [language feed-name evt]
  (let [[_ slug] (string/split (.-id (events/target evt)) ":")]
    (document/delete-document-shortcut
     slug
     #(list-documents! language feed-name))))

(defhandler add-feed-button-callback
  "Navigates to the add new feed form."
  [evt]
  (util/navigate "new-feed" "New Feed"))

(defhandler add-document-button-callback
  "Navigates to the add new document form."
  [language feed-name evt]
  (util/navigate (str language "/" feed-name "/new") "New Feed"))

(defn list-feeds!
  "Loads all available feeds and creates and displays the feed
  overview UI."
  []
  (util/set-page-title! "Feeds overview")
  (document/get-feeds-list
   (fn [status feeds]
     (when (= status 200)
       (ui/show!
        (doall (map list-feeds-for-language (group-feeds-by-language feeds)))
        {:snippet :feed/add-feed-button})

       (util/xhrify-internal-links! (util/get-internal-links!))
       
       (events/listen! (sel "a.delete-feed")
                       :click
                       delete-feed-callback)
       (events/listen! (sel "#add-feed-button")
                       :click
                       add-feed-button-callback)
       nil))))

(defhandler default-slug-format-change-callback
  "Handles default slug format changes."
  [evt]
  (let [value (.-value (events/target evt))]
    (if (= value "custom")
      (domina/remove-attr! (sel "#default-slug-format") :disabled)
      (doto (sel "#default-slug-format")
        (domina/set-value! value)
        (domina/set-attrs! {:disabled "disabled"})))))

(defhandler feed-name-validation-callback
  "Performs validation when the feed name value changes and displays
  and removes errors when necessary."
  [evt]
  (let [feed-name-el (events/target evt)]
    (ui/add-or-remove-errors (sel "#status-message")
                             feed-name-el
                             (validate-feed-name
                              (ui/get-form-value feed-name-el)))))

(defhandler default-slug-format-validation-callback
  "Performs validation when the default-slug-format value changes and
  displays and removes errors when necessary."
  [evt]
  (let [dsf-el (events/target evt)]
    (ui/add-or-remove-errors
     (sel "#status-message")
     dsf-el
     (validate-default-slug-format (.-value dsf-el)))))

(defn get-feed-data-from-dom!
  "Returns a map with the feed data extracted from the form elements
   in the DOM."
  []
  (let [[language language-full]
        (util/pair-from-string (ui/get-form-value "language"))]
    (merge
     (zipmap [:name
              :title
              :subtitle
              :default-slug-format
              :default-document-type]
             (map ui/get-form-value ["feed-name"
                                     "title"
                                     "subtitle"
                                     "default-slug-format"
                                     "default-document-type"]))
     {:language language
      :language-full language-full
      :searchable (string? (ui/get-form-value "searchable"))})))

(defhandler save-feed-button-click-callback
  "Performs validation when the default-slug-format value changes and
  displays and removes errors when necessary."
  [new? [current-feed-state & _] evt]
  (let [save-button-el (events/target evt)
        status-message-el (sel "#status-message")
        future-state (get-feed-data-from-dom!)
        feed-name-validation (validate-feed-name (:name future-state))]
    (cond
     (not (= :pass feed-name-validation))
     (ui/display-error status-message-el
                       (:message feed-name-validation))
     (string/blank? (:title future-state))
     (ui/display-error status-message-el
                       (:feed-title-required-error error-messages))
     :default
     (when-not (= (select-keys current-feed-state (keys future-state))
                  future-state) ;; don't save if there are no changes
       (domina/set-attr! save-button-el :disabled "disabled")
       (document/append-to-feed
        (merge future-state
               (if new?
                 {:action :create}
                 {:action :update
                  :previous-id (:_id current-feed-state)}))
        (fn [status feed-states]
          (cond
           (= status 201)
           (let [{:keys [language name]} (first feed-states)]
             (util/navigate-replace-state
              (str "edit-feed/" language "/" name)
              (str "Edit feed \"" name "\"")))
           (= status 200)
           (display-feed-form! status feed-states)
           :else
           (ui/display-error status-message-el
                             (if (= "The provided feed already exists."
                                    feed-states)
                               (:feed-already-exists-error error-messages)
                               (:could-not-save-feed-error error-messages))))
          (domina/remove-attr! save-button-el :disabled)))))))

(defn display-feed-form!
  "Displays the feed form UI for both new and existing feeds. Requires
   a numeric HTTP status code and a feed-states sequence to display
   the edit feed UI."
  [& [status feed-states]]
  (let [new? (nil? status)
        [{:keys [_id
                 language
                 language-full
                 name
                 title
                 subtitle
                 default-slug-format
                 default-document-type
                 searchable]}]
        feed-states
        custom-slug?
        ((complement some) #{default-slug-format}
         ["/{language}/{feed-name}/{document-title}"
          "/{language}/{feed-name}/{document-title}.{ext}"
          "/{language}/{document-title}"
          "/{language}/{year}/{month}/{day}/{document-title}"])]
    (ui/show!
     {:snippet :ui/status-message}
     {:snippet :ui/caption
      :transformations
      [{:selector "//h3[@id='caption']"
        :text (if new? "Add Feed" "Edit Feed")}]}
     {:snippet :feed/feed-language-row
      :transformations
      [(merge
        {:selector "//select[@id='language']"
         :value (if new?
                  "['en','English']"
                  (str
                   "['" language "','" language-full"']"))}
        (when (not new?)
          {:attrs {:disabled "disabled"}}))]}
     {:snippet :feed/feed-name-row
      :transformations
      [{:selector "//input[@id='feed-name']"
        :value name}
       (when (not new?)
         {:selector "//input[@id='feed-name']"
          :attrs {:disabled "disabled"}})]}
     {:snippet :feed/feed-title-row
      :transformations
      [{:selector "//input[@id='title']"
        :value title}]}
     {:snippet :feed/feed-subtitle-row
      :transformations
      [{:selector "//input[@id='subtitle']"
        :value subtitle}]}
     {:snippet :feed/feed-default-slug-format-row
      :transformations
      [(when (not new?)
         {:selector "//select[@id='default-slug-format-select']"
          :value (if custom-slug?
                   "custom"
                   default-slug-format)})]}
     {:snippet :feed/feed-custom-slug-format-row
      :transformations
      [(merge
        {:selector "//input[@id='default-slug-format']"
         :value default-slug-format}
        (when custom-slug?
          {:remove-attr :disabled}))]}
     {:snippet :feed/feed-default-document-type-row
      :transformations
      [(when (not new?)
         {:selector "//select[@id='default-document-type']"
          :value default-document-type})]}
     {:snippet :feed/feed-searchable-row
      :transformations
      [{:selector "//input[@id='searchable']"
        :checked searchable}]}
     {:snippet :feed/save-feed})

    (events/listen! (sel "#default-slug-format-select")
                    :change
                    default-slug-format-change-callback)

    (events/listen! (sel "input#feed-name")
                    :keyup
                    feed-name-validation-callback)

    (events/listen! (sel "input#default-slug-format")
                    :keyup
                    default-slug-format-validation-callback)

    (events/listen! (sel "button#save-feed")
                    :click
                    (partial save-feed-button-click-callback
                             new?
                             feed-states)))

  nil)

(defn display-edit-feed-form!
  "Loads the feed data for given language and feed-name and passes the
   result to the display-feed-form! function."
  [language feed-name]
  (document/get-feed language
                     feed-name
                     display-feed-form!))

(defn list-documents!
  "Displays an overview of the documents for the given feed."
  [language feed-name]
  (document/get-documents-for-feed
   language
   feed-name
   (fn [status data]
     (when (= status 200)
       (ui/show!
        {:snippet :ui/caption
         :transformations
         [{:selector "//h3[@id='caption']"
           :text (str "Document overview for '" feed-name "'")}]}
        {:snippet :feed/document-list
         :children
         (doall
          (map
           (fn [{:keys [slug title]}]
             {:snippet :feed/.document-list-row
              :parent "//ul[@id='document-list']"
              :transformations
              [{:selector "(//li[@class='document-list-row'])[last()]/a[1]"
                :text title
                :attrs {:href (str "/admin/"
                                   language
                                   "/"
                                   feed-name
                                   "/edit"
                                   slug)}}
               {:selector "(//li[@class='document-list-row'])[last()]/a[2]"
                :attrs {:id (str "delete_link:" slug)}}]})
           (:documents data)))}
        {:snippet :feed/add-document-button})

       (util/xhrify-internal-links! (util/get-internal-links!))

       (events/listen! (sel "#add-document-button")
                       :click
                       (partial add-document-button-callback
                                language
                                feed-name))

       (events/listen! (sel "a.delete-document")
                       :click
                       (partial delete-document-callback language feed-name))
       nil))))