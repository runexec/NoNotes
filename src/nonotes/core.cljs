(ns nonotes.core
  (:require [reagent.core :as reagent]
            [cljs.nodejs :as nodejs]
            [markdown.core :refer [md->html]]
            [clojure.string :as string]
            [goog.string :as gstring]
            [goog.string.format]
            [cljs.core.async :as async])
  (:require-macros [cljs.core.async.macros :as async :refer [go go-loop]]))

(def fs
  "Native FIleSystem"
  (nodejs/require "fs"))

(def nix?
  "OS Detection"
  (not= "win32" js/process.platform))

(def slash
  "OS Sensative Path Directory Delimiter"
  (if nix? "/" "\\"))

(let [data-path js/nw.App.dataPath]
  (defonce +state+
    (reagent/atom
     {:paths {:data js/nw.App.dataPath
              :notes data-path}
      :toggle {:editor false
               :about false}
      :note-area ""})))

;; Toggle Handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn toggle!*
  "Simple boolean inversion"
  [& ks]
  (swap! +state+ update-in (into [:toggle] ks) not))

(defn toggle!
  "Async toggle!*"
  [& ks]
  (go
    (apply toggle!* ks)))

(defn toggle
  "Toggle State"
  [k]
  (-> @+state+ :toggle k))

(defn toggle-editor! []
  (toggle! :editor))

(defn toggle-editor!*
  "Synchronous toggle-editor!"
  []
  (toggle!* :editor))

(defn toggle-editor
  "Current value"
  []
  (toggle :editor))

(defn toggle-about! []
  (toggle! :about))

(defn toggle-about
  "Current Value"
  []
  (toggle :about))

;; Note Handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def notes-chan
  "Note area data channel"
  (async/chan))

(go-loop [data (<! notes-chan)]
  (swap! +state+ assoc :note-area data)
  (recur
   (<! notes-chan)))

(defn notes-path
  "For Exporting Notes"
  ([] (-> @+state+ :paths :notes))
  ([pdf-file-name]
   (str (notes-path) slash pdf-file-name)))

(defn note-area
  "Current Note Data"
  []
  (:note-area @+state+))

(defn note-area!
  "Async Note value setting"
  [evt]
  (async/put! notes-chan (.. evt -currentTarget -value)))

(defn html-notes
  "Markdown to HTML"
  []
  (-> (note-area)
      (string/replace #"\n" "<br />\n")
      md->html))

(defn current-notes-fp
  "Loaded Notes FilePath for export"
  []
  (notes-path
   (str js/PDFViewerApplication.documentFingerprint
        ".nonotes")))

;; Page Handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn current-page
  "Reading Page Value"
  []
  (.. js/document (getElementById "pageNumber") -value))

(def inject-page-string
  "Annotation injection when opening note editor"
  (str "%s\n<p><a class=\"annotation note-started\">Note Started : %s</a><br />"
       "<a class=\"annotation page-link\" target=\"_parent\" href=\"#page=%d\">Page %d</a>"
       "<a class=\"annotation page-book\" target=\"_parent\" href=\"#page=1\">%s</a></p>\n\n   "))

(defn inject-note-page!
  "Annotation injection"
  []
  (swap! +state+
         update-in
         [:note-area]
         (fn [na]
           (let [cp (current-page)]
             (gstring/format inject-page-string
                             na
                             (js/Date.)
                             cp
                             cp
                             js/document.title)))))

;; Elements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn editor
  "HTML Element"
  []
  (.. js/document (getElementById "noteInput")))


;; Event Handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn toggle-event!
  "Display Editor Toggle"
  []
  (toggle-editor!))

(defn about-event!
  "About Page Toggle"
  [_]
  (toggle-about!))

(defn open-event!
  "Invoke PDF.js Open File"
  [_]
  (let [fp #(.. js/document (getElementById "openFile"))]
    (.click (fp))))

(defn scroll-preview-event!
  "Sync preview box with input box tail input"
  [_]
  (go
    (let [p (.. js/document
                (getElementById "htmlPreview"))]
      (-> p
          .-scrollTop
          (set! (.-scrollHeight p))))))

(defn new-event!
  "Opens Editor"
  [_]
  (toggle-editor!*)
  (when (toggle-editor)
    (inject-note-page!)
    (js/setTimeout
     (fn []
       (scroll-preview-event! nil)
       (let [e (editor)]
         (-> e
             .-scrollTop
             (set! (.-scrollHeight e)))
         (.focus e)
         (.click e)))
     100)))

(defn load-event!
  "Load Notes"
  [_]
  (.readFile fs
             (current-notes-fp)
             "utf8"
             (fn [err data]
               (if err
                 (.log js/console
                       (str "A Loading Error: " err)))
               (swap! +state+ assoc :note-area (or data "")))))

(defn save-event!
  "Save Notes"
  [_]
  (if (js/confirm "Are you sure you want to save? Old data will be overwritten.")
    (.writeFile fs
                (current-notes-fp)
                (note-area)
                "utf8"
                (fn [err]
                  (if err
                    (js/alert
                     (str "Couldn't save! Error: " err)))))))

(defn note-area-event!
  "Set Note input data"
  [evt]
  (note-area! evt))

;; Link Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn link [href]
  [:a
   {:href href
    :target "_blank"}
   href])

;; Views ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn index
  "The Only View"
  []
  (let [editor? (toggle-editor)]
    [:div.editor-toggle
     {:style {:position :fixed
              :background-color :#FFFFFF
              :width :100%
              :height (if editor? :200px :20px)
              :z-index 10
              :bottom (if editor? :40px :15px)}}

     (when-not editor?
       [:div
        {:style {:width :100% :background :#FFFFFF :padding :2px}}
        [:button  {:style {:width :100%} :on-click new-event!} "Open"]])
     
     [:div
      {:style {:visibility (if editor? "visible" "hidden")}}

      ;; Menu Buttons
      [:button {:style {:width :5%} :on-click about-event!} "?"]
      [:button {:style {:width :5%} :on-click toggle-event!} "✖"]
      [:button {:style {:width :25%} :on-click open-event!} "Open PDF"]
      [:button {:style {:width :25%} :on-click load-event!} "Load Notes"]

      ;; The viewer can't be found until full-page load
      ;; this is a workaround and shouldn't be removed
      (if editor?
        [:a
         {:href (str "file://" (current-notes-fp))
          :on-click #(js/alert "Warning: Anything unsaved will not be exported!")
          :download (gstring/format "NoNotes - %s - %s.md"
                                    js/document.title
                                    (js/Date.))}
         [:button {:style {:width :25%}}
          "Export Notes"]])          

      [:button {:style {:width :15%} :on-click save-event!} "Save"]

      [:div
       {:style {:display (if (toggle-about) "block" "none")
                :position :fixed
                :margin-bottom :100px
                :z-index 20
                :background-color :white
                :width :100%
                :height :200px
                :max-height :200px
                :overflow :auto
                :text-align :center}}

       ;; Close about display
       [:button
        {:style {:left :2px
                 :bottom 170
                 :width :10%
                 :position :fixed}
         :on-click about-event!}
        "close"]
       
       [:h1 "About NoNotes"]

       "Ryan Kelker - 2016"
       
       [:table.aboutTable
       [:tbody
        [:tr 
         [:td "Source Code via Github"]         
         [:td (link "https://www.github.com/runexec/NoNotes")]]

        [:tr
         [:td "Mozilla PDF.js via Github"]
         [:td (link "https://mozilla.github.io/pdf.js/")]]

        [:tr
         [:td "Mozilla PDF.js License"]
         [:td (link "https://github.com/mozilla/pdf.js/blob/master/LICENSE")]]

        [:tr
         [:td "Reagent via Github"]
         [:td (link "https://reagent-project.github.io/")]]
        
        [:tr
         [:td "Reagent License"]
         [:td (link "https://opensource.org/licenses/MIT")]]
        
        [:tr
         [:td "NW.js"]
         [:td (link "https://nwjs.io/")]]
        
        [:tr
         [:td "NW.js License"]
         [:td (link "https://github.com/nwjs/nw.js/blob/nw17/LICENSE")]]]]]

      
      [:table {:style {:width :100%}}
       [:tbody
        [:tr         
         [:td
          {:style {:width :50%}}
          [:textarea#noteInput
           {:value (note-area)
            :onKeyPress scroll-preview-event!
            :onChange note-area-event!
            :style {:width :100% :height :195px}}]]
         
         [:td#htmlPreview.html-preview
          {:dangerouslySetInnerHTML {:__html (html-notes)}
           :style {:position :absolute
                   :width :50%
                   :height :195px
                   :overflow :scroll
                   :background :white}}]]]]]]))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [index] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
