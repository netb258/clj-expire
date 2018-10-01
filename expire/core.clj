(ns expire.core
  (:require [expire.data-store :as data-store]
            [clj-time.core :as date]
            [clj-time.coerce :as date-conv])
  (:import  (net.sourceforge.jdatepicker.impl JDatePickerImpl)
            (net.sourceforge.jdatepicker.impl JDatePanelImpl)
            (net.sourceforge.jdatepicker.impl UtilDateModel)))

(println "Loading part 2/2")

(use '[seesaw.core :only [frame vertical-panel flow-panel button dialog table text label scrollable config! hide! show! native!]])

(native!)

(def data-file "database.db")

(def date-dialog
  (dialog
    :width 250
    :height 160))

(def data-table
  (table
    :font "COURIER-PLAIN-17"))

(defn init-date-dialog!
  [dialog date-picker name-field]
  (config!
    dialog
    :content
    (vertical-panel
      :items
      [(flow-panel :align :left :items [(label :text "Name:")])
       name-field
       (flow-panel :align :left :items [(label "Expire Date:")])
       date-picker
       (flow-panel
         :items
         [(button :text "Save" :listen [:action (fn [event] (hide! date-dialog))])
          (button :text "Cancel" :listen [:action
                                          (fn [event]
                                            ;; This should prevent prevent the data from being saved.
                                            (config! name-field :text "")
                                            (hide! date-dialog))])])])))

(defn show-date-dialog
  "Shows the date dialog and returns the java.util.Date that was picked or nil if none was picked."
  ([] (show-date-dialog ""))
  ([task-name]
   (let [name-field (text task-name)
         date-picker (JDatePickerImpl. (JDatePanelImpl. (UtilDateModel.)))]
     (init-date-dialog! date-dialog date-picker name-field)
     (-> date-dialog (.setLocationRelativeTo nil))
     (show! date-dialog)
     [(text name-field) (-> date-picker (.getModel) (.getValue))])))

(defn make-main-window [child-widgets]
  (frame :title "Expire"
         :height 480
         :width 800
         :content child-widgets
         :on-close :exit))

(defn date-diff [date1 date2]
  (try
    (inc (date/in-days (date/interval date1 (date-conv/from-date date2))))
    (catch Exception _ 0)))

(defn format-date [date]
  (.format (java.text.SimpleDateFormat. "dd MMMM - yyyy") date))

(defn attention [string]
  (str "<html><font color='#F62817'><b>" string "</b></font></html>"))

(defn add-days-left [db]
  (for [record db
        :let [now-date (date/now)
              record-date (:expire-date record)
              days-left (date-diff now-date record-date)]]
    (if (< days-left 60)
      {:name (attention (:name record)) :expire-date (attention (format-date record-date)) :days-left (attention days-left)}
      {:name (:name record) :expire-date (format-date record-date) :days-left (str days-left)})))

(defn display-database! [data-table db]
  (config!
    data-table
    :model
    [:columns [{:key :name  :text "Task Name"}
               {:key :expire-date :text "Expire Date"}
               {:key :days-left :text "Days Left"}]
     :rows (add-days-left db)]))

(defn drop-nth [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

(defn drop-all [indexes coll]
  (keep-indexed #(if (not (.contains indexes %1)) %2) coll))

;; Multiple rows may be deleted at once.
(defn delete-marked-rows [data-table]
  (let [current-data (data-store/read-ds-from-file data-file)
        selected-rows (into [] (.getSelectedRows data-table))]
    (when (not (empty? selected-rows))
      (data-store/write-ds-to-file (drop-all selected-rows current-data) data-file)
      (display-database! data-table (data-store/read-ds-from-file data-file)))))

(defn add-row [data-table]
  (let [[task-name task-date] (show-date-dialog)
        current-data (data-store/read-ds-from-file data-file)]
    (when (and (not= task-name "") (not= nil task-date))
      (data-store/write-ds-to-file
        (data-store/add-new-task {:name task-name :expire-date task-date} current-data)
        data-file)
      (display-database! data-table (data-store/read-ds-from-file data-file)))))

(defn edit-marked-row [data-table]
  (when (> (.getSelectedRow data-table) -1)
    (let [current-data     (data-store/read-ds-from-file data-file)
          selected-row-idx (.getSelectedRow data-table)
          [task-name task-date] (show-date-dialog (:name (nth current-data selected-row-idx)))]
      (when (and (not= task-name "") (not= nil task-date))
        (data-store/write-ds-to-file
          (data-store/replace-task selected-row-idx {:name task-name :expire-date task-date} current-data)
          data-file)
        (display-database! data-table (data-store/read-ds-from-file data-file))))))

(defn make-button-panel []
  (flow-panel
    :items
    [(button :text "Add task" :preferred-size [170 :by 27] :listen [:action (fn [event] (add-row data-table))])
     (button :text "Remove task" :preferred-size [170 :by 27] :listen [:action (fn [event] (delete-marked-rows data-table))])
     (button :text "Edit task" :preferred-size [170 :by 27] :listen [:action (fn [event] (edit-marked-row data-table))])]))

(defn -main []
  (display-database! data-table (data-store/read-ds-from-file data-file))
  (let [main-window (make-main-window
                      (vertical-panel
                        :items [(scrollable data-table)
                                (make-button-panel)]))]
    (-> main-window (.setLocationRelativeTo nil)) ;; Center the main window.
    (show! main-window)))
