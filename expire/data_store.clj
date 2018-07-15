(ns expire.data-store)

(defn read-ds-from-file
  "Read a Clojure data structure from a file"
  [fname]
  (read-string (slurp fname)))

(defn write-ds-to-file
  "Saves a Clojure data structure to a file."
  [ds fname]
  (spit fname (with-out-str (pr ds))))

(defn add-new-task
  {:test #(let [date-now   (java.util.Date.)
                test-data  {:name "TEST" :expire-date date-now}
                test-data2 (list {:name "TEST" :expire-date date-now} {:name "Test2" :expire-date date-now})]
            (assert (= (list test-data) (add-new-task test-data '())))
            (assert (= test-data2
                       (add-new-task test-data (list {:name "Test2" :expire-date date-now})))))}
  [new-task current-tasks]
  (conj current-tasks new-task))

(defn remove-task
  {:test #(let [date-now   (java.util.Date.)
                test-data  {:name "TEST" :expire-date date-now}
                test-data2 (list {:name "TEST" :expire-date date-now} {:name "Test2" :expire-date date-now})]
            (assert (= '() (remove-task "some-name" '())))
            (assert (= (list test-data)
                       (remove-task "Test2" test-data2))))}
  [task-name tasks]
  (filter #(not (= task-name (:name %))) tasks))

(defn replace-task [index new-task current-tasks]
  (let [vec-tasks (into [] current-tasks)]
    (assoc vec-tasks index new-task)))
