(ns query-planner.core-test
  (:require schema.test
            [clojure.test :refer :all]
            [query-planner.core :refer :all]))

(deftest optimize-empty
  (let [q {:rows [[(load-n "a") (load-n "b")]
                  [(select-n)   (empty-n)]]}
        e {:rows [[(load-n "a")]
                  [(select-n)]]}]
    (is (= (optimize q) e))))

(deftest optimize-filter
  (let [q {:rows [[(load-n "a")]
                  [(map-n "ident")]
                  [(filter-n "true")]]}
        e {:rows [[(load-n "a")]
                  [(filter-n "true")]
                  [(map-n "ident")]]}]
    (is (= (optimize q) e))))

(deftest optimize-filter-with-join
  ;; Filter on right hand side of join
  (let [q {:rows [[(load-n "a")]
                  [(join-n)   (load-n "b")]
                  [(empty-n)  (map-n "ident")]
                  [(empty-n)  (filter-n "true")]
                  [(select-n) (select-n)]]}
        e {:rows [[(load-n "a")]
                  [(join-n)   (load-n "b")]
                  [(empty-n)  (filter-n "true")]
                  [(empty-n)  (map-n "ident")]
                  [(select-n) (select-n)]]}]
    (is (= (optimize q) e)))
  ;; Filter on left hand side of join
  (let [q {:rows [[(load-n "a")]
                  [(join-n)          (load-n "b")]
                  [(map-n "ident")   (empty-n)]
                  [(filter-n "true") (empty-n)]
                  [(select-n)        (select-n)]]}
        e {:rows [[(load-n "a")]
                  [(filter-n "true") (empty-n)]
                  [(join-n)          (load-n "b")]
                  [(map-n "ident")   (empty-n)]
                  [(select-n)        (select-n)]]}]
    (is (= (optimize q) e))))

(use-fixtures :once schema.test/validate-schemas)
