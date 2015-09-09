(ns query-planner.core-test
  (:require schema.test
            [clojure.test :refer :all]
            [query-planner.core :refer :all]))

(deftest validate-neighbours
  (let [q (new-query [[(load-n "a") (select-n)]])]
    (is (not (validate q))))
  (let [q (new-query [[(load-n "a") (load-n "b")]
                      [(select-n)   (empty-n)]])]
    (is (validate q))))

(deftest optimize-empty
  (let [q (new-query [[(load-n "a") (load-n "b")]
                      [(select-n)   (empty-n)]])
        e (new-query [[(load-n "a")]
                      [(select-n)]])]
    (is (= (optimize q) e))))

(deftest optimize-filter
  (let [q (new-query [[(load-n "a")]
                      [(map-n "ident")]
                      [(filter-n "true")]])
        e (new-query [[(load-n "a")]
                      [(filter-n "true")]
                      [(map-n "ident")]])]
    (is (= (optimize q) e))))

(deftest optimize-filter-with-join
  ;; Filter on right hand side of join
  (let [q (new-query [[(load-n "a")]
                      [(join-n)   (load-n "b")]
                      [(empty-n)  (map-n "ident")]
                      [(empty-n)  (filter-n "true")]
                      [(select-n) (select-n)]])
        e (new-query [[(load-n "a")]
                      [(join-n)   (load-n "b")]
                      [(empty-n)  (filter-n "true")]
                      [(empty-n)  (map-n "ident")]
                      [(select-n) (select-n)]])]
    (is (= (optimize q) e)))
  ;; Filter on left hand side of join
  (let [q (new-query [[(load-n "a")]
                      [(join-n)          (load-n "b")]
                      [(map-n "ident")   (empty-n)]
                      [(filter-n "true") (empty-n)]
                      [(select-n)        (select-n)]])
        e (new-query [[(load-n "a")]
                      [(filter-n "true") (empty-n)]
                      [(join-n)          (load-n "b")]
                      [(map-n "ident")   (empty-n)]
                      [(select-n)        (select-n)]])]
    (is (= (optimize q) e))))

(use-fixtures :once schema.test/validate-schemas)
