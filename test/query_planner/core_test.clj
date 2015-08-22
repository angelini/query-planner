(ns query-planner.core-test
  (:require schema.test
            [clojure.test :refer :all]
            [query-planner.core :refer :all]))

(deftest optimize-empty
  (let [q {:rows [[(load-n "a"), (load-n "b")]
                  [(select-n),   (empty-n)]]}
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

(use-fixtures :once schema.test/validate-schemas)
