(ns query-planner.core
  (:require [schema.core :as s])
  (:gen-class))

(def Node
  {:action (s/enum :empty :select :load, :filter, :map, :join, :sort, :group)
   :args [s/Str]})

(def Row [Node])

(def Col [Node])

(def Query
  {:rows [Row]})

(defn empty-n  []    {:action :empty  :args []})
(defn select-n []    {:action :select :args []})
(defn load-n   [col] {:action :load   :args [col]})
(defn filter-n [fn]  {:action :filter :args [fn]})
(defn map-n    [fn]  {:action :map    :args [fn]})
(defn join-n   []    {:action :join   :args []})
(defn sort-n   []    {:action :sort   :args []})
(defn group-n  []    {:action :group  :args []})

(defn print-nodes [ns]
  (doseq [node ns]
    (printf "%-14s" (str (:action node) (:args node)))))

(defn print-query [q]
  (doseq [row (:rows q)]
    (print-nodes row)
    (println)))

(defn remove-by-idxs [coll idxs]
  (let [idxs (set idxs)]
    (keep-indexed #(when-not (contains? idxs %1) %2)
                  coll)))

(s/defn col-empty? :- s/Bool
  [col :- Col]
  (loop [node (first col)
         rem (rest col)]
    (cond
      (nil? node) true
      (#{:select :filter :map :join :group} (:action node)) false
      :else (recur (first rem) (rest rem)))))

(s/defn rows->cols :- [Col]
  [rows :- [Row]]
  (for [ci (range (-> rows last count))]
    (for [ri (range (count rows))]
      (get-in rows [ri ci] (empty-n)))))

(s/defn empty-col-idxs :- [s/Int]
  [q :- Query]
  (->> (:rows q)
       rows->cols
       (map col-empty?)
       (map-indexed (fn [idx empty?] (when empty? idx)))
       (filter identity)
       vec))

(s/defn remove-cols :- Query
  [q :- Query
   idxs :- [s/Int]]
  (update q :rows (fn [rows]
                    (mapv #(vec (remove-by-idxs % idxs)) rows))))

(s/defn row-type? :- s/Bool
  [row :- Row
   type :- s/Keyword]
  (-> (map :action row) set type boolean))

(defn filter-row? [row] (row-type? row :filter))
(defn map-row?    [row] (row-type? row :map))
(defn join-row?   [row] (row-type? row :join))

(s/defn border-index-of :- s/Int
  [row :- Row
   type :- s/Keyword
   dir]
  (let [type-idxs (->> (map :action row)
                       (map-indexed (fn [idx a] (when (= a type) idx)))
                       (filter identity))]
    (if (empty? type-idxs)
      -1
      (dir type-idxs))))

(defn right-index-of [row type]
  (border-index-of row type last))

(defn left-index-of [row type]
  (border-index-of row type first))

(s/defn swappable?
  [upper :- Row
   lower :- Row]
  (or (and (map-row? upper)
           (filter-row? lower))
      (and (join-row? upper)
           (filter-row? lower)
           (< (right-index-of lower :filter) (left-index-of upper :load)))))

(s/defn swap-rows :- Query
  [q :- Query]
  (update q :rows
          (fn [rows]
            (let [rev-rows (reverse rows)
                  rev-swapped (reduce (fn [acc row]
                                        (let [lower-row (last acc)]
                                          (if (and lower-row
                                                   (swappable? row lower-row))
                                            (conj (pop acc) row lower-row)
                                            (conj acc row))))
                                      [] rev-rows)]
              (reverse rev-swapped)))))

(s/defn optimize :- Query
  [q :- Query]
  (let [q (remove-cols q (empty-col-idxs q))
        q (swap-rows q)]
    q))

(defn- optimize-and-print [q]
  (println "-> Query")
  (print-query q)
  (println)
  (println "-> Optimized")
  (print-query (optimize q))
  (println))

(defn -main []
  (let [q1 {:rows [[(load-n "a") (load-n "b")]
                   [(select-n)   (empty-n)]]}
        q2 {:rows [[(load-n "a")]
                   [(map-n "ident")]
                   [(filter-n "true")]]}]
    (optimize-and-print q1)
    (optimize-and-print q2)))
