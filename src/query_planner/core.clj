(ns query-planner.core
  (:require [schema.core :as s])
  (:gen-class))

(def Node
  {:action (s/enum :load :map :filter :join :select :group :sort :none :empty)
   :args [s/Str]})

(def Row [Node])

(def Col [Node])

(def Query
  {:rows [Row]
   :cols [Col]})

(def valid-neighbours
  {:load   #{:load :empty :none :join}
   :map    #{:map :empty :none}
   :filter #{:filter :empty :none}
   :join   #{:join :empty :none :load}
   :select #{:select :empty}
   :group  #{:empty :none}
   :sort   #{:sort :empty :none}
   :none   #{:none :empty :group :join :filter :map :load}
   :empty  #{:empty :none :group :join :filter :map :load :select}})

(defn load-n   [col] {:action :load   :args [col]})
(defn map-n    [fid] {:action :map    :args [fid]})
(defn filter-n [fid] {:action :filter :args [fid]})
(defn join-n   [col] {:action :join   :args [col]})
(defn select-n []    {:action :select :args []})
(defn group-n  []    {:action :group  :args []})
(defn sort-n   [idx] {:action :sort   :args [idx]})
(defn none-n   []    {:action :none   :args []})
(defn empty-n  []    {:action :empty  :args []})

(defn print-nodes [ns]
  (doseq [node ns]
    (printf "%-14s" (str (:action node) (:args node)))))

(defn print-query [q]
  (doseq [row (:rows q)]
    (print-nodes row)
    (println)))

(defn remove-by-idxs [coll idxs]
  (let [idxs (set idxs)]
    (-> (keep-indexed #(when-not (contains? idxs %1) %2)
                      coll)
        vec)))

(s/defn rows->cols :- [Col]
  [rows :- [Row]]
  (vec (for [ci (range (-> rows last count))]
         (vec (for [ri (range (count rows))]
                (get-in rows [ri ci] (empty-n)))))))

(s/defn actions :- #{s/Keyword}
  [ns :- [Node]]
  (set (map :action ns)))

(s/defn col-empty? :- s/Bool
  [col :- Col]
  (-> (actions col)
      (clojure.set/intersection #{:select :filter :map :join :group :sort})
      empty?))

(s/defn empty-col-idxs :- [s/Int]
  [q :- Query]
  (->> (:cols q)
       (map col-empty?)
       (map-indexed (fn [idx empty?] (when empty? idx)))
       (filter identity)
       vec))

(s/defn new-query :- Query
  [rows :- [Row]]
  {:rows rows
   :cols (rows->cols rows)})

(s/defn remove-cols :- Query
  [q :- Query
   idxs :- [s/Int]]
  (let [rows (mapv (fn [row] (remove-by-idxs row idxs))
                   (:rows q))]
    (new-query rows)))

(s/defn row-type? :- s/Bool
  [row :- Row
   type :- s/Keyword]
  (-> (actions row) type boolean))

(defn filter-row? [row] (row-type? row :filter))
(defn map-row?    [row] (row-type? row :map))
(defn join-row?   [row] (row-type? row :join))
(defn group-row?  [row] (row-type? row :group))

(s/defn col-valid? :- s/Bool
  [col :- Col]
  (let [without-empty (->> col
                           (mapv :action)
                           (filter (fn [a] (not= a :empty))))]
    (and (= :load (first without-empty))
         (not ((set (rest without-empty)) :load)))))

(s/defn cols-valid? :- s/Bool
  [cols :- [Col]]
  (let [valid-cols (map col-valid? cols)]
    (every? true? valid-cols)))

(s/defn row-valid? :- s/Bool
  [row :- Row]
  (let [row-actions (actions row)
        valid-nodes (map (fn [node]
                           (->> (:action node)
                                (get valid-neighbours)
                                (clojure.set/intersection row-actions)
                                count
                                (= (count row-actions))))
                         row)]
    (every? true? valid-nodes)))

(s/defn rows-valid? :- s/Bool
  [rows :- [Row]]
  (let [valid-rows (map row-valid? rows)]
    (every? true? valid-rows)))

(s/defn validate :- s/Bool
  [q :- Query]
  (and (rows-valid? (:rows q))
       (cols-valid? (:cols q))))

(defn- border-index-of [row type dir]
  (let [type-idxs (->> (map :action row)
                       (map-indexed (fn [idx a] (when (= a type) idx)))
                       (filter identity))]
    (if (empty? type-idxs)
      -1
      (dir type-idxs))))

(s/defn right-index-of :- s/Int
  [row :- Row
   type :- s/Keyword]
  (border-index-of row type last))

(s/defn left-index-of :- s/Int
  [row :- Row
   type :- s/Keyword]
  (border-index-of row type first))

(s/defn swappable?
  [upper :- Row
   lower :- Row]
  (and (filter-row? lower)
       (or (map-row? upper)
           (group-row? upper)
           (and (join-row? upper)
                (< (right-index-of lower :filter) (left-index-of upper :load))))))

(s/defn swap-rows :- Query
  [q :- Query]
  (let [rev-rows (reverse (:rows q))
        rev-swapped (reduce (fn [acc row]
                              (let [lower-row (last acc)]
                                (if (and lower-row
                                         (swappable? row lower-row))
                                  (conj (pop acc) row lower-row)
                                  (conj acc row))))
                            [] rev-rows)]
    (new-query (vec (reverse rev-swapped)))))

(s/defn optimize :- Query
  [q :- Query]
  (-> q
      (remove-cols (empty-col-idxs q))
      (swap-rows)))

;; To remove

(defn optimize-and-print [q]
  (println "-> Query")
  (print-query q)
  (println)
  (println "-> Optimized")
  (print-query (optimize q))
  (println))


(def q1 (new-query [[(load-n "a") (load-n "b")]
                    [(select-n)   (empty-n)]]))

(def q2 (new-query [[(load-n "a")]
                    [(map-n "ident")]
                    [(filter-n "true")]]))

(defn -main []
  (optimize-and-print q1)
  (optimize-and-print q2))
