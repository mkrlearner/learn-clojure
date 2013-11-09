(ns algo.core)

(defn sorted-insert [f xs a]
  (let [[less more] (split-with #(f % a) xs)]
    (concat less [a] more)))

(defn insertion-sort [f xs]
  (reduce #(sorted-insert f % %2) [] xs))

(defn selection-sort [f xs]
  (let [f-min (fn [a b] (if (f a b) a b))
        xs-min (reduce f-min xs)
        {eq-min true not-eq-min false} (group-by #(= xs-min %) xs)]
    (if (empty? not-eq-min) eq-min
        (concat eq-min (selection-sort f not-eq-min)))))

(defn- count<=1? [xs]
  (or (empty? xs) (empty? (next xs))))

(defn- bubble-iter [f swapped? [x y & xs :as all]]
  (cond (count<=1? all) all
        (f x y) (cons x (bubble-iter f swapped? (cons y xs)))
        :else (cons y (bubble-iter f true (cons x xs)))))

(defn bubble-sort [f xs]
  (nth (iterate (partial bubble-iter f false) xs) (count xs)))

(defn- merge [f merged [x & xrest :as xs] [y & yrest :as ys]]
  (cond (or (empty? xs) (empty? ys)) (concat merged xs ys)
        (f x y) (recur f (concat merged [x]) xrest ys)
        :else (recur f (concat merged [y]) yrest xs)))

(defn merge-sort [f xs]
  (if (count<=1? xs) xs
      (let [[first-half last-half] (split-at (/ (count xs) 2) xs)
            sorted-first-half (merge-sort f first-half)
            sorted-last-half (merge-sort f last-half)]
        (merge f [] sorted-first-half sorted-last-half))))

(defn- comparison [f x y]
  (cond (= x y) :equal
        (f x y) :greater
        :else :lesser))

(defn- partition [f [x xs :as all]]
  (group-by (partial comparison f x) all))

(defn quick-sort [f xs]
  (if (count<=1? xs) xs
      (let [{:keys [equal greater lesser]} (partition f xs)]
        (concat (quick-sort f lesser)
                equal
                (quick-sort f greater)))))
