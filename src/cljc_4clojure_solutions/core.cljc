(ns cljc-4clojure-solutions.core
  (:require [clojure.set]
            [clojure.string]))

(defn double-down [x]
  (* x 2))

(defn hello-world [name]
  (str "Hello, " name "!"))

(defn last-element [coll]
  "Returns last element in a sequence."
  (nth coll (- (count coll) 1)))

(defn penultimate-element [coll]
  "Returns penultimate element in a sequence."
  (nth coll (- (count coll) 2)))

(defn nth-element [coll n]
  "Returns nth element in a sequence."
  (first (nthrest coll n)))

(defn count-elements [coll]
  (reduce (fn [v &_] (inc v)) 0 coll))

(defn a-nil-value? [key map]
  (and (contains? map key) (nil? (key map))))

(defn create-map [keys vals]
  (into {} (map vector keys vals)))

(defn palindrome? [coll]
  (= (reverse coll) (seq coll)))

(defn fib [n]
  (letfn [(fib-seq [a b]
            (cons a (lazy-seq (fib-seq b (+ b a)))))]
    (take n (fib-seq 1 1))))

(defn my-max [& xs]
  (reduce #(if (> %1 %2) %1 %2) xs))

(defn get-caps [s]
  (clojure.string/replace s #"[^A-Z]" ""))

(defn duplicate-seq [coll]
  (interleave coll coll))

(defn my-range [from to]
  (take (- to from) (iterate inc from)))

(defn my-flatten [coll]
  (filter (complement sequential?)
          (tree-seq sequential? identity coll)))

(defn my-interleave [coll1 coll2]
  (mapcat list coll1 coll2))

(defn factorial [n]
  (reduce * (range 1 (inc n))))

(defn compress-seq [coll]
  (map first (partition-by identity coll)))

(defn replicate-seq [coll n]
  (mapcat #(repeat n %) coll))

(defn my-interpose [s coll]
  (drop-last (mapcat #(list % s) coll)))

(defn drop-every [coll n]
  (apply concat (partition-all (dec n) n coll)))

(defn my-split-at [n coll]
  [(take n coll) (drop n coll)])

(defn half-truth [& xs]
  (and (not-every? true? xs) (not-every? false? xs)))

(defn create-default-map [default keys]
  (reduce (fn [coll val] (into coll {val default}))
          {}
          keys))

(defn gcd [a b]
  (if (= b 0)
    a
    (recur b (rem a b))))

(defn my-compare [r a b]
  (cond
    (r a b) :lt
    (r b a) :gt
    :else :eq))

(defn my-intersection [a b]
  (clojure.set/difference a (clojure.set/difference a b)))

(defn my-iterate [f x]
  (cons x (lazy-seq (my-iterate f (f x)))))

(defn pow [n]
  (fn [x]
    (reduce * (repeat n x))))

(defn mul-digits [a b]
  (letfn
      [(digits [n]
         (loop [res '() n n]
           (if (zero? n)
             res
             (recur (conj res (rem n 10))
                    (quot n 10)))))]
    (digits (* a b))))

(defn cartesian [a b]
  (set (for [x a y b]
         [x y])))

(defn my-group-by [f coll]
  (reduce (fn [map x]
            (let [k (f x)
                  v (get map k)]
              (if (nil? v)
                (assoc map k [x])
                (assoc map k (conj v x)))))
          {}
          coll ))

(defn binary-value [s]
  (letfn [(str->int [s]
            #?(:clj  (Integer/parseInt s)
               :cljs (js/parseInt s)))]
    (loop [res    0
           weight 1
           s      (reverse s)]
      (if (empty? s)
        res
        (recur (->> (first s)
                    str
                    str->int
                    (* weight)
                    (+ res))
               (* weight 2)
               (rest s))))))

(defn symmetric-diff [a b]
  (clojure.set/difference (clojure.set/union a b) (clojure.set/intersection a b)))

(defn dot-product [a b]
  (reduce + (map * a b)))

(defn inflix-calc [x & s]
  (reduce (fn [res [op val]]
            (op res val))
          x
          (partition 2 s)))

(defn my-map-indexed [coll]
  (map vector coll (range)))

(defn pascal-row [n]
  (let [n (dec n)]
    (reduce (fn [v k]
              (conj v (/ (* (get v k)
                            (- n k))
                         (inc k))))
            [1]
            (range n))))

(defn my-map [f coll]
  (if (empty? coll)
    '()
    (cons (f (first coll))
          (lazy-seq (my-map f (rest coll))))))

(defn f120 [coll]
  (letfn
      [(digits [n]
         (loop [res '() n n]
           (if (zero? n)
             res
             (recur (conj res (rem n 10))
                    (quot n 10)))))
       (sum-of-squares [coll]
         (reduce (fn [result x]
                   (+ result (* x x)))
                 0
                 coll))]
    (count (filter #(< % (sum-of-squares (digits %)))
                   coll))))

(defn binary-tree? [tree]
  (letfn [(tree-nodes [tree]
            (filter sequential?
                    (tree-seq sequential? identity tree)))
          (binary-tree-node? [[first & rest :as node]]
            (and (= (count node) 3)
                 (every? #(or (nil? %) (sequential? %)) rest)
                 (#(or (number? %) (keyword? %)) first)))]
    (->> (tree-nodes tree)
         (map binary-tree-node?)
         (every? true?))))

(defn f128 [s]
  (let [[suit rank] s]
    {:suit (cond (= suit \D) :diamond
                 (= suit \H) :heart
                 (= suit \C) :club
                 (= suit \S) :spade)
     :rank (cond (= rank \2) 0
                 (= rank \3) 1
                 (= rank \4) 2
                 (= rank \5) 3
                 (= rank \6) 4
                 (= rank \7) 5
                 (= rank \8) 6
                 (= rank \9) 7
                 (= rank \T) 8
                 (= rank \J) 9
                 (= rank \Q) 10
                 (= rank \K) 11
                 (= rank \A) 12)}))

(defn lcm [& xs]
  (let [gcd (fn [a b]
              (if (= b 0)
                a
                (recur b (rem a b))))
        lcm (fn [a b]
              ( * b (/ a (gcd a b))))]
    (reduce lcm xs)))

(defn pascal-seq [coll]
  (let [+' #?(:clj  +'
              :cljs +)]
    (iterate #(mapv +' (cons 0 %) (conj % 0))
             coll)))

(defn symmetric-tree? [tree]
  (let [walk (fn walk [node mirror]
               (if (sequential? node)
                 (let [[key left right] node]
                   (if mirror
                     (concat (list key)
                             (walk right mirror)
                             (walk left mirror))
                     (concat (list key)
                             (walk left mirror)
                             (walk right mirror))))
                 (list node)))
        [_ left-tree right-tree] tree]
    (= (walk left-tree false)
       (walk right-tree true))))

(defn f146 [coll]
  (into {} (for [[k1 m] coll
                 [k2 v] m]
             [[k1 k2] v])))

(defn f153 [s]
  (let [combinations #(->> (for [x % y %] (if (= x y) nil #{x y}))
                           (filter (complement nil?))
                           set
                           (map vec))]
    (->> (combinations s)
         (map (fn [[a b]] (clojure.set/intersection a b)))
         (every? empty?))))

(defn f46 [f]
  (fn [& args]
    (apply f (reverse args))))

(defn rotate [n coll]
  (let [p (mod n (count coll))]
    (concat (drop p coll)
            (take p coll))))

(defn reverse-interleave [coll n]
  (->> (partition n coll)
       (apply map list)))

(defn group-by-type [coll]
  (vals (group-by type coll)))

(defn occurence-map [coll]
  (->>
   (group-by identity coll)
   (map (fn [[k v]] {k (count v)}))
   (into {})))

(defn my-distinct [coll]
  (loop [result []
         seen   #{}
         remain   coll]
    (if (empty? remain)
      result
      (let [val (first remain)
            rest (rest remain)]
        (if (contains? seen val)
          (recur result seen rest)
          (recur (conj result val)
                 (conj seen val)
                 rest))))))

(defn my-comp [& fs]
  (let [fs (reverse fs)]
    (fn [& args]
      (loop [ret (apply (first fs) args)
             fs  (next fs)]
        (if fs
          (recur ((first fs) ret) (next fs))
          ret)))))

(defn my-partition [n coll]
  (lazy-seq
   (let [part (take n coll)]
     (when (= (count part) n)
       (cons part
             (my-partition n (drop n coll)))))))

(defn my-juxt [& fs]
  (fn [& args]
    (map #(apply % args) fs)))

(defn sort-words [sentence]
  (sort-by clojure.string/upper-case
           (re-seq #"[A-Za-z]+" sentence)))

(defn prime [n]
  (letfn [(smallest-divisor [n]
            (loop [i 3]
              ;; need only test odd divisors between 3 and (sqrt n)
              (when (<= (* i i) n)
                (if (zero? (mod n i))
                  i
                  (recur (+ i 2))))))
          (next-prime [prev-prime]
            (loop [prime (+ 2 prev-prime)]
              (if (smallest-divisor prime)
                (recur (+ prime 2))
                prime)))
          (prime-seq []
            (cons 2 (iterate next-prime 3)))]
    (take n (prime-seq))))

(defn f74 [s]
  (letfn [(perfect-square? [n]
            (let [d (Math/sqrt n)
                  i (double (int d))]
              (= d i)))
          (str->int [s]
            #?(:clj  (Integer/parseInt s)
               :cljs (js/parseInt s)))]
    (->> (re-seq #"\d+" s)
         (map str->int)
         (filter perfect-square?)
         (interpose ",")
         (apply str))))

(defn f53 [coll]
  (letfn [(bigger [x y] (if (> (count y) (count x)) y x))]
    (loop [coll coll cur [] max []]
      (if (empty? coll)
        max
        (let [[next & rest] coll
              prev (last cur)]
          (if (and (not (nil? prev))
                   (= next (inc prev)))
            (let [cur (conj cur next)]
              (recur rest cur (bigger max cur)))
            (recur rest [next] max)))))))

(defn f65 [coll]
  (let [t (into coll [[:x 99][:y 98]])]
    (cond (= (get t :x) 99) :map
          (= (get t [:x 99]) [:x 99]) :set
          (= (take 2 t) [[:y 98] [:x 99]]) :list
          (= (take-last 2 t) [[:x 99] [:y 98]]) :vector
          :else nil)))

(defn perfect-num? [n]
  (letfn [(next-divisor [t n]
            (when (<= t (quot n 2))
              (if (zero? (mod n t))
                t
                (recur (inc t) n))))
          (divisor-seq
            ([n]
             (divisor-seq 1 n))
            ([prev n]
             (cons prev
                   (lazy-seq (if-let [next (next-divisor (inc prev) n)]
                               (divisor-seq next n))))))]
    (= n (reduce + (divisor-seq n)))))

(defn f77 [coll]
  (->>
   (map #(hash-map (set %) #{%}) coll)
   (reduce #(merge-with clojure.set/union %1 %2) {})
   vals
   set
   (clojure.set/select #(> (count %) 1))))

(defn my-reductions
  ([f coll]
   (my-reductions f (first coll) (rest coll)))
  ([f val coll]
   (if-let [first (first coll)]
     (cons val (lazy-seq (my-reductions f
                                        (f val first)
                                        (rest coll))))
     (list val))))

(defn my-merge-with [f & maps]
  (letfn [(merge-maps [m1 m2]
            (loop [dest m1 r m2]
              (if-let [[k v] (first r)]
                (if-let [dv (get dest k)]
                  (recur (assoc dest k (f dv v)) (next r))
                  (recur (assoc dest k v) (next r)))
                dest)))]
    (reduce merge-maps
            (first maps)
            (rest maps))))

(defn f102 [s]
  (->>
   (partition 2 1 (cons nil s))
   (map (fn [[prev char]]
          (if (= prev \-)
            (clojure.string/upper-case char)
            char)))
   (filter #(not= % \-))
   (apply str)))

(defn f75 [x]
  (letfn [(gcd [a b]
            (if (= b 0)
              a
              (recur b (rem a b))))]
    (->>
     (range 2 x)
     (map #(gcd % x))
     (filter #(= % 1))
     count
     inc)))

(defn f73 [board]
  (let [lines-coordinates [[[0 0] [1 0] [2 0]]
                           [[0 1] [1 1] [2 1]]
                           [[0 2] [1 2] [2 2]]
                           [[0 0] [0 1] [0 2]]
                           [[1 0] [1 1] [1 2]]
                           [[2 0] [2 1] [2 2]]
                           [[0 0] [1 1] [2 2]]
                           [[2 0] [1 1] [0 2]]]
        line-val          (fn [board line-coordinates]
                            (let [vals (map #(get-in board %) line-coordinates)]
                              (cond (every? #(= % :x) vals) :x
                                    (every? #(= % :o) vals) :o
                                    :else nil)))]
    (some #{:x :o}
          (map #(line-val board %)
               lines-coordinates))))


(defn happy? [x]
  (letfn
      [(digits [n]
         (loop [res '() n n]
           (if (zero? n)
             res
             (recur (conj res (rem n 10))
                    (quot n 10)))))]
    (loop [x x]
      (let [n (->> (digits x)
                   (map #(* % %))
                   (reduce +))]
        (cond (= n 1) true
              (= n 4) false
              :else (recur n))))))

(defn my-trampoline [f & args]
  (loop [f (apply f args)]
    (if (fn? f)
      (recur (f))
      f)))

;; []      -> {}
;; [1]     -> {} ∪
;;            {1}
;; [1 2]   -> {}  {1} ∪
;;            {2} {1 2}
;; [1 2 3] -> {}  {1}   {2}   {1 2} ∪
;;            {3} {1 3} {2 3} {1 2 3}
(defn powerset [coll]
  (reduce (fn [res next]
            (clojure.set/union res (map #(conj % next) res)))
          #{#{}}
          coll))

(defn f115 [n]
  (letfn
      [(digits [n]
         (loop [res '() n n]
           (if (zero? n)
             res
             (recur (conj res (rem n 10))
                    (quot n 10)))))]
    (let [digits (digits n)
          half-count (quot (count digits) 2)]
      (= (reduce + (take half-count digits))
         (reduce + (take-last half-count digits))))))

(defn f98 [f domain]
  (->>
   (group-by f domain)
   vals
   (map set)
   set))

(defn f105 [coll]
  (loop [res {}
         last-key nil
         coll coll]
    (if (empty? coll)
      res
      (let [v (first coll)]
        (if (keyword? v)
          (recur (assoc res v [])
                 v
                 (rest coll))
          (recur (merge-with conj res {last-key v})
                 last-key
                 (rest coll)))))))

(defn digits [n base]
  (if (zero? n)
    '(0)
    (loop [res '() n n]
      (if (zero? n)
        res
        (recur (conj res (rem n base))
               (quot n base))))))

(defn f110 [coll]
  (letfn [(pronounce [coll]
            (->>
             (partition-by identity coll)
             (map (juxt count first))
             (apply concat)))]
    (let [pronounced (pronounce coll)]
      (cons pronounced (lazy-seq (f110 pronounced))))))

(defn f144 [val & fs]
  (letfn [(val-seq [val fs]
            (cons val (lazy-seq
                       (let [val ((first fs) val)
                             fs  (rest fs)]
                         (val-seq val fs)))))]
    (let [fs (cycle fs)]
      (val-seq val fs))))

(defn f108 [& colls]
  (let [head (map first colls)]
    (if (apply = head)
      (first head)
      (if (some nil? head)
        nil
        (let [min-val (apply min head)]
          (recur (map #(if (= (first %) min-val) (rest %) %)
                      colls)))))))

(defn decurry [f]
  (fn [& args]
    (reduce (fn [f arg] (f arg))
            f
            args)))

(defn partial-flatten [coll]
  (->>
   (tree-seq sequential? identity coll)
   (filter #(and (sequential? %) (every? (complement sequential?) %)))))

(defn roman->num [s]
  (let [cm (zipmap "IVXLCDM" [1 5 10 50 100 500 1000])
        nums (map #(get cm %) (reverse s))]
    (first (reduce (fn [[result max-val] v]
                     (if (>= v max-val)
                       [(+ result v) v]
                       [(- result v) max-val]))
                   [0 0]
                   nums))))

(defn f114 [up-to pred coll]
  (letfn [(val-seq [up-to pred coll accepted]
            (lazy-seq (when-let [s (seq coll)]
                        (let [v        (first coll)
                              accepted (if (pred v) (inc accepted) accepted)]
                          (when (< accepted up-to)
                            (cons v
                                  (val-seq up-to pred (rest coll) accepted)))))))]
    (val-seq up-to pred coll 0)))

(defn triangle-minimal-path [triangle]
  (letfn [(partition-row [row]
            (vec (concat (list (list (first row)))
                         (partition 2 1 row)
                         (list (list (last row))))))
          (cumulative-row [prev-cumulative-row row]
            (map (fn [prev-val val]
                   (+ (apply min prev-val) val))
                 (partition-row prev-cumulative-row)
                 row))]
    (apply min (reduce cumulative-row
                       triangle))))

(defn f132 [f separator coll]
  (if (empty? coll)
    '()
    (cons (first coll)
          (mapcat #(let [[a b] %]
                     (if (f a b)
                       [separator b]
                       [b]))
                  (partition 2 1 coll)))))

(defn num->roman [n]
  (let [rv [[1 "I"][4 "IV"][5 "V"][9 "IX"][10 "X"][40 "XL"][50 "L"]
            [90 "XC"][100 "C"][400 "CD"][500 "D"][900 "CM"][1000 "M"]]
        max-roman-digit-ngt (fn [n] (last (take-while #(>= n (first %)) rv)))]
    (loop [n n roman-num ""]
      (if (zero? n)
        roman-num
        (let [[x roman-digit] (max-roman-digit-ngt n)]
          (recur (- n x) (str roman-num roman-digit)))))))

(defn k-kombinations [k coll]
  (letfn [(first-rest-seq
            [k coll]
            (lazy-seq (when (<= k (count coll))
                        (let [f (first coll)
                              r (rest coll)]
                          (cons [f r]
                                (first-rest-seq k r))))))
          (combine
            [e coll]
            (set (map #(clojure.set/union #{e} %)
                      coll)))]
    (if (= k 1)
      (set (map hash-set coll))
      (apply clojure.set/union (map (fn [[f r]]
                                      (combine f (k-kombinations (dec k) r)))
                                    (first-rest-seq k coll))))))

(defn balanced-prime? [n]
  (letfn [(smallest-divisor [n]
            (loop [i 3]
              ;; need only test odd divisors between 3 and (sqrt n)
              (when (<= (* i i) n)
                (if (zero? (mod n i))
                  i
                  (recur (+ i 2))))))
          (prime? [n]
            (or (= n 2)
                (and (> n 1)
                     (odd? n)
                     (not (smallest-divisor n)))))
          (next-prime [prime]
            (if (= prime 2)
              3
              (loop [prime (+ prime 2)]
                (if (smallest-divisor prime)
                  (recur (+ prime 2))
                  prime))))
          (prev-prime [prime]
            (if (= prime 2)
              nil
              (if (= prime 3)
                2
                (loop [prime (- prime 2)]
                  (if (smallest-divisor prime)
                    (recur (- prime 2))
                    prime)))))]
    (true? (and (prime? n)
                (when-let [pp (prev-prime n)]
                  (let [np (next-prime n)]
                    (= n (/ (+ pp np)
                            2))))))))

(defn uce [elist]
  (fn [params]
    (let [lookup #(cond (symbol? %) (get params %)
                        (list? %) ((uce %) params)
                        :else %)
          f      ((first elist) {'/ / '+ + '- - '* *})
          args   (rest elist)]
      (apply f (map lookup args)))))

(defn f148 [n c1 c2]
  (let [+' #?(:clj +'
              :cljs +)
        *' #?(:clj *'
              :cljs *)
        arithmetic-series (fn [first-val up-to]
                            (let [c (quot (dec up-to) first-val)]
                              (/ (+' (*' c first-val)
                                     (*' c c first-val))
                                 2)))]
    (- (+' (arithmetic-series c1 n)
           (arithmetic-series c2 n))
       (arithmetic-series (*' c1 c2) n))))

(defn f84 [e]
  (letfn [(dag [edges]
            (->> edges
                 (map (fn [[f s]] {f #{s}}))
                 (apply merge-with clojure.set/union)))

          (reachable-nodes [dag from-node]
            (let [connected-nodes (get dag from-node)
                  next-nodes      (set (mapcat #(reachable-nodes dag %)
                                               connected-nodes))]
              (clojure.set/union connected-nodes next-nodes)))
          (transitive-closure-dag [dag]
            (reduce (fn [dag node]
                      (assoc dag node (reachable-nodes dag node)))
                    dag
                    (keys dag)))
          (edges [dag]
            (set (mapcat (fn [[node connected-nodes]]
                           (map #(vector node %) connected-nodes))
                         dag)))]
    (->> e
         dag
         transitive-closure-dag
         edges)))

(defn f171 [coll]
  (letfn [(group-intervals [coll]
            (loop [result  []
                   current []
                   coll    (sort coll)
                   prev    (first coll)]
              (if coll
                (let [val (first coll)]
                  (if (< (- val prev) 2)
                    (recur result
                           (conj current val)
                           (next coll)
                           val)
                    (recur (conj result current)
                           [val]
                           (next coll)
                           val)))
                (if (empty? current)
                  result
                  (conj result current)))))]
    (if (empty? coll)
      []
      (map #(vector (first %) (last %))
           (group-intervals coll)))))

(defn word-chain? [words]
  (letfn [(levenshtein-dist [s1 s2]
            (let [l1 (count s1)
                  l2 (count s2)]
              (cond (zero? l1) l2
                    (zero? l2) l1
                    :else (let [cost (if (= (first s1) (first s2)) 0 1)]
                            (min (inc (levenshtein-dist (rest s1) s2))
                                 (inc (levenshtein-dist s1 (rest s2)))
                                 (+ cost (levenshtein-dist (rest s1) (rest s2))))))))
          (distance-1-relation [words]
            (set (for [s1 words
                       s2 words
                       :when (and (not= s1 s2)
                                  (= 1 (levenshtein-dist s1 s2)))]
                   #{s1 s2})))
          (graph [relation]
            (->> relation
                 (mapcat (fn [v]
                           (let [f (first v)
                                 s (second v)]
                             [{f #{s}} {s #{f}}])))
                 (apply merge-with clojure.set/union)))
          (hamiltonian-path [graph path]
            (let [seen               (set path)
                  connected-not-seen (filter #(not (seen %))
                                             (get graph (peek path)))]
              (if (empty? connected-not-seen)
                (when (= (count path)
                         (count graph))
                  path)
                (some identity (map #(hamiltonian-path graph (conj path %))
                                    connected-not-seen)))))
          (has-hamiltonian-path? [graph]
            (not (nil? (some identity (map #(hamiltonian-path graph [%])
                                           (keys graph))))))]
    (-> words
        distance-1-relation
        graph
        has-hamiltonian-path?)))

(defn f91 [edges]
  (letfn [(graph [edges]
            (->> edges
                 (mapcat (fn [v]
                           (let [f (first v)
                                 s (second v)]
                             [{f #{s}} {s #{f}}])))
                 (apply merge-with clojure.set/union)))
          (dsf [graph node]
            (loop [seen #{node}
                   path [node]
                   todo (vec (get graph node))]
              (if (empty? todo)
                path
                (let [node (peek todo)]
                  (if (seen node)
                    (recur seen
                           path
                           (pop todo))
                    (recur (conj seen node)
                           (conj path node)
                           (into (pop todo) (get graph node))))))))
          (connected-graph? [g]
            (= (count g)
               (count (dsf g (ffirst g)))))]
    (-> edges
        graph
        connected-graph?)))

(defn f131 [& number-sets]
  (letfn [(powerset [coll]
            (reduce (fn [res next]
                      (clojure.set/union res (map #(conj % next) res)))
                    #{#{}}
                    coll))]
    (not (empty? (apply clojure.set/intersection
                        (map (fn [s]
                               (set (map #(reduce + %)
                                         (disj (powerset s) #{}))))
                             number-sets))))))

(defn f112 [max-sum coll]
  (letfn [(sequs-horribilis [remain-sum coll]
            (loop [remain-sum remain-sum
                   coll       coll
                   result     []]
              (if (empty? coll)
                [remain-sum result]
                (let [x (first coll)]
                  (let [[remain-sum x] (if (sequential? x)
                                         (sequs-horribilis remain-sum x)
                                         [(- remain-sum x) x])]
                    (if (neg? remain-sum)
                      (if (and (sequential? x)
                               (not-empty x))
                        [remain-sum (conj result x)]
                        [remain-sum result])
                      (recur remain-sum
                             (rest coll)
                             (conj result x))))))))]
    (second (sequs-horribilis max-sum coll))))

(defn balanced-brackets? [s]
  (let [bm               { \] \[ \) \( \} \{ }
        [balanced stack] (loop [stack []
                                xs    (seq s)]
                           (if xs
                             (let [ch (first xs)]
                               (cond
                                 (#{ \[ \( \{ } ch) (recur (conj stack ch) (next xs))
                                 (#{ \] \) \} } ch) (if (= (bm ch) (peek stack))
                                                      (recur (pop stack) (next xs))
                                                      [false stack])
                                 :else (recur stack (next xs))))
                             [true stack]))]
    (and balanced (empty? stack))))

(defn f141 [trump]
  (letfn [(card-score [card trump led]
            (+ (:rank card)
               (if (= led (:suit card)) 13 0)
               (if (= trump (:suit card)) 14 0)))
          (max-card [card1 card2 trump led]
            (if (> (card-score card1 trump led)
                   (card-score card2 trump led))
              card1
              card2))]
    (fn [cards]
      (let [first-card (first cards)
            led        (:suit first-card)]
        (reduce (fn [highest card]
                  (max-card card highest trump led))
                first-card
                (rest cards))))))

(defn next-game-of-life-board [board]
  (letfn [(neighbors-coordinates [[x y] size-x size-y]
            (let [deltas [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
              (filter (fn [[x y]]
                        (and (>= x 0)
                             (>= y 0)
                             (< x size-x)
                             (< y size-y)))
                      (map (fn [[dx dy]]
                             [(+ x dx ) (+ y dy)])
                           deltas))))
          (neighbors [board xy]
            (let [size-x (count (first board))
                  size-y (count board)]
              (map #(get-in board %)
                   (neighbors-coordinates xy size-x size-y))))
          (cell-score [board xy]
            (count (filter #{\#}
                           (neighbors board xy))))
          (next-cell-value [board xy]
            (let [cell-val   (get-in board xy)
                  cell-score (cell-score board xy)]
              (condp = cell-val
                \space (if (= cell-score 3) \# \space)
                \#     (if (some #{cell-score} [2 3]) \# \space))))]
    (let [size-x (count (first board))
          size-y (count board)]
      (->> (for [x (range size-x) y (range size-y)] [x y])
           (map #(next-cell-value board %))
           (partition size-x)
           (map #(apply str %))))))

(defn f106 [from to]
  (let [ops [#(* % 2)
             #(if (and (even? %) (pos? %))
                (/ % 2)
                nil)
             #(+ % 2)]]
    (loop [path-stack [[from]]]
      (let [current-path (first path-stack)
            val          (peek current-path)]
        (if (= val to)
          (count current-path)
          (let [new-vals  (keep #(% val) ops)
                new-paths (map #(conj current-path %) new-vals)]
            (recur (reduce conj
                           (subvec path-stack 1)
                           new-paths))))))))

;; Wagner–Fischer algorithm
(defn levenshtein-dist [s1 s2]
  (let [width  (inc (count s1))
        height (inc (count s2))
        matrix (vec (repeat height (vec (repeat width 0))))
        matrix (assoc matrix 0 (vec (range 0 width)))
        matrix (reduce #(assoc-in %1 [%2 0] %2) matrix (range 0 height))]
    (peek
     (peek
      (reduce (fn [matrix [y x]]
                (if (= (get s1 (dec x)) (get s2 (dec y)))
                  (assoc-in matrix [y x] (get-in matrix [(dec y) (dec x)]))
                  (assoc-in matrix [y x] (min (inc (get-in matrix [     y  (dec x)]))
                                              (inc (get-in matrix [(dec y)      x]))
                                              (inc (get-in matrix [(dec y) (dec x)]))))))
              matrix
              (for [y (range 1 height)
                    x (range 1 width)] [y x]))))))

(defn f89 [edges]
  (letfn [(graph [edges]
            (->> edges
                 (mapcat (fn [v]
                           (let [f (first v)
                                 s (second v)]
                             (if (= f s)
                               [{f [f]}]
                               [{f [s]} {s [f]}]))))
                 (apply merge-with into)))
          (dsf [graph node]
            (loop [seen #{node}
                   path [node]
                   todo (get graph node)]
              (if (empty? todo)
                path
                (let [node (peek todo)]
                  (if (seen node)
                    (recur seen
                           path
                           (pop todo))
                    (recur (conj seen node)
                           (conj path node)
                           (into (pop todo) (get graph node))))))))
          (connected-graph? [g]
            (= (count g)
               (count (dsf g (ffirst g)))))
          (num-of-odd-nodes [graph]
            (count (filter odd?
                           (map #(count (second %)) graph))))]
    (let [g (graph edges)]
      (if (connected-graph? g)
        (let [n (num-of-odd-nodes g)]
          (or (zero? n)
              (= n 2))) ; see http://www.ctl.ua.edu/math103/euler/howcanwe.htm
        false))))

(defn f119 [k board]
  (let [lines-coordinates [[[0 0] [1 0] [2 0]]
                           [[0 1] [1 1] [2 1]]
                           [[0 2] [1 2] [2 2]]
                           [[0 0] [0 1] [0 2]]
                           [[1 0] [1 1] [1 2]]
                           [[2 0] [2 1] [2 2]]
                           [[0 0] [1 1] [2 2]]
                           [[2 0] [1 1] [0 2]]]]
    (set (keep (fn [line-coordinates]
                 (let [line-vals (map #(get-in board %)
                                      line-coordinates)
                       line-vals-without-k (keep-indexed (fn [i v]
                                                           (if (not= v k)
                                                             [i v]))
                                                         line-vals)]
                   (if (= 1 (count line-vals-without-k))
                     (let [[i v] (first line-vals-without-k)]
                       (if (= v :e)
                         (get line-coordinates i))))))
               lines-coordinates))))

(defn f117 [board]
  (letfn [(neighbors-coordinates [[y x] width height]
            (let [deltas [[0 -1] [-1 0] [0 1] [1 0]]]
              (filter (fn [[y x]]
                        (and (>= x 0)
                             (>= y 0)
                             (< x width)
                             (< y height)))
                      (map (fn [[dy dx]]
                             [(+ y dy) (+ x dx )])
                           deltas))))
          (next-neighbors-coordinates [board seen yx]
            (let [width      (count (first board))
                  height     (count board)
                  next-coord (set (keep (fn [yx]
                                          (let [v (get-in board yx)]
                                            (if (not= v \#)
                                              yx)))
                                        (neighbors-coordinates yx width height)))]
              (clojure.set/difference next-coord seen)))
          (find-mouse-coordinates [board]
            (let [width  (count (first board))
                  height (count board)
                  coords (for [y (range 0 height)
                               x (range 0 width)]
                           [y x])]
              (loop [coords coords]
                (when coords
                  (let [yx (first coords)]
                    (if (= \M (get-in board yx))
                      yx
                      (recur (next coords))))))))]
    (let [mouse-coord (find-mouse-coordinates board)]
      (loop [stack [mouse-coord]
             seen  #{mouse-coord}]
        (if (empty? stack)
          false
          (let [yx (first stack)]
            (if (= \C (get-in board yx))
              true
              (let [next-coord (next-neighbors-coordinates board seen yx)]
                (recur (subvec (into stack next-coord) 1)
                       (into seen next-coord))))))))))

(defn palindromic-seq [from]
  (let [+' #?(:clj +'
              :cljs +)
        *' #?(:clj *'
              :cljs *)]
    (letfn [(reverse-num [n]
              (loop [r 0 n n]
                (if (zero? n)
                  r
                  (recur (+ (* r 10) (rem n 10))
                         (quot n 10)))))
            (num-of-digits [n]
              (if (zero? n)
                1
                (loop [r 0 n n]
                  (if (zero? n)
                    r
                    (recur (inc r)
                           (quot n 10))))))
            (state->num [[cur-val parity max-val]]
              (condp = parity
                :o (+' (*' max-val
                           (quot cur-val 10))
                       (reverse-num cur-val))
                :e (+' (*' max-val
                           cur-val)
                       (reverse-num cur-val))))
            (num->state [n]
              (let [nd      (num-of-digits n)
                    parity  (if (odd? nd) :o :e)
                    half-nd (if (= parity :o)
                              (inc (quot nd 2))
                              (quot nd 2))
                    max-val (reduce *' (repeat half-nd 10))
                    cur-val (if (= parity :o)
                              (quot n (quot max-val 10))
                              (quot n max-val))
                    cur-val (if (< (state->num [cur-val parity max-val]) n)
                              (inc cur-val)
                              cur-val)]
                [cur-val parity max-val]))
            (next-state [[cur-val parity max-val]]
              (let [new-val (inc cur-val)]
                (if (< new-val max-val)
                  [new-val parity max-val]
                  (if (= parity :o)
                    [(quot max-val 10) :e max-val]
                    [max-val :o (*' max-val 10)]))))
            (next-palindromic-seq [state]
              (lazy-seq (cons (state->num state)
                              (next-palindromic-seq (next-state state)))))]
      (next-palindromic-seq (num->state from)))))

;; This seems not solvable in ClojureScript.
#?(:clj
   (defn f113 [& coll]
     (reify
       clojure.lang.Seqable
       (toString [_]
         (apply str (interpose ", " (sort coll))))
       (seq [_]
         (seq (distinct coll))))))

(defn infinite-matrix
  ([f]
   (infinite-matrix f 0 0 nil nil))
  ([f m n]
   (infinite-matrix f m n nil nil))
  ([f m n s t]
   (letfn [(next-inner-seq [f i j max-j]
             (lazy-seq
              (when (or (nil? max-j) (< j max-j))
                (cons (f i j)
                      (next-inner-seq f i (inc j) max-j)))))
           (next-outer-seq [f i j max-i max-j]
             (lazy-seq
              (when (or (nil? max-i) (< i max-i))
                (cons (next-inner-seq f i j max-j)
                      (next-outer-seq f (inc i) j max-i max-j)))))]
     (let [max-i (if (nil? s) nil (+ m s))
           max-j (if (nil? t) nil (+ n t))]
       (next-outer-seq f m n max-i max-j)))))


(defn f111 [word board]
  (letfn [(remove-spaces [board]
            (map #(clojure.string/replace % " " "") board))
          (columns [board]
            (map (fn [i]
                   (apply str (map #(get % i) board)))
                 (range 0 (count (first board)))))
          (partition-by-hash [s]
            (->> s
                 (partition-by #(= % \#))
                 (map #(apply str %))
                 (filter #(not (re-find #"#" %)))))
          (match? [s1 s2]
            (and (= (count s1) (count s2))
                 (every? true? (->> (interleave s1 s2)
                                    (partition 2)
                                    (map (fn [[c1 c2]]
                                           (or (= c1 c2)
                                               (= c2 \_)
                                               (= c1 \_))))))))]
    (let [board  (remove-spaces board)
          places (->> (columns board)
                      (into board)
                      (mapcat partition-by-hash))]
      (boolean (some #(match? word %) places)))))

(defn analyze-reversi [board color]
  (letfn [(color-positions [board color]
            (filter #(= color (get-in board %))
                    (for [y (range 4) x (range 4)] [y x])))
          (line-from-position [[y x] [dy dx]]
            (loop [res [] y y x x]
              (let [l (+ y dy)
                    m (+ x dx)]
                (if (and (< l 4) (< m 4) (>= l 0) (>= m 0))
                  (recur (conj res [l m]) l m)
                  (when (> (count res) 1)
                    res)))))
          (lines-from-position [yx]
            (let [deltas [[-1 -1][-1 0][-1 1][0 -1][0 1][1 -1][1 0][1 1]]]
              (keep #(line-from-position yx %) deltas)))
          (opposite-color [color]
            (condp = color
              'b 'w
              'w 'b))
          (line->move [board flipped-color line]
            (let [line-left (drop-while #(= flipped-color (get-in board %)) line)]
              (when-not (= (count line-left) (count line))
                (when-let [last-el (first line-left)]
                  (when (= 'e (get-in board last-el))
                    last-el)))))
          (line->flipped [board color flipped-color line]
            (when-let [flipped (seq (take-while #(= flipped-color (get-in board %)) line))]
              (when-let [first-not-flipped (->> line
                                                (filter #(not= flipped-color (get-in board %)))
                                                first
                                                (get-in board))]
                (when (= first-not-flipped color)
                  flipped))))
          (moves-from-position [board flipped-color yx]
            (keep #(line->move board flipped-color %)
                  (lines-from-position yx)))
          (flipped-from-position [board color flipped-color yx]
            (apply concat (keep #(line->flipped board color flipped-color %)
                                (lines-from-position yx))))]
    (let [flipped-color (opposite-color color)
          positions     (color-positions board color)
          moves         (mapcat #(moves-from-position board flipped-color %)
                                positions)]
      (reduce (fn [result move]
                (assoc result move (set (flipped-from-position board color flipped-color move))))
              {}
              moves))))

(defn reparent-tree [node-name tree]
  (letfn [(remove-node
            ;; Removes node-name branch from tree.
            [tree node-name]
            (when (not= (first tree) node-name)
              (keep (fn [node]
                      (if (sequential? node)
                        (remove-node node node-name)
                        node))
                    tree)))

          (subtree
            ;;Returns subtree of tree which root node is node-name.
            [tree node-name]
            (if (= (first tree) node-name)
              tree
              (loop [leafs (rest tree)]
                (when-let [leaf (first leafs)]
                  (if-let [st (and (sequential? leaf)
                                   (subtree leaf node-name))]
                    st
                    (recur (rest leafs)))))))

          (make-parents-map
            ;;Creates map in which keys are nodes and values are their parents.
            ([tree]
             (make-parents-map tree nil))
            ([tree top-parent]
             (let [parent     (first tree)
                   result-map {parent top-parent}]
               (reduce (fn [result-map child]
                         (if (sequential? child)
                           (merge result-map (make-parents-map child parent))
                           (assoc result-map child parent)))
                       result-map
                       (rest tree)))))

          (make-node-path
            ;;Returns path from node-name to root in given tree.
            [tree node-name]
            (let [par-map (make-parents-map tree)]
              (when (contains? par-map node-name)
                (loop [np        [node-name]
                       node-name node-name]
                  (if-let [parent (get par-map node-name)]
                    (recur (conj np parent) parent)
                    np)))))

          (reparent-by-path
            ;; Appends second node in path to first node in path. The second node
            ;; is created by appending third to second, etc.
            [tree node-path]
            (if-let [node-name (first node-path)]
              (let [node-subtree (subtree tree node-name)]
                (if-let [parent-subtree (reparent-by-path (remove-node tree node-name)
                                                          (rest node-path))]
                  (concat node-subtree (list parent-subtree))
                  node-subtree))
              nil))]

    (reparent-by-path tree
                      (make-node-path tree node-name))))

(defn squares-squared [x max-x]
  (letfn [(directions-seq
            ;; Returns sequence of directions when walk from the center of spiral.
            []
            (cycle [[1 1] [1 -1] [-1 -1] [-1 1]]))

          (steps-seq
            ;; Returns sequence of step deltas when walk from the center of spiral.
            ([]
             (steps-seq 1 (directions-seq)))
            ([size directions]
             (lazy-cat (concat (repeat size (first directions))
                               (repeat size (second directions)))
                       (steps-seq (inc size) (nthrest directions 2)))))

          (steps
            ;; Returns sequence of steps.
            [n]
            (take n (steps-seq)))

          (coordinates [yx n]
            ;; Coordinates of sequence of n steps when walk from the center of spiral.
            ;; Argument yx is coordinate of center of spiral.
            (reduce (fn [coords [dy dx]]
                      (let [[y x] (peek coords)]
                        (conj coords [(+ y dy) (+ x dx)])))
                    [yx]
                    (steps (dec n))))

          (digits
            ;; Returns collection of digits of given number.
            [x]
            (loop [res '() x x]
              (if (zero? x)
                res
                (recur (conj res (rem x 10))
                       (quot x 10)))))

          (pow
            ;; Power of x.
            [x]
            (* x x))

          (sqrt
            ;; Largest integer value that is less than or equal to the sqrt of x.
            [x]
            (int (Math/sqrt x)))

          (pows [x max-x]
            ;; Returns collection of successive squares from the x which is not larger than max-x.
            (loop [res [x] x x]
              (let [nx (pow x)]
                (if (> nx max-x)
                  res
                  (recur (conj res nx) nx)))))

          (remain-to-next-square [x]
            ;; Return remain from x to next element of sequence (2, 4, 9, 16, 36, 49, 64, ...).
            (let [sqrt-val (sqrt x)]
              (if (= x (pow sqrt-val))
                0
                (- (pow (inc sqrt-val))
                   x))))

          (pows-digits [x max-x]
            ;; Returns digits of (pow x max-x). If number of elements is not one of sequence
            ;; (2, 4, 9, 16, 36, 49, 64, ...) then it is filled up with asterics.
            (let [digs (mapcat #(digits %) (pows x max-x))]
              (concat digs
                      (repeat (remain-to-next-square (count digs))
                              \*))))

          (make-board
            ;; Make board with size
            [size]
            (->> \space
                 (repeat size)
                 vec
                 (repeat size)
                 vec))

          (board-size [coll-count]
            ;; Calculates board size which is enought for rotated spiral of coll
            ;; with coll-count elements
            (let [non-roteted-size (sqrt coll-count)]
              (dec (* non-roteted-size 2))))

          (center-coord [board-size]
            ;; Calculates center coordinates for board-size.
            (let [non-rotated-size (/ (inc board-size) 2)]
              (if (odd? non-rotated-size)
                [(dec non-rotated-size) (dec non-rotated-size)]
                [(- non-rotated-size 2) (dec non-rotated-size)])))

          (board->vec-of-string [board]
            ;; Converts board to vector of strings.
            (map #(apply str %)
                 board))]

    (let [pows-digits (pows-digits x max-x)
          size        (count pows-digits)
          board-size  (board-size size)
          board       (make-board board-size)
          center-yx   (center-coord board-size)
          coords      (coordinates center-yx size)]
      (board->vec-of-string
       (reduce (fn [board [yx v]]
                 (assoc-in board yx v))
               board
               (map (fn [c v] [c v])
                    coords
                    pows-digits))))))

(defn best-hand [cards]
  (letfn [(rank-map [cards]
            ;; Return map of {rank, occurence_count} for occurence of ranks in the given cards.
            ;; (rank-map ["HA" "DA" "CA" "HJ" "DQ"]) => {\A 3, \J 1, \Q 1}
            (->> cards
                 (map second)
                 frequencies))

          (has-rank-count? [rank-count cards]
            ;; Check whether expected number of cards with same rank exist in the given cards
            (->> (rank-map cards)
                 (filter #(= (second %) rank-count))
                 empty?
                 not))

          (has-two-pairs? [cards]
            ;; Check whether there are two paits in the given cards
            (->> (rank-map cards)
                 (filter #(= (second %) 2))
                 count
                 (= 2)))

          (rank-weight [rank]
            ;; Return rank weight.
            (cond (= rank \2) 0
                  (= rank \3) 1
                  (= rank \4) 2
                  (= rank \5) 3
                  (= rank \6) 4
                  (= rank \7) 5
                  (= rank \8) 6
                  (= rank \9) 7
                  (= rank \T) 8
                  (= rank \J) 9
                  (= rank \Q) 10
                  (= rank \K) 11
                  (= rank \A) 12))

          (card-sequence? [cards]
            ;; Check that 5 cards are sequence. Note that A and 2 is also sequence.
            (let [card-weights (->> cards
                                    (map #(->> % second rank-weight))
                                    sort)
                  card-sequence-count (->> card-weights
                                           (partition 2 1)
                                           (map (fn [[first second]] (- second first)))
                                           (filter #(= % 1))
                                           count)
                  additional-count (if (and (= (first card-weights) 0)
                                            (= (last card-weights) 12))
                                     1
                                     0)]
              (= (+ card-sequence-count additional-count)
                 4)))

          (same-suit? [cards]
            (->> cards
                 (map first)
                 frequencies
                 count
                 (#(= 1 %))))]

    (cond (and (card-sequence? cards)
               (same-suit? cards)) :straight-flush
          (has-rank-count? 4 cards) :four-of-a-kind
          (and (has-rank-count? 3 cards )
               (has-rank-count? 2 cards)) :full-house
          (same-suit? cards) :flush
          (card-sequence? cards) :straight
          (has-rank-count? 3 cards) :three-of-a-kind
          (has-two-pairs? cards) :two-pair
          (has-rank-count? 2 cards) :pair
          :else :high-card)))

(defn f164
  "Returns a sequence enumerating all strings in the language recognized by the DFA.
  It uses internally a zip structure which looks like {:state 'q1 :word \"Hello\" }"
  [dfa]
  (letfn [(has-accept? [dfa state]
            (contains? (:accepts dfa) state))

          (next-zips [dfa zip]
            (let [word (:word zip)]
              (map (fn [[ch next-state]]
                     {:state next-state :word (str word ch)})
                   (get (:transitions dfa) (:state zip)))))

          (next-zip-queue [zip-queue next-zips]
            (-> zip-queue
                (into next-zips)
                (subvec 1)))

          (next-word [dfa zip-queue]
            (lazy-seq
             (loop [zip-queue zip-queue]
               (when-let [current-zip (first zip-queue)]
                 (let [word (:word current-zip)
                       next-queue (next-zip-queue zip-queue
                                                  (next-zips dfa current-zip))]
                   (if (and (has-accept? dfa (:state current-zip))
                            word)
                     (cons word (next-word dfa next-queue))
                     (recur next-queue)))))))]

    (next-word dfa [{:state (:start dfa) :word nil}])))

(defn f195 [n]
  (letfn [(left-zip [open-left closed-left word]
            (if (> open-left 0)
              [(dec open-left) (inc closed-left) (str word "(")]
              nil))

          (right-zip [open-left closed-left word]
            (if (> closed-left 0)
              [open-left (dec closed-left) (str word ")")]
              nil))]
    (loop [queue [[n 0 ""]]
           res #{}]
      (if-let [zip (first queue)]
        (let [[open-left closed-left word] zip]
          (if (and (zero? open-left)
                   (zero? closed-left))
            (recur (subvec queue 1)
                   (conj res word))
            (recur (-> queue
                       (subvec 1)
                       (into (keep seq [(left-zip open-left closed-left word)
                                        (right-zip open-left closed-left word)])))
                   res)))
        res))))

(defn f127
  [board-vec]
  (letfn [(deltas-n
            ;; (deltas-n 2) => [-2 2] [-2 1] [-2 0] [-2 -1] [-2 -2]
            [step]
            (map vector
                 (repeat (* -1 step))
                 (range step (dec (* -1 step)) -1)))

          (deltas-ne
            ;; (deltas-ne 3) => ([-3 0] [-2 -1] [-1 -2] [0 -3])
            [step]
            (map vector
                 (range (* step -1) 1 1)
                 (range 0 (dec (* step -1)) -1)))

          (deltas-e
            ;; (deltas-e 2) => [-2 -2] [-1 -2] [0 -2] [1 -2] [2 -2]
            [step]
            (map vector
                 (range (* -1 step) (inc step))
                 (repeat (* -1 step))))

          (deltas-se
            ;; (deltas-se 3) => ([3 0] [2 -1] [1 -2] [0 -3])
            [step]
            (map vector
                 (range step -1 -1)
                 (range 0 (dec (* step -1)) -1)))

          (deltas-s
            ;; (deltas-s 2) => [2 2] [2 1] [2 0] [2 -1] [2 -2]
            [step]
            (map vector
                 (repeat step)
                 (range step (dec (* -1 step)) -1)))

          (deltas-sw
            ;; (deltas-sw 3) => ([3 0] [2 1] [1 2] [0 3])
            [step]
            (map vector
                 (range step -1 -1)
                 (range 0 (inc step) 1)))

          (deltas-w
            ;; (deltas-w 2) => [-2 2] [-1 2] [0 2] [1 2] [2 2]
            [step]
            (map vector
                 (range (* -1 step) (inc step))
                 (repeat step)))

          (deltas-nw
            ;; (deltas-nw 3) => ([-3 0] [-2 1] [-1 2] [0 3])
            [step]
            (map vector
                 (range (* step -1) 1 1)
                 (range 0 (inc step) 1)))

          (deltas
            [step direction]
            (condp = direction
              :n (deltas-n step)
              :ne (deltas-ne step)
              :e (deltas-e step)
              :se (deltas-se step)
              :s (deltas-s step)
              :sw (deltas-sw step)
              :w (deltas-w step)
              :nw (deltas-nw step)))

          (bitset?
            [board-vec [y x]]
            (not (zero? (bit-and (nth board-vec y 0)
                                 (bit-shift-left 1 x)))))

          (valid-point
            [[y x]]
            (if (and ((comp not neg?) y)
                     ((comp not neg?) x))
              [y x]
              nil))

          (triangle-line-harvestable?
            [board-vec yx direction step]
            (->> (deltas step direction)
                 (map (partial mapv + yx))
                 (map valid-point)
                 (every? #(and (not (nil? %))
                               (bitset? board-vec %)))))

          (max-harvestable-triangle-side-length
            [board-vec yx direction]
            (if (bitset? board-vec yx)
              (->> (iterate inc 1)
                   (take-while #(triangle-line-harvestable? board-vec yx direction %))
                   (last)
                   (#(if % (inc %) 0)))
              0))

          (triangle-size-equilateral
            [n]
            (/(* n
                 (inc n))
              2))

          (triangle-size-isosceles
            [n]
            (* (/ (+ 2
                     (* 2 (dec n)))
                  2)
               n))

          (triangle-size
            [n direction]
            (if (direction #{:n :e :s :w})
              (triangle-size-isosceles n)
              (triangle-size-equilateral n)))

          (max-harvestable-triangle-size
            [board-vec yx direction]
            (triangle-size (max-harvestable-triangle-side-length board-vec yx direction)
                           direction))

          (bits-needed
            [n]
            (loop [r 1
                   v 2]
              (if (> v n)
                r
                (recur (inc r)
                       (bit-shift-left v 1)))))]

    (->> (for [x (range (bits-needed (apply max board-vec)))
               y (range (count board-vec))
               d [:n :ne :e :se :s :sw :w :nw]]
           [x y d])
         (map (fn [[x y direction]]
                (max-harvestable-triangle-size board-vec [y x] direction)))
         (apply max)
         (#(if (> % 0) % nil)))))

(defn f152
  ;; Uses workaround for slow for macro in 4clojure sandbox from:
  ;; https://groups.google.com/d/msg/4clojure/9D0Ky7wiobw/jW9boI8GTxIJ
  [board]
  (letfn [(subv
            ;; Works same as subvec but returns nil instead
            ;; of throwing IndexOutOfBoundsException.
            [v start end]
            (if (and (>= start 0)
                     (<= end (count v))
                     (> end start))
              (subvec v start end)
              nil))

          (for2 [coll1 coll2 f]
            ;; Works similar to for macro
            (mapcat (fn [a]
                      (map (fn [b]
                             (f a b))
                           coll2))
                    coll1))

          (subsquare
            ;; Returns subsquare of given board with given size,
            ;; start point and shifts for all rows.
            ;; Returns nil if there is no such subsquare.
            [board shifts [y x] size]
            (let [v (map #(subv %1 (- x %2) (+ (- x %2) size))
                         (subv board y (+ y size))
                         (subv shifts y (+ y size)))]
              (if (not-any? nil? v)
                (vec v)
                nil)))

          (all-in?
            ;; Returns true when all elements in coll
            ;; are different and exists in given set.
            [coll s]
            (and (= (count coll) (count s))
                 (= (count (set coll)) (count s))
                 (every? s coll)))

          (transpose
            ;; Rotate matrix given as seq of colls.
            ;; All colls must have same number of elements.
            [colls]
            (apply map list colls))

          (latin?
            ;; Returns true if given colls are latin square.
            ;; Returns false otherwise.
            [colls]
            (let [s (set (first colls))]
              (every? #(all-in? % s)
                      (concat colls
                              (transpose colls)))))

          (cart
            ;; Results with cartesian product from given colls.
            ;; Taken from http://stackoverflow.com/a/18248031
            [colls]
            (if (empty? colls)
              '(())
              #_(for [x (first colls)
                      more (cart (rest colls))]
                  (cons x more))
              (for2 (first colls)
                    (cart (rest colls))
                    (fn [x more]
                      (cons x more)))))

          (cart-ranges
            ;; Results with cartesian product of ranges (starting from 0)
            ;; with given max values
            [maxs]
            (cart (map #(range (inc %))
                       maxs)))

          (max-shifts
            ;; Returns vector of maximum shifts for all rows on the board.
            [board]
            (let [max-len (apply max (map count board))]
              (map #(- max-len (count %)) board)))

          (shifts-seq
            ;; Return all combinations of shifts for given board.
            [board]
            (->> board
                 max-shifts
                 cart-ranges))

          (start-points-seq
            ;; Returns sequence of all start points for getting subsquares
            ;; of given size from board with given rows and cols.
            [rows cols size]
            #_(for [y (range (inc (- rows size)))
                    x (range (inc (- cols size)))]
                [y x])
            (for2 (range (inc (- rows size)))
                  (range (inc (- cols size)))
                  #(vector %1 %2)))

          (subsquares-seq
            ;; Returns all subsquares of given board
            [board]
            (let [rows     (count board)
                  cols     (apply max (map count board))
                  max-size (min rows cols)]
              (keep identity
                    #_(for [shifts (shifts-seq board)
                            size  (range 2 (inc max-size))
                            yx    (start-points-seq rows cols size)]
                        (subsquare board (vec shifts) yx size))
                    (mapcat (fn [shifts]
                              (mapcat (fn [size]
                                        (map (fn [yx]
                                               (subsquare board (vec shifts) yx size))
                                             (start-points-seq rows cols size)))
                                      (range 2 (inc max-size))))
                            (shifts-seq board)))))]
    (->> board
         subsquares-seq
         distinct
         (filter latin?)
         (map count)
         frequencies)))

(defn f140
  ;; Minimize boolean functions described by minterms with
  ;; Quine–McCluskey algorithm. The implementation is not complete.
  ;; It only returns essential prime implicants which not always
  ;; cover all min terms, but for given test cases it always does ;)
  ;; It also doesn't try to optimise number of combining of terms
  ;; and just combine all permutations of them.
  [min-terms]
  (letfn [(term-set->bin
            ;; Converts set representation of term to binary.
            ;;
            ;; (term-set->bin '#{A c D}) => [1 - 0 1]
            [min-term]
            (map (fn [[sym0 sym1]]
                   (cond (min-term sym0) 0
                         (min-term sym1) 1
                         :else '-))
                 '[[a A][b B][c C][d D]]))

          (term-bin->set
            ;; Converts binary representation of term to set.
            ;;
            ;; (term-bin->set [0 1 - 1]) => #{a B D}
            [min-term]
            (->> (map (fn [v [sym0 sym1]]
                        (condp = v
                          0 sym0
                          1 sym1
                          nil))
                      min-term
                      '[[a A][b B][c C][d D]])
                 (remove nil?)
                 set))

          (same-domain?
            ;; Checks that terms are from the same domain,
            ;; that is, they had same bit merged before.
            ;;
            ;; (same-domain? '[1 - - 1] '[0 - - 0]) => true
            [term1 term2]
            (->> (map #(or (and (= %1 '-)
                                (= %2 '-))
                           (and (not= %1 '-)
                                (not= %2 '-)))
                      term1
                      term2)
                 (every? true?)))

          (term-changes-count
            ;; Returns number of changes between terms.
            ;;
            ;; (term-changes-count '[1 1 - 1] '[1 0 - 1]) => 1
            [term1 term2]
            (reduce +
                    (map #(if (= %1 %2) 0 1)
                         term1
                         term2)))

          (combine-terms
            ;; Return combined terms. Returns nil if terms are from differend domain
            ;; or number of differences is more than 1.
            ;;
            ;; (combine-terms [1 1 0 0] [1 1 1 0]) => [1 1 - 0]
            [term1 term2]
            (if (and (same-domain? term1 term2)
                     (<= (term-changes-count term1 term2) 1))
              (map #(if (= %1 %2) %1 '-)
                   term1
                   term2)
              nil))

          (terms-matches?
            ;; Check if terms matches.
            ;;
            ;; (terms-matches? '[1 - - 0] '[1 - 1 0]) => true
            [term1 term2]
            (->> (map #(or (= %1 %2)
                           (= %1 '-)
                           (= %2 '-))
                      term1
                      term2)
                 (every? true?)))

          (permutations
            ;; Returns all permutations of given coll.
            ;;
            ;; (permutations [1 2 3]) => ([1 2] [1 3] [2 1] [2 3] [3 1] [3 2])
            [coll]
            (for [e1 coll
                  e2 coll
                  :when (not= e1 e2)]
              [e1 e2]))

          (group-merge-by-first
            ;; Return map which groups rest of each collection by first element.
            ;; Rest of colls are merged for same first element.
            ;;
            ;; (group-merge-by-first [[1 2][1 3 4][2 5]]) => {1 (2 3 4), 2 (5)}
            [colls]
            (->> colls
                 (map (fn [[x & xs]] {x xs}))
                 (apply merge-with concat)))

          (qm-iterate
            ;; Quine–McCluskey algorithm - one iteration.
            ;; Terms count must be more at least 2.
            ;; It returns final terms and terms which needs to be
            ;; combined in next iteration of Quine–McCluskey algorithm.
            [terms]
            (let [res (->> terms
                           permutations
                           group-merge-by-first
                           (map (fn [[key-term terms]]
                                  [key-term (keep #(combine-terms key-term %)
                                                  terms)])))
                  final-terms (->> res
                                   (filter #(empty? (second %)))
                                   (map first))
                  next-turn-terms (->> res
                                       (map second)
                                       (apply concat)
                                       set)]
              [final-terms next-turn-terms]))

          (qm
            ;; Execute Quine–McCluskey algorithm on given terms.
            [terms]
            (loop [final-terms #{}
                   terms terms]
              (if (< (count terms) 2)
                (into final-terms terms)
                (let [[iteration-final-terms terms] (qm-iterate terms)]
                  (recur (into final-terms iteration-final-terms)
                         terms)))))

          (extract-essential-terms
            ;; Extracts essential prime implicants from terms returned
            ;; from Quine–McCluskey algorithm for given initial min-terms
            [min-terms qm-terms]
            (->> min-terms
                 (map (fn [min-term]
                        (filter #(terms-matches? min-term %) qm-terms)))
                 (filter #(= 1 (count %)))
                 (apply concat)
                 (distinct)))]

    (let [min-terms (map term-set->bin min-terms)]
      (->> (extract-essential-terms min-terms
                                    (qm min-terms))
           (map term-bin->set)
           set))))
