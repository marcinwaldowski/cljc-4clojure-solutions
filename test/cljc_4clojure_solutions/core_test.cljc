(ns cljc-4clojure-solutions.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [cljc-4clojure-solutions.core :as c]))

(deftest elementary-01
  (testing "Nothing but the Truth"
    (is (= true true))))

(deftest elementary-02
  (testing "Simple Math"
    (is (= (- 10 (* 2 3)) 4))))

(deftest elementary-03
  (testing "Intro to Strings"
    (is (= "HELLO WORLD" (.toUpperCase "hello world")))))

(deftest elementary-04
  (testing "Intro to Lists"
    (is (= (list :a :b :c) '(:a :b :c)))))

(deftest elementary-05
  (testing "Lists: conj"
    (is (= '(1 2 3 4) (conj '(2 3 4) 1)))
    (is (= '(1 2 3 4) (conj '(3 4) 2 1)))))

(deftest elementary-06
  (testing "Intro to Vectors"
    (is (= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c)))))

(deftest elementary-07
  (testing "Vectors: conj"
    (is (= [1 2 3 4] (conj [1 2 3] 4)))
    (is (= [1 2 3 4] (conj [1 2] 3 4)))))

(deftest elementary-08
  (testing "Intro to Sets"
    (is (= #{:a :b :c :d} (set '(:a :a :b :c :c :c :c :d :d))))
    (is (= #{:a :b :c :d} (clojure.set/union #{:a :b :c} #{:b :c :d})))))

(deftest elementary-09
  (testing "Sets: conj"
    (is (= #{1 2 3 4} (conj #{1 4 3} 2)))))

(deftest elementary-10
  (testing "Intro to Maps"
    (is (= 20 ((hash-map :a 10, :b 20, :c 30) :b)))
    (is (= 20 (:b {:a 10, :b 20, :c 30})))))

(deftest elementary-11
  (testing "Maps: conj"
    (is (= {:a 1, :b 2, :c 3} (conj {:a 1} {:b 2} [:c 3])))))

(deftest elementary-12
  (testing "Intro to Sequences"
    (is (= 3 (first '(3 2 1))))
    (is (= 3 (second [2 3 4])))
    (is (= 3 (last (list 1 2 3))))))

(deftest elementary-13
  (testing "Sequences: rest"
    (is (= '(20 30 40)(rest [10 20 30 40])))))

(deftest elementary-14
  (testing "Intro to Functions"
    (is (= 8 ((fn add-five [x] (+ x 5)) 3)))
    (is (= 8 ((fn [x] (+ x 5)) 3)))
    (is (= 8 (#(+ % 5) 3)))
    (is (= 8 ((partial + 5) 3)))))

(deftest elementary-15
  (testing "Double Down"
    (is (= (c/double-down 2) 4))
    (is (= (c/double-down 3) 6))
    (is (= (c/double-down 11) 22))
    (is (= (c/double-down 7) 14))))

(deftest elementary-16
  (testing "Hello World"
    (is (= (c/hello-world "Dave") "Hello, Dave!"))
    (is (= (c/hello-world "Jenn") "Hello, Jenn!"))
    (is (= (c/hello-world "Rhea") "Hello, Rhea!"))))

(deftest elementary-17
  (testing "Sequences: map"
    (is (= '(6 7 8) (map #(+ % 5) '(1 2 3))))))

(deftest elementary-18
  (testing "Sequences: filter"
    (is (= [6 7] (filter #(> % 5) '(3 4 5 6 7))))))

(deftest elementary-35
  (testing "Local bindings"
    (is (= 7 (let [x 5] (+ 2 x))))
    (is (= 7 (let [x 3, y 10] (- y x))))
    (is (= 7 (let [x 21] (let [y 3] (/ x y)))))))

(deftest elementary-36
  (testing "Let it be"
    (is (= 10 (let [x 7 y 3 z 1] (+ x y))))
    (is (= 4 (let [x 7 y 3 z 1] (+ y z))))
    (is (= 1 (let [x 7 y 3 z 1] z)))))

(deftest elementary-37
  (testing "Regular Expressions"
    (is (= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce "))))))

(deftest elementary-57
  (testing "Simple Recursion"
    (= [5 4 3 2 1] ((fn foo [x]
                      (when (> x 0)
                        (conj (foo (dec x)) x)))
                    5))))

(deftest elementary-64
  (testing "Intro to Reduce"
    (is (= 15 (reduce + [1 2 3 4 5])))
    (is (=  0 (reduce + [])))
    (is (=  6 (reduce + 1 [2 3])))))

(deftest elementary-68
  (testing "Recurring Theme"
    (is (= [7 6 5 4 3]
           (loop [x 5 result []]
             (if (> x 0)
               (recur (dec x)
                      (conj result (+ 2 x)))
               result))))))

(deftest elementary-71
  (testing "Rearranging Code: ->"
    (is (= (last (sort (rest (reverse [2 5 4 1 3 6]))))
           (-> [2 5 4 1 3 6] reverse rest sort last)
           5))))

(deftest elementary-72
  (testing "Rearranging Code: ->>"
    (is (= (apply + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
           (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (apply +))
           11))))

(deftest elementary-134
  (testing "A nil key"
    (is (true?  (c/a-nil-value? :a {:a nil :b 2})))
    (is (false? (c/a-nil-value? :b {:a nil :b 2})))
    (is (false? (c/a-nil-value? :c {:a nil :b 2})))))

(deftest elementary-145
  (testing "For the win"
    (is (= [1 5 9 13 17 21 25 29 33 37] (for [x (range 40)
                                              :when (= 1 (rem x 4))]
                                          x)))
    (is (= [1 5 9 13 17 21 25 29 33 37] (for [x (iterate #(+ 4 %) 0)
                                              :let [z (inc x)]
                                              :while (< z 40)]
                                          z)))
    (is (= [1 5 9 13 17 21 25 29 33 37] (for [[x y] (partition 2 (range 20))]
                                          (+ x y))))))

(deftest elementary-156
  (testing "Map Defaults"
    (is (= (c/create-default-map 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
    (is (= (c/create-default-map "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
    (is (= (c/create-default-map [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]}))))

(deftest elementary-161
  (testing "Subset and Superset"
    (is (clojure.set/superset? #{1 2} #{2}))
    (is (clojure.set/subset? #{1} #{1 2}))
    (is (clojure.set/superset? #{1 2} #{1 2}))
    (is (clojure.set/subset? #{1 2} #{1 2}))))

(deftest elementary-162
  (testing "Logical falsity and truth"
    (is (= 1 (if-not false 1 0)))
    (is (= 1 (if-not nil 1 0)))
    (is (= 1 (if true 1 0)))
    (is (= 1 (if [] 1 0)))
    (is (= 1 (if [0] 1 0)))
    (is (= 1 (if 0 1 0)))
    (is (= 1 (if 1 1 0)))))

(deftest easy-19
  (testing "Last Element"
    (is (= (c/last-element [1 2 3 4 5]) 5))
    (is (= (c/last-element '(5 4 3)) 3))
    (is (= (c/last-element ["b" "c" "d"]) "d"))))

(deftest easy-20
  (testing "Penultimate Element"
    (is (= (c/penultimate-element (list 1 2 3 4 5)) 4))
    (is (= (c/penultimate-element ["a" "b" "c"]) "b"))
    (is (= (c/penultimate-element [[1 2] [3 4]]) [1 2]))))

(deftest easy-21
  (testing "Penultimate Element"
    (is (= (c/nth-element '(4 5 6 7) 2) 6))
    (is (= (c/nth-element [:a :b :c] 0) :a))
    (is (= (c/nth-element [1 2 3 4] 1) 2))
    (is (= (c/nth-element '([1 2] [3 4] [5 6]) 2) [5 6]))))

(deftest easy-22
  (testing "Count a Sequence"
    (is (= (c/count-elements '(1 2 3 3 1)) 5))
    (is (= (c/count-elements "Hello World") 11))
    (is (= (c/count-elements [[1 2] [3 4] [5 6]]) 3))
    (is (= (c/count-elements '(13)) 1))
    (is (= (c/count-elements '(:a :b :c)) 3))))

(deftest easy-23
  (testing "Reverse a Sequence"
    (is (= (into '() [1 2 3 4 5]) [5 4 3 2 1]))
    (is (= (into '() (sorted-set 5 7 2 7)) '(7 5 2)))
    (is (= (into '() [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]]))))

(deftest easy-24
  (testing "Sum It All Up"
    (is (= (reduce + [1 2 3]) 6))
    (is (= (reduce + (list 0 -2 5 5)) 8))
    (is (= (reduce + #{4 2 1}) 7))
    (is (= (reduce + '(0 0 -1)) -1))
    (is (= (apply + '(1 10 3)) 14))))

(deftest easy-25
  (testing "Find the odd numbers"
    (is (= (filter odd? #{1 2 3 4 5}) '(1 3 5)))
    (is (= (filter odd? [4 2 1 6]) '(1)))
    (is (= (filter odd? [2 2 4 6]) '()))
    (is (= (filter odd? [1 1 1 3]) '(1 1 1 3)))))

(deftest easy-26
  (testing "Fibonacci Sequence"
    (is (= (c/fib 3) '(1 1 2)))
    (is (= (c/fib 6) '(1 1 2 3 5 8)))
    (is (= (c/fib 8) '(1 1 2 3 5 8 13 21)))))

(deftest easy-27
  (testing "Palindrome Detector"
    (is (false? (c/palindrome? '(1 2 3 4 5))))
    (is (true? (c/palindrome? "racecar")))
    (is (true? (c/palindrome? [:foo :bar :foo])))
    (is (true? (c/palindrome? '(1 1 3 3 1 1))))
    (is (false? (c/palindrome? '(:a :b :c))))))

(deftest easy-28
  (testing "Flatten a Sequence"
    (is (= (c/my-flatten '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
    (is (= (c/my-flatten ["a" ["b"] "c"]) '("a" "b" "c")))
    (is (= (c/my-flatten '((((:a))))) '(:a)))))

(deftest easy-29
  (testing "Get the Caps"
    (is (= (c/get-caps "HeLlO, WoRlD!") "HLOWRD"))
    (is (empty? (c/get-caps "nothing")))
    (is (= (c/get-caps "$#A(*&987Zf") "AZ"))))

(deftest easy-30
  (testing "Compress a Sequence"
    (is (= (apply str (c/compress-seq "Leeeeeerrroyyy")) "Leroy"))
    (is (= (c/compress-seq [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
    (is (= (c/compress-seq [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))))

(deftest easy-31
  (testing "Pack a Sequence"
    (is (= (partition-by identity [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
    (is (= (partition-by identity [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
    (is (= (partition-by identity [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))))

(deftest easy-32
  (testing "Duplicate a Sequence"
    (is (= (c/duplicate-seq [1 2 3]) '(1 1 2 2 3 3)))
    (is (= (c/duplicate-seq [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
    (is (= (c/duplicate-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
    (is (= (c/duplicate-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))))

(deftest easy-33
  (testing "Replicate a Sequence"
    (is (= (c/replicate-seq [1 2 3] 2) '(1 1 2 2 3 3)))
    (is (= (c/replicate-seq [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
    (is (= (c/replicate-seq [4 5 6] 1) '(4 5 6)))
    (is (= (c/replicate-seq [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
    (is (= (c/replicate-seq [44 33] 2) [44 44 33 33]))))

(deftest easy-34
  (testing "Implement range"
    (is (= (c/my-range 1 4) '(1 2 3)))
    (is (= (c/my-range -2 2) '(-2 -1 0 1)))
    (is (= (c/my-range 5 8) '(5 6 7)))))

(deftest easy-38
  (testing "Maximum value"
    (is (= (c/my-max 1 8 3 4) 8))
    (is (= (c/my-max 30 20) 30))
    (is (= (c/my-max 45 67 11) 67))))

(deftest easy-39
  (testing "Interleave Two Seqs"
    (is (= (c/my-interleave [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
    (is (= (c/my-interleave [1 2] [3 4 5 6]) '(1 3 2 4)))
    (is (= (c/my-interleave [1 2 3 4] [5]) [1 5]))
    (is (= (c/my-interleave [30 20] [25 15]) [30 25 20 15]))))

(deftest easy-40
  (testing "Interpose a Seq"
    (is (= (c/my-interpose 0 [1 2 3]) [1 0 2 0 3]))
    (is (= (apply str (c/my-interpose ", " ["one" "two" "three"])) "one, two, three"))
    (is (= (c/my-interpose :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))))

(deftest easy-41
  (testing "Drop Every Nth Item"
    (is (= (c/drop-every [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
    (is (= (c/drop-every [:a :b :c :d :e :f] 2) [:a :c :e]))
    (is (= (c/drop-every [1 2 3 4 5 6] 4) [1 2 3 5 6]))))

(deftest easy-42
  (testing "Factorial Fun"
    (= (c/factorial 1) 1)
    (= (c/factorial 3) 6)
    (= (c/factorial 5) 120)
    (= (c/factorial 8) 40320)))

(deftest easy-45
  (testing "Intro to Iterate"
    (is (= [1 4 7 10 13] (take 5 (iterate #(+ 3 %) 1))))))

(deftest easy-47
  (testing "Contain Yourself"
    (is (contains? #{4 5 6} 4))
    (is (contains? [1 1 1 1 1] 4))
    (is (contains? {4 :a 2 :b} 4))))

(deftest easy-48
  (testing "Intro to some"
    (is (= 6 (some #{2 7 6} [5 6 7 8])))
    (is (= 6 (some #(when (even? %) %) [5 6 7 8])))))

(deftest easy-44
  (testing "Split a sequence"
    (is (= (c/my-split-at 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
    (is (= (c/my-split-at 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
    (is (= (c/my-split-at 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))))

(deftest easy-51
  (testing "Advanced Destructuring"
    (is (= [1 2 [3 4 5] [1 2 3 4 5]]
           (let [[a b & c :as d] [1 2 3 4 5]]
             [a b c d])))))

(deftest easy-52
  (testing "Intro to Destructuring"
    (is (= [2 4]
           (let [[a b c d e f g] (range)]
             [c e])))))

(deftest easy-61
  (testing "Map Construction"
    (is (= (c/create-map [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
    (is (= (c/create-map [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
    (is (= (c/create-map [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"}))))

(deftest easy-62
  (testing "Re-implement Iterate"
    (is (= (take 5 (c/my-iterate #(* 2 %) 1)) [1 2 4 8 16]))
    (is (= (take 100 (c/my-iterate inc 0)) (take 100 (range))))
    (is (= (take 9 (c/my-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3]))))))

(deftest easy-63
  (testing "Group a Sequence"
    (is (= (c/my-group-by #(> % 5) [1 3 6 8])
           {false [1 3], true [6 8]}))
    (is (= (c/my-group-by #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
           {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
    (is (= (c/my-group-by count [[1] [1 2] [3] [1 2 3] [2 3]])
           {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))))

(deftest easy-66
  (testing "Greatest Common Divisor"
    (is (= (c/gcd 2 4) 2))
    (is (= (c/gcd 10 5) 5))
    (is (= (c/gcd 5 7) 1))
    (is (= (c/gcd 1023 858) 33))))

(deftest easy-81
  (testing "Set Intersection"
    (is (= (c/my-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3}))
    (is (= (c/my-intersection #{0 1 2} #{3 4 5}) #{}))
    (is (= (c/my-intersection #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))))

(deftest easy-83
  (testing "A Half-Truth"
    (is (= false (c/half-truth false false)))
    (is (= true (c/half-truth true false)))
    (is (= false (c/half-truth true)))
    (is (= true (c/half-truth false true false)))
    (is (= false (c/half-truth true true true)))
    (is (= true (c/half-truth true true true false)))))

(deftest easy-88
  (testing "Symmetric Difference"
    (is (= (c/symmetric-diff #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
    (is (= (c/symmetric-diff #{:a :b :c} #{}) #{:a :b :c}))
    (is (= (c/symmetric-diff #{} #{4 5 6}) #{4 5 6}))
    (is (= (c/symmetric-diff #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]}))))

(deftest easy-90
  (testing "Cartesian Product"
    (is (= (c/cartesian #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
           #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
             ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
             ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
    (is (= (c/cartesian #{1 2 3} #{4 5})
           #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
    (is (= 300 (count (c/cartesian (into #{} (range 10))
                                   (into #{} (range 30))))))))

(deftest easy-95
  (testing "To Tree, or not to Tree"
    (is (= (c/binary-tree? '(:a (:b nil nil) nil))
           true))
    (is (= (c/binary-tree? '(:a (:b nil nil)))
           false))
    (is (= (c/binary-tree? [1 nil [2 [3 nil nil] [4 nil nil]]])
           true))
    (is (= (c/binary-tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]])
           false))
    (is (= (c/binary-tree? [1 [2 [3 [4 nil nil] nil] nil] nil])
           true))
    (is (= (c/binary-tree? [1 [2 [3 [4 false nil] nil] nil] nil])
           false))
    (is (= (c/binary-tree? '(:a nil ()))
           false))))

(deftest easy-96
  (testing "Beauty is Symmetry"
    (is (= (c/symmetric-tree? '(:a (:b nil nil) (:b nil nil))) true))
    (is (= (c/symmetric-tree? '(:a (:b nil nil) nil)) false))
    (is (= (c/symmetric-tree? '(:a (:b nil nil) (:c nil nil))) false))
    (is (= (c/symmetric-tree? [1 [2 nil [3 [4  [5 nil nil] [6 nil nil]] nil]]
                               [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
           true))
    (is (= (c/symmetric-tree? [1 [2 nil [3 [4  [5 nil nil] [6 nil nil]] nil]]
                               [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
           false))
    (is (= (c/symmetric-tree? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                               [2 [3 nil [4 [6 nil nil] nil]] nil]])
           false))))

(deftest easy-97
  (testing "Pascal's Triangle"
    (is (= (c/pascal-row 1) [1]))
    (is (= (map c/pascal-row (range 1 6))
           [[1]
            [1 1]
            [1 2 1]
            [1 3 3 1]
            [1 4 6 4 1]]))
    (is (= (c/pascal-row 11)
           [1 10 45 120 210 252 210 120 45 10 1]))))

(deftest easy-99
  (testing "Product Digits"
    (is (= (c/mul-digits 1 1) [1]))
    (is (= (c/mul-digits 99 9) [8 9 1]))
    (is (= (c/mul-digits 999 99) [9 8 9 0 1]))))

(deftest easy-100
  (testing "Least Common Multiple"
    (is (== (c/lcm 2 3) 6))
    (is (== (c/lcm 5 3 7) 105))
    (is (== (c/lcm 1/3 2/5) 2))
    (is (== (c/lcm 3/4 1/6) 3/2))
    (is (== (c/lcm 7 5/7 2 3/5) 210))))

(deftest easy-107
  (testing "Simple closures"
    (is (= 256 ((c/pow 2) 16) ((c/pow 8) 2)))
    (is (= [1 8 27 64] (map (c/pow 3) [1 2 3 4])))
    (is (= [1 2 4 8 16] (map #((c/pow %) 2) [0 1 2 3 4])))))

(deftest easy-118
  (testing "Re-implement Map"
    (is (= [3 4 5 6 7]
           (c/my-map inc [2 3 4 5 6])))
    (is (= (repeat 10 nil)
           (c/my-map (fn [_] nil) (range 10))))
    (is (= [1000000 1000001]
           (->> (c/my-map inc (range))
                (drop (dec 1000000))
                (take 2))))))

(deftest easy-120
  (testing "Sum of square of digits"
    (is (= 8 (c/f120 (range 10))))
    (is (= 19 (c/f120 (range 30))))
    (is (= 50 (c/f120 (range 100))))
    (is (= 50 (c/f120 (range 1000))))))

(deftest easy-122
  (testing "Read a binary number"
    (is (= 0     (c/binary-value "0")))
    (is (= 7     (c/binary-value "111")))
    (is (= 8     (c/binary-value "1000")))
    (is (= 9     (c/binary-value "1001")))
    (is (= 255   (c/binary-value "11111111")))
    (is (= 1365  (c/binary-value "10101010101")))
    (is (= 65535 (c/binary-value "1111111111111111")))))

(deftest easy-126
  (testing "Through the Looking Class"
    (is (let [x Class]
          (and (= (class x) x) x)))))

(deftest easy-128
  (testing "Recognize Playing Cards"
    (is (= {:suit :diamond :rank 10} (c/f128 "DQ")))
    (is (= {:suit :heart :rank 3} (c/f128 "H5")))
    (is (= {:suit :club :rank 12} (c/f128 "CA")))
    (is (= (range 13) (map (comp :rank c/f128 str)
                           '[S2 S3 S4 S5 S6 S7 S8 S9 ST SJ SQ SK SA])))))

(deftest easy-135
  (testing "Infix Calculator"
    (is (= 7  (c/inflix-calc 2 + 5)))
    (is (= 42 (c/inflix-calc 38 + 48 - 2 / 2)))
    (is (= 8  (c/inflix-calc 10 / 2 - 1 * 2)))
    (is (= 72 (c/inflix-calc 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)))))

(deftest easy-143
  (testing "dot product"
    (is (= 0 (c/dot-product [0 1 0] [1 0 0])))
    (is (= 3 (c/dot-product [1 1 1] [1 1 1])))
    (is (= 32 (c/dot-product [1 2 3] [4 5 6])))
    (is (= 256 (c/dot-product [2 5 6] [100 10 1])))))

(deftest easy-146
  (testing "Trees into tables"
    (is (= (c/f146 '{a {p 1, q 2}
                     b {m 3, n 4}})
           '{[a p] 1, [a q] 2
             [b m] 3, [b n] 4}))
    (is (= (c/f146 '{[1] {a b c d}
                     [2] {q r s t u v w x}})
           '{[[1] a] b, [[1] c] d,
             [[2] q] r, [[2] s] t,
             [[2] u] v, [[2] w] x}))
    (is (= (c/f146 '{m {1 [a b c] 3 nil}})
           '{[m 1] [a b c], [m 3] nil}))))

(deftest easy-147
  (testing "Pascal's Trapezoid"
    (is (= (second (c/pascal-seq [2 3 2])) [2 5 5 2]))
    (is (= (take 5 (c/pascal-seq [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))
    (is (= (take 2 (c/pascal-seq [3 1 2])) [[3 1 2] [3 4 3 2]]))
    (is (= (take 100 (c/pascal-seq [2 4 2])) (rest (take 101 (c/pascal-seq [2 2])))))))

(deftest easy-153
  (testing "Pairwise Disjoint Sets"
    (is (= (c/f153 #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
           true))
    (is (= (c/f153 #{#{:a :b :c :d :e}
                     #{:a :b :c :d}
                     #{:a :b :c}
                     #{:a :b}
                     #{:a}})
           false))
    (is (= (c/f153 #{#{[1 2 3] [4 5]}
                     #{[1 2] [3 4 5]}
                     #{[1] [2] 3 4 5}
                     #{1 2 [3 4] [5]}})
           true))
    (is (= (c/f153 #{#{'a 'b}
                     #{'c 'd 'e}
                     #{'f 'g 'h 'i}
                     #{''a ''c ''f}})
           true))
    (is (= (c/f153 #{#{'(:x :y :z) '(:x :y) '(:z) '()}
                     #{#{:x :y :z} #{:x :y} #{:z} #{}}
                     #{'[:x :y :z] [:x :y] [:z] [] {}}})
           false))
    (is (= (c/f153 #{#{(= "true") false}
                     #{:yes :no}
                     #{(class 1) 0}
                     #{(symbol "true") 'false}
                     #{(keyword "yes") ::no}
                     #{(class '1) (int \0)}})
           false))
    (is (= (c/f153 #{#{distinct?}
                     #{#(-> %) #(-> %)}
                     #{#(-> %) #(-> %) #(-> %)}
                     #{#(-> %) #(-> %) #(-> %)}})
           true))
    (is (= (c/f153 #{#{(#(-> *)) + (quote mapcat) #_ nil}
                     #{'+ '* mapcat (comment mapcat)}
                     #{(do) set contains? nil?}
                     #{, , , #_, , empty?}})
           false))))

(deftest easy-157
  (testing "Indexing Sequences"
    (is (= (c/my-map-indexed [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))
    (is (= (c/my-map-indexed [0 1 3]) '((0 0) (1 1) (3 2))))
    (is (= (c/my-map-indexed [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]]))))

(deftest easy-166
  (testing "Comparisons"
    (is (= :gt (c/my-compare < 5 1)))
    (is (= :eq (c/my-compare (fn [x y] (< (count x) (count y))) "pear" "plum")))
    (is (= :lt (c/my-compare (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
    (is (= :gt (c/my-compare > 0 2)))))

(deftest easy-173
  (testing "Intro to Destructuring 2"
    (is (= 3
           (let [[op x] [+ (range 3)]] (apply op x))
           (let [[[op x] b] [[+ 1] 2]] (op x b))
           (let [[op x] [inc 2]] (op x))))))

(deftest medium-43
  (testing "Reverse Interleave"
    (is (= (c/reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
    (is (= (c/reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
    (is (= (c/reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))))

(deftest medium-44
  (testing "Rotate Sequence"
    (is (= (c/rotate 2 [1 2 3 4 5]) '(3 4 5 1 2)))
    (is (= (c/rotate -2 [1 2 3 4 5]) '(4 5 1 2 3)))
    (is (= (c/rotate 6 [1 2 3 4 5]) '(2 3 4 5 1)))
    (is (= (c/rotate 1 '(:a :b :c)) '(:b :c :a)))
    (is (= (c/rotate -4 '(:a :b :c)) '(:c :a :b)))))

(deftest medium-46
  (testing "Flipping out"
    (is (= 3 ((c/f46 nth) 2 [1 2 3 4 5])))
    (is (= true ((c/f46 >) 7 8)))
    (is (= 4 ((c/f46 quot) 2 8)))
    (is (= [1 2 3] ((c/f46 take) [1 2 3 4 5] 3)))))

(deftest medium-50
  (testing "Split by Type"
    (is (= (set (c/group-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
    (is (= (set (c/group-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
    (is (= (set (c/group-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))))

(deftest medium-53
  (testing "Longest Increasing Sub-Seq"
    (is (= (c/f53 [1 0 1 2 3 0 4 5]) [0 1 2 3]))
    (is (= (c/f53 [5 6 1 3 2 7]) [5 6]))
    (is (= (c/f53 [2 3 3 4 5]) [3 4 5]))
    (is (= (c/f53 [7 6 5 4]) []))))

(deftest medium-54
  (testing "Partition a Sequence"
    (is (= (c/my-partition 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
    (is (= (c/my-partition 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
    (is (= (c/my-partition 3 (range 8)) '((0 1 2) (3 4 5))))))

(deftest medium-55
  (testing "Count Occurrences"
    (is (= (c/occurence-map [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
    (is (= (c/occurence-map [:b :a :b :a :b]) {:a 2, :b 3}))
    (is (= (c/occurence-map '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))))

(deftest medium-56
  (testing "Find Distinct Items"
    (is (= (c/my-distinct [1 2 1 3 1 2 4]) [1 2 3 4]))
    (is (= (c/my-distinct [:a :a :b :b :c :c]) [:a :b :c]))
    (is (= (c/my-distinct '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
    (is (= (c/my-distinct (range 50)) (range 50)))))

(deftest medium-58
  (testing "Function Composition"
    (is (= [3 2 1] ((c/my-comp rest reverse) [1 2 3 4])))
    (is (= 5 ((c/my-comp (partial + 3) second) [1 2 3 4])))
    (is (= true ((c/my-comp zero? #(mod % 8) +) 3 5 7 9)))
    (is (= "HELLO" ((c/my-comp #(clojure.string/upper-case %) #(apply str %) take) 5 "hello world")))))

(deftest medium-59
  (testing "Juxtaposition"
    (is (= [21 6 1] ((c/my-juxt + max min) 2 3 5 1 6 4)))
    (is (= ["HELLO" 5] ((c/my-juxt #(clojure.string/upper-case %) count) "hello")))
    (is (= [2 6 4] ((c/my-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))))

(deftest medium-60
  (testing "Sequence Reductions"
    (is (= (take 5 (c/my-reductions + (range))) [0 1 3 6 10]))
    (is (= (c/my-reductions conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
    (is (= (last (c/my-reductions * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))))

(deftest medium-65
  (testing "Black Box Testing"
    (is (= :map (c/f65 {:a 1, :b 2})))
    (is (= :list (c/f65 (range (rand-int 20)))))
    (is (= :vector (c/f65 [1 2 3 4 5 6])))
    (is (= :set (c/f65 #{10 (rand-int 5)})))
    (is (= [:map :set :vector :list] (map c/f65 [{} #{} [] ()])))))

(deftest medium-67
  (testing "Prime Numbers"
    (is (= (c/prime 2) [2 3]))
    (is (= (c/prime 5) [2 3 5 7 11]))
    (is (= (last (c/prime 100)) 541))))

(deftest medium-69
  (testing "Merge with a Function"
    (is (= (c/my-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
           {:a 4, :b 6, :c 20}))
    (is (= (c/my-merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
           {1 7, 2 10, 3 15}))
    (is (= (c/my-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
           {:a [3 4 5], :b [6 7], :c [8 9]}))))

(deftest medium-70
  (testing "Word Sorting"
    (is (= (c/sort-words "Have a nice day.")
           ["a" "day" "Have" "nice"]))
    (is (= (c/sort-words "Clojure is a fun language!")
           ["a" "Clojure" "fun" "is" "language"]))
    (is (= (c/sort-words "Fools fall for foolish follies.")
           ["fall" "follies" "foolish" "Fools" "for"]))))

(deftest medium-74
  (testing "Filter Perfect Squares"
    (is (= (c/f74 "4,5,6,7,8,9") "4,9"))
    (is (= (c/f74 "15,16,25,36,37") "16,25,36"))))

(deftest medium-75
  (testing "Euler's Totient Function"
    (is (= (c/f75 1) 1))
    (is (= (c/f75 10) (count '(1 3 7 9)) 4))
    (is (= (c/f75 40) 16))
    (is (= (c/f75 99) 60))))

(deftest medium-76
  (testing "Intro to Trampoline"
    (is (= [1 3 5 7 9 11]
           (letfn
               [(foo [x y] #(bar (conj x y) y))
                (bar [x y] (if (> (last x) 10)
                             x
                             #(foo x (+ 2 y))))]
             (trampoline foo [] 1))))))

(deftest medium-77
  (testing "Anagram Finder"
    (is (= (c/f77 ["meat" "mat" "team" "mate" "eat"])
           #{#{"meat" "team" "mate"}}))
    (is (= (c/f77 ["veer" "lake" "item" "kale" "mite" "ever"])
           #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}))))

(deftest medium-78
  (testing "Reimplement Trampoline"
    (is (= (letfn [(triple [x] #(sub-two (* 3 x)))
                   (sub-two [x] #(stop?(- x 2)))
                   (stop? [x] (if (> x 50) x #(triple x)))]
             (c/my-trampoline triple 2))
           82))
    (is (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
                   (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
             (map (partial c/my-trampoline my-even?) (range 6)))
           [true false true false true false]))))

(deftest medium-80
  (testing "Perfect Numbers"
    (is (= (c/perfect-num? 6) true))
    (is (= (c/perfect-num? 7) false))
    (is (= (c/perfect-num? 496) true))
    (is (= (c/perfect-num? 500) false))
    (is (= (c/perfect-num? 8128) true))))

(deftest medium-85
  (testing "Power Set"
    (is (= (c/powerset #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
    (is (= (c/powerset #{}) #{#{}}))
    (is (= (c/powerset #{1 2 3})
           #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))
    (is (= (count (c/powerset (into #{} (range 10)))) 1024))))

(deftest medium-86
  (testing "Perfect Numbers"
    (is (= (c/happy? 7) true))
    (is (= (c/happy? 986543210) true))
    (is (= (c/happy? 2) false))
    (is (= (c/happy? 3) false))))

(deftest medium-93
  (testing "Partially Flatten a Sequence"
    (is (= (c/partial-flatten [["Do"] ["Nothing"]])
           [["Do"] ["Nothing"]]))
    (is (= (c/partial-flatten [[[[:a :b]]] [[:c :d]] [:e :f]])
           [[:a :b] [:c :d] [:e :f]]))
    (is (= (c/partial-flatten '((1 2)((3 4)((((5 6)))))))
           '((1 2)(3 4)(5 6))))))

(deftest medium-98
  (testing "Equivalence Classes"
    (is (= (c/f98 #(* % %) #{-2 -1 0 1 2})
           #{#{0} #{1 -1} #{2 -2}}))
    (is (= (c/f98 #(rem % 3) #{0 1 2 3 4 5 })
           #{#{0 3} #{1 4} #{2 5}}))
    (is (= (c/f98 identity #{0 1 2 3 4})
           #{#{0} #{1} #{2} #{3} #{4}}))
    (is (= (c/f98 (constantly true) #{0 1 2 3 4})
           #{#{0 1 2 3 4}}))))

(deftest medium-102
  (testing "Perfect Numbers"
    (is (= (c/f102 "something") "something"))
    (is (= (c/f102 "multi-word-key") "multiWordKey"))
    (is (= (c/f102 "leaveMeAlone") "leaveMeAlone"))))

(deftest medium-103
  (testing "Generating k-combinations"
    (is (= (c/k-kombinations 1 #{4 5 6}) #{#{4} #{5} #{6}}))
    (is (= (c/k-kombinations 10 #{4 5 6}) #{}))
    (is (= (c/k-kombinations 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))
    (is (= (c/k-kombinations 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                               #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))
    (is (= (c/k-kombinations 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))
    (is (= (c/k-kombinations 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                                            #{:a "abc"} #{:a "efg"} #{"abc" "efg"}}))))

(deftest medium-104
  (testing "Write Roman Numerals"
    (is (= "I" (c/num->roman 1)))
    (is (= "XXX" (c/num->roman 30)))
    (is (= "IV" (c/num->roman 4)))
    (is (= "CXL" (c/num->roman 140)))
    (is (= "DCCCXXVII" (c/num->roman 827)))
    (is (= "MMMCMXCIX" (c/num->roman 3999)))
    (is (= "XLVIII" (c/num->roman 48)))))

(deftest medium-105
  (testing "Identify keys and values"
    (is (= {} (c/f105 [])))
    (is (= {:a [1]} (c/f105 [:a 1])))
    (is (= {:a [1], :b [2]} (c/f105 [:a 1, :b 2])))
    (is (= {:a [1 2 3], :b [], :c [4]} (c/f105 [:a 1 2 3 :b :c 4])))))

(deftest medium-108
  (testing "Lazy Searching"
    (is (= 3 (c/f108 [3 4 5])))
    (is (= 4 (c/f108 [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
    (is (= 7 (c/f108 (range) (range 0 100 7/6) [2 3 5 7 11 13])))
    (is (= 64 (c/f108 (map #(* % % %) (range)) ;; perfect cubes
                      (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                      (iterate inc 20)))))) ;; at least as large as 20

(deftest medium-110
  (testing "Sequence of pronunciations"
    (is (= [[1 1] [2 1] [1 2 1 1]] (take 3 (c/f110 [1]))))
    (is (= [3 1 2 4] (first (c/f110 [1 1 1 4 4]))))
    (is (= [1 1 1 3 2 1 3 2 1 1] (nth (c/f110 [1]) 6)))
    (is (= 338 (count (nth (c/f110 [3 2]) 15))))))

(deftest medium-112
  (testing "Sequs Horribilis"
    (is (= (c/f112 10 [1 2 [3 [4 5] 6] 7])
           '(1 2 (3 (4)))))
    (is (= (c/f112 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
           '(1 2 (3 (4 (5 (6 (7))))))))
    (is (= (c/f112 9 (range))
           '(0 1 2 3)))
    (is (= (c/f112 1 [[[[[1]]]]])
           '(((((1)))))))
    (is (= (c/f112 0 [1 2 [3 [4 5] 6] 7])
           '()))
    (is (= (c/f112 0 [0 0 [0 [0]]])
           '(0 0 (0 (0)))))
    (is (= (c/f112 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
           '(-10 (1 (2 3 (4))))))))

(deftest medium-114
  (testing "Global take-while"
    (is (= [2 3 5 7 11 13]
           (c/f114 4 #(= 2 (mod % 3))
                   [2 3 5 7 11 13 17 19 23])))
    (is (= ["this" "is" "a" "sentence"]
           (c/f114 3 #(some #{\i} %)
                   ["this" "is" "a" "sentence" "i" "wrote"])))
    (is (= ["this" "is"]
           (c/f114 1 #{"a"}
                   ["this" "is" "a" "sentence" "i" "wrote"])))))

(deftest medium-115
  (testing "The Balance of N"
    (is (= true (c/f115 11)))
    (is (= true (c/f115 121)))
    (is (= false (c/f115 123)))
    (is (= true (c/f115 0)))
    (is (= false (c/f115 88099)))
    (is (= true (c/f115 89098)))
    (is (= true (c/f115 89089)))
    (is (= (take 20 (filter c/f115 (range)))
           [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101]))))

(deftest medium-116
  (testing "Prime Sandwich"
    (is (= false (c/balanced-prime? 4)))
    (is (= true (c/balanced-prime? 563)))
    (is (= 1103 (nth (filter c/balanced-prime? (range)) 15)))))

(deftest medium-121
  (testing "Universal Computation Engine"
    (is (= 2 ((c/uce '(/ a b))
              '{b 8 a 16})))
    (is (= 8 ((c/uce '(+ a b 2))
              '{a 2 b 4})))
    (is (= [6 0 -4]
           (map (c/uce '(* (+ 2 a)
                           (- 10 b)))
                '[{a 1 b 8}
                  {b 5 a -2}
                  {a 2 b 11}])))
    (is (= 1 ((c/uce '(/ (+ x 2)
                         (* 3 (+ y 1))))
              '{x 4 y 1})))))

(deftest medium-131
  (testing "Sum Some Set Subsets"
    (is (= true  (c/f131 #{-1 1 99}
                         #{-2 2 888}
                         #{-3 3 7777}))) ; ex. all sets have a subset which sums to zero
    (is (= false (c/f131 #{1}
                         #{2}
                         #{3}
                         #{4})))
    (is (= true  (c/f131 #{1})))
    (is (= false (c/f131 #{1 -3 51 9}
                         #{0}
                         #{9 2 81 33})))
    (is (= true  (c/f131 #{1 3 5}
                         #{9 11 4}
                         #{-3 12 3}
                         #{-3 4 -2 10})))
    (is (= false (c/f131 #{-1 -2 -3 -4 -5 -6}
                         #{1 2 3 4 5 6 7 8 9})))
    (is (= true  (c/f131 #{1 3 5 7}
                         #{2 4 6 8})))
    (is (= true  (c/f131 #{-1 3 -5 7 -9 11 -13 15}
                         #{1 -3 5 -7 9 -11 13 -15}
                         #{1 -1 2 -2 4 -4 8 -8})))
    (is (= true  (c/f131 #{-10 9 -8 7 -6 5 -4 3 -2 1}
                         #{10 -9 8 -7 6 -5 4 -3 2 -1})))))

(deftest medium-132
  (testing "Insert between two items"
    (is (= '(1 :less 6 :less 7 4 3) (c/f132 < :less [1 6 7 4 3])))
    (is (= '(2) (c/f132 > :more [2])))
    (is (= [0 1 :x 2 :x 3 :x 4]  (c/f132 #(and (pos? %) (< % %2)) :x (range 5))))
    (is (empty? (c/f132 > :more ())))
    (is (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
           (take 12 (->> [0 1]
                         (iterate (fn [[a b]] [b (+ a b)]))
                         (map first) ; fibonacci numbers
                         (c/f132 (fn [a b] ; both even or both odd
                                   (= (mod a 2) (mod b 2)))
                                 :same)))))))

(deftest medium-137
  (testing "Digits and bases"
    (is (= [1 2 3 4 5 0 1] (c/digits 1234501 10)))
    (is (= [0] (c/digits 0 11)))
    (is (= [1 0 0 1] (c/digits 9 2)))
    (is (= [1 0] (let [n (rand-int 100000)](c/digits n n))))
    (is (= [16 18 5 24 15 1] (c/digits Integer/MAX_VALUE 42)))))

(deftest medium-141
  (testing "Tricky card games"
    (is (let [notrump (c/f141 nil)]
          (and (= {:suit :club :rank 9}  (notrump [{:suit :club :rank 4}
                                                   {:suit :club :rank 9}]))
               (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                                   {:suit :club :rank 10}])))))
    (is (= {:suit :club :rank 10} ((c/f141 :club) [{:suit :spade :rank 2}
                                                   {:suit :club :rank 10}])))
    (is (= {:suit :heart :rank 8}
           ((c/f141 :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                             {:suit :diamond :rank 10} {:suit :heart :rank 4}])))))

(deftest medium-144
  (testing "Oscilrate"
    (is (= (take 3 (c/f144 3.14 int double)) [3.14 3 3.0]))
    (is (= (take 5 (c/f144 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
    (is (= (take 12 (c/f144 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3]))))

(deftest medium-148
  (testing "The Big Divide"
    (is (= 0 (c/f148 3 17 11)))
    (is (= 23 (c/f148 10 3 5)))
    (is (= 233168 (c/f148 1000 3 5)))
    (is (= "2333333316666668" (str (c/f148 100000000 3 5))))
    (is (= "110389610389889610389610"
           (str (c/f148 (* 10000 10000 10000) 7 11))))
    (is (= "1277732511922987429116"
           (str (c/f148 (* 10000 10000 10000) 757 809))))
    (is (= "4530161696788274281"
           (str (c/f148 (* 10000 10000 1000) 1597 3571))))))

(deftest medium-150
  (testing "Palindromic Numbers"
    (is (= (take 26 (c/palindromic-seq 0))
           [0 1 2 3 4 5 6 7 8 9
            11 22 33 44 55 66 77 88 99
            101 111 121 131 141 151 161]))
    (is (= (take 16 (c/palindromic-seq 162))
           [171 181 191 202
            212 222 232 242
            252 262 272 282
            292 303 313 323]))
    (is (= (take 6 (c/palindromic-seq 1234550000))
           [1234554321 1234664321 1234774321
            1234884321 1234994321 1235005321]))
    (is (= (first (c/palindromic-seq (* 111111111 111111111)))
           (* 111111111 111111111)))
    (is (= (set (take 199 (c/palindromic-seq 0)))
           (set (map #(first (c/palindromic-seq %)) (range 0 10000)))))
    (is (= true
           (apply < (take 6666 (c/palindromic-seq 9999999)))))
    (is (= (nth (c/palindromic-seq 0) 10101)
           9102019))))

(deftest medium-158
  (testing "Decurry"
    (is (= 10 ((c/decurry (fn [a]
                            (fn [b]
                              (fn [c]
                                (fn [d]
                                  (+ a b c d))))))
               1 2 3 4)))
    (is (= 24 ((c/decurry (fn [a]
                            (fn [b]
                              (fn [c]
                                (fn [d]
                                  (* a b c d))))))
               1 2 3 4)))
    (is (= 25 ((c/decurry (fn [a]
                            (fn [b]
                              (* a b))))
               5 5)))))

(deftest medium-168
  (testing "Infinite Matrix"
    (is (= (take 5 (map #(take 6 %) (c/infinite-matrix str)))
           [["00" "01" "02" "03" "04" "05"]
            ["10" "11" "12" "13" "14" "15"]
            ["20" "21" "22" "23" "24" "25"]
            ["30" "31" "32" "33" "34" "35"]
            ["40" "41" "42" "43" "44" "45"]]))
    (is (= (take 6 (map #(take 5 %) (c/infinite-matrix str 3 2)))
           [["32" "33" "34" "35" "36"]
            ["42" "43" "44" "45" "46"]
            ["52" "53" "54" "55" "56"]
            ["62" "63" "64" "65" "66"]
            ["72" "73" "74" "75" "76"]
            ["82" "83" "84" "85" "86"]]))
    (is (= (c/infinite-matrix * 3 5 5 7)
           [[15 18 21 24 27 30 33]
            [20 24 28 32 36 40 44]
            [25 30 35 40 45 50 55]
            [30 36 42 48 54 60 66]
            [35 42 49 56 63 70 77]]))
    (is (= (c/infinite-matrix #(/ % (inc %2)) 1 0 6 4)
           [[1/1 1/2 1/3 1/4]
            [2/1 2/2 2/3 1/2]
            [3/1 3/2 3/3 3/4]
            [4/1 4/2 4/3 4/4]
            [5/1 5/2 5/3 5/4]
            [6/1 6/2 6/3 6/4]]))
    (is (= (class (c/infinite-matrix (juxt bit-or bit-xor)))
           (class (c/infinite-matrix (juxt quot mod) 13 21))
           (class (lazy-seq))))
    (is (= (class (nth (c/infinite-matrix (constantly 10946)) 34))
           (class (nth (c/infinite-matrix (constantly 0) 5 8) 55))
           (class (lazy-seq))))
    (is (= (let [m 377 n 610 w 987
                 check (fn [f s] (every? true? (map-indexed f s)))
                 row (take w (nth (c/infinite-matrix vector) m))
                 column (take w (map first (c/infinite-matrix vector m n)))
                 diagonal (map-indexed #(nth %2 %) (c/infinite-matrix vector m n w w))]
             (and (check #(= %2 [m %]) row)
                  (check #(= %2 [(+ m %) n]) column)
                  (check #(= %2 [(+ m %) (+ n %)]) diagonal)))
           true))))

(deftest medium-171
  (testing "Intervals"
    (is (= (c/f171 [1 2 3]) [[1 3]]))
    (is (= (c/f171 [10 9 8 1 2 3]) [[1 3] [8 10]]))
    (is (= (c/f171 [1 1 1 1 1 1 1]) [[1 1]]))
    (is (= (c/f171 []) []))
    (is (= (c/f171 [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
           [[1 4] [6 6] [9 11] [13 17] [19 19]]))))

(deftest medium-177
  (testing "Balancing Brackets"
    (is (c/balanced-brackets? "This string has no brackets."))
    (is (c/balanced-brackets? "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }"))
    (is (not (c/balanced-brackets? "(start, end]")))
    (is (not (c/balanced-brackets? "())")))
    (is (not (c/balanced-brackets? "[ { ] } ")))
    (is (c/balanced-brackets? "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))"))
    (is (not (c/balanced-brackets? "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))")))
    (is (not (c/balanced-brackets? "[")))))

(deftest medium-195
  (testing "Parentheses... Again"
    (is (= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (c/f195 n)) [0 1 2])))
    (is (= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (c/f195 3)))
    (is (= 16796 (count (c/f195 10))))
    (is (= (nth (sort (filter #(.contains ^String % "(()()()())") (c/f195 9))) 6) "(((()()()())(())))"))
    (is (= (nth (sort (c/f195 12)) 5000) "(((((()()()()()))))(()))"))))

(deftest hard-73
  (testing "Analyze a Tic-Tac-Toe Board"
    (is (= nil (c/f73 [[:e :e :e]
                       [:e :e :e]
                       [:e :e :e]])))
    (is (= :x (c/f73 [[:x :e :o]
                      [:x :e :e]
                      [:x :e :o]])))
    (is (= :o (c/f73 [[:e :x :e]
                      [:o :o :o]
                      [:x :e :x]])))
    (is (= nil (c/f73 [[:x :e :o]
                       [:x :x :e]
                       [:o :x :o]])))
    (is (= :x (c/f73 [[:x :e :e]
                      [:o :x :e]
                      [:o :e :x]])))
    (is (= :o (c/f73 [[:x :e :o]
                      [:x :o :e]
                      [:o :e :x]])))
    (is (= nil (c/f73 [[:x :o :x]
                       [:x :o :x]
                       [:o :x :o]])))))

(deftest hard-79
  (testing "Triangle Minimal Path"
    (is (= 7 (c/triangle-minimal-path '([1]
                                        [2 4]
                                        [5 1 4]
                                        [2 3 4 5])))) ; 1->2->1->3
    (is (= 20 (c/triangle-minimal-path '([3]
                                         [2 4]
                                         [1 9 3]
                                         [9 9 2 4]
                                         [4 6 6 7 8]
                                         [5 7 3 5 1 4])))))) ; 3->4->3->2->7->1

(deftest hard-84
  (testing "Transitive Closure"
    (is (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
          (= (c/f84 divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]})))
    (is (let [more-legs
              #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
          (= (c/f84 more-legs)
             #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
               ["spider" "cat"] ["spider" "man"] ["spider" "snake"]})))
    (is (let [progeny
              #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
          (= (c/f84 progeny)
             #{["father" "son"] ["father" "grandson"]
               ["uncle" "cousin"] ["son" "grandson"]})))))

(deftest hard-82
  (testing "Word Chains"
    (is (= true (c/word-chain? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})))
    (is (= true (c/word-chain? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})))
    (is (= false (c/word-chain? #{"cot" "hot" "bat" "fat"})))
    (is (= false (c/word-chain? #{"to" "top" "stop" "tops" "toss"})))
    (is (= true (c/word-chain? #{"spout" "do" "pot" "pout" "spot" "dot"})))
    (is (= true (c/word-chain? #{"share" "hares" "shares" "hare" "are"})))
    (is (= false (c/word-chain? #{"share" "hares" "hare" "are"})))))

(deftest hard-89
  (testing "Graph Tour"
    (is (= true (c/f89 [[:a :b]])))
    (is (= false (c/f89 [[:a :a] [:b :b]])))
    (is (= false (c/f89 [[:a :b] [:a :b] [:a :c] [:c :a]
                         [:a :d] [:b :d] [:c :d]])))
    (is (= true (c/f89 [[1 2] [2 3] [3 4] [4 1]])))
    (is (= true (c/f89 [[:a :b] [:a :c] [:c :b] [:a :e]
                        [:b :e] [:a :d] [:b :d] [:c :e]
                        [:d :e] [:c :f] [:d :f]])))
    (is (= false (c/f89 [[1 2] [2 3] [2 4] [2 5]])))))

(deftest hard-91
  (testing "Graph Connectivity"
    (is (= true (c/f91 #{[:a :a]})))
    (is (= true (c/f91 #{[:a :b]})))
    (is (= false (c/f91 #{[1 2] [2 3] [3 1]
                          [4 5] [5 6] [6 4]})))
    (is (= true (c/f91 #{[1 2] [2 3] [3 1]
                         [4 5] [5 6] [6 4] [3 4]})))
    (is (= false (c/f91 #{[:a :b] [:b :c] [:c :d]
                          [:x :y] [:d :a] [:b :e]})))
    (is (= true (c/f91 #{[:a :b] [:b :c] [:c :d]
                         [:x :y] [:d :a] [:b :e] [:x :a]})))))

(deftest hard-92
  (testing "Read Roman numerals"
    (is (= 14 (c/roman->num "XIV")))
    (is (= 827 (c/roman->num "DCCCXXVII")))
    (is (= 3999 (c/roman->num "MMMCMXCIX")))
    (is (= 48 (c/roman->num "XLVIII")))))

(deftest hard-94
  (testing "Game of Life"
    (is (= (c/next-game-of-life-board ["      "
                                       " ##   "
                                       " ##   "
                                       "   ## "
                                       "   ## "
                                       "      "])
           ["      "
            " ##   "
            " #    "
            "    # "
            "   ## "
            "      "]))
    (is (= (c/next-game-of-life-board ["     "
                                       "     "
                                       " ### "
                                       "     "
                                       "     "])
           ["     "
            "  #  "
            "  #  "
            "  #  "
            "     "]))
    (is (= (c/next-game-of-life-board ["      "
                                       "      "
                                       "  ### "
                                       " ###  "
                                       "      "
                                       "      "])
           ["      "
            "   #  "
            " #  # "
            " #  # "
            "  #   "
            "      "]))))

(deftest hard-101
  (testing "Levenshtein Distance"
    (is (= (c/levenshtein-dist "kitten" "sitting") 3))
    (is (= (c/levenshtein-dist "closure" "clojure")
           (c/levenshtein-dist "clojure" "closure") 1))
    (is (= (c/levenshtein-dist "xyx" "xyyyx") 2))
    (is (= (c/levenshtein-dist "" "123456") 6))
    (is (= (c/levenshtein-dist "Clojure" "Clojure")
           (c/levenshtein-dist "" "")
           (c/levenshtein-dist [] []) 0))
    (is (= (c/levenshtein-dist [1 2 3 4] [0 2 3 4 5]) 2))
    (is (= (c/levenshtein-dist '(:a :b :c :d) '(:a :d)) 2))
    (is (= (c/levenshtein-dist "ttttattttctg" "tcaaccctaccat") 10))
    (is (= (c/levenshtein-dist "gaattctaatctc" "caaacaaaaaattt") 9))))

(deftest hard-106
  (testing "Number Maze"
    (is (= 1 (c/f106 1 1)))    ; 1
    (is (= 3 (c/f106 3 12)))   ; 3 6 12
    (is (= 3 (c/f106 12 3)))   ; 12 6 3
    (is (= 3 (c/f106 5 9)))    ; 5 7 9
    (is (= 9 (c/f106 9 2)))    ; 9 18 20 10 12 6 8 4 2
    (is (= 5 (c/f106 9 12))))) ; 9 11 22 24 12

(deftest hard-111
  (testing "Crossword puzzle"
    (is (= true  (c/f111 "the" ["_ # _ _ e"])))
    (is (= false (c/f111 "the" ["c _ _ _"
                                "d _ # e"
                                "r y _ _"])))
    (is (= true  (c/f111 "joy" ["c _ _ _"
                                "d _ # e"
                                "r y _ _"])))
    (is (= false (c/f111 "joy" ["c o n j"
                                "_ _ y _"
                                "r _ _ #"])))
    (is (= true  (c/f111 "clojure" ["_ _ _ # j o y"
                                    "_ _ o _ _ _ _"
                                    "_ _ f _ # _ _"])))))

(deftest hard-113
  (testing "Making Data Dance"
    (is (= "1, 2, 3" (str (c/f113 2 1 3))))
    (is (= '(2 1 3) (seq (c/f113 2 1 3))))
    (is (= '(2 1 3) (seq (c/f113 2 1 3 3 1 2))))
    (is (= '(1) (seq (apply c/f113 (repeat 5 1)))))
    (is (= "1, 1, 1, 1, 1" (str (apply c/f113 (repeat 5 1)))))
    (is (and (= nil (seq (c/f113)))
             (=  "" (str (c/f113)))))))

(deftest hard-117
  (testing "For Science!"
    (is (= true  (c/f117 ["M   C"])))
    (is (= false (c/f117 ["M # C"])))
    (is (= true  (c/f117 ["#######"
                          "#     #"
                          "#  #  #"
                          "#M # C#"
                          "#######"])))
    (is (= false (c/f117 ["########"
                          "#M  #  #"
                          "#   #  #"
                          "# # #  #"
                          "#   #  #"
                          "#  #   #"
                          "#  # # #"
                          "#  #   #"
                          "#  #  C#"
                          "########"])))
    (is (= false (c/f117 ["M     "
                          "      "
                          "      "
                          "      "
                          "    ##"
                          "    #C"])))
    (is (= true  (c/f117 ["C######"
                          " #     "
                          " #   # "
                          " #   #M"
                          "     # "])))
    (is (= true  (c/f117 ["C# # # #"
                          "        "
                          "# # # # "
                          "        "
                          " # # # #"
                          "        "
                          "# # # #M"])))))

(deftest hard-119
  (testing "Win at Tic-Tac-Toe"
    (is (= (c/f119 :x [[:o :e :e]
                       [:o :x :o]
                       [:x :x :e]])
           #{[2 2] [0 1] [0 2]}))
    (is (= (c/f119 :x [[:x :o :o]
                       [:x :x :e]
                       [:e :o :e]])
           #{[2 2] [1 2] [2 0]}))
    (is (= (c/f119 :x [[:x :e :x]
                       [:o :x :o]
                       [:e :o :e]])
           #{[2 2] [0 1] [2 0]}))
    (is (= (c/f119 :x [[:x :x :o]
                       [:e :e :e]
                       [:e :e :e]])
           #{}))
    (is (= (c/f119 :o [[:x :x :o]
                       [:o :e :o]
                       [:x :e :e]])
           #{[2 2] [1 1]}))))

(deftest hard-124
  (testing "Analyze Reversi"
    (is (= {[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}
           (c/analyze-reversi '[[e e e e]
                                [e w b e]
                                [e b w e]
                                [e e e e]] 'w)))
    (is (= {[3 2] #{[2 2]}, [3 0] #{[2 1]}, [1 0] #{[1 1]}}
           (c/analyze-reversi '[[e e e e]
                                [e w b e]
                                [w w w e]
                                [e e e e]] 'b)))
    (is (= {[0 3] #{[1 2]}, [1 3] #{[1 2]}, [3 3] #{[2 2]}, [2 3] #{[2 2]}}
           (c/analyze-reversi '[[e e e e]
                                [e w b e]
                                [w w b e]
                                [e e b e]] 'w)))
    (is (= {[0 3] #{[2 1] [1 2]}, [1 3] #{[1 2]}, [2 3] #{[2 1] [2 2]}}
           (c/analyze-reversi '[[e e w e]
                                [b b w e]
                                [b w w e]
                                [b w w w]] 'b)))))

(deftest hard-125
  (testing "Gus' Quinundrum"
    (is (= (str '(fn []
                   (let [q (char 34) s (char 32)
                         lines ["(fn [] "
                                "(let [q (char 34) s (char 32) lines ["
                                "]] "
                                "(apply str (concat (take 2 lines) "
                                "(interpose (str s) (map (fn [l] (str q l q)) lines)) "
                                "(drop 2 lines)))))"]]
                     (apply str (concat (take 2 lines)
                                        (interpose (str s) (map (fn [l] (str q l q)) lines))
                                        (drop 2 lines))))))
           ((fn []
              (let [q (char 34) s (char 32)
                    lines ["(fn [] "
                           "(let [q (char 34) s (char 32) lines ["
                           "]] "
                           "(apply str (concat (take 2 lines) "
                           "(interpose (str s) (map (fn [l] (str q l q)) lines)) "
                           "(drop 2 lines)))))"]]
                (apply str (concat (take 2 lines)
                                   (interpose (str s) (map (fn [l] (str q l q)) lines))
                                   (drop 2 lines))))))))))

(deftest hard-127
  (testing "Love Triangle"
    (is (= 10 (c/f127 [15 15 15 15 15])))
    (is (= 15 (c/f127 [1 3 7 15 31])))
    (is (= 3 (c/f127 [3 3])))
    (is (= 4 (c/f127 [7 3])))
    (is (= 6 (c/f127 [17 22 6 14 22])))
    (is (= 9 (c/f127 [18 7 14 14 6 3])))
    (is (= nil (c/f127 [21 10 21 10])))
    (is (= nil (c/f127 [0 31 0 31 0])))))

(deftest hard-130
  (testing "Tree reparenting"
    (is (= '(n)
           (c/reparent-tree 'n '(n))))
    (is (= '(a (t (e)))
           (c/reparent-tree 'a '(t (e) (a)))))
    (is (= '(e (t (a)))
           (c/reparent-tree 'e '(a (t (e))))))
    (is (= '(a (b (c)))
           (c/reparent-tree 'a '(c (b (a))))))
    (is (= '(d
             (b
              (c)
              (e)
              (a
               (f
                (g)
                (h)))))
           (c/reparent-tree 'd '(a
                                 (b
                                  (c)
                                  (d)
                                  (e))
                                 (f
                                  (g)
                                  (h))))))
    (is (= '(c
             (d)
             (e)
             (b
              (f
               (g)
               (h))
              (a
               (i
                (j
                 (k)
                 (l))
                (m
                 (n)
                 (o))))))
           (c/reparent-tree 'c '(a
                                 (b
                                  (c
                                   (d)
                                   (e))
                                  (f
                                   (g)
                                   (h)))
                                 (i
                                  (j
                                   (k)
                                   (l))
                                  (m
                                   (n)
                                   (o)))))))))

(deftest hard-138
  (testing "Squares Squared"
    (is (= (c/squares-squared 2 2) ["2"]))
    (is (= (c/squares-squared 2 4) [" 2 "
                                    "* 4"
                                    " * "]))
    (is (= (c/squares-squared 3 81) [" 3 "
                                     "1 9"
                                     " 8 "]))
    (is (= (c/squares-squared 4 20) [" 4 "
                                     "* 1"
                                     " 6 "]))
    (is (= (c/squares-squared 2 256) ["  6  "
                                      " 5 * "
                                      "2 2 *"
                                      " 6 4 "
                                      "  1  "]))
    (is (= (c/squares-squared 10 10000) ["   0   "
                                         "  1 0  "
                                         " 0 1 0 "
                                         "* 0 0 0"
                                         " * 1 * "
                                         "  * *  "
                                         "   *   "]))))

(deftest hard-140
  (testing "Veitch, Please!"
    ;; (6 8 9 10 11 12 13 14)
    (is (= (c/f140 #{#{'a 'B 'C 'd}
                     #{'A 'b 'c 'd}
                     #{'A 'b 'c 'D}
                     #{'A 'b 'C 'd}
                     #{'A 'b 'C 'D}
                     #{'A 'B 'c 'd}
                     #{'A 'B 'c 'D}
                     #{'A 'B 'C 'd}})
           #{#{'A 'c}
             #{'A 'b}
             #{'B 'C 'd}}))
    ;; (14 15)
    (is (= (c/f140 #{#{'A 'B 'C 'D}
                     #{'A 'B 'C 'd}})
           #{#{'A 'B 'C}}))
    ;; (0 1 4 5 10 11 14 15)
    (is (= (c/f140 #{#{'a 'b 'c 'd}
                     #{'a 'B 'c 'd}
                     #{'a 'b 'c 'D}
                     #{'a 'B 'c 'D}
                     #{'A 'B 'C 'd}
                     #{'A 'B 'C 'D}
                     #{'A 'b 'C 'd}
                     #{'A 'b 'C 'D}})
           #{#{'a 'c}
             #{'A 'C}}))
    ;; (0 1 2 3 4 5 6 7)
    (is (= (c/f140 #{#{'a 'b 'c 'd}
                     #{'a 'b 'c 'D}
                     #{'a 'B 'c 'd}
                     #{'a 'B 'c 'D}
                     #{'a 'b 'C 'd}
                     #{'a 'b 'C 'D}
                     #{'a 'B 'C 'd}
                     #{'a 'B 'C 'D}})
           #{#{'a}}))
    ;; (1 4 7 11 13 14)
    (is (= (c/f140 #{#{'a 'B 'c 'd}
                     #{'A 'B 'c 'D}
                     #{'A 'b 'C 'D}
                     #{'a 'b 'c 'D}
                     #{'a 'B 'C 'D}
                     #{'A 'B 'C 'd}})
           #{#{'a 'B 'c 'd}
             #{'A 'B 'c 'D}
             #{'A 'b 'C 'D}
             #{'a 'b 'c 'D}
             #{'a 'B 'C 'D}
             #{'A 'B 'C 'd}}))
    ;; (0 1 4 5 12 13)
    (is (= (c/f140 #{#{'a 'b 'c 'd}
                     #{'a 'B 'c 'd}
                     #{'A 'B 'c 'd}
                     #{'a 'b 'c 'D}
                     #{'a 'B 'c 'D}
                     #{'A 'B 'c 'D}})
           #{#{'a 'c}
             #{'B 'c}}))
    ;; (1 3 4 6 9 11 12 14)
    (is (= (c/f140 #{#{'a 'B 'c 'd}
                     #{'A 'B 'c 'd}
                     #{'a 'b 'c 'D}
                     #{'a 'b 'C 'D}
                     #{'A 'b 'c 'D}
                     #{'A 'b 'C 'D}
                     #{'a 'B 'C 'd}
                     #{'A 'B 'C 'd}})
           #{#{'B 'd}
             #{'b 'D}}))
    ;; (0 2 5 7 8 10 13 15)
    (is (= (c/f140 #{#{'a 'b 'c 'd}
                     #{'A 'b 'c 'd}
                     #{'a 'B 'c 'D}
                     #{'A 'B 'c 'D}
                     #{'a 'B 'C 'D}
                     #{'A 'B 'C 'D}
                     #{'a 'b 'C 'd}
                     #{'A 'b 'C 'd}})
           #{#{'B 'D}
             #{'b 'd}}))))

(deftest hard-152
  (testing "Latin Square Slicing"
    (is (= (c/f152 '[[A B C D]
                     [A C D B]
                     [B A D C]
                     [D C A B]])
           {}))
    (is (= (c/f152 '[[A B C D E F]
                     [B C D E F A]
                     [C D E F A B]
                     [D E F A B C]
                     [E F A B C D]
                     [F A B C D E]])
           {6 1}))
    (is (= (c/f152 '[[A B C D]
                     [B A D C]
                     [D C B A]
                     [C D A B]])
           {4 1, 2 4}))
    (is (= (c/f152 '[[B D A C B]
                     [D A B C A]
                     [A B C A B]
                     [B C A B C]
                     [A D B C A]])
           {3 3}))
    (is (= (c/f152 [[2 4 6 3]
                    [3 4 6 2]
                    [6 2 4]])
           {}))
    (is (= (c/f152 [[1]
                    [1 2 1 2]
                    [2 1 2 1]
                    [1 2 1 2]
                    []])
           {2 2}))
    (is (= (c/f152 [[3 1 2]
                    [1 2 3 1 3 4]
                    [2 3 1 3]])
           {3 1, 2 2}))
    (is (= (c/f152 [[8 6 7 3 2 5 1 4]
                    [6 8 3 7]
                    [7 3 8 6]
                    [3 7 6 8 1 4 5 2]
                    [1 8 5 2 4]
                    [8 1 2 4 5]])
           {4 1, 3 1, 2 7}))))

(deftest hard-164
  (testing "Language of a DFA"
    (is (= #{"a" "ab" "abc"}
           (set (c/f164 '{:states #{q0 q1 q2 q3}
                          :alphabet #{a b c}
                          :start q0
                          :accepts #{q1 q2 q3}
                          :transitions {q0 {a q1}
                                        q1 {b q2}
                                        q2 {c q3}}}))))
    (is (= #{"hi" "hey" "hello"}
           (set (c/f164 '{:states #{q0 q1 q2 q3 q4 q5 q6 q7}
                          :alphabet #{e h i l o y}
                          :start q0
                          :accepts #{q2 q4 q7}
                          :transitions {q0 {h q1}
                                        q1 {i q2, e q3}
                                        q3 {l q5, y q4}
                                        q5 {l q6}
                                        q6 {o q7}}}))))
    (is (= (set (let [ss "vwxyz"] (for [i ss, j ss, k ss, l ss] (str i j k l))))
           (set (c/f164 '{:states #{q0 q1 q2 q3 q4}
                          :alphabet #{v w x y z}
                          :start q0
                          :accepts #{q4}
                          :transitions {q0 {v q1, w q1, x q1, y q1, z q1}
                                        q1 {v q2, w q2, x q2, y q2, z q2}
                                        q2 {v q3, w q3, x q3, y q3, z q3}
                                        q3 {v q4, w q4, x q4, y q4, z q4}}}))))
    (is (let [res (take 2000 (c/f164 '{:states #{q0 q1}
                                       :alphabet #{0 1}
                                       :start q0
                                       :accepts #{q0}
                                       :transitions {q0 {0 q0, 1 q1}
                                                     q1 {0 q1, 1 q0}}}))]
          (and (every? (partial re-matches #"0*(?:10*10*)*") res)
               (= res (distinct res)))))
    (is (let [res (take 2000 (c/f164 '{:states #{q0 q1}
                                       :alphabet #{n m}
                                       :start q0
                                       :accepts #{q1}
                                       :transitions {q0 {n q0, m q1}}}))]
          (and (every? (partial re-matches #"n*m") res)
               (= res (distinct res)))))
    (is (let [res (take 2000 (c/f164 '{:states #{q0 q1 q2 q3 q4 q5 q6 q7 q8 q9}
                                       :alphabet #{i l o m p t}
                                       :start q0
                                       :accepts #{q5 q8}
                                       :transitions {q0 {l q1}
                                                     q1 {i q2, o q6}
                                                     q2 {m q3}
                                                     q3 {i q4}
                                                     q4 {t q5}
                                                     q6 {o q7}
                                                     q7 {p q8}
                                                     q8 {l q9}
                                                     q9 {o q6}}}))]
          (and (every? (partial re-matches #"limit|(?:loop)+") res)
               (= res (distinct res)))))))

(deftest hard-178
  (testing "Best Hand"
    (is (= :high-card (c/best-hand ["HA" "D2" "H3" "C9" "DJ"])))
    (is (= :pair (c/best-hand ["HA" "HQ" "SJ" "DA" "HT"])))
    (is (= :two-pair (c/best-hand ["HA" "DA" "HQ" "SQ" "HT"])))
    (is (= :three-of-a-kind (c/best-hand ["HA" "DA" "CA" "HJ" "HT"])))
    (is (= :straight (c/best-hand ["HA" "DK" "HQ" "HJ" "HT"])))
    (is (= :straight (c/best-hand ["HA" "H2" "S3" "D4" "C5"])))
    (is (= :flush (c/best-hand ["HA" "HK" "H2" "H4" "HT"])))
    (is (= :full-house (c/best-hand ["HA" "DA" "CA" "HJ" "DJ"])))
    (is (= :four-of-a-kind (c/best-hand ["HA" "DA" "CA" "SA" "DJ"])))
    (is (= :straight-flush (c/best-hand ["HA" "HK" "HQ" "HJ" "HT"])))))
