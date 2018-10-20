(ns stream.core
  (:refer-clojure :exclude [delay force])
  (:require [is-prime.core :refer [is-prime]]))

;; 3.5.1

(defn memo-proc [proc]
  (let [alreay-run? (atom false)
        result (atom false)]
    (fn []
      (if (not @alreay-run?)
        (do (reset! result (proc))
            (reset! alreay-run? true)
            @result)
        @result))))

(defmacro delay [exp]
  `(memo-proc (fn []
                ~exp)))

(defn force [exp]
  (exp))

(def the-empty-stream '())

(defn stream-null? [s]
  (empty? s))

(defn stream-car [s]
  (first s))

(defn stream-cdr [s]
  (force (second s)))

(defmacro cons-stream [a b]
  `(list ~a (delay ~b)))

(defn stream-ref [s n]
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

;; 3.50
(defn stream-map [proc & argstreams]
  (if (stream-null? (first argstreams))
    the-empty-stream
    (cons-stream (apply proc (map stream-car argstreams))
                 (apply stream-map (cons proc (map stream-cdr argstreams))))))

(defn stream-enumerate-interval [low high]
  (if (> low high)
    the-empty-stream
    (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(defn stream-filter [pred stream]
  (cond
    (stream-null? stream) the-empty-stream
    (pred (stream-car stream)) (cons-stream (stream-car stream)
                                            (stream-filter pred
                                                           (stream-cdr stream)))
    :else (stream-filter pred (stream-cdr stream))))

;; 3.51
(defn show [x]
  (println x)
  x)

(def x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 5)

(stream-ref x 7)

;; 3.52
(def sum (atom 0))

(defn accum [x]
  (swap! sum inc)
  @sum)

(def seq (stream-map accum (stream-enumerate-interval 1 20)))

(def y (stream-filter even? seq))

(def z (stream-filter (fn [x]
                        (= (mod x 5) 0))
                      seq))

(stream-ref y 7)

(defn stream-for-each [proc s]
  (if (stream-null? s)
    'done
    (do (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(defn display-stream [s]
  (stream-for-each println s))

(defn prime? [n]
  (is-prime n))

(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000 1000000))))

;; 3.5.2

(defn integers-starting-from [n]
  (cons-stream n (integers-starting-from (+ n 1))))

(def integers (integers-starting-from 1))

(defn divisible? [x y]
  (= (mod x y) 0))

(def no-sevens
  (stream-filter (fn [x]
                   (not (divisible? x 7)))
                 integers))

(stream-ref no-sevens 100)

(defn fibgen [a b]
  (cons-stream a (fibgen b (+ a b))))

(def fibs (fibgen 0 1))

(stream-ref fibs 10)

(defn sieve [stream]
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter (fn [x]
                           (not (divisible? x (stream-car stream))))
                         (stream-cdr stream)))))

(def primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)

(def ones (cons-stream 1 ones))

(defn add-streams [s1 s2]
  (stream-map + s1 s2))

(def integers (cons-stream 1 (add-streams ones integers)))

(def fibs (cons-stream 0
                       (cons-stream 1
                                    (add-streams (stream-cdr fibs)
                                                 fibs))))

(stream-ref fibs 10)

(defn scale-stream [stream factor]
  (stream-map (fn [x]
                (* x factor))
              stream))

(def double (cons-stream 1 (scale-stream double 2)))

(def primes (cons-stream 2 (stream-filter prime? (integers-starting-from 3))))

;; 3.53
;; (1 2 4 8 ...)

;; 3.54
(defn mul-streams [s1 s2]
  (stream-map * s1 s2))

(def factorials (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))

(stream-ref factorials 10)

;; 3.55
(defn partial-sums [s]
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

(stream-ref (partial-sums integers) 4)

;; 3.56
(defn merge [s1 s2]
  (cond
    (stream-null? s1) s2
    (stream-null? s2) s1
    :else (let [s1car (stream-car s1)
                s2car (stream-car s2)]
            (cond
              (< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2))
              (> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2)))
              :else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))

(def S (cons-stream 1 (merge (scale-stream S 2)
                             (merge (scale-stream S 3)
                                    (scale-stream S 5)))))
