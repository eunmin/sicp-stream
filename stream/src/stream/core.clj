(ns stream.core
  (:refer-clojure :exclude [delay force])
  (:require [is-prime.core :refer [is-prime]]))

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
    (stream-ref (stream-car s) (- n 1))))

(defn stream-map [proc s]
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

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
