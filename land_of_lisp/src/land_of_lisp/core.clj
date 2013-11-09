(ns land-of-lisp.core)

(defn average [& xs]
  (/ (apply + xs)
     (count xs)))

(defn next-guess [low-limit up-limit]
  (average low-limit up-limit))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
