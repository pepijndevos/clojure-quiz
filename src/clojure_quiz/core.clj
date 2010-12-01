(ns clojure-quiz.core
  (:use [clojure.set :only [map-invert]]))

(def ^{:dynamic true} *target* 'clojure.core)
(def ^{:dynamic true} *number* 100)
(def ^{:dynamic true} *choices* 5)

(defn get-answers []
  (into
   {}
   (map
    (juxt (comp name key) (comp :doc meta val))
    (take *number*
          (shuffle
           (filter (comp :doc meta val)
                   (ns-publics *target*)))))))

(defn guess-fn-name [answers]
  (let [[k v :as question] (rand-nth (seq answers))]
    (println "What is the name of this function?")
    (println v)
    [k k]))

(defn answer-map [[k v] answers]
  (when (> (count answers) 1)
    (apply hash-map
           (-> (conj (take (dec *choices*) (shuffle (vals (dissoc answers k)))) v)
               shuffle
               (interleave "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))))

(defn print-or-nil [x] (and x (println x)))

(defn format-choices [header ansmap]
  (when ansmap
    (str header "\n"
         (apply str (for [[ka va] (sort-by val ansmap)] (str va ": " ka "\n"))))))

(defn choose-fn-name [answers]
  (let [[k v :as answer] (rand-nth (seq answers))
        ansmap (answer-map (reverse answer) (map-invert answers))]
    (print-or-nil (format-choices (str "What is the name of this function?\n" v) ansmap))
    [k (when (> (count answers) 1) (.toLowerCase (str (ansmap k))))]))

(defn choose-doc [answers]
  (let [[k v :as answer] (rand-nth (seq answers))
        ansmap (answer-map answer answers)]
    (print-or-nil (format-choices (str "What is the correct documentation for " k "?") ansmap))
    [k (when (> (count answers) 1) (.toLowerCase (str (ansmap v))))]))

(defn print-score [right wrong]
  (println (str "Right: " right ", Wrong: " wrong ", Percentage: ")
           (int (* 100 (/ right (+ right wrong)))) "\n"))

(defn quiz [right wrong questioner answers]
  (let [answers? (seq answers)
        [rem correct] (when answers? (questioner answers))
        answer (when correct (.toLowerCase (read-line)))]
    (cond
     (and (string? answer) (empty? answer))
     (do (println "Skipping this one for now.\n")
         (recur right wrong questioner answers))
     (nil? correct)
     (do (print "\nThat's all folks! Final score: ")
         (print-score right wrong))
     (= correct answer)
     (do (print "\nCorrect! ")
         (let [new-right (inc right)]
           (print-score new-right wrong)
           (recur new-right wrong questioner (dissoc answers rem))))
     :else
     (do (print (str "\nIncorrect! Was: " correct "; "))
         (let [new-wrong (inc wrong)]
           (print-score right new-wrong)
           (recur right new-wrong questioner (dissoc answers rem)))))))

(defn start-quiz [q]
  (quiz 0 0 q (get-answers)))

(def doc-quiz       #(start-quiz #'choose-doc))
(def name-quiz      #(start-quiz #'guess-fn-name))
(def name-quiz-easy #(start-quiz #'choose-fn-name))
