(ns clojure-quiz.core)

(def target 'clojure.core)
(def number 3)

(defn get-answers []
  (map
    (juxt (comp name key) (comp :doc meta val))
    (take number
          (shuffle
            (filter (comp :doc meta val)
                    (seq (ns-publics target)))))))

(defn guess-fn-name []
  (let [answers (get-answers)
        question (rand-nth answers)]
    (println "What is the name of this function?")
    (println (last question))
    (first question)))

(defn choose-fn-name []
  (let [answers (get-answers)
        question (rand-nth answers)
        ansmap (apply array-map (interleave (map first answers) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))]
    (println "What is the name of this function?")
    (println (last question))
    (doseq [a ansmap]
      (println (val a) ":" (key a)))
    (.toLowerCase (str (get ansmap (first question))))))

(defn choose-doc []
  (let [answers (get-answers)
        question (rand-nth answers)
        ansmap (apply array-map (interleave (map last answers) "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))]
    (println "What is the correct documentation for" (first question))
    (doseq [a ansmap]
      (println (val a) ":" (key a)))
    (.toLowerCase (str (get ansmap (last question))))))

(defn quiz [right wrong questioner]
  (let [correct (questioner)
        answer (.toLowerCase (read-line))]
    (if (= correct answer)
      (do
        (println "Correct!")
        (let [right (inc right)]
          (println "Right:" right ", Wrong:" wrong ", Percentage:" (int (* 100 (/ right (+ right wrong)))))
          (recur right wrong questioner)))
      (do
        (println "Wrong!")
        (println "Was:" correct)
        (let [wrong (inc wrong)]
          (println "Right:" right ", Wrong:" wrong ", Percentage:" (int (* 100 (/ right (+ right wrong)))))
          (recur right wrong questioner))))))

(def doc-quiz       (partial quiz 0 0 choose-doc))
(def name-quiz      (partial quiz 0 0 guess-fn-name))
(def name-quiz-easy (partial quiz 0 0 choose-fn-name))
