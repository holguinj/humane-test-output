(ns pjstadig.humane-test-output
  (:use [clojure.test])
  (:require [clojure.data :as data]
            [clojure.pprint :as pp]))

(defonce activation-body
  (delay
   (when (not (System/getenv "INHUMANE_TEST_OUTPUT"))
     (defmethod assert-expr '= [msg [_ a & more]]
       `(let [a# ~a]
          (if-let [more# (seq (list ~@more))]
            (let [result# (apply = a# more#)]
              (if result#
                (do-report {:type :pass, :message ~msg,
                            :expected a#, :actual more#})
                (do-report {:type :fail, :message ~msg,
                            :expected a#, :actual more#,
                            :diffs (map vector
                                        more#
                                        (map #(take 2 (data/diff a# %))
                                             more#))}))
              result#)
            (throw (Exception. "= expects more than one argument")))))

     (defmethod report :fail
       [{:keys [type expected actual diffs message] :as event}]
       (with-test-out
         (inc-report-counter :fail)
         (println "\033[1;31m\nFAIL in" (testing-vars-str event) "\033[0m")
         (when (seq *testing-contexts*) (println (testing-contexts-str)))
         (when message (println message))
         (binding [*out* (pp/get-pretty-writer *out*)]
           (let [print-expected (fn [actual]
                                  (print "expected: ")
                                  (pp/pprint expected)
                                  (print "  actual: ")
                                  (pp/pprint actual))]
             (if (seq diffs)
               (doseq [[actual [a b]] diffs
                       :when (or a b)]
                 (print-expected actual)
                 (print "    diff:")
                 (if a
                   (do (print " - ")
                       (pp/pprint a)
                       (print "          + "))
                   (print " + "))
                 (when b
                   (pp/pprint b)))
               (print-expected actual)))))))))

(defmethod report :summary [m]
  (with-test-out
   (println "\nRan" (:test m) "tests containing"
            (+ (:pass m) (:fail m) (:error m)) "assertions.")
   (let [start-colorize (if (or (pos? (:fail m)) (pos? (:error m)))
                          "\033[1;31m"  ;; red
                          "\033[1;32m") ;; green
         end-colorize "\033[0m"] ;; black
     (println (str start-colorize (:fail m) " failures, " (:error m) " errors." end-colorize)))))

(defn activate! []
  @activation-body)
