(ns http-load-test.cmdline
  (:require [http-load-test.core :as clobber]
            [clojure.string :as string]
            [clojure.tools.cli :as cmdline])
  (:gen-class :main true))


(defn labeled-line
  ([results key]
   (labeled-line results key (string/capitalize (string/replace (string/replace key ":" "") "-" " "))))
  ([results key title]
   (format "%s:  \t\t%s\n" title (key results))))

(defn print-results [results]
  (let [format-percent (fn [[percent time]] (format "\t%s%%\t%s ms\n" percent time))
        preamble [:total-time :average-time, :successes, :redirects, :errors]
        output []]
    (println (reduce str (map (fn[x] (labeled-line results x)) preamble)))
    (println "Percentages:")
    (println (reduce str (map format-percent (:breakdowns results))))))

(defn clobber-that-url [config-file num-requests]
  (let [config (read-string (slurp config-file))
        results (clobber/clobber (:url config) (:templates config) num-requests)]
    (println)
    (print-results results)))

(def results {:total-time "10s",
              :average-time "506ms",
              :successes 0,
              :redirects 0,
              :errors 20,
              :breakdowns '([50 449]
                             [60 450]
                             [70 451]
                             [80 451]
                             [90 459]
                             [95 463]
                             [99 463]
                             [100 1582]),
              :qps 1.9762846})
(defn -main [& args]
  (let [[options args banner] (cmdline/cli args
                                           ["-c" "--config-file" "Configuration file"]
                                           ["-n" "-num-requests" "Number of requests" :parse-fn #(Integer. %)])
        file-sep (System/getProperty "file.separator")
        pwd (System/getProperty "user.dir")
        config-file (str pwd file-sep (:config-file options))]

    (clobber-that-url config-file (:num-requests options))))

