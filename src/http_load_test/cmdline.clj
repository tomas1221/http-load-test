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


(defn usage [options-summary]
  (->> [
        ""
        "Usage: program-name [options]"
        ""
        "Options:"
        options-summary
        ]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (cmdline/parse-opts args
                                           ["-c" "--config-file" "Configuration file"]
                                           ["-n" "-num-requests" "Number of requests" :parse-fn #(Integer/parseInt %)])
        file-sep (System/getProperty "file.separator")
        pwd (System/getProperty "user.dir")
        config-file (str pwd file-sep (:config-file options))]

    (cond
      (:help options) (exit 0 (usage summary))
      (not= (count arguments) 0) (exit 1 (usage summary))
      errors (exit 1 (error-msg errors)))

    (clobber-that-url config-file (:num-requests options))))

