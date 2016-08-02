(ns http-load-test.core
  (:require [clojure.string :as string]
            [clj-http.client :as client]))

(def println-now
  (comp (fn [_] (. *out* (flush))) println))

(def print-now
  (comp (fn[_] (. *out* (flush))) print))

(defn- between [x [a b]]
  (and (>= x a) (<= x b)))

(defn apply-template [url-spec vars]
  (let [variables (keys vars)
        apply-var (fn [tmpl replacement]
                    (string/replace tmpl (str replacement) (rand-nth (replacement vars))))]
    (doall (reduce apply-var url-spec variables))))

(defn- now-millis []
  (System/currentTimeMillis))

(defn- signal-requests [requests-made]
  (cond (= 0 requests-made) (println-now "Starting...")
        (= 0 (mod requests-made 100)) (println-now requests-made " requests")))

(defn- format-time [ms]
  (cond (< ms 1000) (str ms "ms")
        (< ms 60000) (str (int (/ ms 1000)) "s")
        :else (str (int (/ ms 60000)) "m")))

(defn make-timed-request [url params]
  (let [before (now-millis)
        all-params (merge params {:throw-exceptions false, :coerce :always})
        response (client/get url all-params {:as :byte-array, :decompress-body false, :coerce :always, :throw-exception false})]
    (if (between (:status response) [200 299])
      (print-now "+")
      (print-now "-"))
    {:response response
     :elapsed (- (now-millis) before)}))

(defn- url-seq [url replacements]
  (repeatedly #(apply-template url replacements)))

(defn make-requests [url replacements num-times]
  (loop [urls (take num-times (url-seq url replacements))
         timings []]
    (let [requests-made (- num-times (count urls))]
      (signal-requests requests-made)
      (if (empty? urls)
        timings
        (recur (rest urls)
               (conj timings (make-timed-request (first urls) {})))))))

(defn- sum-field [field-name responses]
  (if (nil? responses) 0
                       (reduce + (map (fn [n] (get n field-name 0)) responses))))

(defn- break-it-down [responses breakdowns]
  (let [response-times (sort (map (fn [x] (:elapsed x)) responses))
        breakdown-positions (map (fn [x] {:position (Math/floor (* x (count responses))), :percentage x}) breakdowns)]
    (map (fn [x] [(int (* 100 (:percentage x)))
                  (nth response-times (dec (:position x)))]) breakdown-positions)))

(defn- count-statuses [statuses range]
  (count (filter (fn [status] (between status range)) statuses)))

(defn summarize [responses]
  (let [total-time (sum-field :elapsed responses)
        http-responses (map :response responses)
        statuses (map :status http-responses)
        headers (map :headers http-responses)
        average-time (int (/ total-time (count http-responses)))]

    {:total-time (format-time total-time)
     :average-time (format-time average-time)
     :successes (count-statuses statuses [200 299])
     :redirects (count-statuses statuses [300 399])
     :errors (count-statuses statuses [400 599])
     :breakdowns (break-it-down responses [0.5 0.6 0.7 0.8 0.9 0.95 0.99 1.0])
     :qps (float (/ 1000 average-time))}))

(defn clobber
  ([url replacements num-requests]
   (clobber url replacements num-requests {}))

  ([url replacements num-requests opts]
   (summarize (make-requests url replacements num-requests))))


