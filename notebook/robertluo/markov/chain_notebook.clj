;; # The simplest Markov chain
;;
;; A show room for `robertluo.markov.chain`: finitely many states, discrete time, and
;; transition probabilities that do not change over time.

(ns robertluo.markov.chain-notebook
  (:require [malli.core :as m]
            [malli.error :as me]
            [robertluo.markov.chain :as chain]
            [robertluo.markov.instrument :as instrument]
            [scicloj.kindly.v4.kind :as kind]))

;; Everything below runs against guarded functions, as the tests do.

(instrument/instrument!)

;; ## A chain
;;
;; A chain maps each state to its row, and a row maps the next states to their
;; probabilities. Tomorrow's weather, knowing only today's:

(def weather
  {:sunny  {:sunny 0.7 :cloudy 0.2 :rainy 0.1}
   :cloudy {:sunny 0.3 :cloudy 0.4 :rainy 0.3}
   :rainy  {:sunny 0.2 :cloudy 0.4 :rainy 0.4}})

(m/validate chain/Chain weather)

(defn- diagram [chain]
  (kind/mermaid
   (apply str "stateDiagram-v2\n"
          (for [[from row] chain
                [to p] row
                :when (pos? p)]
            (str "  " (name from) " --> " (name to) ": " p "\n")))))

(diagram weather)

;; Each state keeps one colour in every chart below.

(def ^:private by-state
  {:type :nominal
   :sort ["sunny" "cloudy" "rainy"]
   :scale {:domain ["sunny" "cloudy" "rainy"]
           :range ["#f2b705" "#9aa5b1" "#2f6db5"]}})

;; The same chain as a transition matrix, one row per state today:

(defn- matrix [chain]
  (let [states (keys chain)]
    (kind/table
     {:column-names (cons "today \\ tomorrow" (map name states))
      :row-vectors (for [from states]
                     (cons (name from) (map #(get-in chain [from %] 0.0) states)))})))

(matrix weather)

;; ## One step
;;
;; `next-state` lays a row out as consecutive intervals of [0, 1), one per next state,
;; and a uniform draw picks the interval it falls in. The ticks are the draws below.

(def some-draws [0.05 0.5 0.75 0.95])

(defn- intervals [row]
  (->> (filter (comp pos? val) row)
       (reductions (fn [{:keys [to]} [s p]] {:state (name s) :from to :to (+ to p)})
                   {:to 0.0})
       rest))

(kind/vega-lite
 {:width 500 :height 60
  :layer [{:data {:values (intervals (:sunny weather))}
           :mark :bar
           :encoding {:x {:field :from :type :quantitative :title "draw"
                          :scale {:domain [0 1]}}
                      :x2 {:field :to}
                      :color (assoc by-state :field :state)}}
          {:data {:values (map #(hash-map :u %) some-draws)}
           :mark {:type :tick :color "black" :thickness 2}
           :encoding {:x {:field :u :type :quantitative}}}]})

(for [u some-draws]
  [u '-> (chain/next-state (:sunny weather) u)])

;; ## A walk
;;
;; Randomness is an argument: a walk is told its draws. A seeded generator makes the
;; draws, so this page renders the same every time.

(defn- draws [seed n]
  (let [r (java.util.Random. seed)]
    (vec (repeatedly n #(.nextDouble r)))))

(def month (chain/walk weather :sunny (draws 42 30)))

(kind/vega-lite
 {:width 600 :height 30
  :data {:values (map-indexed #(hash-map :day %1 :weather (name %2)) month)}
  :mark {:type :rect :stroke "white"}
  :encoding {:x {:field :day :type :ordinal}
             :color (assoc by-state :field :weather)}})

;; ## The long run
;;
;; Over many steps, the share of days spent in each state settles, and it settles to
;; the same shares whichever state the walk starts from. Each walk gets draws of its
;; own: two walks on the same draws move together once they meet, and would agree for
;; that reason alone.

(defn- shares [path]
  (let [n (count path)]
    (for [[s k] (frequencies path)]
      {:state (name s) :share (double (/ k n))})))

(kind/vega-lite
 {:width 400 :height 100
  :data {:values (for [[start seed] {:sunny 7 :rainy 8}
                       share (shares (chain/walk weather start (draws seed 10000)))]
                   (assoc share :start (str "from " (name start))))}
  :mark :bar
  :encoding {:y {:field :start :type :nominal :title nil}
             :x {:field :share :type :quantitative :stack :normalize}
             :order {:field :order}
             :color (assoc by-state :field :state)}
  :transform [{:calculate "indexof(['sunny', 'cloudy', 'rainy'], datum.state)"
               :as :order}]})

;; Those shares are the chain's stationary distribution. `robertluo.markov.chain` does not
;; compute it yet; it is a candidate for the next lab notebook.

;; ## Guarded
;;
;; A call outside the schemas is refused, for instance a row that does not sum to one:

(try
  (chain/next-state {:sunny 0.5} 0.3)
  (catch clojure.lang.ExceptionInfo e
    (let [{:keys [input args]} (:data (ex-data e))]
      (me/humanize (m/explain input args)))))
