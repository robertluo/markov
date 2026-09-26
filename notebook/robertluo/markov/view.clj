(ns robertluo.markov.view
  "Ways of showing a chain and its walks, shared by the notebooks: kindly values
   (mermaid, tables, vega-lite) built from the data in robertluo.markov.chain.

   A palette gives each state one colour, in order, and every chart that takes one
   colours and sorts its states by it, so a state looks the same across a page."
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [robertluo.markov.chain :as chain]
            [scicloj.kindly.v4.kind :as kind]))

(def Palette
  "Each state with its colour, in the order charts list them."
  [:vector {:min 1} [:tuple chain/State :string]])

(defn draws
  "`n` uniform draws from a generator seeded with `seed`, so a page renders the same
   every time."
  {:malli/schema [:=> [:cat :int nat-int?] [:vector chain/Draw]]}
  [seed n]
  (let [r (java.util.Random. seed)]
    (vec (repeatedly n #(.nextDouble r)))))

(defn draw-stream
  "Unbounded uniform draws for `seed`, as an eduction: reducible and seqable, and caching
   nothing, so holding it holds no draws, and every pass sees the same ones. The i-th draw
   depends on the seed and i alone. Bound it with `(take n)` where it is consumed."
  {:malli/schema [:=> [:cat :int] [:fn seqable?]]}
  [seed]
  (let [base (.nextLong (java.util.SplittableRandom. seed))]
    (eduction (map #(.nextDouble (java.util.SplittableRandom. (+ base %))))
              (range))))

(defn diagram
  "The chain as a state diagram, one arrow per positive transition."
  {:malli/schema [:=> [:cat chain/Chain] any?]}
  [chain]
  (kind/mermaid
   (apply str "stateDiagram-v2\n"
          (for [[from row] chain
                [to p] row
                :when (pos? p)]
            (str "  " (name from) " --> " (name to) ": " p "\n")))))

(defn matrix
  "The chain as a transition matrix, one row per state today."
  {:malli/schema [:=> [:cat chain/Chain] :map]}
  [chain]
  (let [states (keys chain)]
    (kind/table
     {:column-names (cons "today \\ tomorrow" (map name states))
      :row-vectors (for [from states]
                     (cons (name from) (map #(get-in chain [from %] 0.0) states)))})))

(defn- state-names [palette]
  (mapv (comp name first) palette))

(defn- color
  "A vega-lite colour encoding of `field` by `palette`."
  [palette field]
  {:field field
   :type :nominal
   :sort (state-names palette)
   :scale {:domain (state-names palette)
           :range (mapv second palette)}})

(defn- intervals [row]
  (->> (filter (comp pos? val) row)
       (reductions (fn [{:keys [to]} [s p]] {:state (name s) :from to :to (+ to p)})
                   {:to 0.0})
       rest))

(defn row-intervals
  "A row laid out as consecutive intervals of [0, 1), the way next-state reads it, with a
   tick at each of `us`."
  {:malli/schema [:=> [:cat Palette chain/Row [:sequential chain/Draw]] :map]}
  [palette row us]
  (kind/vega-lite
   {:width 500 :height 60
    :layer [{:data {:values (intervals row)}
             :mark :bar
             :encoding {:x {:field :from :type :quantitative :title "draw"
                            :scale {:domain [0 1]}}
                        :x2 {:field :to}
                        :color (color palette :state)}}
            {:data {:values (map #(hash-map :u %) us)}
             :mark {:type :tick :color "black" :thickness 2}
             :encoding {:x {:field :u :type :quantitative}}}]}))

(defn timeline
  "A walk as a strip, one coloured cell per step."
  {:malli/schema [:=> [:cat Palette [:sequential chain/State]] :map]}
  [palette path]
  (kind/vega-lite
   {:width 600 :height 30
    :data {:values (map-indexed #(hash-map :step %1 :state (name %2)) path)}
    :mark {:type :rect :stroke "white"}
    :encoding {:x {:field :step :type :ordinal}
               :color (color palette :state)}}))

(defn- shares [path]
  (let [n (count path)]
    (for [[s k] (frequencies path)]
      {:state (name s) :share (double (/ k n))})))

(defn share-bars
  "The share of steps each walk spends in each state, one bar per labelled walk, in
   order: `walks` pairs each label with its path."
  {:malli/schema [:=> [:cat Palette
                       [:sequential [:tuple :string [:sequential {:min 1} chain/State]]]]
                  :map]}
  [palette walks]
  (kind/vega-lite
   {:width 400 :height (* 50 (count walks))
    :data {:values (for [[label path] walks
                         share (shares path)]
                     (assoc share :walk label))}
    :mark :bar
    :encoding {:y {:field :walk :type :nominal :title nil}
               :x {:field :share :type :quantitative :stack :normalize}
               :order {:field :order}
               :color (color palette :state)}
    :transform [{:calculate (str "indexof([" (str/join ", " (map pr-str (state-names palette)))
                                 "], datum.state)")
                 :as :order}]}))

(defn refusal
  "Why the guard refuses the call `f`, humanized; nil when it does not. Any other
   exception is rethrown."
  {:malli/schema [:=> [:cat fn?] any?]}
  [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (let [{:keys [input args output value]} (:data (ex-data e))]
        (cond input  (me/humanize (m/explain input args))
              output (me/humanize (m/explain output value))
              :else  (throw e))))))

(defn heatmaps
  "Transition matrices side by side, one per labelled chain, on one colour scale and with
   the states in the order of the first chain, so they compare at a glance."
  {:malli/schema [:=> [:cat [:sequential {:min 1} [:tuple :string chain/Chain]]] :map]}
  [chains]
  (let [order (mapv name (keys (second (first chains))))]
    (kind/vega-lite
     {:hconcat
      (for [[label chain] chains]
        {:title label
         :width 180 :height 180
         :data {:values (for [from (keys chain)
                              to (keys chain)]
                          {:from (name from) :to (name to)
                           :p (get-in chain [from to] 0.0)})}
         :encoding {:x {:field :to :type :nominal :sort order :title "tomorrow"}
                    :y {:field :from :type :nominal :sort order :title "today"}}
         :layer [{:mark :rect
                  :encoding {:color {:field :p :type :quantitative
                                     :scale {:domain [0 1] :scheme "blues"}
                                     :legend nil}}}
                 {:mark {:type :text :format ".2f"}
                  :encoding {:text {:field :p :type :quantitative :format ".2f"}
                             :color {:condition {:test "datum.p > 0.5" :value "white"}
                                     :value "black"}}}]})})))

(defn error-curve
  "Error against the number of observations, on a log scale, one line per labelled
   series of [n error] points."
  {:malli/schema [:=> [:cat [:sequential [:tuple :string
                                          [:sequential [:tuple pos-int? number?]]]]]
                  :map]}
  [series]
  (kind/vega-lite
   {:width 500 :height 250
    :data {:values (for [[label points] series
                         [n error] points]
                     {:series label :n n :error error})}
    :mark {:type :line :point true}
    :encoding {:x {:field :n :type :quantitative :scale {:type :log}
                   :title "observed transitions"}
               :y {:field :error :type :quantitative :title "error"}
               :color {:field :series :type :nominal :title nil}}}))
