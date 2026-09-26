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
