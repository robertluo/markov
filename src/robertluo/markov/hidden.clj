(ns robertluo.markov.hidden
  "Chains with hidden states that emit symbols. A machine moves between hidden states and
   emits a symbol on each move; only the symbols are observed. Its state is not seen, but
   a belief about it, how likely each hidden state is given the symbols so far, can be
   kept and updated symbol by symbol, and that belief is a state in the Markov sense:
   everything in the past that matters for the next symbol.

   A machine is unifilar when its next state is fixed by its state and the symbol
   emitted. Then the belief, once the symbols pin the state down, stays certain."
  (:require [robertluo.markov.chain :as chain]))

(def Probability [:and number? [:>= 0] [:<= 1]])

(defn- rows-sum-to-one? [machine]
  (every? (fn [row]
            (< (abs (- 1 (transduce (mapcat vals) + 0 (vals row)))) chain/tolerance))
          (vals machine)))

(defn- closed? [machine]
  (every? #(contains? machine %) (mapcat #(mapcat keys (vals %)) (vals machine))))

(def Machine
  "Each hidden state's row: for every symbol it may emit, the states it may go to, with
   the probability of emitting that symbol and going there. Symbols are keywords. Each
   row sums to 1, and every state a row goes to is a state of the machine."
  [:and
   [:map-of {:min 1} chain/State [:map-of chain/State [:map-of chain/State Probability]]]
   [:fn {:error/message "each row must sum to 1"} rows-sum-to-one?]
   [:fn {:error/message "every next state must be a state of the machine"} closed?]])

(defn- unifilar? [machine]
  (every? (fn [row] (every? #(<= (count (filter (comp pos? val) %)) 1) (vals row)))
          (vals machine)))

(def Unifilar
  "A machine whose state and emitted symbol fix its next state."
  [:and Machine [:fn {:error/message "a state and a symbol must fix the next state"}
                 unifilar?]])

(def Belief
  "How likely the machine is to be in each hidden state; states ruled out are left out."
  [:map-of chain/State [:and number? [:> 0] [:<= 1]]])

(defn emit
  "A transducer from pairs of draws to the symbols `machine` emits from `start`: one draw
   for the symbol, one for the state it goes to. Each use keeps its own hidden state."
  {:malli/schema [:=> [:cat Machine chain/State] ifn?]}
  [machine start]
  (fn [rf]
    (let [state (volatile! start)]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc [u1 u2]]
         (let [row   (get machine @state)
               sym   (chain/next-state (update-vals row #(double (reduce + 0 (vals %)))) u1)
               nexts (get row sym)
               total (reduce + 0 (vals nexts))]
           (vreset! state (chain/next-state (update-vals nexts #(double (/ % total))) u2))
           (rf acc sym)))))))

(defn predict
  "How likely each symbol is to come next, for a machine believed to be as `belief` says."
  {:malli/schema [:=> [:cat Machine Belief] [:map-of chain/State number?]]}
  [machine belief]
  (reduce-kv (fn [acc s b]
               (reduce-kv (fn [acc sym nexts]
                            (update acc sym (fnil + 0) (* b (reduce + 0 (vals nexts)))))
                          acc (get machine s)))
             {} belief))

(defn- tidy
  "A probability as a long when it is whole, so exact beliefs read {:a 1}, not {:a 1N}."
  [q]
  (if (integer? q) (long q) q))

(defn observe
  "The belief after `sym` is observed, from `belief` before it. A symbol the belief rules
   out leaves nothing to go on, and the belief is kept as it was."
  {:malli/schema [:=> [:cat Machine Belief chain/State] Belief]}
  [machine belief sym]
  (let [moved (reduce-kv (fn [acc s b]
                           (reduce-kv (fn [acc to p]
                                        (if (pos? p) (update acc to (fnil + 0) (* b p)) acc))
                                      acc (get-in machine [s sym])))
                         {} belief)
        total (reduce + 0 (vals moved))]
    (if (pos? total)
      (into {} (keep (fn [[s m]] (let [q (/ m total)] (when (pos? q) [s (tidy q)])))) moved)
      belief)))

(defn track
  "A transducer from symbols to what a predictor that knows `machine` makes of them,
   starting from `belief`: for each symbol, the probability it was given before it came
   (`:p`), and the belief after it."
  {:malli/schema [:=> [:cat Machine Belief] ifn?]}
  [machine belief]
  (fn [rf]
    (let [current (volatile! belief)]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc sym]
         (let [b @current]
           (rf acc {:symbol sym
                    :p      (get (predict machine b) sym 0)
                    :belief (vreset! current (observe machine b sym))})))))))
