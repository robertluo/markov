;; # Find the state, don't widen the window
;;
;; A lab. The Markov property says the next step depends on the present state alone. When
;; a process seems to remember more, a common fix is a k-th order chain: take the last k
;; observations as the state. That is still a first-order chain, only with a guessed
;; state, and the guess can be hopeless. The **even process** has an exact description
;; with two states, yet no finite window predicts it as well as those two states do.

(ns robertluo.markov.even-notebook
  (:require [robertluo.markov.chain :as chain]
            [robertluo.markov.estimate :as estimate]
            [robertluo.markov.instrument :as instrument]
            [robertluo.markov.learner :as learner]
            [robertluo.markov.view :as view]
            [scicloj.kindly.v4.kind :as kind]))

;; ## The even process
;;
;; A machine with hidden states that emits a symbol on each transition. From A it emits
;; 0 and stays, or emits 1 and goes to B, each half the time; from B it always emits 1
;; and goes back to A. So 1s come in pairs: every run of 1s between two 0s has even
;; length. Where the machine goes is fixed by where it is and what it emits (it is
;; *unifilar*), so its state can be read off the symbols once they reveal it.

(def Probability [:and number? [:>= 0] [:<= 1]])

(defn- total-one? [machine]
  (every? (fn [row] (== 1 (transduce (map (comp second val)) + 0 row))) (vals machine)))

(def Machine
  "Each hidden state's row: every symbol it may emit, with the state it goes to and the
   probability. Symbols are keywords; each row's probabilities sum to 1."
  [:and
   [:map-of chain/State [:map-of chain/State [:tuple chain/State Probability]]]
   [:fn {:error/message "each row must sum to 1"} total-one?]])

(def even
  {:A {:0 [:A 1/2] :1 [:B 1/2]}
   :B {:1 [:A 1]}})

(defn- machine-diagram [machine]
  (kind/mermaid
   (apply str "stateDiagram-v2\n"
          (for [[from row] machine
                [sym [to p]] row]
            (str "  " (name from) " --> " (name to) ": " (name sym) " | " p "\n")))))

(machine-diagram even)

;; Emitting is like walking a chain, one draw per symbol, built lazily from an unbounded
;; source of draws:

(defn emit
  "A transducer from draws to the symbols `machine` emits from `start`. Each use keeps
   its own hidden state."
  {:malli/schema [:=> [:cat Machine chain/State] ifn?]}
  [machine start]
  (fn [rf]
    (let [state (volatile! start)]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc u]
         (let [row (get machine @state)
               sym (chain/next-state (update-vals row (comp double second)) u)]
           (vreset! state (first (get row sym)))
           (rf acc sym)))))))

^:kindly/hide-code (kind/hidden (instrument/instrument!))

(defn- symbols [seed n]
  (into [] (comp (emit even :A) (take n)) (view/draw-stream seed)))

(def sample (symbols 1 10000))

(apply str (map name (take 80 sample)))

;; Every run of 1s is even. (The last run is cut off by the sample's end, so it is left
;; out.)

(defn- run-lengths [syms]
  (->> syms
       (into [] (comp (partition-by identity) (filter #(= :1 (first %))) (map count)))
       butlast
       frequencies
       (sort-by key)))

(kind/table
 {:column-names ["run of 1s" "how many"]
  :row-vectors (run-lengths sample)})

;; ## Measuring prediction
;;
;; A predictor gives a probability to each next symbol before it comes. Its cost on the
;; symbol that does come is -log₂ p bits: 0 for a certain and right prediction, 1 for a
;; coin toss, unbounded for a symbol it ruled out. Averaged over a long sequence, it is
;; bits per symbol, and lower is better.
;;
;; No predictor can do better than the process's own uncertainty. The machine spends ⅔
;; of its time in A, where the next symbol is a fair coin (1 bit), and ⅓ in B, where it
;; is certain (0 bits). The floor is ⅔ bit per symbol, and a predictor that knows the
;; hidden state reaches it.

(def floor 2/3)

(defn- bits [p] (- (/ (Math/log p) (Math/log 2))))

(defn- entropy [dist]
  (transduce (comp (map val) (filter pos?) (map #(* % (bits %)))) + 0.0 dist))

;; ## Windows as states
;;
;; A k-th order model takes the last k symbols as its state. Turn the symbols into a walk
;; of windows, and it is exactly a first-order chain on windows: the counting learner
;; learns it unchanged.

(defn windows
  "A transducer from symbols to the windows of the last `k` of them, each as a keyword
   (:0110), from the k-th symbol on."
  {:malli/schema [:=> [:cat pos-int?] ifn?]}
  [k]
  (fn [rf]
    (let [window (volatile! [])]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc sym]
         (let [w (vswap! window #(let [w (conj % sym)] (if (> (count w) k) (subvec w 1) w)))]
           (if (= k (count w))
             (rf acc (keyword (apply str (map name w))))
             acc)))))))

^:kindly/hide-code (kind/hidden (instrument/instrument!))

(def ^:private window-walk (into [] (windows 2) (take 12 sample)))

window-walk

(estimate/estimate (learner/learn estimate/counting {} (into [] (windows 2) sample)))

;; A window's next window is the window shifted by one symbol, so each row of that chain
;; is a prediction of the next symbol given the last k. Already at k = 2, the window
;; :11 is unsure: it ends a pair of 1s, or sits inside a longer run.

;; ## The best any window can do
;;
;; What a window says about the next symbol can be computed exactly from the machine:
;; start from the long-run belief about the hidden state, {A ⅔, B ⅓}, and update it on
;; each symbol of the window. A window with a 0 in it pins the state down: after a 0 the
;; machine is in A, and the 1s since then tell it where it is now. Only a window of all 1s
;; leaves it unsure, and such a window grows rarer, but never impossible, as k grows.

(def Belief
  "How likely the machine is to be in each hidden state; states ruled out are left out."
  [:map-of chain/State [:and number? [:> 0] [:<= 1]]])

(def stationary {:A 2/3 :B 1/3})

(defn- prediction [machine belief]
  (reduce-kv (fn [acc s b]
               (reduce-kv (fn [acc sym [_ p]] (update acc sym (fnil + 0) (* b p)))
                          acc (get machine s)))
             {} belief))

(defn- updated [machine belief sym]
  (let [moved (reduce-kv (fn [acc s b]
                           (if-let [[to p] (get-in machine [s sym])]
                             (if (pos? p) (update acc to (fnil + 0) (* b p)) acc)
                             acc))
                         {} belief)
        total (reduce + (vals moved))]
    (update-vals moved #(let [q (/ % total)] (if (integer? q) (long q) q)))))

(defn- window-beliefs
  "Every belief the windows of length k lead to, with how likely such windows are."
  [machine k]
  (nth (iterate (fn [beliefs]
                  (reduce-kv (fn [acc belief p]
                               (reduce-kv (fn [acc sym px]
                                            (if (pos? px)
                                              (update acc (updated machine belief sym)
                                                      (fnil + 0) (* p px))
                                              acc))
                                          acc (prediction machine belief)))
                             {} beliefs))
                {stationary 1})
       k))

(defn- best-window-loss [k]
  (transduce (map (fn [[belief p]] (* p (entropy (prediction even belief)))))
             + 0.0 (window-beliefs even k)))

(def ^:private exact
  (for [k (range 0 21)]
    {:k k :bits (best-window-loss k) :beliefs (count (window-beliefs even k))}))

(kind/table
 {:column-names ["k" "windows" "beliefs they lead to" "best bits per symbol" "above the floor"]
  :row-vectors (for [{:keys [k bits beliefs]} (take-nth 2 exact)]
                 [k (long (Math/pow 2 k)) beliefs (format "%.5f" bits)
                  (format "%.5f" (- bits floor))])})

;; The 2ᵏ windows lead to only a handful of beliefs: the windows are a very redundant
;; way to write the state down. And the best any window can do stays above ⅔ bit for
;; every k: the gap roughly halves every two symbols of window, but a run of 1s can
;; always be longer than the window, and then the window cannot tell where the pair
;; boundaries fall.

;; ## With finite data
;;
;; The limit above assumes a window's predictions are known exactly. Learned from data,
;; each of the 2ᵏ windows needs its own evidence, and wide windows are seen too rarely to
;; learn. Learn each k from 10,000 symbols, test on 10,000 others, predicting each
;; symbol from its window with add-one smoothing:

(def train (symbols 2 10000))
(def test-symbols (symbols 3 10000))

(def ^:private max-k 10)

(defn- window-loss
  "Bits per symbol of the k-window model learned from `counts`, on `syms`, over the same
   positions for every k: from the max-k-th symbol on."
  [k counts syms]
  (let [losses (into []
                     (comp (windows k)
                           chain/transitions
                           (drop (- max-k k -1))
                           (map (fn [[before after]]
                                  (let [row (get counts before {})
                                        n   (reduce + 0 (vals row))]
                                    (bits (/ (+ 1 (get row after 0)) (+ n 2)))))))
                     syms)]
    (/ (reduce + losses) (count losses))))

(def ^:private learned-windows
  (for [k (range 1 (inc max-k))]
    [k (learner/learn estimate/counting {} (eduction (windows k) train))]))

(def ^:private window-results
  (for [[k counts] learned-windows]
    {:k k :bits (window-loss k counts test-symbols)}))

;; ## The right state: a belief
;;
;; Instead of a window, keep the belief about the hidden state, updating it on each
;; symbol. It is the state the Markov property asks for: all of the past that matters
;; for the future, in two numbers. From {A ⅔, B ⅓} it collapses to certainty at the
;; first 0, and stays certain.

(defn track
  "A transducer from symbols to what a predictor that knows `machine` makes of them,
   starting from `belief`: for each symbol, the probability it was given before it came,
   and the belief after it."
  {:malli/schema [:=> [:cat Machine Belief] ifn?]}
  [machine belief]
  (fn [rf]
    (let [current (volatile! belief)]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc sym]
         (let [b @current
               p (get (prediction machine b) sym 0)]
           (rf acc {:symbol sym :p p :belief (vreset! current (updated machine b sym))})))))))

^:kindly/hide-code (kind/hidden (instrument/instrument!))

(kind/table
 {:column-names ["symbol" "probability it was given" "belief after"]
  :row-vectors (for [{:keys [symbol p belief]} (into [] (comp (track even stationary) (take 12))
                                                     test-symbols)]
                 [(name symbol) p (pr-str belief)])})

(def ^:private tracked-bits
  (/ (transduce (comp (track even stationary) (drop max-k) (map (comp bits :p)))
                + 0.0 test-symbols)
     (- (count test-symbols) max-k)))

tracked-bits

;; Windows against the belief, on the same test symbols:

(defn- loss-chart [exact data tracked]
  (kind/vega-lite
   {:width 500 :height 300
    :layer [{:data {:values (for [{:keys [k bits]} exact :when (<= 1 k max-k)]
                              {:k k :bits bits :series "best any k-window can do"})}
             :mark {:type :line :point true}
             :encoding {:x {:field :k :type :quantitative :title "window length k"}
                        :y {:field :bits :type :quantitative :title "bits per symbol"
                            :scale {:zero false}}
                        :color {:field :series :type :nominal :title nil}}}
            {:data {:values (for [{:keys [k bits]} data]
                              {:k k :bits bits :series "k-window learned from 10,000 symbols"})}
             :mark {:type :line :point true}
             :encoding {:x {:field :k :type :quantitative}
                        :y {:field :bits :type :quantitative}
                        :color {:field :series :type :nominal :title nil}}}
            {:data {:values [{:bits tracked :series "2-state belief, on the same symbols"}]}
             :mark {:type :rule :strokeDash [4 4]}
             :encoding {:y {:field :bits :type :quantitative}
                        :color {:field :series :type :nominal :title nil}}}]}))

(loss-chart exact window-results tracked-bits)

;; The learned windows improve at first, then get worse: past a point, each extra symbol
;; of window doubles the contexts to learn and halves the evidence for each. The belief
;; beats every one of them with two states, and sits at the floor (up to the noise of a
;; finite test).

;; ## Given the right state, learning is counting
;;
;; The belief is certain after the first 0, so the tracked symbols give a walk of hidden
;; states. Since the machine is unifilar, each hidden transition is a symbol (A → A is a
;; 0, A → B a 1), and the counting learner, unchanged, learns the machine from that
;; walk:

(def ^:private hidden-walk
  (eduction (comp (track even stationary)
                  (keep (fn [{:keys [belief]}] (when (= 1 (count belief)) (key (first belief))))))
            train))

(estimate/estimate (learner/learn estimate/counting {} hidden-walk))

;; Two states, two numbers to learn, against 2ᵏ contexts for a window that still falls
;; short.

;; ## What is next
;;
;; This page was handed the states: it tracked beliefs with the true machine. Learning
;; the states themselves from symbols alone is the real task. The belief table above
;; hints at how: many windows lead to the same belief, so windows that predict alike can
;; be merged into one state. Finding those classes from the window counts (as the CSSR
;; algorithm does) is for the next notebook.
