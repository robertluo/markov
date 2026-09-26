;; # Learning the states: Baum–Welch
;;
;; A lab, and the other answer to the question the even notebook left. CSSR built the
;; states from the data, testing which pasts predict alike. Here the number of hidden
;; states is **assumed**, and their probabilities are fitted to make the data as likely
;; as possible, by expectation–maximisation (Baum–Welch).

(ns robertluo.markov.hmm-notebook
  (:require [robertluo.markov.chain :as chain]
            [robertluo.markov.estimate :as estimate]
            [robertluo.markov.hidden :as hidden]
            [robertluo.markov.instrument :as instrument]
            [robertluo.markov.learner :as learner]
            [robertluo.markov.view :as view]
            [scicloj.kindly.v4.kind :as kind]
            [malli.core :as m]))

;; ## The data
;;
;; The same symbols as the CSSR page: 10,000 to learn from, 10,000 to test on, from the
;; even process. The learner sees only these.

(def ^:private even
  {:A {:0 {:A 1/2} :1 {:B 1/2}}
   :B {:1 {:A 1}}})

(defn- symbols [seed n]
  (into [] (comp (partition-all 2) (hidden/emit even :A) (take n)) (view/draw-stream seed)))

(def train (symbols 2 10000))
(def test-symbols (symbols 3 10000))

;; ## One step of EM
;;
;; Given a machine, how likely was each hidden move at each moment? The **forward** pass
;; is `hidden/track`: a belief updated symbol by symbol. The **backward** pass revises
;; each belief with the symbols that came after it. Together they give, for every step,
;; the probability of each hidden move: **expected counts**, fractions of a count. The
;; **maximisation** step normalises them per state, exactly as counting's readout does,
;; and that is the next machine. Each step makes the data at least as likely as before.
;;
;; Unlike the learners so far, this is not one fold. The events (the expected counts)
;; depend on the current machine, so each step reads all the data again, and the backward
;; pass needs the forward pass's beliefs for the whole sequence: EM holds its data. The
;; work is done on primitive arrays, with index loops over time; that is the price of
;; holding it.

(defn- indexed [machine]
  (let [states (vec (sort (keys machine)))
        syms   (vec (sort (into #{} (mapcat keys) (vals machine))))]
    {:states states :syms syms :n (count states) :m (count syms)
     :state-index (zipmap states (range)) :sym-index (zipmap syms (range))}))

(defn- ->array
  "The machine's moves as one flat array: T[(i·m + x)·n + j] = P(emit x, go to j | i)."
  [machine {:keys [n m state-index sym-index]}]
  (let [t (double-array (* n m n))]
    (doseq [[s row] machine [x nexts] row [to p] nexts]
      (aset t (+ (* (+ (* (state-index s) m) (sym-index x)) n) (state-index to)) (double p)))
    t))

(defn- ->machine [^doubles t {:keys [states syms n m]}]
  (into {}
        (for [i (range n)]
          [(states i)
           (into {}
                 (keep (fn [x]
                         (let [nexts (into {}
                                           (keep (fn [j]
                                                   (let [p (aget t (+ (* (+ (* i m) x) n) j))]
                                                     (when (pos? p) [(states j) p]))))
                                           (range n))]
                           (when (seq nexts) [(syms x) nexts]))))
                 (range m))])))

(defn- e-step
  "Forward–backward over `obs`, symbol indices: the expected count of every move, and the
   log-likelihood of the data. Beliefs are kept normalised (scaled), so that ten thousand
   steps do not underflow."
  [^doubles t ^longs obs n m]
  (let [len   (alength obs)
        n     (long n)
        m     (long m)
        alpha (double-array (* (inc len) n))
        beta  (double-array (* (inc len) n))
        scale (double-array len)
        xi    (double-array (* n m n))]
    (dotimes [j n] (aset alpha j (/ 1.0 n)))
    ;; forward: alpha(t+1) = alpha(t)·T(x_t) / c_t
    (dotimes [k len]
      (let [x (aget obs k)]
        (dotimes [j n]
          (let [s (double (loop [i 0 s 0.0]
                            (if (< i n)
                              (recur (inc i) (+ s (* (aget alpha (+ (* k n) i))
                                                     (aget t (+ (* (+ (* i m) x) n) j)))))
                              s)))]
            (aset alpha (+ (* (inc k) n) j) s)))
        (let [c (double (loop [j 0 c 0.0] (if (< j n) (recur (inc j) (+ c (aget alpha (+ (* (inc k) n) j)))) c)))]
          (aset scale k c)
          (dotimes [j n] (aset alpha (+ (* (inc k) n) j) (/ (aget alpha (+ (* (inc k) n) j)) c))))))
    ;; backward, and the expected counts: xi(i, x_t, j) += alpha_t(i)·T·beta_t+1(j) / c_t
    (dotimes [j n] (aset beta (+ (* len n) j) 1.0))
    (loop [k (dec len)]
      (when (>= k 0)
        (let [x (aget obs k) c (aget scale k)]
          (dotimes [i n]
            (let [b (double (loop [j 0 b 0.0]
                              (if (< j n)
                                (let [w (/ (* (aget t (+ (* (+ (* i m) x) n) j)) (aget beta (+ (* (inc k) n) j))) c)
                                      idx (+ (* (+ (* i m) x) n) j)]
                                  (aset xi idx (+ (aget xi idx) (* (aget alpha (+ (* k n) i)) w)))
                                  (recur (inc j) (+ b w)))
                                b)))]
              (aset beta (+ (* k n) i) b))))
        (recur (dec k))))
    {:xi xi
     :log-likelihood (loop [k 0 ll 0.0] (if (< k len) (recur (inc k) (+ ll (Math/log (aget scale k)))) ll))}))

(defn- m-step
  "Each state's expected counts over their total; a state never visited keeps its row."
  [^doubles xi ^doubles t n m]
  (let [n (long n) m (long m) t' (double-array (alength t))]
    (dotimes [i n]
      (let [base (* i m n)
            total (loop [k 0 s 0.0] (if (< k (* m n)) (recur (inc k) (+ s (aget xi (+ base k)))) s))]
        (dotimes [k (* m n)]
          (aset t' (+ base k) (if (pos? total) (/ (aget xi (+ base k)) total) (aget t (+ base k)))))))
    t'))

(def Fit
  [:map
   [:machine hidden/Machine]
   [:log-likelihood :double]])

(defn baum-welch
  "The fits EM makes of `symbols` from `machine`: an unbounded lazy sequence, each fit a
   step on from the one before, with the log-likelihood of the data under the machine it
   was computed from. The symbols are held whole, as EM re-reads them every step, and
   checked only for being a vector, since checking every symbol every step costs more
   than the step."
  {:malli/schema [:=> [:cat hidden/Machine [:fn vector?]] [:fn seqable?]]}
  [machine symbols]
  (let [{:keys [n m sym-index] :as ix} (indexed machine)
        obs (long-array (map sym-index symbols))]
    (->> {:t (->array machine ix)}
         (iterate (fn [{:keys [t]}]
                    (let [{:keys [xi log-likelihood]} (e-step t obs n m)]
                      {:t (m-step xi t n m) :log-likelihood log-likelihood})))
         rest
         (map (fn [{:keys [t log-likelihood]}]
                {:machine (->machine t ix) :log-likelihood log-likelihood})))))

^:kindly/hide-code (kind/hidden (instrument/instrument!))

;; ## Where it starts
;;
;; EM needs a machine to start from. Assume n hidden states, and let every state emit
;; every symbol and go anywhere, with probabilities drawn at random: the draws are the
;; argument, so each restart is a seed.

(defn- random-machine [n seed]
  (let [states (mapv #(keyword (str "h" %)) (range n))
        edges  (for [x [:0 :1] to states] [x to])
        draws  (into [] (take (* n (count edges))) (view/draw-stream seed))]
    (zipmap states
            (for [ws (partition (count edges) draws)
                  :let [total (reduce + ws)]]
              (reduce (fn [row [[x to] w]] (assoc-in row [x to] (/ w total)))
                      {} (map vector edges ws))))))

(defn- bits-per-symbol [log-likelihood n]
  (/ (- log-likelihood) n (Math/log 2)))

(defn- converge
  "Fits until the log-likelihood gains less than 1e-7 per symbol, or 300 steps."
  [start]
  (let [n (count train)]
    (reduce (fn [fits fit]
              (let [prev (peek fits) fits (conj fits fit)]
                (if (and prev (< (- (:log-likelihood fit) (:log-likelihood prev)) (* 1e-7 n)))
                  (reduced fits)
                  fits)))
            []
            (take 300 (baum-welch start train)))))

(defn- bits [p] (- (/ (Math/log p) (Math/log 2))))

(defn- test-loss [machine]
  (let [start (zipmap (keys machine) (repeat (/ 1.0 (count machine))))]
    (/ (transduce (comp (hidden/track machine start) (drop 10) (map (comp bits :p)))
                  + 0.0 test-symbols)
       (- (count test-symbols) 10))))

(defn- tidy
  "The machine with moves below `eps` dropped and rows renormalised: EM only approaches
   zero, so an impossible move shows as a tiny one."
  [machine eps]
  (update-vals machine
               (fn [row]
                 (let [kept  (into {} (keep (fn [[x nexts]]
                                              (let [ns (into {} (filter #(>= (val %) eps)) nexts)]
                                                (when (seq ns) [x ns]))))
                                   row)
                       total (transduce (mapcat vals) + 0.0 (vals kept))]
                   (update-vals kept #(update-vals % (fn [p] (/ p total))))))))

;; ## Ten restarts with two states
;;
;; Each line is one restart: the data's cost in bits per symbol under the machine, step
;; by step. The floor, ⅔, is the best any machine can do on average.

(def ^:private restarts
  (for [seed (range 1 11)]
    (let [fits (converge (random-machine 2 (* 7919 seed)))]
      {:seed seed :fits fits :machine (:machine (peek fits))})))

(defn- em-chart [restarts]
  (kind/vega-lite
   {:width 550 :height 300
    :layer [{:data {:values (for [{:keys [seed fits]} restarts
                                  [i {:keys [log-likelihood]}] (map-indexed vector fits)]
                              {:restart (str "seed " seed) :step (inc i)
                               :bits (bits-per-symbol log-likelihood (count train))})}
             :mark :line
             :encoding {:x {:field :step :type :quantitative :title "EM step"
                            :scale {:type :log}}
                        :y {:field :bits :type :quantitative :title "bits per symbol (training)"
                            :scale {:zero false}}
                        :color {:field :restart :type :nominal :legend nil}}}
            {:data {:values [{:bits (/ 2.0 3)}]}
             :mark {:type :rule :strokeDash [4 4]}
             :encoding {:y {:field :bits :type :quantitative}}}]}))

(em-chart restarts)

(defn- restart-row [{:keys [seed fits machine]}]
  (let [t (tidy machine 0.01)]
    [seed (count fits)
     (format "%.4f" (bits-per-symbol (:log-likelihood (peek fits)) (count train)))
     (format "%.4f" (test-loss machine))
     (m/validate hidden/Unifilar t)]))

(kind/table
 {:column-names ["seed" "EM steps" "bits per symbol, training" "bits per symbol, test"
                 "unifilar (moves under 1% dropped)"]
  :row-vectors (map restart-row restarts)})

;; Every restart found the even process: the same cost, and the same machine up to the
;; names of its states. EM can stop at a local optimum, which is why it is restarted;
;; with two states and two symbols, this data gave it none. That is this problem, not a
;; guarantee.
;;
;; One of the fits, with moves under 1% dropped:

(defn- machine-diagram [machine]
  (kind/mermaid
   (apply str "stateDiagram-v2\n"
          (for [[from row] machine
                [sym nexts] row
                [to p] nexts]
            (str "  " (name from) " --> " (name to) ": " (name sym) " | "
                 (format "%.2f" (double p)) "\n")))))

(machine-diagram (tidy (:machine (apply min-key #(test-loss (:machine %)) restarts)) 0.01))

;; ## More, or fewer, states than there are
;;
;; With one state, the machine can only learn how often each symbol comes: about 0.918
;; bits (a little less on these symbols). With three, it has a state to spare, and
;; predicts no better, since there is nothing more to learn.

(def ^:private other-sizes
  (for [n [1 3] seed (range 1 4)]
    (let [fits (converge (random-machine n (* 104729 seed)))]
      {:n n :seed seed :fits fits :machine (:machine (peek fits))})))

(kind/table
 {:column-names ["states" "seed" "EM steps" "bits per symbol, test"]
  :row-vectors (for [{:keys [n seed fits machine]} other-sizes]
                 [n seed (count fits) (format "%.4f" (test-loss machine))])})

(let [{:keys [machine]} (last other-sizes)]
  (machine-diagram (tidy machine 0.01)))

;; The spare state is spent on splitting the coin-tossing state in two: a 0 from either
;; copy goes to either copy, at random. The fit predicts as well, but it is not unifilar,
;; so a belief over its states never becomes certain: its states are a description of the
;; process, no longer the states the process is in.

;; ## Side by side
;;
;; On the same test symbols:

(defn- window-loss
  "Bits per symbol of the 8-symbol window model learned by counting, with add-one
   smoothing, as in the even notebook."
  [k]
  (let [counts (learner/learn estimate/counting {} (eduction (chain/windows k) train))
        losses (into [] (comp (chain/windows k)
                              chain/transitions
                              (drop 1)
                              (map (fn [[before after]]
                                     (let [row (get counts before {})
                                           n   (reduce + 0 (vals row))]
                                       (bits (/ (+ 1 (get row after 0)) (+ n 2)))))))
                     test-symbols)]
    (/ (reduce + losses) (count losses))))

(kind/table
 {:column-names ["predictor" "states" "bits per symbol, test"]
  :row-vectors [["the true machine" 2 (format "%.4f" (test-loss even))]
                ["Baum–Welch, best of 10 restarts" 2
                 (format "%.4f" (apply min (map (comp test-loss :machine) restarts)))]
                ["CSSR (see its page), same symbols" 2 "0.6647"]
                ["8-symbol window, counted" 256 (format "%.4f" (window-loss 8))]
                ["floor (entropy rate)" "" "0.6667"]]})

;; ## What it took
;;
;; - Baum–Welch needs the number of states, and a start. Given the right number, it fits
;;   probabilities that predict as well as the true machine; given too many, it predicts no
;;   better, and its states are no longer the process's own.
;; - It is not a fold: each step re-reads all the data, and holds it for the backward
;;   pass. CSSR gathered its evidence in one fold and did its work in the readout.
;; - EM only approaches zero: a move the process never makes shows as a tiny probability,
;;   and whether the fit is unifilar, whether its states are the causal states, has to be
;;   judged after the fact. CSSR's states are unifilar by construction.
