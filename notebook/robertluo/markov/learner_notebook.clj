;; # Beyond counting: learning over time
;;
;; *Part 3 of 6 · previous: [learning a chain](robertluo.markov.estimate_notebook.html)
;; · next: [hidden states](robertluo.markov.even_notebook.html)*
;;
;; A show room for `robertluo.markov.learner`. Counting has a shape: observations become
;; **events**; the model keeps a **statistic** of them, starting from an **empty** one (or
;; a prior); each event is folded in by a **step**; two statistics learned apart
;; **combine** into one; and a **readout** turns a statistic into a model. A learner is
;; those parts as one value; what is known stays plain data, apart from it.
;;
;; `robertluo.markov.estimate/counting` is one learner. This page shows two others, both
;; about time, to see which parts change and which stay: a chain that drifts, learned by
;; counts that fade (`robertluo.markov.fade`); and a chain in continuous time, learned
;; from how long each state lasts (`robertluo.markov.timed`). A table at the end lines
;; the three up.
;;
;; ## Background
;;
;; **Why a shape at all.** Counting in the previous page had a useful property: learn
;; January, then February, and you know exactly what learning both at once would tell you.
;; That is what lets a model learn from data as it arrives, split a big job across
;; machines, or merge what two sites observed. It holds because counts **combine** (by
;; adding) in a way that obeys three simple rules, which the tests check for every
;; learner:
;;
;; - combining with "nothing learned yet" changes nothing;
;; - combining is associative: grouping does not matter, (a + b) + c = a + (b + c);
;; - learning in parts, then combining, knows the same as learning at once.
;;
;; Mathematicians call a set with such an operation a *monoid*; the laws are what make
;; progressive learning safe. This page tries two problems that are not plain counting,
;; to see whether the shape still holds.
;;
;; **Change over time.** A chain whose probabilities never change is *stationary*
;; (time-homogeneous). Real processes drift: seasons, wear, fashion. One remedy is to
;; forget gradually, weighing recent evidence more, as an exponentially weighted moving
;; average does for prices. How fast to forget is a trade-off between following change
;; and averaging away noise, a form of the *bias–variance trade-off*.
;;
;; **Continuous time.** Some processes do not tick once a day. A machine can fail at any
;; moment. A **continuous-time Markov chain** keeps the Markov property in time: how long
;; a state has lasted says nothing about how much longer it will last. That forces the
;; time spent in a state to follow an *exponential* distribution, the one memoryless
;; waiting time, and makes the chain a set of **rates** rather than step probabilities.
;; One more idea from survival analysis appears here: an observation cut off by the end of
;; watching (a machine still running when we stop looking) is **censored**. It still
;; carries information: the state lasted *at least* that long.

(ns robertluo.markov.learner-notebook
  (:require [robertluo.markov.chain :as chain]
            [robertluo.markov.estimate :as estimate]
            [robertluo.markov.fade :as fade]
            [robertluo.markov.learner :as learner]
            [robertluo.markov.timed :as timed]
            [robertluo.markov.view :as view]
            [scicloj.kindly.v4.kind :as kind]))

(defn- approx=
  "Whether two nested maps of numbers agree to within rounding."
  [a b]
  (if (and (map? a) (map? b))
    (and (= (set (keys a)) (set (keys b)))
         (every? #(approx= (get a %) (get b %)) (keys a)))
    (< (abs (- a b)) 1e-9)))

;; # 1. A chain that drifts
;;
;; Counting assumes the chain never changes: a transition seen a year ago weighs as much
;; as one seen yesterday. Weather has seasons. Here the first half of a year is dry and
;; the second wet:

(def dry
  {:sunny  {:sunny 0.7 :cloudy 0.2 :rainy 0.1}
   :cloudy {:sunny 0.3 :cloudy 0.4 :rainy 0.3}
   :rainy  {:sunny 0.2 :cloudy 0.4 :rainy 0.4}})

(def wet
  {:sunny  {:sunny 0.3 :cloudy 0.4 :rainy 0.3}
   :cloudy {:sunny 0.1 :cloudy 0.4 :rainy 0.5}
   :rainy  {:sunny 0.1 :cloudy 0.2 :rainy 0.7}})

(def ^:private palette
  [[:sunny "#f2b705"] [:cloudy "#9aa5b1"] [:rainy "#2f6db5"]])

(def ^:private states (set (keys dry)))

(def year
  (let [first-half (chain/walk dry :sunny (view/draws 21 180))]
    (into first-half (chain/steps wet (peek first-half)) (view/draws 22 180))))

(view/timeline palette year)

;; ## Counts that fade
;;
;; Let every weight fade by a factor λ per day, then add the day's transition. A
;; transition seen n days ago weighs λⁿ, so the model remembers roughly the last
;; 1/(1 − λ) days. The **events** are the same transitions as for counting; the
;; **statistic** is the faded counts and the number of days they span.
;;
;; With λ = 1 nothing fades, and this is counting again:

(= (:weights (learner/learn (fade/fading 1) (fade/faded {}) year))
   (learner/learn estimate/counting {} year))

;; ## Combining what was learned apart
;;
;; Counts combine by adding. Faded counts cannot just add: what was learned first has
;; faded further by the time the second part ends, by λ for each of the second part's
;; days. So **combine** needs to know how long the later part lasted, which is why the
;; statistic keeps `:elapsed`, and the order of the two parts now matters.
;;
;; Learning the year in two halves and combining them knows the same as learning it at
;; once, when the halves are in order (the second starting on the first's last day):

(let [fading (fade/fading 0.97)
      known  (fade/faded (estimate/prior {:states states :alpha 1}))
      [a b]  [(subvec year 0 181) (subvec year 180)]
      whole  (learner/learn fading known year)
      learned-a (learner/learn fading known a)
      learned-b (learner/learn fading (:empty fading) b)]
  {:in-order (approx= whole (learner/combine fading learned-a learned-b))
   :swapped  (approx= whole (learner/combine fading learned-b learned-a))})

;; ## Tracking the seasons
;;
;; The **readout** is the same as for counts: normalise the weights into a chain. Day by
;; day, how far is each model from the chain of the season it is in? Every model starts
;; from the same prior, α = 1.

(defn- trace [series]
  (kind/vega-lite
   {:width 600 :height 250
    :layer [{:data {:values (for [[label points] series
                                  [day error] points]
                              {:series label :day day :error error})}
             :mark :line
             :encoding {:x {:field :day :type :quantitative :title "day"}
                        :y {:field :error :type :quantitative :title "error"}
                        :color {:field :series :type :nominal :title nil}}}
            {:data {:values [{:day 180}]}
             :mark {:type :rule :strokeDash [4 4]}
             :encoding {:x {:field :day :type :quantitative}}}]}))

(defn- tracking [lambda]
  (let [fading (fade/fading lambda)
        known  (fade/faded (estimate/prior {:states states :alpha 1}))]
    (->> (partition 2 1 year)
         (reductions #(learner/learn fading %1 %2) known)
         rest
         (map-indexed (fn [day k]
                        [(inc day) (estimate/distance (if (< day 180) dry wet)
                                                      (learner/readout fading k))])))))

(trace (for [lambda [1 0.97 0.9]]
         [(str "λ = " lambda (when (< lambda 1) (format " (~%d days)" (Math/round (/ 1 (- 1 lambda))))))
          (tracking lambda)]))

;; Plain counts (λ = 1) learn the dry season best, then carry it into the wet one and
;; recover slowly: half their evidence is out of date. Short memory (λ = 0.9) follows
;; the change within weeks but never settles, since it only ever has about ten days to go
;; on. The factor trades how fast a model follows a change against how noisy it is
;; between changes.

;; # 2. A chain in continuous time
;;
;; Weather changes once a day. A machine fails whenever it fails: it runs, degrades,
;; breaks, and is repaired, at any moment. In continuous time, a chain is its **rates**:
;; from each state, how often per hour it moves to each other state. The time spent in a
;; state is exponential, with the total rate out of it; where it goes then is picked in
;; proportion to the rates.

(def machine
  {:up       {:degraded 0.2 :down 0.02}
   :degraded {:up 0.5 :down 0.3}
   :down     {:up 1.0}})

(def ^:private machine-palette
  [[:up "#2e9e44"] [:degraded "#f2b705"] [:down "#c0392b"]])

;; A **sojourn** is a state and how long it lasted. A trajectory is sojourns one after
;; another, made from pairs of draws (`timed/sojourns`): one for how long, one for where
;; next. As before, they are built lazily from an unbounded source of draws, and bounded
;; where consumed: here by a clock, `timed/for-hours`.

(defn- trajectory [seed hours]
  (into [] (comp (partition-all 2) (timed/sojourns machine :up) (timed/for-hours hours))
        (view/draw-stream seed)))

(defn- sojourn-strip [palette sojourns]
  (kind/vega-lite
   {:width 600 :height 40
    :data {:values (map (fn [[s held] start] {:state (name s) :from start :to (+ start held)})
                        sojourns
                        (reductions + 0.0 (map second sojourns)))}
    :mark :bar
    :encoding {:x {:field :from :type :quantitative :title "hour"}
               :x2 {:field :to}
               :color {:field :state :type :nominal
                       :sort (mapv (comp name first) palette)
                       :scale {:domain (mapv (comp name first) palette)
                               :range (mapv second palette)}}}}))

(def week (trajectory 31 168))

(sojourn-strip machine-palette week)

;; ## Time held, and jumps
;;
;; For a rate, the evidence is how many times the machine left a state for another, and
;; how long it had spent in that state in all: rate = jumps / time held. (Just as counting
;; divided counts by their total, this is the maximum-likelihood estimate of a rate: two
;; failures in 100 hours of running estimates 0.02 per hour.) So the **events**
;; are a sojourn with where it went next, and the **statistic** keeps two things: time
;; held per state, and jumps.
;;
;; The last sojourn watched has no next: the watch ended before it did. It is censored:
;; its time still counts, and says the state lasted at least that long; it just has no
;; jump. That event can only be emitted once the trajectory is over. (In the code, events
;; are produced by a *transducer*, a reusable step-by-step transformation of a stream;
;; its *completion* is the moment the stream ends, when it can emit what it held back.)

(learner/learn timed/timed (:empty timed/timed) (take 3 week))

(defn- rate-table [truth & learned]
  (kind/table
   {:column-names (concat ["from" "to" "true rate"] (map first learned))
    :row-vectors (for [[from row] truth
                       [to q] row]
                   (concat [(name from) (name to) q]
                           (map (fn [[_ r]] (format "%.3f" (get-in r [from to] 0.0)))
                                learned)))}))

(defn- learned-rates [sojourns]
  (learner/readout timed/timed (learner/learn timed/timed (:empty timed/timed) sojourns)))

(rate-table machine
            ["a week" (learned-rates week)]
            ["a year" (learned-rates (trajectory 32 8760))])

;; The rare jump (up → down, once in fifty hours of running) happens about once in a
;; week, if at all, so a week's rate for it rests on one event or none; a year pins every
;; rate down.

;; ## Combining time windows
;;
;; Watching is windows of time, and a window's edge can fall in the middle of a sojourn.
;; The first window ends it without a jump; the second begins it and has the jump. Time
;; adds, jumps add, so **combine** is plain addition, in either order, as for counts.

(defn- split-at-hour
  "The sojourns before and after hour `t`, one cut in two where `t` falls inside it."
  [sojourns t]
  (let [starts (reductions + 0.0 (map second sojourns))
        i      (dec (count (take-while #(<= % t) starts)))
        [s held] (nth sojourns i)
        before (- t (nth starts i))]
    [(conj (subvec sojourns 0 i) [s before])
     (into [[s (- held before)]] (subvec sojourns (inc i)))]))

(let [l     timed/timed
      [a b] (split-at-hour week 100.5)
      whole (learner/learn l (:empty l) week)
      learned-a (learner/learn l (:empty l) a)
      learned-b (learner/learn l (:empty l) b)]
  {:in-order (approx= whole (learner/combine l learned-a learned-b))
   :swapped  (approx= whole (learner/combine l learned-b learned-a))})

;; ## Longer watching, better rates
;;
;; The whole pipeline streams: draws from an unbounded source, paired, turned into
;; sojourns, cut off by the clock, turned into events, folded into the statistic.
;; Nothing is held but the statistic. The error is the largest relative error over the
;; true rates, averaged over 20 watches for each length. (Under a hundred hours, a single
;; early jump can make a rate several times too large, which would dwarf the rest.)

(defn- rate-error [truth learned]
  (transduce (map (fn [[from to q]]
                    (/ (abs (- q (get-in learned [from to] 0.0))) q)))
             max 0.0
             (for [[from row] truth [to q] row] [from to q])))

(defn- watch [seed hours]
  (learner/learn timed/timed (:empty timed/timed)
                 (eduction (partition-all 2) (timed/sojourns machine :up)
                           (timed/for-hours hours)
                           (view/draw-stream seed))))

(view/error-curve
 [["machine"
   (for [hours [100 300 1000 3000 10000]]
     [hours (/ (transduce (map #(rate-error machine
                                            (learner/readout timed/timed
                                                             (watch (+ (* 100 hours) %) hours))))
                          + 0.0 (range 20))
               20)])]]
 {:x-title "hours watched"})

;; # The three side by side

(kind/md
 "
| | `estimate/counting` | `(fade/fading λ)` | `timed/timed` |
|---|---|---|---|
| **events** | transitions of a walk | transitions of a walk | sojourns with their next state |
| **statistic** | counts | counts, and the steps they span | time held per state, and jumps |
| **empty / prior** | `{}`, or pseudo-counts | same, spanning 0 steps | nothing held, no jumps |
| **step** | count one more | fade all by λ, count one more | add the time, count the jump |
| **combine** | add | fade the earlier by λ^(later's steps), add | add |
| **order matters?** | no | yes | no |
| **readout** | normalise rows → chain | normalise rows → chain | jumps / time held → rates |
| **needs completion?** | no | no | yes: the watch ends mid-sojourn |
")

;; What the three have in common is what `robertluo.markov.learner` asks of a learner:
;;
;; - The same six parts, each problem changing different ones: fading changes the step
;;   and combine; time changes the events, the statistic and the readout.
;; - The same three laws: combining with the empty changes nothing; combine is
;;   associative; and learning in parts knows the same as learning at once. Order
;;   independence is not among them: fading breaks it and is still sound.
;; - Events are a transducer, not a map: producing them can need its completion.
;; - The statistic is the learner's own, not always counts, and the readout may yield a
;;   different kind of model: rates, not a chain.
