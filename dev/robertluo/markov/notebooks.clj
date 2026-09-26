(ns robertluo.markov.notebooks
  "Keeps target/notebook/ in step with the code. `watch!` renders every notebook, then
   renders them all again whenever a .clj file under src/ or notebook/ changes: a changed
   namespace (and whatever depends on it) is reloaded first, so a page always shows the
   current code, including helpers the notebooks share, such as robertluo.markov.view.
   Needs the :notebook alias; `devenv up` runs `-main` as a process.

   Clay's own live reload is not enough: it watches the notebook files only, and renders
   against whatever version of src/ was loaded when the JVM started."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.namespace.dir :as dir]
            [clojure.tools.namespace.reload :as reload]
            [clojure.tools.namespace.track :as track]
            [nextjournal.beholder :as beholder]
            [scicloj.clay.v2.api :as clay]))

(defn- notebook? [path]
  (str/ends-with? path "_notebook.clj"))

(defn- clj-files [dir]
  (->> (file-seq (io/file dir))
       (filter #(str/ends-with? (str %) ".clj"))))

(defn- notebook-paths []
  (->> (clj-files "notebook") (map str) (filter notebook?) sort vec))

(defonce ^:private tracker (atom (track/tracker)))

(defn- reload!
  "Reloads what changed under src/ and notebook/, and what depends on it, but no notebook
   page: Clay evaluates those. Answers the error, if any; a failed namespace is retried on
   the next call."
  []
  (let [files (concat (clj-files "src") (remove (comp notebook? str) (clj-files "notebook")))
        scanned (dir/scan-files @tracker files)]
    (when-let [changed (seq (::track/load scanned))]
      (println :reloading changed))
    ;; reloading calls in-ns, which needs a thread binding of *ns* on a watcher thread
    (let [reloaded (binding [*ns* *ns*] (reload/track-reload scanned))]
      (reset! tracker reloaded)
      (when-let [e (::reload/error reloaded)]
        (println :error-while-loading (::reload/error-ns reloaded) (ex-message e))
        e))))

(defn- forget-notebooks!
  "A notebook's ns still aliases the namespaces reload! just replaced, and re-evaluating
   its ns form would refuse to re-alias them. Clay recreates it."
  []
  (doseq [n (all-ns)
          :when (str/ends-with? (name (ns-name n)) "-notebook")]
    (remove-ns (ns-name n))))

(defn render-all!
  "Reloads changed namespaces, then renders every notebook to target/notebook/.
   A failure is printed, not thrown, so a watcher survives a broken edit. Answers
   whether the render went through."
  []
  (let [paths (notebook-paths)
        start (System/nanoTime)]
    (println "Rendering" (count paths) "notebook(s)...")
    (try
      (if (reload!)
        (do (println "Reload failed, notebooks not rendered.") false)
        (do (forget-notebooks!)
            (clay/make! {:source-path paths :render true})
            (printf "Rendered in %.1fs%n" (/ (- (System/nanoTime) start) 1e9))
            true))
      (catch Throwable e
        (println "Render failed:" (ex-message e))
        false)
      (finally (flush)))))

(defonce ^:private dirty (atom false))
(defonce ^:private renderer (agent nil))
(defonce ^:private watcher (atom nil))

(defn- render-if-dirty [_]
  (Thread/sleep 100)                    ; an editor's save is often several events
  (when (compare-and-set! dirty true false)
    (render-all!))
  nil)

(defn- on-change [{:keys [path]}]
  (when (str/ends-with? (str path) ".clj")
    (reset! dirty true)
    (send-off renderer render-if-dirty)))

(defn watch!
  "Renders every notebook now and on every change under src/ or notebook/."
  []
  (when-let [w @watcher] (beholder/stop w))
  (reset! watcher (beholder/watch on-change "src" "notebook"))
  (reset! dirty true)
  (send-off renderer render-if-dirty)
  :watching)

(defn unwatch! []
  (some-> @watcher beholder/stop)
  (reset! watcher nil)
  :stopped)

(defn -main
  "Watches and renders until killed; with `--once`, renders once and exits."
  [& args]
  (if (= ["--once"] args)
    (System/exit (if (render-all!) 0 1))
    (do (watch!) @(promise))))
