(ns unrepl.make-blob
  (:require
    [clojure.java.io :as io]
    [clojure.edn :as edn]
    [clojure.string :as str]
    [unrepl.shade-libs :as shade]))

(defn- strip-spaces-and-comments [s]
  #_(I had this nice #"(?s)(?:\s|;[^\n\r]*)+|((?:[^;\"\\\s]|\\.|\"(?:[^\"\\]|\\.)*\")+)"
      but it generates stack overflows...
      so let's write the state machine!)
  (let [sb (StringBuilder.)]
    (letfn [(regular [c]
              (case c
                \; comment
                \# dispatch
                \" (do (.append sb c) string)
                \\ (do (.append sb c) regular-esc)
                (\newline \return) strip-nl
                (\tab \space \,) strip
                (do (.append sb c) regular)))
            (strip [c]
              (case c
                (\newline \return) strip-nl
                (\tab \space \,) strip
                \; comment
                (do (.append sb " ") (regular c))))
            (strip-nl [c]
              (case c
                (\newline \return \tab \space \,) strip-nl
                \; comment
                (do (.append sb "\n") (regular c))))
            (dispatch [c]
              (case c
                \! comment
                \" (do (.append sb "#\"") string)
                (do (-> sb (.append "#") (.append c)) regular)))
            (comment [c]
              (case c
                \newline strip-nl
                comment))
            (string [c]
              (.append sb c)
              (case c
                \" regular
                \\ string-esc
                string))
            (string-esc [c]
              (.append sb c)
              string)
            (regular-esc [c]
              (.append sb c)
              regular)]
      (reduce
        #(%1 %2)
        regular s))
    (str sb)))

(defn- gen-blob [session-actions options]
  (let [template (slurp (io/resource "unrepl/blob-template.clj"))
        code     (let [shaded-code-sb (StringBuilder.)
                       shaded-libs (shade/shade 'unrepl.repl
                                                (merge options
                                                       {:writer (fn [_ ^String code] (.append shaded-code-sb code))}))]
                   (-> template
                     (str/replace "unrepl.repl"
                                  (str (shaded-libs 'unrepl.repl)))
                     (str/replace "<BLOB-PAYLOAD>" (str shaded-code-sb))))]
    (str (strip-spaces-and-comments code) "\n" session-actions "\n"))) ; newline to force eval by the repl

(defn -main
  ([& args]
   (let [options (loop [args    (seq args)
                        options {:except ['unrepl.core]
                                 :provided [#"clojure\..*"]
                                 :session-actions "{}"
                                 :target "resources/unrepl/blob.clj"}]
                   (if args
                     (condp contains? (first args)
                       #{"--noshade"}       (recur (next args)  (assoc options :except [#".+"]))
                       #{"-e" "--except"}   (recur (nnext args)
                                                   (if-let [args (next args)]
                                                     (update-in options [:except] conj (read-string (first args)))
                                                     (throw (ex-info (str "Missing argument for --except")))))
                       #{"-p" "--provided"} (recur (nnext args)
                                                   (if-let [args (next args)]
                                                     (update-in options [:provided] conj (read-string (first args)))
                                                     (throw (ex-info (str "Missing argument for --provided")))))
                       #{"-o" "--output"}   (recur (nnext args) (assoc options :target (fnext args)))
                       #{"-a" "--actions"}  (recur (nnext args) (assoc options :session-actions (fnext args)))
                       #{"-h" "--help"}     (do
                                              (println "clj -m unrepl.make-blob [--noshade] [-e <exception>]* [-p <provided>]* [--output|-o <file>] [--actions|-a <map-or-file>]")
                                              (System/exit 1))
                       (throw (ex-info (str "Unknown argument: " (first args)) {:arg (first args)})))
                     options))
         session-actions-source (if (re-find #"^\s*\{" (:session-actions options)) (:session-actions options) (slurp (:session-actions options)))
         session-actions-map (edn/read-string {:default (fn [tag data] (tagged-literal 'unrepl-make-blob-unquote (list 'tagged-literal (tagged-literal 'unrepl-make-blob-quote tag) data)))} session-actions-source)]
     (-> options :target io/file .getAbsoluteFile .getParentFile .mkdirs)
     (if (map? session-actions-map)
       (let [session-actions-map (into session-actions-map
                                       (map (fn [[k v]]
                                              [k (tagged-literal 'unrepl-make-blob-syntaxquote
                                                                 (if (and (seq? v) (symbol? (first v)) (namespace (first v)))
                                                                   (list 'unrepl.repl/ensure-ns v)
                                                                   v))]))
                                       session-actions-map)
             session-actions (pr-str session-actions-map)]
         (spit (:target options) (gen-blob session-actions options)))
       (println "The arguments must be: a target file name and an EDN map.")))))
