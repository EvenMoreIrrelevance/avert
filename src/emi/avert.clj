(ns emi.avert)

(declare -unknown-to-kondo)

(defn -swallow
  {:clj-kondo/macroexpand-hook true}
  [& _vals]
  nil)

(defmacro -if-kondo
  {:clj-kondo/macroexpand-hook true}
  [kondof runtimef]
  (if-not (resolve 'emi.avert/-unknown-to-kondo)
    kondof
    runtimef))

(defmacro -when-kondo
  {:clj-kondo/macroexpand-hook true}
  [& forms]
  `(-if-kondo
     ~@(if (< (count forms) 2)
         forms
         `(do ~@forms))
     nil))

(defn -parse-pairs-and-tail
  {:clj-kondo/macroexpand-hook true}
  [syntax]
  (if (< (bounded-count 2 syntax) 2)
    [nil (first syntax)]
    (let [[pairs [[tail]]] (partition-by count (partition-all 2 syntax))]
      [pairs tail])))

(comment
  (-parse-pairs-and-tail '[1 2 3 4 5])
  (-parse-pairs-and-tail '[1 2 3 4])
  (-parse-pairs-and-tail '[1])
  *e)

(defn throw-errform
  "Either throws the given error or an ex-info with the given message and error map"
  {:clj-kondo/macroexpand-hook true}
  [err-or-map message]
  (throw
    (if (map? err-or-map)
      (ex-info (or (::message err-or-map) message) (dissoc err-or-map ::message))
      err-or-map)))

(defmacro mavert
  "Throws an error (implicitly coerced with `throw-errform`) if any of the conditions are truthy;
   When building the error, `not-averted` is let-bound to `[condition form-representation]`"
  {:clj-kondo/macroexpand-hook true}
  [not-averted & averted-err-pairs-and-ok]
  (let [[averted->err ok]
        (-parse-pairs-and-tail averted-err-pairs-and-ok)]
    `(do
       ~@(for [[avertion errform] averted->err]
           `(when-let [not-averted# ~avertion]
              (let [~not-averted [not-averted# (quote ~avertion)]]
                (-when-kondo ~not-averted)
                (throw-errform ~errform ~(pr-str avertion)))))
       ~ok)))

(defmacro $avert
  "Shorthand for mavert with `not-averted` = `[$ $form]`"
  {:clj-kondo/macroexpand-hook true}
  [& averted-err-pairs-and-ok]
  `(mavert ~'[$ $form] ~@averted-err-pairs-and-ok))

(defn enriched-error
  "Builds an ex-info having the same message as the original error 
   (unless ::message is bound in the enrichment-map), `(merge (ex-data err) enrichment-map)` as the data
   and `err` as the cause."
  {:clj-kondo/macroexpand-hook true}
  [err enrichment-map]
  (ex-info
    (or (::message enrichment-map) (ex-message err))
    (merge (ex-data err) (dissoc enrichment-map ::message))
    err))

(defmacro with-cleanup
  "try-finally with the finally clause at the top."
  {:clj-kondo/macroexpand-hook true}
  [cleanup & body]
  `(try (do ~@body)
     (finally ~cleanup)))

(defn -coerce-catching-clauses 
  {:clj-kondo/macroexpand-hook true}
  [clauses]
  (if-not (simple-symbol? (first clauses))
    clauses
    `[(java.lang.Exception ~@clauses)]))

(defmacro catching
  "try-catch with the catch body at the top, expressed either as a vector of `catch` clauses
   or as a simple symbol followed by a body (which will catch and bind `Exception`)."
  {:clj-kondo/macroexpand-hook true}
  [catches & body]
  body
  ($avert
    (not (vector? catches)) {::message "catches must be a vector of catch clauses"}
    `(try (do ~@body)
       ~@(doall
           (for [clause (-coerce-catching-clauses catches)]
             ($avert
               (not (seq? clause)) {::message "multiple catch clauses must be sequences"}
               `(catch ~@clause)))))))

(defn -coerce-rethrowing-clauses
  {:clj-kondo/macroexpand-hook true}
  [clauses]
  (if (map? clauses)
    `[(java.lang.Exception e# (enriched-error e# ~clauses))]
    (-coerce-catching-clauses clauses)))

(defmacro rethrowing
  "Like catching, but throws the evaluation of catch clauses.
   If `rethrows` is a map, `Exception` is caught and `(enriched-error e# ~clauses)` is thrown."
  {:clj-kondo/macroexpand-hook true}
  [rethrows & body]
  `(try (do ~@body)
     ~@(doall
         (for [clause (-coerce-rethrowing-clauses rethrows)]
           ($avert
             (not (seq? clause)) {::message "multiple catch clauses must be sequences"}
             (let [[ety evar & bod] clause]
               (-when-kondo (-swallow ety evar bod))
               `(catch ~ety ~evar
                  ~@(butlast bod)
                  (throw-errform ~(last bod) ""))))))))
