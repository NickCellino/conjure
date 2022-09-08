(module conjure.client.python.stdio
  {autoload {a conjure.aniseed.core
             extract conjure.extract
             str conjure.aniseed.string
             nvim conjure.aniseed.nvim
             stdio conjure.remote.stdio
             config conjure.config
             text conjure.text
             mapping conjure.mapping
             client conjure.client
             log conjure.log
             ts conjure.tree-sitter}
   require-macros [conjure.macros]})

(config.merge
  {:client
   {:python
    {:stdio
     {:mapping {:start "cs"
                :stop "cS"
                :interrupt "ei"}
      :command "ipython -i --colors=NoColor"
      :prompt_pattern "In%s%[%d+%]:%s"}}}})

(def- cfg (config.get-in-fn [:client :python :stdio]))

(defonce- state (client.new-state #(do {:repl nil})))

(def buf-suffix ".py")
(def comment-prefix "# ")

; The output of an expression gets printed like so in ipython:
; Out[<##>]: <output>
; where <##> is basically some auto-incrementing id ipython assigns
; to each input/output pair
(def expression-output-pattern "^Out%[%d+%]:%s")

(defn- with-repl-or-warn [f opts]
  (let [repl (state :repl)]
    (if repl
      (f repl)
      (log.append [(.. comment-prefix "No REPL running")
                   (.. comment-prefix
                       "Start REPL with "
                       (config.get-in [:mapping :prefix])
                       (cfg [:mapping :start]))]))))

; If python cannot determine whether you are done with your statement after
; a single newline, it will sit and wait for you to enter another newline.
; This happens if you are in the body of a for loop for example.
; We always send 2 newlines because this should cover us in all cases.
(defn- prep-code [s]
  (.. s "\n\n"))

(defn unbatch [msgs]
  (->> msgs
       (a.map #(or (a.get $1 :out) (a.get $1 :err)))
       (str.join "")))

(defn format-msg [msg]
  (->> (str.split msg "\n")
       (a.filter #(~= "" $1))))

(defn- is-expression-output? [m]
  (if (m:find expression-output-pattern)
    true
    false))

; This will remove any "secondary prompts" that the REPL outputs.
; These look like this "...:" and get printed by the REPL when you start
; a statement whose next line is expected to be indented (def statement, for loop, etc.)
; They are also surrounded by whitespace on the left (4 spaces) and on the right (amount
; of spaces depends on how nested your current statement is).
(defn- remove-secondary-prompt [s]
  (str.trim (pick-values 1 (s:gsub "%.%.%.:" ""))))

(defn- get-console-output-msgs [msgs]
  (->> msgs
       (a.map remove-secondary-prompt)
       (a.filter #(not (a.empty? $)))
       (a.filter #(not (is-expression-output? $)))
       (a.map #(.. comment-prefix "(out) " $1))))

(defn- extract-expression-output [m]
  (pick-values 1 (m:gsub expression-output-pattern "")))

; There should typically only be 1 result, but we do not make that assumption
(defn- get-expression-output [msgs]
  (let [results (a.filter is-expression-output? msgs)]
    (a.map extract-expression-output results)))

(defn- log-repl-output [msgs]
  (let [msgs (-> msgs unbatch format-msg)
        console-outputs (get-console-output-msgs msgs)
        expression-outputs (get-expression-output msgs)]
    (when (not (a.empty? console-outputs))
      (log.append console-outputs))
    (when (not (a.empty? expression-outputs))
      (log.append expression-outputs))))

(defn eval-str [opts]
  (with-repl-or-warn
    (fn [repl]
      (repl.send
        (prep-code opts.code)
        (fn [msgs]
          (log-repl-output msgs)
          (when opts.on-result
            (opts.on-result (str.join " " msgs))))
        {:batch? true}))))

(defn eval-file [opts]
  (eval-str (a.assoc opts :code (a.slurp opts.file-path))))

(defn- display-repl-status [status]
  (let [repl (state :repl)]
    (when repl
      (log.append
        [(.. comment-prefix (a.pr-str (a.get-in repl [:opts :cmd])) " (" status ")")]
        {:break? true}))))

(defn stop []
  (let [repl (state :repl)]
    (when repl
      (repl.destroy)
      (display-repl-status :stopped)
      (a.assoc (state) :repl nil))))

(defn start []
  (if (state :repl)
    (log.append [(.. comment-prefix "Can't start, REPL is already running.")
                 (.. comment-prefix "Stop the REPL with "
                     (config.get-in [:mapping :prefix])
                     (cfg [:mapping :stop]))]
                {:break? true})
    (a.assoc
      (state) :repl
      (stdio.start
        {:prompt-pattern (cfg [:prompt_pattern])
         :cmd (cfg [:command])

         :on-success
         (fn []
           (display-repl-status :started))

         :on-error
         (fn [err]
           (display-repl-status err))

         :on-exit
         (fn [code signal]
           (when (and (= :number (type code)) (> code 0))
             (log.append [(.. comment-prefix "process exited with code " code)]))
           (when (and (= :number (type signal)) (> signal 0))
             (log.append [(.. comment-prefix "process exited with signal " signal)]))
           (stop))

         :on-stray-output
         (fn [msg]
           (log.dbg (-> [msg] unbatch format-msg) {:join-first? true}))}))))

(defn on-load []
  (start))

(defn on-exit []
  (stop))

(defn interrupt []
  (with-repl-or-warn
    (fn [repl]
      (let [uv vim.loop]
        (uv.kill repl.pid uv.constants.SIGINT)))))

(defn on-filetype []
  (mapping.buf :n :PythonStart (cfg [:mapping :start]) *module-name* :start)
  (mapping.buf :n :PythonStop (cfg [:mapping :stop]) *module-name* :stop)
  (mapping.buf :n :PythonInterrupt (cfg [:mapping :interrupt]) *module-name* :interrupt))
