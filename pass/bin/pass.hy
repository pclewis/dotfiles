#! /usr/bin/env nix-shell
;; -*- inferior-lisp-program: "nix-shell -p hy python35Packages.pyyaml pass xdotool xclip rofi --run hy"; -*-
"
#! nix-shell -i hy -p hy python35Packages.pyyaml pass xdotool xclip rofi
"

(import yaml sys csv os io
        [subprocess :as sp]
        [hy.lex.parser [hy-symbol-mangle]]
        [functools [partial]])

(defn $home/ [path] (os.path.join (get os.environ "HOME") path))
(def $user (get os.environ "USER"))

(def password-store  ($home/ ".password-store"))
(def password-img    ($home/ "password-container.img"))
(def password-key    ($home/ "password-container.key.gpg"))
(def lastpass-csv    "lastpass.csv")
(def dmcrypt-mapping (+ $user "-passwords"))
(def dmcrypt-device  (os.path.join "/dev/mapper" dmcrypt-mapping))

(defn dict-walk [f v]
  (cond
   [(instance? dict v)
    (dict-comp (f k)
               (dict-walk f (get v k))
               [k (.keys v)])]
   [(coll? v) (list (map (partial dict-walk f) v))]
   [True v]))

(defn mangle-keys [x]
  (dict-walk (comp hy-symbol-mangle name) x))

(defn keywordize-keys [x]
  (dict-walk keyword x))

(defmacro assoc* [m k v]
  `(doto (dict ~m) (assoc ~k ~v)))

(defmacro when-let [bindings &rest forms]
  (let [exp (fn [bindings forms]
              (let [[name value] (take 2 bindings)
                    rest (list (drop 2 bindings))]
                `(let [~name ~value]
                   (when ~name
                     ~@(if (empty? rest)
                         forms
                         [(exp rest forms)])))))]
    (exp bindings forms)))

(defn run [&rest args &kwargs in-kwargs]
  (let [defaults {:stdout             sp.PIPE
                  :check              True
                  :universal_newlines True}
        kwargs (merge-with (fn [a b] b) (mangle-keys defaults) in-kwargs)]
    (-> (apply sp.run [(map str args)] kwargs)
        (. stdout)
        (.strip))))

(defn sudo [&rest args &kwargs kwargs]
  (let [sudo-env (-> (.get kwargs "env" (os.environ.copy))
                     (assoc* "SUDO_ASKPASS" ($home/ "bin/askpass-pinentry.sh")))]
    (apply run
           (+ ["sudo" "-A"] (list args))
           (assoc* kwargs "env" sudo-env))))

(defn notify [title text &optional [id-to-replace 0] [timeout -1] [urgency 1]]
  (int
   (run "notify-send.py"
        "-r" id-to-replace
        "-t" timeout
        "-u" urgency
        "-p"
        title text)))

(defn notify-close [id]
  (run "notify-send.py" "-c" id))

(defn get-active-window-title []
  (run "xdotool" "getactivewindow" "getwindowname"))

(defn user-select [prompt opts]
  (let [opt-keys (list opts)
        selected (run "rofi"
                      "-dmenu"
                      "-p" prompt
                      :input (.join "\n" opt-keys))]
    (get opts selected)))

(defn paste-once [s]
  (run "xclip"
       "-quiet"
       "-selection" "CLIPBOARD"
       "-l" "2"
       :input s))

(defn extract-hostname-from-window-title [s]
  (when (in "|" s)
    (get (.rsplit s "|" 1) 1)))

(defn pass/show [name]
  (try (run "pass" "show" name)
       (except [e sp.CalledProcessError] None)))

(defn pass/insert [name value]
  (run "pass" "insert" "-m" name :input value))

(defn hostname->pass-store-key [host]
  (os.path.join "by-host" host))

(defn get-yaml-for-host [host]
  (let [s (pass/show (hostname->pass-store-key host))]
    (when s (keywordize-keys (list (yaml.safe_load_all s))))))

(defn set-yaml-for-host [host docs]
  (let [s (yaml.safe-dump-all (mangle-keys docs) :default-flow-style False)]
    (pass/insert (hostname->pass-store-key host) s)))

(defmacro/g! with-notification [title text urgency &rest body]
  `(let [~g!msg-id (notify ~title ~text :urgency ~urgency)]
    (try
     ~@body
     (finally (notify-close ~g!msg-id)))))

(defn get-yaml-from-lastpass [host]
  (when-let [lpassinfo (pass/show lastpass-csv)
             lp-columns [:lp_url :user :pass :lp_extra :lp_name :lp_grouping :lp_fav]
             info (->> lpassinfo
                       (io.StringIO)
                       (csv.reader)
                       (remove empty?)
                       (filter (fn [row] (in host (nth row 0))))
                       (map (partial (comp dict zip) lp-columns))
                       (list))]
            [(-> info (nth 0) :pass) info]))

(defn mapper-present? []
  (os.path.exists dmcrypt-device))

(defn store-mounted? []
  (try
   (and (os.path.ismount password-store)
        (= (-> password-store (os.stat) (. st_dev))
           (-> dmcrypt-device (os.stat) (. st_rdev))))
   (except [e FileNotFoundError] False)))

(defn attach-mapper []
  (let [key (run "gpg2" "--decrypt" password-key :universal-newlines False)]
    (sudo "cryptsetup" "luksOpen" "--key-file=-" password-img
          dmcrypt-mapping
          :input key
          :universal-newlines False)))

(defn mount-store []
  (sudo "mount" "-onoexec,nosuid,nodev" dmcrypt-device password-store))

(defn die [&rest msg]
  (notify "pass error" (reduce + msg) :urgency 2)
  (sys.exit 1))

(defn get-docs [host]
  (let [docs (get-yaml-for-host host)]
    (if docs
      docs
      (when-let [docs (get-yaml-from-lastpass host)]
                (set-yaml-for-host host docs)
                docs))))

(defn describe-entry [entry]
  (or
   (some (fn [k] (.get entry k None))
         [:user :lp_name :lp_url])
   "(no description)"))

(defn pick-doc [docs]
  (if (= 1 (len docs))
    {:pass (first docs)}
    (if (< 1 (len (second docs)))
      (user-select "user:" (dict-comp (describe-entry entry)
                                      entry
                                      [entry (second docs)]))
      (first (second docs)))))

(defmain [&rest args]
  (let [host (or (second args) (extract-hostname-from-window-title (get-active-window-title)))]
    (unless host
      (die "No host provided and none found in active window title."))

    (print host)

    (unless (mapper-present?)
      (attach-mapper)
      (unless (mapper-present?) (die "Couldn't attach mapper.")))

    (unless (store-mounted?)
      (mount-store)
      (unless (store-mounted?) (die "Couldn't mount store.")))

    (let [docs (get-docs host)]
      (unless docs (die "Couldn't find login info for " host))

      (let [doc (pick-doc docs)]
        (for [field [:user :pass]]
          (when-let [val (.get doc field None)]
            (with-notification host (+ (name field) " in clipboard") 2
              (paste-once val))))))

    (notify host "Done!" :timeout 1000 :urgency 0)))
