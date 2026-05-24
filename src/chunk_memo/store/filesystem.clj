(ns chunk-memo.store.filesystem
  (:require [chunk-memo.cache :as cache]
            [chunk-memo.store :as store]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defrecord FilesystemStore [root])

(defn filesystem-store
  "Create a filesystem-backed cache store rooted at `root`."
  [root]
  (->FilesystemStore (io/file root)))

(defn filesystem-name
  "Encode a universe id for the current simple filesystem layout."
  [universe-id]
  (let [value (name universe-id)]
    (when (or (str/blank? value)
              (#{"." ".."} value)
              (re-find #"[/\\]" value))
      (throw (ex-info "universe id is not safe as a filesystem name"
                      {:universe universe-id})))
    value))

(defn universe-dir
  "Return the directory for `universe-id` in `store`."
  [{:keys [root]} universe-id]
  (io/file root (filesystem-name universe-id)))

(defn chunk-file-name
  "Return the CSV file name for `chunk-id`."
  [chunk-id]
  (str "chunk-" chunk-id ".csv"))

(defn chunk-file
  "Return the CSV file for `universe-id` and `chunk-id` in `store`."
  [store universe-id chunk-id]
  (io/file (universe-dir store universe-id) (chunk-file-name chunk-id)))

(defn payload-dir
  "Return the payload directory for `universe-id` and `chunk-id` in `store`."
  [store universe-id chunk-id]
  (io/file (universe-dir store universe-id) (str "chunk-" chunk-id)))

(defn payload-file
  "Return the EDN payload file for `address` in `store`."
  [store {:keys [universe chunk-id offset]}]
  (io/file (payload-dir store universe chunk-id) (str offset ".edn")))

(defn address-row
  "Serialize a mapped address to the current simple CSV row format."
  [{:keys [universe chunk-id offset]}]
  (str (filesystem-name universe) "," chunk-id "," offset))

(defn- parse-long-field
  [value field line]
  (try
    (Long/parseLong value)
    (catch NumberFormatException e
      (throw (ex-info "invalid numeric field in cache row"
                      {:field field
                       :value value
                       :line line}
                      e)))))

(defn parse-address-row
  "Parse one CSV row into a mapped address map."
  [line]
  (let [fields (str/split line #"," -1)]
    (when-not (= 3 (count fields))
      (throw (ex-info "cache row must contain universe, chunk id, and offset"
                      {:line line
                       :fields fields})))
    (let [[universe chunk-id offset] fields]
      {:universe (keyword universe)
       :chunk-id (parse-long-field chunk-id :chunk-id line)
       :offset   (parse-long-field offset :offset line)})))

(defn- csv-file?
  [file]
  (and (.isFile file)
       (str/ends-with? (.getName file) ".csv")))

(defn- file-lines
  [file]
  (with-open [reader (io/reader file)]
    (doall (line-seq reader))))

(defn scan-addresses
  "Scan every CSV row under `store` and return observed mapped addresses."
  [{:keys [root]}]
  (if-not (.exists root)
    #{}
    (->> (file-seq root)
         (filter csv-file?)
         (mapcat file-lines)
         (remove str/blank?)
         (map parse-address-row)
         set)))

(defn- edn-file?
  [file]
  (and (.isFile file)
       (str/ends-with? (.getName file) ".edn")))

(defn- parse-payload-address
  [root file]
  (let [relative (.relativize (.toPath root) (.toPath file))
        parts    (mapv str relative)]
    (when (= 3 (count parts))
      (let [[universe chunk-dir filename] parts]
        (when-let [[_ chunk-id offset] (re-matches #"chunk-(\d+)/(\d+)\.edn" (str chunk-dir "/" filename))]
          {:universe (keyword universe)
           :chunk-id (Long/parseLong chunk-id)
           :offset   (Long/parseLong offset)})))))

(defn scan-payload-addresses
  "Scan every EDN payload file under `store` and return observed mapped addresses."
  [{:keys [root]}]
  (if-not (.exists root)
    #{}
    (->> (file-seq root)
         (filter edn-file?)
         (keep #(parse-payload-address root %))
         set)))

(extend-type FilesystemStore
  store/CacheStore
  (present-mapped-addresses [store _mapped-cache]
    (into (scan-addresses store) (scan-payload-addresses store)))
  (read-payload [store _mapped-cache item]
    (edn/read-string (slurp (payload-file store (:mapped-address item)))))
  (write-payload! [store _mapped-cache item payload]
    (let [file (payload-file store (:mapped-address item))]
      (.mkdirs (.getParentFile file))
      (spit file (pr-str payload))
      payload)))

(defn write-mapped-cache!
  "Materialize `mapped-cache` in `store` using the simple CSV layout.

  Files are grouped by mapped universe and mapped chunk id. Each row records the
  mapped address as `universe,chunk-id,offset`."
  [store mapped-cache]
  (doseq [[[universe-id chunk-id] items]
          (group-by (fn [item]
                      (let [{:keys [universe chunk-id]} (:mapped-address item)]
                        [universe chunk-id]))
                    (cache/mapped-items mapped-cache))]
    (let [file (chunk-file store universe-id chunk-id)]
      (.mkdirs (.getParentFile file))
      (spit file
            (str (str/join "\n"
                           (map (comp address-row :mapped-address)
                                (sort-by (comp :offset :mapped-address) items)))
                 "\n"))))
  store)
