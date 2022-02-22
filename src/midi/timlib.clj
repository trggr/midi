(ns midi.timlib
  (:require [clojure.string :as str]))

(defn third [coll] (nth coll 2))

(defn fourth [coll] (nth coll 3))


 (defn mapcat2
   "Applies f to every two elements in the reduction of coll, concatenating the result
   colls of (f x y)."
   [f coll]
   (loop [acc [] xs coll]
     (if (empty? xs)
       acc
       (let [[a b & _] xs]
         (recur (concat acc (f a b))
                (rest xs))))))

(defn col
  "Returns n-th column of matrix as vector"
  [n matrix]
  (map #(nth % n) matrix))

(defn now
  "Returns current time"
  []
  (-> (new java.text.SimpleDateFormat "MM/dd/yyyy HH:mm:ss")
      (.format (new java.util.Date))))

(defn pad
  "Returns string of length n. Short strings are padded with spaces,
   long ones are truncated"
  ([n s] (pad n s \space))
  ([n s fill]
   (let [x   (str s)
         len (count x)]
     (cond (= len n) x
           (> len n) (subs x 0 n)
           :else     (apply str x (take (- n len) (repeat fill)))))))

(defn deu
  "Display Clojure structure as tabulated text"
  [xs]
  (let [ys      (vec xs)
        ncols   (count (first ys))
        maxlens (for [i (range ncols)]
                  (->> ys (col i) (map str) (map count) (reduce max)))]
    (str/join \newline
              (for [row ys]
                (str/join "  " (map pad maxlens row))))))

 (defn view
   ([coll t d] (deu (take t (drop d coll))))
   ([coll t]   (view coll t 0))
   ([coll]     (view coll 40 0)))

(defn cute-headers
  "Converts Excel-like structure xs into text where columns are vertically aligned"
  [xs]
  (let [vs      (vec xs)
        [header & rows] vs
        ncols   (count header)
        maxlens (for [i (range ncols)]
                  (->> rows (col i) (map str) (map count) (reduce max)))]
    (str/join \newline
          (for [row vs]
            (str/join "  " (map pad maxlens row))))))

(defn pre
  "Turns a string into HTML snippet which looks like program code.
  Separates elements with new line character"
  ([s]
   (str "<pre>" s "</pre>"))
  ([s & more]
   (pre (str/join \newline (flatten [s more])))))

(defn in?
 "Returns true when xs contains x"
  [xs x]
  (contains? (set xs) x))

;(defn connect
;  "Connects to db via JDBC. The db is a vector of URL, username, and password"
;  [db]
;  (let [[url user pass] db
;        ods (doto (new oracle.jdbc.pool.OracleDataSource)
;              (.setURL url)
;              (.setUser user)
;              (.setPassword pass))]
;    (.getConnection ods)))

; Example of SQLite operations
; (def conn (connect-sqlite "sample3.db"))
; (-> conn .createStatement (.executeUpdate "create table person (id number, name string);"))
; (batch-update conn "insert into person(id, name) values (?, ?)"  [["5" "Yolochka"]["6" "Slooshie"]])
; (cursor conn "select * from person")

(defn connect-sqlite
  "Connects to SQLite database db"
  [db]
  (java.sql.DriverManager/getConnection (str "jdbc:sqlite:" db)))

(defn cursor
  "Takes open connection and query, runs query and returns
   a lazy sequence of retrieved rows. When param return-map? each returned
   element is a map, otherwise it's vector"
  ([conn query]
   (cursor conn query [] false))
  ([conn query params]
   (cursor conn query params false))
  ([conn query params as-map?]
   (let [rs       (.executeQuery
                   (reduce (fn [acc i]
                             (.setString acc (inc i) (nth params i))
                             acc)
                           (.prepareStatement conn query)
                           (range (count params))))
         meta      (.getMetaData rs)
         cols      (range 1 (inc (.getColumnCount meta)))
         header    (map #(.getColumnLabel meta %) cols)
         kheader   (map #(-> % str/lower-case keyword) header)
         fetch-1   (fn [] (mapv #(.getObject rs %1) cols))
         fetch-all (fn f [wrap]
                     (when (.next rs)
                       (lazy-seq (cons (wrap (fetch-1)) (f wrap)))))]
     (if as-map?
       (fetch-all (partial zipmap kheader))
       (conj (fetch-all identity) header)))))

(defn dbname
  "Returns Oracle database name"
  [conn]
  (-> conn
      (cursor "select global_name dbname from global_name" [] true)
      first
      :dbname))

(defn batch-update
  "Runs DML via JDBC batch mode"
   ([conn dml rows]
    (batch-update conn dml rows 50))
   ([conn dml rows batchsize]
    (let [cur (.prepareStatement conn dml)]
        (loop [xs (seq rows) counter 0 batch batchsize]
            (if xs
                 (let [row (first xs)]
                   (reduce (fn [acc i]
                             (.setString acc (inc i) (nth row i))
                             acc)
                           cur
                           (range (count row)))
                   (.addBatch cur)
                   (when (zero? batch)
                     (.executeBatch cur))
                   (recur (next xs)
                          (inc counter)
                          (if (zero? batch) 50 (dec batch))))
                 (do (.executeBatch cur)
                     (.close cur)
                     counter))))))

(defn parse-xlsx
  "Converts XLSX into collection of collections"
  [xlsx]
  (when xlsx
    (for [line (str/split-lines xlsx)]
      (map str/trim (str/split line #"\t")))))


(defn pair=
  [s]
  (let [[a b] (split-with (partial not= \=) s)
        x     (str/trim (apply str a))
        y     (str/trim (apply str (rest b)))]
    [x y]))


(defn ampersand-split
  "Splits string at ampersand"
  [s]
  (str/split s #"&"))


(defn extract-params
  "Extracts parameters from HTTP response"
  [response]
  (->> response
       :body
       slurp
       ampersand-split
       (map #(java.net.URLDecoder/decode %))
       (map pair=)
       (filter (comp (partial < 0) count first))
       (map (juxt (comp keyword first) second))
       (into {})))


(defn respond
  "Send response to HTTP client"
  ([txt]        (respond txt 200))
  ([txt status] {:status  status
                 :headers {"Content-Type" "text/html"}
                 :body    txt}))
