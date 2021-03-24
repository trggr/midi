(ns midi.timlib
  (:use [clojure.string :only [blank? join lower-case split starts-with?
                                   ends-with? trim split-lines]]))


(defn deu [xs]  
   (println (join \newline
                (for [row xs]
                    (join \tab row)))))

(defn view
  ([coll t d] (deu (take t (drop d coll))))
  ([coll t]   (view coll t 0))
  ([coll]     (view coll 40 0)))

(defn integer [s] (. Integer parseInt s))

;; returns n-th column of matrix as vector
(defn col [n matrix]
    (map #(nth % n) matrix))

(defn now []
   (-> (new java.text.SimpleDateFormat "MM/dd/yyyy HH:mm:ss")
       (.format (new java.util.Date))))

;; returns string of length n, if original is shorted, it's padded with spaces,
;; if it's longer it's truncated to n
(defn pad 
    ([n s] (pad n s \space))
    ([n s fill]
        (let [x   (str s)
              len (count x)]
            (cond (= len n) x
                  (> len n) (subs x 0 n)
                  :else     (apply str x (take (- n len) (repeat fill)))))))

;(defn de-uglify [xs]
;    (let [ys      (vec xs)
;          ncols   (count (first ys))
;          maxlens (for [i (range ncols)]
;                      (->> ys (col i) (map str) (map count) (reduce max)))]
;        (join \newline
;            (for [row ys]
;                 (join "  " (map pad maxlens row))))))

(defn cute-headers [xs]
    (let [vs      (vec xs)
          [header & rows] vs
          ncols   (count header)
          maxlens (for [i (range ncols)]
                      (->> rows (col i) (map str) (map count) (reduce max)))]
        (join \newline
            (for [row vs]
                 (join "  " (map pad maxlens row))))))

(defn pre ([s]        (str "<pre>" s "</pre>"))
          ([s & more] (pre (join \newline (flatten [s more])))))

(defn in? [xs x]      (contains? (set xs) x))


(defmacro for-loop [[sym init check change :as params] & steps]
 `(loop [~sym ~init value# nil]
    (if ~check
      (let [new-value# (do ~@steps)]
        (recur ~change new-value#))
      value#)))

; --- don't have Oracle installed here
;(defn connect [db]    (let [[url user pass] db
;                            ods (doto (new oracle.jdbc.pool.OracleDataSource)
;                                      (.setURL url)
;                                      (.setUser user)
;                                      (.setPassword pass))]
;                         (.getConnection ods)))


; Example of SQLite operations
; (def conn (connect-sqlite "sample3.db"))
; (-> conn .createStatement (.executeUpdate "create table person (id number, name string);"))
; (batch-update conn "insert into person(id, name) values (?, ?)"  [["5" "Yolochka"]["6" "Slooshie"]])
; (cursor conn "select * from person")

(defn connect-sqlite [db]
   (java.sql.DriverManager/getConnection (str "jdbc:sqlite:" db)))

;; Takes open connection and query, runs query and returns results as 
;; a lazy sequence with elements as vectors (when the third param is false)
;; or as maps where column names represents the keys.
(defn cursor
   ([conn query] (cursor conn query [] false))
   ([conn query params return-map?]
      (let [stmt    (let [st (.prepareStatement conn query)]
                       (for-loop [i 0 (< i (count params)) (inc i)]
                                (.setString st (inc i) (nth params i)))
                       st)
            rs      (.executeQuery stmt)
            meta    (.getMetaData rs)
            ncols   (.getColumnCount meta)
            ns      (range 1 (inc ncols))
            header  (map #(.getColumnLabel meta %) ns)
            kheader (mapv #(keyword (lower-case %)) header)
;            values  (fn [] (mapv #(.getString rs %1) ns))
            values  (fn [] (mapv #(.getObject rs %1) ns))
            as-map  (fn f1 []
                      (when (.next rs)
                         (lazy-seq (cons (zipmap kheader (values)) (f1)))))
            as-list (fn f1 []
                      (when (.next rs)
                         (lazy-seq (cons (values) (f1)))))]
       (if return-map? (as-map) (conj (as-list) header)))))

(defn dbname [conn]
    (-> conn
        (cursor "select global_name dbname from global_name" [] true)
        first
        :dbname))

(defn batch-update
   ([conn dml rows]           (batch-update conn dml rows 50))
   ([conn dml rows batchsize]
    (let [cur (.prepareStatement conn dml)]
        (loop [xs (seq rows) counter 0 batch batchsize]
            (if xs
                 (let [row (first xs)]
                      (for-loop [i 0 (< i (count row)) (inc i)]
                            (if (instance? Long (nth row i))
                               (.setLong cur (inc i) (nth row i))
                               (.setString cur (inc i) (nth row i))))
                      (.addBatch cur)
                      (when (zero? batch)
                          (.executeBatch cur))
                      (recur (next xs)
                             (inc counter)
                             (if (zero? batch) 50 (dec batch))))
                 (do (.executeBatch cur)
                     (.close cur)
                     counter))))))

(defn parse-xlsx [xlsx]
    (when xlsx
        (for [line (split-lines xlsx)]
            (map trim (split line #"\t")))))
       
(defn pair= [s] (let [[a b] (split-with (partial not= \=) s)
                      x     (trim (apply str a))
                      y     (trim (apply str (rest b)))]
                                 [x y]))

(defn ampersand-split [s] (split s #"&"))

(defn tee [x]
    (spit "debug.log" x :append true)
    x)

(defn extract-params [response] (->> response
                                     :body
                                     slurp
                                     ampersand-split
                                     (map #(java.net.URLDecoder/decode %))
                                     (map pair=)
                                     (filter (comp (partial < 0) count first))
                                     (map (juxt (comp keyword first) second))
                                     (into {})))

(defn respond ([txt]        (respond txt 200))
              ([txt status] {:status  status
                             :headers {"Content-Type" "text/html"}
                             :body    txt}))
