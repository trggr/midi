(ns midi.core-test
  (:require [clojure.test :refer [deftest is are]]
            [midi.dbload :as db]
            [midi.core :as core]))

(def sample-edn
  {:id 0, :nm "TEST SONG", :numer 4, :denom 4,
   :ppq 400000, :bb 8, :bpm 90,
   :drum "drums-swing",
   :bass "bass-15-68",
   :tab-score "Am | Am Dm | Am Dm G | Am Dm G C"})

(comment 
   (remove-ns 'midi.core-test)
)

(deftest get-song-bbcs-test
  (let [_ (-> sample-edn
              db/enhance-song-map
              db/save-song-to-db)]
    (is (= [[1 1 :Am] [1 2 :Am] [1 3 :Am] [1 4 :Am]
            [2 1 :Am] [2 2 :Am] [2 3 :Dm] [2 4 :Dm]
            [3 1 :Am] [3 2 :Am] [3 3 :Dm] [3 4 :G]
            [4 1 :Am] [4 2 :Dm] [4 3 :G]  [4 4 :C]]
           (core/get-song-bbcs 0)))))

(deftest compress-test
 (are [result args] (= result (core/compress args))
   [5 1 1 3 2 4 4 1]    [5 1 1 1 2 2 2 2 4]
   [2 1]                [2]
   []                   []))
