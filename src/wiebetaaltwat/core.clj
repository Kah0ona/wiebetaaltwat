#!/usr/bin/env bb

(defn parse-input-in-list [fname]
  (with-open [r (reader fname)]
    (doall (line-seq r))))

(defn update-single-declaration [balance amount participant]
  (let [new-balance (+ (if (contains? balance participant) (get balance participant) 0) amount)]
    (assoc balance participant new-balance)))

(defn update-declarations [balance amount participants]
  (reduce #(update-single-declaration %1 amount %2) balance participants))

(defn make-keywords [lst]
  (map keyword lst))

(defn trim-all [lst]
  (map s/trim lst))

(defn lazy-contains? [col key]
  (some #{key} col))


(defn calculate-balance
  [current-balance line]
  "current-balance is a map of balances for each participant, and line is a line from the input file. Returns an updated balance map"
  (if (or (.startsWith line ";") (empty? line))
    ; comment/empty line just returns the balance unchanged
    current-balance
    (let [pieces            (s/split line #",")
          declarer          (keyword (s/trim (first pieces)))
          amount            (* 100 (read-string (nth pieces 1))) ; //make it in cents
          desc              (nth pieces 2)
          participants      (-> (nth pieces 3)
                           (s/trim)
                           (s/split #"\s")
                           trim-all
                           make-keywords)
          num-participants  (if (lazy-contains? participants :iedereen) (count (keys current-balance)) (count participants))
          per-person-amount (/ amount num-participants)
          parts             (if (lazy-contains? participants :iedereen) (keys current-balance) participants)]
      (-> current-balance
          ; all participants balance should be deducted by amount/num-participants
          (update-declarations (* -1 per-person-amount) parts)
          ; then, the declarer's balance should be increased by amount
          (update-single-declaration amount declarer)))))

(defn parse-participants [line]
  "returns a map of 0 from a space separated line of names, where each name is a keyword. initial balance is 0"
  (let [names (-> line
                  (s/trim)
                  (s/split #"\s")
                  trim-all
                  make-keywords)]
    (reduce #(assoc %1 %2 0) {} names)))


(defn process-input-file [input-file]
  (let [lines           (parse-input-in-list input-file)
        initial-balance (parse-participants (first lines))
        declarations    (rest lines)]
    (reduce calculate-balance initial-balance declarations)))

(defn get-print-entry
  [string balance-entry]
  {:name (name (key balance-entry))
   :val  (str "€ " (format "%.2f" (double (/ (val balance-entry) 100)) ) "\n")})


(defn print-report [balance out]
  (let [report (map get-print-entry balance)]
    (spit out
          (with-out-str (clojure.pprint/print-table report))
          :append true)))

(defn is-zero? [input]
    (and (> input -0.01)  (< input 0.01)))

(defn all-zeroes? [input]
  (every?  #(is-zero? (second %1)) input))

;(all-zeroes? {:m 0.001})

(defn get-pair [balance]
    "Grab two entries, whose balance is not 0; one with a positive and one with a negative balance"
  (let [from-entry (first (filter #(and (not (is-zero? (second %1))) (<= (second %1) -0.01)) balance))
        to-entry   (first (filter #(and (not (is-zero? (second %1))) (>= (second %1)  0.01)) balance))]
    [from-entry to-entry]))

(get-pair {:m 0 :n 0 :s -1 :x 1})

(defn abs [n] (max n (- n)))


(defn get-smallest-amount [recs]
  (let [
    r1 (first recs)
    r2 (second recs)
    v1 (second r1)
    v2 (second r2)]
    (if (<= (abs v1) v2) (abs v1) v2)))


;(get-smallest-amount {:m -5 :n 3})

(defn calculate-payments [balance transactions]
  "given a balance map, returns a list of lists that shows who has to pay who
    Each list entry is a tuple of a transaction, <from, to, amount>."
   (if (all-zeroes? balance)
       transactions
       (let [
         pair (get-pair balance)
         amount (get-smallest-amount pair)
         updated-balance0 (update-single-declaration balance amount (first (first pair)))
         updated-balance  (update-single-declaration updated-balance0 (* -1 amount) (first (second pair)))
         transaction-record [(first (first pair)) (first (second pair)) amount]
         updated-transactions (conj transactions transaction-record)
       ]
         (recur updated-balance updated-transactions))))

(defn get-transaction-line [string transaction]
   (str string "\n" (name (first transaction)) " betaalt " (format "%.2f" (/ (nth transaction 2) 100)) " aan " (name (second transaction))))


(defn print-who-pays-who [balance]
  (let [transactions (calculate-payments balance [])
        string (reduce get-transaction-line "" transactions)]
    (spit "resources/output-jorus.txt" (str string "\n\n\n") :append true)))


(def endbalance (process-input-file "resources/input-bachelor.txt"))
(print-report endbalance)
(print-who-pays-who endbalance)
;(e/e)
