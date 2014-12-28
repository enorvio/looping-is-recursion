(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc k] (if (== 0 k) acc (recur (* acc base) (dec k))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc] (if (empty? acc) nil (if (empty? (rest acc)) (first acc) (recur (rest acc)))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc1 acc2] (if (and (empty? acc1) (empty? acc2)) true (if (and (empty? acc1) (not (empty? acc2))) false (if (and (empty? acc2) (not (empty? acc1))) false
      (if (not (= (first acc1) (first acc2))) false (recur (rest acc1) (rest acc2)))))))]
    (helper seq1 seq2)))


(defn find-first-index [pred a-seq]
  (loop [acc 0
         seqq a-seq]
    (if (empty? seqq) nil (if (pred (first seqq)) acc (recur (inc acc) (rest seqq))))))

(defn avg [a-seq]
  (loop [acc1 0
         acc2 0
         seqq a-seq]
    (if (empty? seqq) (if (= acc1 0) 0 (/ acc2 acc1)) (recur (inc acc1) (+ acc2 (first seqq)) (rest seqq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
  (loop [seqq a-seq
         sett #{}]
    (if (empty? seqq) sett (recur (rest seqq) (toggle sett (first seqq)))))))

(defn fast-fibo [n]
  (loop [acc1 0
         acc2 1
         i 1]
    (if (= 0 n) 0 (if (= i n) acc2 (recur acc2 (+ acc1 acc2) (inc i))))))


(defn cut-at-repetition [a-seq]
  (loop [seqq a-seq
         acc '()]
    (if (empty? seqq) (reverse acc) (if (= (count (set acc)) (count acc)) (recur (rest seqq) (conj acc (first seqq))) (reverse (rest acc))))))


