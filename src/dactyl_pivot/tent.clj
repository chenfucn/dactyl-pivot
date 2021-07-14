(ns dactyl-keyboard.lightcycle
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [unicode-math.core :refer :all]))


(defn hbeam [len sradius bradius tip-len]
    (let [
        ilen         (- len  (* tip-len 2) 8)
        neck-offset  (+ (/ ilen 2) 2)
    ]
        (union
            (->>
                (cylinder [bradius sradius] 4)
                (translate [0 0 neck-offset])
            )
            (cylinder bradius ilen)       
            (cylinder sradius len)       
            (->>
                (cylinder [sradius bradius] 4)
                (translate [0 0 (- 0 neck-offset)])
            )
        )
    )
)

(def slots
    (difference
        (->> (hull
                (translate [-3 0 0] (cylinder 3 150 ))
                (translate [3 0 0] (cylinder 3 150 ))
            )
        )
        (apply union 
            (concat 
                (for [pos (range -9 8)]
                    (->> (cylinder 3 6)
                        (rotate (/ π 2) [0 1 0])
                        (translate [-3 -3 (* pos 8)])) 
                )
            )
        )
    )
)

(def frame
    (union
        (->> slots
            (mirror [1 0 0])
            (rotate (/ π -2) [1 0 0])
            (translate [35 0 0])
        )
        (->> slots
            (rotate (/ π -2) [1 0 0])
            (translate [-35 0 0])
        )
        (->>  (hbeam 79 2.2 3 9)
            (rotate (/ π 2) [0 1 0])
            (translate [0 90 1.6]))

        (->>
            (hull 
                ;connecting to two slotted beams
                (->> (sphere 3) (translate [38 0 0]))
                (->> (sphere 3) (translate [-38 0 0]))

                ; connecting to the hbeam
                (->> (sphere 3) (translate [20 15 1.6]))
                (->> (sphere 3) (translate [-20 15 1.6]))
            )
            (translate [0 75 0])
        )
    )
)

(def top-poles
    (union
        (->> (hbeam 100 2.2 3 7)
            (rotate (/ π 2) [0 1 0]))
        (->> (cylinder 3 60)
            (rotate (/ π 2) [1 0 0])
            (translate [38 -30 0])
        )
        (->> (cylinder 3 60)
            (rotate (/ π 2) [1 0 0])
            (translate [-38 -30 0])
        )

    )
)

(def tent-rack
    (union
        frame
        (->> top-poles
            (translate [0 -90 0]))
    )
)


(spit "stls/tent.scad"
      (write-scad tent-rack))
