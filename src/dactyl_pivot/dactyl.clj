(ns dactyl-pivot.dactyl
    (:refer-clojure :exclude [use import])
    (:require [clojure.core.matrix :refer [array matrix mmul]]
              [scad-clj.scad :refer :all]
              [scad-clj.model :refer :all]
              [unicode-math.core :refer :all]))


(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 5)
(def ncols 7)

(def α (/ π 10))                        ; curvature of the columns
(def β (/ π 32))                        ; curvature of the rows
(def centerrow (- nrows 3))             ; controls front-back tilt
(def centercol 4)                       ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 12))            ; or, change this for more precise tenting control

(def pinky-15u true)                    ; controls whether the outer column uses 1.5u keys
(def first-15u-row 0)                   ; controls which should be the first row to have 1.5u keys on the outer column
(def last-15u-row 3)                    ; controls which should be the last row to have 1.5u keys on the outer column

(def extra-row false)                   ; adds an extra bottom row to the outer columns
(def inner-column true)                 ; adds an extra inner column (two less rows than nrows)

(def column-style :standard)

(defn column-offset [column]
  (if inner-column
    (cond (<= column 1) [0 -2 0]
          (= column 3) [0 2.82 -4.5]
          (= column 5) [0 -12 5.64]    ; original [0 -5.8 5.64]
          (> column 5) [0 -12 7.3]
          :else [0 0 0])
    (cond (= column 2) [0 2.82 -4.5]
          (= column 4) [0 -12 5.64]    ; original [0 -5.8 5.64]
          (> column 4) [0 -12 7.3]
          :else [0 0 0])))

(def thumb-offsets [-8 18 21])
;(def thumb-offsets [-6 18 25])

(def keyboard-z-offset 8)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 2)                     ; extra space between the base of keys; original= 2
(def extra-height 0.5)                  ; original= 0.5

(def wall-z-offset -8)                 ; length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 5)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 2)                  ; wall thickness parameter; originally 5

;; Settings for column-style == :fixed
;; The defaults roughly match Maltron settings
;;   http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

(def create-side-nubs? true)

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))
(def extra-cornerrow (if extra-row lastrow cornerrow))
(def innercol-offset (if inner-column 1 0))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.25) ;; Was 14.1, then 14.25
(def keyswitch-width 14.25)

(def sa-profile-key-height 12.7)

(def plate-thickness 4)
(def side-nub-thickness 4)
(def retention-tab-thickness 1.5)
(def retention-tab-hole-thickness (- (+ plate-thickness 0.5) retention-tab-thickness))
(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))

(def single-plate
    (let [top-wall (->> (cube (+ keyswitch-width 3) 1.5 plate-thickness)
                        (translate [0
                                    (+ (/ 1.5 2) (/ keyswitch-height 2))
                                    (/ plate-thickness 2)]))
          left-wall (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                         (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                     0
                                     (/ plate-thickness 2)]))
          side-nub (->> (binding [*fn* 30] (cylinder 0.75 2.75))
                        (rotate (/ π 2) [1 0 0])
                        (translate [(+ (/ keyswitch-width 2)) 0 1])
                        (hull (->> (cube 1.5 2.75 plate-thickness)
                                   (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                               0
                                               (/ side-nub-thickness 2)])))
                        (translate [0 0 (- plate-thickness side-nub-thickness)]))
          plate-half (union top-wall left-wall (if create-side-nubs? (with-fn 100 side-nub)))]
      (union plate-half
             (->> plate-half
                  (mirror [1 0 0])
                  (mirror [0 1 0])))))


;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap {1 (let [bl2 (/ 18.5 2)
                     m (/ 17 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 (/ sa-double-length 2)
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 28 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color [240/255 223/255 175/255 1])))})

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range (+ innercol-offset 0) ncols))
(def rows (range 0 nrows))

(def innercolumn 0)
(def innerrows (range 0 (- nrows 2)))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))

(defn offset-for-column [col, row]
  (if (and pinky-15u
           (= col lastcol)
           (<= row last-15u-row)
           (>= row first-15u-row))
    3.7625
    0))

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [column-angle (* β (- centercol column))
        placed-shape (->> shape
                          (translate-fn [(offset-for-column column, row) 0 (- row-radius)])
                          (rotate-x-fn  (* α (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn  column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- row-radius)])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                (rotate-y-fn  column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))
        placed-shape-fixed (->> shape
                                (rotate-y-fn  (nth fixed-angles column))
                                (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                (translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 (+ row-radius (nth fixed-z column))])
                                (rotate-y-fn  fixed-tenting)
                                (translate-fn [0 (second (column-offset column)) 0])
                                )]
    (->> (case column-style
          :orthographic placed-shape-ortho
          :fixed        placed-shape-fixed
                        placed-shape)
         (rotate-y-fn  tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
    (fn [angle obj] (rotate angle [1 0 0] obj))
    (fn [angle obj] (rotate angle [0 1 0] obj))
    column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))


(def key-holes
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [(+ innercol-offset 1) (+ innercol-offset 2) (+ innercol-offset 3)] column)
                         (and (.contains [(+ innercol-offset 4) (+ innercol-offset 5)] column) extra-row (= ncols (+ innercol-offset 6)))
                         (and (.contains [(+ innercol-offset 4)] column) extra-row (= ncols (+ innercol-offset 5)))
                         (and inner-column (not= row cornerrow)(= column 0))
                         (not= row lastrow))]
           (->> single-plate
                (key-place column row)))))

(def caps
  (apply union
         (conj (for [column columns
               row rows
               :when (or (and (= column 0) (< row 3))
                         (and (.contains [1 2] column) (< row 4))
                         (.contains [3 4 5 6] column))]
               (->> (sa-cap (if (and pinky-15u (= column lastcol) (not= row lastrow)) 1.5 1))
                    (key-place column row)))
               (list (key-place 0 0 (sa-cap 1))
                 (key-place 0 1 (sa-cap 1))
                 (key-place 0 2 (sa-cap 1))))))



;placement for the innermost column
(def key-holes-inner
  (if inner-column
    (apply union
           (for [row innerrows]
             (->> single-plate
                  ;               (rotate (/ π 2) [0 0 1])
                  (key-place 0 row))))))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 4)
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))


; wide posts for 1.5u keys in the main cluster
(if pinky-15u
  (do (def wide-post-tr (translate [(- (/ mount-width 1.2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
    (def wide-post-tl (translate [(+ (/ mount-width -1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
    (def wide-post-bl (translate [(+ (/ mount-width -1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
    (def wide-post-br (translate [(- (/ mount-width 1.2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post)))
  (do (def wide-post-tr web-post-tr)
    (def wide-post-tl web-post-tl)
    (def wide-post-bl web-post-bl)
    (def wide-post-br web-post-br)))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))


(def connectors
  (apply union
         (concat
          ;; Row connections
          (for [column (range (+ innercol-offset 0) (dec ncols))
                row (range 0 nrows)
                :when (or (.contains [(+ innercol-offset 1) (+ innercol-offset 2) ] column)
                         (and (.contains [(+ innercol-offset 4) (+ innercol-offset 5)] column) extra-row (= ncols (+ innercol-offset 6)))
                         (and (.contains [(+ innercol-offset 4)] column) extra-row (= ncols (+ innercol-offset 5)))
                         (and inner-column (not= row cornerrow)(= column 0))
                         (not= row lastrow))]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))

          ;; Column connections
          (for [column columns
                row (range 0 lastrow)
                :when (or (.contains [(+ innercol-offset 1) (+ innercol-offset 2) (+ innercol-offset 3)] column)
                         (and (.contains [(+ innercol-offset 4) (+ innercol-offset 5)] column) extra-row (= ncols (+ innercol-offset 6)))
                         (and (.contains [(+ innercol-offset 4)] column) extra-row (= ncols (+ innercol-offset 5)))
                         (and inner-column (not= row cornerrow)(= column 0))
                         (not= row cornerrow))]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))

          ;; Diagonal connections
          (for [column (range 0 (dec ncols))
                row (range 0 lastrow)
                :when (or (.contains [(+ innercol-offset 1) (+ innercol-offset 2) (+ innercol-offset 3)] column)
                         (and (.contains [(+ innercol-offset 4) (+ innercol-offset 5)] column) extra-row (= ncols (+ innercol-offset 6)))
                         (and (.contains [(+ innercol-offset 4)] column) extra-row (= ncols (+ innercol-offset 5)))
                         (and inner-column (not= row cornerrow)(= column 0))
                         (not= row cornerrow))]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl))))

         [(triangle-hulls
             (key-place (+ innercol-offset 3) lastrow web-post-tr)
             (key-place (+ innercol-offset 3) lastrow web-post-br) 
             (key-place (+ innercol-offset 4) lastrow web-post-tl) 
         )]

          ))

(def inner-connectors
  (if inner-column
    (apply union
           (concat
            ;; Row connections
            (for [column (range 0 1)
                  row (range 0 (- nrows 2))]
              (triangle-hulls
               (key-place (inc column) row web-post-tl)
               (key-place column row web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place column row web-post-br)))

            ;; Column connections
            (for [row (range 0  cornerrow)]
              (triangle-hulls
               (key-place innercolumn row web-post-bl)
               (key-place innercolumn row web-post-br)
               (key-place innercolumn (inc row) web-post-tl)
               (key-place innercolumn (inc row) web-post-tr)))

            ;; Diagonal connections
            (for [column (range 0 (dec ncols))
                  row (range 0 2)]
              (triangle-hulls
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place (inc column) (inc row) web-post-tl)))))))

(def extra-connectors
  (if extra-row
    (apply union
           (concat
            (for [column (range 3 ncols)
                  row (range cornerrow lastrow)]
              (triangle-hulls
               (key-place column row web-post-bl)
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tl)
               (key-place column (inc row) web-post-tr)))

            (for [column (range 3 (dec ncols))
                  row (range cornerrow lastrow)]
              (triangle-hulls
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place (inc column) (inc row) web-post-tl)))

            (for [column (range 4 (dec ncols))
                  row (range lastrow nrows)]
              (triangle-hulls
               (key-place (inc column) row web-post-tl)
               (key-place column row web-post-tr)
               (key-place (inc column) row web-post-bl)
               (key-place column row web-post-br)))))))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(def thumborigin
  (map + (key-position (+ innercol-offset 1) cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))
;;;;;;;;;;;;;;;;
;; Mini Thumb ;;
;;;;;;;;;;;;;;;;

(defn minithumb-tr-place [shape]
  (->> shape
       (rotate (deg2rad 0) [1 0 0])
       (rotate (deg2rad -5) [0 1 0])
       (rotate (deg2rad  10) [0 0 1])
       (translate thumborigin)
       (translate [-8 -24 4])))

(defn minithumb-tl-place [shape]
  (->> shape
       (rotate (deg2rad  8) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  25) [0 0 1]) ; original 10
       (translate thumborigin)
       (translate [-31 -13 3]))) ; original 1.5u (translate [-32 -15 -2])))
(defn minithumb-mr-place [shape]
  (->> shape
       (rotate (deg2rad  -2) [1 0 0])
       (rotate (deg2rad -23) [0 1 0])
       (rotate (deg2rad  25) [0 0 1])
       (translate thumborigin)
       (translate [-22 -36 -3])))
(defn minithumb-br-place [shape]
  (->> shape
       (rotate (deg2rad -6) [1 0 0])
       (rotate (deg2rad -36) [0 1 0])
       (rotate (deg2rad  35) [0 0 1])
       (translate thumborigin)
       (translate [-35 -45 -12])))
(defn minithumb-bl-place [shape]
  (->> shape
       (rotate (deg2rad  2) [1 0 0])
       (rotate (deg2rad -36) [0 1 0])
       (rotate (deg2rad  35) [0 0 1])
       (translate thumborigin)
       (translate [-46 -22 -7]))) ;        (translate [-51 -25 -12])))

(defn minithumb-1x-layout [shape]
  (union
   (minithumb-mr-place shape)
   (minithumb-br-place shape)
   (minithumb-tl-place shape)
   (minithumb-bl-place shape)
   ))

(defn minithumb-15x-layout [shape]
  (union
   (minithumb-tr-place shape)))


(def minithumb
  (union
   (minithumb-1x-layout single-plate)
   (minithumb-15x-layout single-plate)))

(def minithumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
(def minithumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
(def minithumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def minithumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post))

(def minithumb-connectors
  (union
   (triangle-hulls    ; top two
    (minithumb-mr-place web-post-tr)
    (minithumb-mr-place web-post-br)
    (minithumb-tr-place minithumb-post-tl)
    (minithumb-tr-place minithumb-post-bl))
   (triangle-hulls    ; bottom two
    (minithumb-br-place web-post-tr)
    (minithumb-br-place web-post-br)
    (minithumb-mr-place web-post-tl)
    (minithumb-mr-place web-post-bl))
  ;;  (triangle-hulls
  ;;   (minithumb-tl-place web-post-tr)
  ;;   (minithumb-tl-place web-post-br)
  ;;   (minithumb-tr-place minithumb-post-tl))
   (triangle-hulls
    (minithumb-mr-place web-post-br)
    (minithumb-tr-place minithumb-post-bl)
    (minithumb-tr-place minithumb-post-br))
   (triangle-hulls    ; between top row and bottom row
    (minithumb-br-place web-post-tl)
    (minithumb-bl-place web-post-bl)
    (minithumb-br-place web-post-tr)
    (minithumb-bl-place web-post-br)
    (minithumb-mr-place web-post-tl)
    (minithumb-tl-place web-post-bl)
    (minithumb-mr-place web-post-tr)
    (minithumb-tl-place web-post-br)
    ;(minithumb-tr-place web-post-bl)
    (minithumb-mr-place web-post-tr)
    (minithumb-tr-place web-post-tl))
   (triangle-hulls    ; top two to the middle two, starting on the left
    (minithumb-tl-place web-post-tl)
    (minithumb-bl-place web-post-tr)
    (minithumb-tl-place web-post-bl)
    (minithumb-bl-place web-post-br)
    (minithumb-mr-place web-post-tr)
    (minithumb-tl-place web-post-bl)
    (minithumb-tl-place web-post-br)
    (minithumb-mr-place web-post-tr))
  ))

(def thumb 
        (union
          minithumb
          minithumb-connectors
        )
)


;;;;;;;;;;;;;;;;;;
;; Mount Holes
;;;;;;;;;;;;;;;;;;

;; hole for m3 screw
(def inner-radius 1.65)
(def out-radius 4)

(defn horizontal-mov [amount direction shape]
    (case direction
     :up (translate [0 amount 0] shape)
     :down (translate [0  (- 0 amount) 0] shape)
     :left (translate [(- 0 amount) 0 0] shape)
     :right (translate [amount 0 0] shape)
    )
)
; move shape from center of a switch hole up to top of the hole, 
; used for position mounting holes right outside of switch holes
(defn ext-out [direction z-off shape]
    (->> shape
        (horizontal-mov (+ (/ mount-height 2) (+ out-radius 1)) direction)
        (translate [0 0 (+ z-off (/ plate-thickness 2))])
    )
)

(defn plate-wall [direction]
    (case direction
     :up (hull web-post-tl web-post-tr)
     :down (hull web-post-bl web-post-br)
     :left (hull web-post-tl web-post-bl)
     :right (hull web-post-tr web-post-br)
    )
)

(defn mount-hole [direction]
    (difference
        (hull 
            (plate-wall direction)
            (ext-out direction 0 (cylinder out-radius plate-thickness))
        )
        (ext-out direction 0 (cylinder inner-radius (+ plate-thickness 2))) ;m3 screw hole
    )
)


(def finger-plate-holes
    (union
        (key-place 0 0 (mount-hole :up))
        (key-place (+  innercol-offset 1) lastrow (mount-hole :down))
        (key-place lastcol 0 (mount-hole :up))
        (key-place lastcol cornerrow ( mount-hole :down))
    )
)
(def thumb-plate-holes
    (union
        (minithumb-mr-place (mount-hole :down))
        (minithumb-bl-place (mount-hole :up))
        ;(thumb-place 2 -1 (mount-hole :down))
        ;(thumb-place 0 -0.9 (mount-hole :down))
    )
)

;;;;;;;;;;;;;;;;;
;; Lower Rack
;;;;;;;;;;;;;;;;;

(defn connect-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 2 1 shapes))))

(defn support-edge [direction z-off]
    (let [
          block (cube (* 2 out-radius) wall-thickness   plate-thickness)
          distance (+ (/ mount-height 2) (* out-radius 2)  wall-thickness)
      ](translate [0 0  (+ z-off (/ plate-thickness 2))]
        (case direction
        :up (horizontal-mov distance :up block)
        :down (horizontal-mov distance :down block)
        :left (horizontal-mov distance :left (rotate (/ π 2) [0 0 1] block))
        :right (horizontal-mov distance :right (rotate (/ π 2) [0 0 1] block))
        )
      )
    )
)

(defn support-m [direction z-off]
        (union
            (hull
                (support-edge direction z-off)
                (ext-out direction z-off (cylinder out-radius plate-thickness))
            )
            ;(cylinder inner-radius (+ plate-thickness 2))
            (support-edge direction (- z-off plate-thickness))

        )
)

(defn support-face [direction z-off]
    (let [
          block (cube (* 2 out-radius) wall-thickness   3)
          distance (+ (/ mount-height 2) (* out-radius 2)  wall-thickness)
      ](translate [0 0  (- z-off plate-thickness )]
        (case direction
        :up (horizontal-mov distance :up block)
        :down (horizontal-mov distance :down block)
        :left (horizontal-mov distance :left (rotate (/ π 2) [0 0 1] block))
        :right (horizontal-mov distance :right (rotate (/ π 2) [0 0 1] block))
        )
      )
    )
)

(def vdisc
    (->>(cylinder out-radius  (* wall-thickness 2))
                  (rotate (/ π 2) [1 0 0]))
)

; position of the lower right anchor
(def floor-anchor-poslr
    (let [
        down-pos [0  (- 0 mount-height) 0]
      ](map * (key-position lastcol cornerrow down-pos) [1 1 0])
    )
)

; position of the upper right anchor
(def floor-anchor-posur
    (let [
        up-pos [0 mount-height 0]
      ](map * (key-position lastcol 0 up-pos) [1 1 0])
    )
)

; position of midway support
(def midpoint-xz
    (map * (key-position (+ innercol-offset 2) centerrow [mount-width 0 0]) [1 0 1])
)

(def midpoint-lower
    (map + midpoint-xz (map * floor-anchor-poslr [0 1 0]))
)
(def midpoint-upper
    (map + midpoint-xz (map * floor-anchor-posur [0 1 0]))
)

; position of left support
(def leftpoint-xz
    (map * (key-position (+ innercol-offset 1) centerrow [0 0 0]) [1 0 1])
)

(def leftpoint-lower
    (map + leftpoint-xz (map * floor-anchor-poslr [0 1 0]))
)
(def leftpoint-upper
    (map + leftpoint-xz (map * floor-anchor-posur [0 1 0]))
)

; position of under thumb support
(def underthumb-pos
    (map + (map * floor-anchor-poslr [0 1 0])
        (map * (key-position innercol-offset centerrow [0 0 0]) [1 0 1])
        [0 (* wall-thickness 2) 0]
    )
)


(def finger-plate-arms
  (union
      ; lower right pole
      (hull
            (->> vdisc
                (translate floor-anchor-poslr))
            (key-place lastcol cornerrow (support-face :down (- -1.5 plate-thickness)))
      )
      (key-place lastcol cornerrow (support-m :down (- -0.1 plate-thickness)))

      ; upper right pole
      (hull
            (->> vdisc
                (translate floor-anchor-posur))
            (key-place lastcol 0 (support-face :up (- -1.5 plate-thickness)))
      )
      (key-place lastcol 0 (support-m :up (- -0.1 plate-thickness)))

    ; north edge supporting arms
    (connect-hulls
      (->> vdisc
          (translate floor-anchor-posur)
      )
      (->> vdisc
          (translate midpoint-upper)
      )
      (->> vdisc
          (translate leftpoint-upper)
      )
      (key-place 0 0 (support-face :up (- -1.5 plate-thickness)))
    )
    (key-place 0 0 (support-m :up (- -0.1 plate-thickness)))

    ; south edge supporting arms
    (connect-hulls
      (->> vdisc
          (translate floor-anchor-poslr)
      )
      (->> vdisc
          (translate midpoint-lower)
      )
      (->> vdisc
          (translate leftpoint-lower)
      )
      (key-place  (+  innercol-offset 1) lastrow (support-face :down (- -1.5 plate-thickness)))
    )
    (key-place  (+  innercol-offset 1) lastrow (support-m :down (- -0.1 plate-thickness)))

  )
)

(def thumb-plate-arms
    (union
        (hull 
          (->> vdisc
              (translate underthumb-pos))
          (minithumb-mr-place  (support-face :down (- -1.5 plate-thickness)))
        )
        (minithumb-mr-place (support-m :down (- -0.1 plate-thickness)))

        (hull 
          (->> vdisc
              (translate underthumb-pos))
          (minithumb-bl-place  (support-face :up (- -1.5 plate-thickness)))
        )
        (minithumb-bl-place (support-m :up (- -0.1 plate-thickness)))

        ; connecting from thumb support to finger plate support
        (hull
                (translate underthumb-pos vdisc)
            (->> vdisc
                (translate leftpoint-lower)
                (translate [0 (* wall-thickness 2) 0]))
        )
    )
)

(def finger-plate-arm-screw-holes
    (let [m3t (->>(cylinder inner-radius  20)
                  (rotate (/ π 2) [1 0 0]))]
        (union
          ; screw holes below mounting holes
          (key-place lastcol cornerrow (ext-out :down (- -0.1 plate-thickness) (cylinder inner-radius 10)))
          (key-place lastcol 0 (ext-out :up (- -0.1 plate-thickness) (cylinder inner-radius 10)))
          (key-place 0 0 (ext-out :up (- -0.1 plate-thickness) (cylinder inner-radius 10)))
          (key-place (+  innercol-offset 1) lastrow (ext-out :down (- -0.1 plate-thickness) (cylinder inner-radius 10)))

          ;holes for vertical beams between north and south beams
          (hull 
              (->> m3t
                  (translate leftpoint-upper)
                  )
              (->> m3t
                  (translate leftpoint-lower))
          )
          (hull 
              (->> m3t
                  (translate floor-anchor-posur)
                  )
              (->> m3t
                  (translate floor-anchor-poslr))
          )
        )
    )
)

(def thumb-plate-arm-screw-holes
    (let [m3t (->>(cylinder inner-radius  20)
                  (rotate (/ π 2) [1 0 0]))]
        (union
          ; screw holes below mounting holes
          ;(thumb-place 2 1 (ext-out :up (- -0.1 plate-thickness) (cylinder inner-radius 10)))
          ;(thumb-place 2 -1 (ext-out :down (- -0.1 plate-thickness) (cylinder inner-radius 10)))
          ;(thumb-place 0 -0.9 (ext-out :down (- -0.1 plate-thickness) (cylinder inner-radius 10)))
          (minithumb-mr-place (ext-out :down (- -0.1 plate-thickness) (cylinder inner-radius 10)))
          (minithumb-bl-place (ext-out :up (- -0.1 plate-thickness) (cylinder inner-radius 10)))

          ;holes for  beams connecting to finger plate arms
           (->> m3t
                  (translate leftpoint-lower)
                  )
        )
    )
)

(def finger-plate-rack
        (difference finger-plate-arms finger-plate-arm-screw-holes)
) 

(def thumb-plate-rack
        (difference thumb-plate-arms thumb-plate-arm-screw-holes)
)


(defn spacer [thickness z-off]
    (ext-out (cylinder out-radius thickness) z-off)
)

(def low-z -9)

(defn low-ball [radius]
    (translate [0 0 -16] (sphere radius))
)

(defn anchor [column row radius z-off]
    (let [
        kpos (map * (key-position column row [0 0 low-z]) [1 1 0])
        spos (map (fn [v snap] (* (Math/ceil (/ v snap)) snap)) kpos [10 1 1])
        fpos (map + spos [0 0 z-off])
    ]
        
        (translate fpos
              (sphere radius))
        
    )
)


;;;;;;;;;;;;;;;;;;
;; Final Export ;;
;;;;;;;;;;;;;;;;;;
(def finger-plate
    (union
        key-holes 
        key-holes-inner 
        connectors 
        extra-connectors 
        inner-connectors 
        finger-plate-holes
    )
)

(def thumb-plate
    (union thumb thumb-plate-holes)
)

(def dactyl-top-right
   (union
        finger-plate
        thumb-plate
        finger-plate-rack
        thumb-plate-rack
    )
)


(spit "things/assembled-right.scad"
      (write-scad dactyl-top-right))

(spit "things/fingers-right.scad"
      (write-scad finger-plate))
(spit "things/thumbs-right.scad"
      (write-scad thumb-plate))
(spit "things/finger-plate-rack-right.scad"
      (write-scad finger-plate-rack))
(spit "things/thumb-plate-rack-right.scad"
      (write-scad thumb-plate-rack))

(spit "things/assembled-left.scad"
      (write-scad (mirror [-1 0 0] dactyl-top-right)))

(spit "things/fingers-left.scad"
      (write-scad (mirror [-1 0 0] finger-plate)))
(spit "things/thumbs-left.scad"
      (write-scad (mirror [-1 0 0] thumb-plate)))
(spit "things/finger-plate-rack-left.scad"
      (write-scad (mirror [-1 0 0] finger-plate-rack)))
(spit "things/thumb-plate-rack-left.scad"
      (write-scad (mirror [-1 0 0] thumb-plate-rack)))

