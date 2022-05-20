(ns dactyl-keyboard.lightcycle
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            ;;[dactyl-keyboard.util :refer :all]
            [unicode-math.core :refer :all]
            [clojure.java.io :as io]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 6)
(def ncols 6)

(def α (/ π 12))                        ; curvature of the columns
(def β (/ π 108))                        ; curvature of the rows
(def centerrow (- nrows 3))             ; controls front-back tilt
(def centercol 4)                       ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 12))            ; or, change this for more precise tenting control

(def pinky-15u false)                   ; controls whether the outer column uses 1.5u keys
(def first-15u-row 0)                   ; controls which should be the first row to have 1.5u keys on the outer column
(def last-15u-row 3)                    ; controls which should be the last row to have 1.5u keys on the outer column

(def extra-row true)                    ; adds an extra bottom row to the outer columns
(def inner-column false)                ; adds an extra inner column (two less rows than nrows)

(def column-style :standard)

(defn column-offset [column]
  (if inner-column
    (cond (<= column 1) [0 -2 0]
          (= column 3) [0 2.82 -4.5]
          (= column 5) [0 -12 5.64]    ; original [0 -5.8 5.64]
          (> column 5) [0 -12 7.8]
          :else [0 0 0])
    (cond (= column 2) [0 2.82 -4.5]
          (= column 4) [0 -12 5.64]    ; original [0 -5.8 5.64]
          (> column 4) [0 -12 7.8]
          :else [0 0 0])))

(def thumb-offsets [-42 -16 9])

(def keyboard-z-offset 8)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

(def extra-width 2)                     ; extra space between the base of keys; original= 2
(def extra-height 0.5)                  ; original= 0.5

(def wall-z-offset -8)                  ; length of the first downward-sloping part of the wall (negative)
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

(def keyswitch-height 14.2) ;; Was 14.1, then 14.25
(def keyswitch-width 14.2)

(def sa-profile-key-height 12.7)

(def plate-thickness 4)
(def side-nub-thickness 4)
(def retention-tab-thickness 1.5)
(def retention-tab-hole-thickness (- (+ plate-thickness 0.5) retention-tab-thickness))
(def mount-width (+ keyswitch-width 3))
(def mount-height (+ keyswitch-height 3))

(def alps-width 15.6)
(def alps-notch-width 15.5)
(def alps-notch-height 1)
(def alps-height 13)

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
    3.8625
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
             (key-place (+ innercol-offset 4) lastrow web-post-bl) 
         )]

          ))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;

(def thumborigin
  (map + (key-position (+ innercol-offset 1) cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))

(defn thumb-place [column row shape]
  (let [cap-top-height (+ plate-thickness sa-profile-key-height)
        α (/ π 9)
        row-radius (+ (/ (/ (+ mount-height 1) 2)
                         (Math/sin (/ α 2)))
                      cap-top-height)
        β (/ π 18)
        column-radius (+ (/ (/ (+ mount-width 2) 2)
                            (Math/sin (/ β 2)))
                         cap-top-height)
        #_(+ (/ (/ (+ pillar-width 5) 2)
                (Math/sin (/ β 2)))
             cap-top-height)]
    (->> shape
         (translate [0 0 (- row-radius)])
         (rotate (* α row) [1 0 0])
         (translate [0 0 row-radius])
         (translate [0 column-radius 0])
         (rotate (* column β) [0 0 1])
         (translate [0 (- column-radius)  0])
         (translate [mount-width 0 0])
         (rotate (/ π 9) [0 0 1])
         (rotate (/ π 15) [1 1 0])
         (rotate (/ π 15) [1 -1 0])
         (translate [0 0 (* 2 row)])
         (translate thumborigin))))

(defn thumb-layout [shape]
  (union
   (thumb-place 0 -1/8 shape)
   (thumb-place 0.5 7/8 shape)

   (thumb-place 1 -1/8 shape)
   (thumb-place 1.3 7/8 shape)

   (thumb-place 2 -1/8 shape)
   (thumb-place 2.1 7/8 shape)   
   ))


(def thumb-connectors
     (union

      (triangle-hulls (thumb-place 0 -1/8 web-post-tl)
                      (thumb-place 0 -1/8 web-post-bl)
                      (thumb-place 1 -1/8 web-post-br)
                      (thumb-place 0 -1/8 web-post-tl)
                      (thumb-place 1 -1/8 web-post-tr)
                      (thumb-place 0.5 7/8 web-post-br))

      (triangle-hulls (thumb-place 0.5 7/8 web-post-br)
                      (thumb-place 0.5 7/8 web-post-bl)
                      (thumb-place 1 -1/8 web-post-tr)
                      (thumb-place 1 -1/8 web-post-tl))

      (triangle-hulls (thumb-place 1.3 7/8 web-post-br)
                      (thumb-place 1.3 7/8 web-post-bl)
                      (thumb-place 2 -1/8 web-post-tr)
                      (thumb-place 2 -1/8 web-post-tl))

      (triangle-hulls (thumb-place 1.3 7/8 web-post-br)
                      (thumb-place 1.3 7/8 web-post-bl)
                      (thumb-place 2 -1/8 web-post-tr)
                      (thumb-place 2 -1/8 web-post-tl))

      (triangle-hulls (thumb-place 2 -1/8 web-post-br)
                      (thumb-place 1 -1/8 web-post-bl)
                      (thumb-place 2 -1/8 web-post-tr)
                      (thumb-place 1 -1/8 web-post-tl)
                      (thumb-place 1.3 7/8 web-post-br)
                      (thumb-place 0.5 7/8 web-post-bl)
                      (thumb-place 1.3 7/8 web-post-tr)
                      (thumb-place 0.5 7/8 web-post-tl)
                      )

      (triangle-hulls (thumb-place 2.1 7/8 web-post-bl)
                      (thumb-place 2 -1/8 web-post-tl)
                      (thumb-place 2.1 7/8 web-post-br)
                      (thumb-place 1.3 7/8 web-post-bl)
                      (thumb-place 2.1 7/8 web-post-tr)
                      (thumb-place 1.3 7/8 web-post-tl)
                      )

      ;;Connecting the thumb to everything
      (triangle-hulls (thumb-place 0 -1/8 web-post-br)
                      (key-place 1 lastrow web-post-bl)
                      (thumb-place 0 -1/8 web-post-tr)
                      (key-place 1 lastrow web-post-tl)
                      (key-place 1 cornerrow web-post-bl)
                      (thumb-place 0 -1/8 web-post-tr)
                      (key-place 0 cornerrow web-post-br)
                      (key-place 0 cornerrow web-post-bl)
                      (thumb-place 0 -1/8 web-post-tr)
                      (thumb-place 0 -1/8 web-post-tl)
                      (key-place 0 cornerrow web-post-bl)
                      (thumb-place 1 -1/8 web-post-tr)
                      (thumb-place 0.5 7/8 web-post-br)
                      (key-place 0 cornerrow web-post-bl)
                      (key-place 0 (- cornerrow 1) web-post-bl)
                      (thumb-place 0.5 7/8 web-post-br)
                      (thumb-place 0.5 7/8 web-post-tr)
                      )))

(def thumb
  (union
   (thumb-layout single-plate)
   (color [1 0 0] thumb-connectors)
  )
)


(def outpostr 
  (+ (/ plate-thickness 2) post-size)
)
(def outpost-disc
  (->> (difference
         (translate [0 post-size 0] (with-fn 30 (cylinder outpostr post-size)))
         (translate [0 (/ plate-thickness -2) 0] (cube plate-thickness plate-thickness (* post-size 2)))
       )
      (rotate (/ π 2) [0 1 0] )
  )
)

(def outpost-up1
  (translate [(- (/ keyswitch-width -2) 1.5) (+ 1.5 (/ keyswitch-height 2)) (/ plate-thickness 2)] outpost-disc)
)

(def outpost-up2
  (translate [(+ 1.5 (/ keyswitch-width 2)) (+ 1.5 (/ keyswitch-height 2)) (/ plate-thickness 2)] outpost-disc)
)

(def outpost-down1
  (translate [(- (/ keyswitch-width -2) 1.5) (- (/ keyswitch-height -2) 1.5) (/ plate-thickness 2)] (rotate π [0 0 1] outpost-disc))
)

(def outpost-down2
  (translate [(+ 1.5 (/ keyswitch-width 2)) (- (/ keyswitch-height -2) 1.5) (/ plate-thickness 2)] (rotate π [0 0 1] outpost-disc))
)

(def outpost-left1
  (translate [(- (/ keyswitch-width -2) 1.5) (+ 1.5 (/ keyswitch-height 2)) (/ plate-thickness 2)] (rotate (/ π 2) [0 0 1] outpost-disc))
)

(def outpost-left2
  (translate [(- (/ keyswitch-width -2) 1.5) (- (/ keyswitch-height -2) 1.5) (/ plate-thickness 2)] (rotate (/ π 2) [0 0 1] outpost-disc))
)

(def outpost-right1
  (translate [(+ 1.5 (/ keyswitch-width 2)) (+ 1.5 (/ keyswitch-height 2)) (/ plate-thickness 2)] (rotate (/ π -2) [0 0 1] outpost-disc))
)

(def outpost-right2
  (translate [(+ 1.5 (/ keyswitch-width 2)) (- (/ keyswitch-height -2) 1.5) (/ plate-thickness 2)] (rotate (/ π -2) [0 0 1] outpost-disc))
)

(def outpost-wall
  (union
    ;; top
    (for [col columns]
        (hull
          (key-place col 0 outpost-up1)
          (key-place col 0 outpost-up2)
        )
    )
    (for [col columns
          :when (not= col lastcol)
          ]
        (hull
          (key-place col 0 outpost-up2)
          (key-place (+ 1 col) 0 outpost-up1)
        )
    )

    (hull
        (key-place lastcol 0 outpost-up2)
        (key-place lastcol 0 outpost-right1)
    )

    ;; right
    (for [row rows]
      (hull
        (key-place lastcol row outpost-right1)
        (key-place lastcol row outpost-right2)
      )
    )
    (for [row rows :when (not= row lastrow)]
      (hull
        (key-place lastcol row outpost-right2)
        (key-place lastcol (+ 1 row) outpost-right1)
      )
    )
    (hull
        (key-place lastcol lastrow outpost-right2)
        (key-place lastcol lastrow outpost-down2)
    )

    ;; bottom
    (for [col (range (+ innercol-offset 1) ncols)]
        (hull
          (key-place col lastrow outpost-down1)
          (key-place col lastrow outpost-down2)
        )
    )
    (for [col (range (+ innercol-offset 1) lastcol)]
        (hull
          (key-place col lastrow outpost-down2)
          (key-place (+ 1 col) lastrow outpost-down1)
        )
    )
    ;; left
    (for [row (range 0 cornerrow)]
      (hull
        (key-place innercol-offset row outpost-left1)
        (key-place innercol-offset row outpost-left2)
      )
    )
    (for [row (range 0 (- cornerrow 1))]
      (hull
        (key-place innercol-offset row outpost-left2)
        (key-place innercol-offset (+ 1 row) outpost-left1)
      )
    )
    (hull
        (key-place innercol-offset 0 outpost-up1)
        (key-place innercol-offset 0 outpost-left1)
    )

    ;; Around thumb keys
    (hull
        (key-place innercol-offset (- cornerrow 1) outpost-left2)
        (thumb-place 0.5 7/8 outpost-up2)
    )
    (hull
        (thumb-place 0.5 7/8 outpost-up2)
        (thumb-place 0.5 7/8 outpost-up1)
    )
    (hull
        (thumb-place 0.5 7/8 outpost-up1)
        (thumb-place 1.3 7/8 outpost-up2)
    )
    (hull
        (thumb-place 1.3 7/8 outpost-up2)
        (thumb-place 1.3 7/8 outpost-up1)
    )
    (hull
        (thumb-place 1.3 7/8 outpost-up1)
        (thumb-place 2.1 7/8 outpost-up2)   
    )
    (hull
        (thumb-place 2.1 7/8 outpost-up2)   
        (thumb-place 2.1 7/8 outpost-up1)   
    )
    (hull
        (thumb-place 2.1 7/8 outpost-up1)   
        (thumb-place 2.1 7/8 outpost-left1)   
    )
    (hull
        (thumb-place 2.1 7/8 outpost-left1)   
        (thumb-place 2.1 7/8 outpost-left2)   
    )
    (hull
        (thumb-place 2.1 7/8 outpost-left2)   
        (thumb-place 2 -1/8 outpost-left1)   
    )
    (hull
        (thumb-place 2 -1/8 outpost-left1)   
        (thumb-place 2 -1/8 outpost-left2)   
    )
    (hull
        (thumb-place 2 -1/8 outpost-left2)   
        (thumb-place 2 -1/8 outpost-down1)   
    )
    (hull
        (thumb-place 2 -1/8 outpost-down1)   
        (thumb-place 2 -1/8 outpost-down2)   
    )
    (hull
        (thumb-place 2 -1/8 outpost-down2)   
        (thumb-place 1 -1/8 outpost-down1)   
    )
    (hull
        (thumb-place 1 -1/8 outpost-down1)   
        (thumb-place 1 -1/8 outpost-down2)   
    )
    (hull
        (thumb-place 1 -1/8 outpost-down2)   
        (thumb-place 0 -1/8 outpost-down1)   
    )
    (hull
        (thumb-place 0 -1/8 outpost-down1)   
        (thumb-place 0 -1/8 outpost-down2)   
    )
    (hull
        (thumb-place 0 -1/8 outpost-down2)   
        (key-place (+ innercol-offset 1) lastrow outpost-down1)
    )
  )
)


;;;;;;;;;;;;;;;;;;
;; Mount Holes
;;;;;;;;;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (/ height 2)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(def outerd
  (with-fn 30 (sphere 3.5))
)

(def innerd
  (with-fn 30 (sphere 2))
)

(defn round-pole [height radius]
  (translate [0 0 (- 0 height)] (bottom-hull (translate [0 0 height] (with-fn 30 (sphere radius)))))
)

(def pole-shell
  (round-pole 12 3.5)
)

(def pole-hole
  (round-pole 20 2)
)

(def support-poles
  (union
    (translate (key-position innercol-offset 0 [(- (/ mount-width -2) 2) (- (/ mount-width -2) 1) 1]) pole-shell)
    (translate (key-position lastcol 0 [(+ (/ mount-width 2) 2) (- (/ mount-width -2) 2) 1]) pole-shell)
    (translate (key-position lastcol cornerrow [(+ (/ mount-width 2) 2) (- (/ mount-width -2) 2) 1]) pole-shell)
    (bottom-hull (translate [0 0 1.5] (thumb-place 2.2 3/8 outerd)))
  )
)

(def support-screw
  (union
    (translate (key-position innercol-offset 0 [(- (/ mount-width -2) 2) (- (/ mount-width -2) 1) 1]) pole-hole)
    (translate (key-position lastcol 0 [(+ (/ mount-width 2) 2) (- (/ mount-width -2) 2) 1]) pole-hole)
    (translate (key-position lastcol cornerrow [(+ (/ mount-width 2) 2) (- (/ mount-width -2) 2) 1]) pole-hole)
    (bottom-hull (translate [0 0 -1] (thumb-place 2.2 3/8 innerd)))
  )
)

;;;;;;;;;;;;;;;;;;
;; Final Export ;;
;;;;;;;;;;;;;;;;;;

(def dactyl-top-right
  (difference
    (union
      key-holes
      connectors
      thumb
      outpost-wall
      support-poles
    )
    support-screw
  )
)

(spit "things/lightcycle-right.scad"
      (write-scad dactyl-top-right))

(spit "things/lightcycle-left.scad"
      (write-scad (mirror [-1 0 0] dactyl-top-right)))
