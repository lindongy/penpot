;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.geom.shapes.grid-layout.layout-data
  (:require
   [app.common.types.shape.layout :as ctl]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes.points :as gpo]))

(defn layout-bounds
  [{:keys [layout-padding] :as parent} shape-bounds]
  (let [[pad-top pad-right pad-bottom pad-left] (ctl/paddings parent)]
    (gpo/pad-points shape-bounds pad-top pad-right pad-bottom pad-left)))

#_(defn set-sample-data
  [parent children]

  (let [parent (assoc parent
                      :layout-grid-columns
                      [{:type :percent :value 25}
                       {:type :percent :value 25}
                       {:type :fixed :value 100}
                       ;;{:type :auto}
                       ;;{:type :flex :value 1}
                       ]

                      :layout-grid-rows
                      [{:type :percent :value 50}
                       {:type :percent :value 50}
                       ;;{:type :fixed :value 100}
                       ;;{:type :auto}
                       ;;{:type :flex :value 1}
                       ])

        num-rows (count (:layout-grid-rows parent))
        num-columns (count (:layout-grid-columns parent))

        layout-grid-cells
        (into
         {}
         (for [[row-idx _row] (d/enumerate (:layout-grid-rows parent))
               [col-idx _col] (d/enumerate (:layout-grid-columns parent))]
           (let [[_bounds shape] (nth children (+ (* row-idx num-columns) col-idx) nil)
                 cell-data {:id (uuid/next)
                            :row (inc row-idx)
                            :column (inc col-idx)
                            :row-span 1
                            :col-span 1
                            :shapes (when shape [(:id shape)])}]
             [(:id cell-data) cell-data])))

        parent (assoc parent :layout-grid-cells layout-grid-cells)]

    [parent children]))

(defn calculate-initial-track-size
  [total-value {:keys [type value] :as track}]

  (let [size (case type
               :percent
               (let [value (/ (* total-value value) 100) ]
                 value)

               :fixed
               value

               ;; flex, auto
               0
               )]
    (assoc track :size size)))


(defn calc-layout-data
  [parent _children transformed-parent-bounds]

  (let [
        hv     #(gpo/start-hv transformed-parent-bounds %)
        vv     #(gpo/start-vv transformed-parent-bounds %)

        layout-bounds (layout-bounds parent transformed-parent-bounds)

        bound-height (gpo/height-points layout-bounds)
        bound-width  (gpo/width-points layout-bounds)
        bound-corner (gpo/origin layout-bounds)

        grid-columns (:layout-grid-columns parent)
        grid-rows    (:layout-grid-rows parent)
        
        ;; Initialize tracks
        column-tracks
        (->> grid-columns
             (map (partial calculate-initial-track-size bound-width)))

        row-tracks
        (->> grid-rows
             (map (partial calculate-initial-track-size bound-height)))

        ;; Go through cells to adjust auto sizes
        ;; TODO

        ;; Once auto sizes have been calculated we get calculate the `fr` unit with the remainining size and adjust the size
        ;; TODO


        ;; Adjust final distances
        calc-tracks-total-size
        (fn [acc {:keys [size]}]
          (+ acc size))

        ;;calc-tracks-total-size
        ;;(fn [[result next-distance] data]
        ;;  (let [result (conj result (assoc data :distance next-distance))
        ;;        next-distance (+ next-distance (:value data))]
        ;;    [result next-distance]))

        [row-gap column-gap] (ctl/gaps parent)

        column-total-size (->> column-tracks (reduce calc-tracks-total-size 0))
        column-total-gap  (* column-gap (dec (count column-tracks)))
        row-total-size    (->> row-tracks (reduce calc-tracks-total-size 0))
        row-total-gap     (* row-gap (dec (count row-tracks)))

        column-margin 0
        row-margin 0

        start-p
        (cond-> bound-corner
          (= :end (:layout-align-content parent))
          (gpt/add (hv (- bound-width (+ column-total-size column-total-gap))))

          (= :center (:layout-align-content parent))
          (gpt/add (hv (/ (- bound-width (+ column-total-size column-total-gap)) 2)))

          (= :end (:layout-justify-content parent))
          (gpt/add (vv (- bound-height (+ row-total-size row-total-gap))))
          
          (= :center (:layout-justify-content parent))
          (gpt/add (vv (/ (- bound-height (+ row-total-size row-total-gap)) 2))))

        column-tracks
        (->> column-tracks
             (reduce (fn [[tracks start-p] {:keys [size] :as track}]
                       [(conj tracks (assoc track :start-p start-p))
                        (gpt/add start-p (hv (+ size column-gap)))])
                     [[] start-p])
             (first))

        row-tracks
        (->> row-tracks
             (reduce (fn [[tracks start-p] {:keys [size] :as track}]
                       [(conj tracks (assoc track :start-p start-p))
                        (gpt/add start-p (vv (+ size row-gap)))])
                     [[] start-p])
             (first))
        
        shape-cells
        (into {}
              (mapcat (fn [[_ cell]]
                        (->> (:shapes cell)
                             (map #(vector % cell)))))
              (:layout-grid-cells parent))
        ]

    {:origin start-p
     :layout-bounds layout-bounds
     :row-tracks row-tracks
     :column-tracks column-tracks
     :shape-cells shape-cells}))

(defn get-cell-data
  [{:keys [origin layout-bounds row-tracks column-tracks shape-cells]} transformed-parent-bounds [_ child]]

  (let [grid-cell (get shape-cells (:id child))]

    (when (some? grid-cell)
      (let [column (nth column-tracks (dec (:column grid-cell)) nil)
            row (nth row-tracks (dec (:row grid-cell)) nil)

            column-start-p (:start-p column)
            row-start-p (:start-p row)

            start-p (gpt/add origin
                             (gpt/add
                              (gpt/to-vec origin column-start-p)
                              (gpt/to-vec origin row-start-p)))
            ]

        (assoc grid-cell :start-p  start-p)))))
