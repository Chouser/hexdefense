(ns n01se.hexdefense
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as dom]
            [goog.color :as color]
            [goog.events :as events]))
(enable-console-print!)
(def ^:static impossible 9999)

(defn mk-canvas []
  (let [body (-> js/window .-document .-body)
        canvas (dom/createDom "canvas" (js-obj "width" 1000 "height" 500 "id" "canvas"))]
    (dom/append body canvas)
    canvas))

(defprotocol ITrig
  (sin-cos [this i])
  (cell-center [this x y])
  (mouse-cell [this [x y]]))

(defrecord Trig [radius pad pan-x pan-y, scs sc1]
  ITrig
  (sin-cos [this i] (aget scs i))
  (cell-center [this x y]
    (let [xd (* 2 (+ pad radius) (+ 1 (aget sc1 0)))
          yd (* (+ pad radius) (aget sc1 1))]
      (array (+ pan-x (* x xd) (if (even? y) 0 (/ xd 2))) (+ pan-y (* y yd)))))

  (mouse-cell [this [x y]]
    (let [xd (* (+ pad radius) (+ 1 (aget sc1 0)))
          yd (* (+ pad radius) (aget sc1 1))

          sx (Math/round (/ (- x pan-x) xd))
          sy (Math/round (/ (+ (* radius (aget sc1 1))
                               (- y pan-y (* 0.5 radius (aget sc1 1)))) yd))
          offset (if (even? sx) 0 1)

          mx (int (/ sx 2))
          my (+ offset (* 2 (int (/ (- sy offset) 2))))]
      [mx my])))

(defn mk-trig [r pad pan-x pan-y]
  (let [scs (clj->js (for [t (range 0 (* 2 Math/PI) (/ Math/PI 3))]
                       [(Math/cos t) (Math/sin t)]))
        sc1 (aget scs 1)]
    (Trig. r pad pan-x pan-y, scs sc1)))

(defn hexpath [ctx trig x y]
  (.beginPath ctx)
  (let [r (:radius trig)]
    (dotimes [t 6]
      (let [sc (sin-cos trig t)]
        (.lineTo ctx (+ x (* r (aget sc 0))) (+ y (* r (aget sc 1)))))))
  (.closePath ctx))

(def red (array 255 0 0))
(def green (array 0 255 0))
(def blue (array 80 80 255))
(def white (array 255 255 255))
(def black (array 0 0 0))
(def purple (array 200 0 200))

(defn gradient [colors i]
  (if-not i
    "#aaa"
    (if (and (<= 0 i) (< i 1))
      (let [scaled (* i (dec (count colors)))
            segment (int scaled)]
        (color/rgbArrayToHex
         (color/blend (aget colors (inc segment))
                      (aget colors segment)
                      (rem scaled 1))))
      "#f0f")))

(defn icolor [i]
  (gradient (array white green blue red
                   white green blue red) i))

(defn baddie-color [i]
  (gradient (array white purple) i))

(defn fillhex [ctx trig x y c]
  (let [[cx cy] (cell-center trig x y)]
    (set! (.-fillStyle ctx) c)
    (hexpath ctx trig cx cy)
    (.fill ctx)))

(defn draw-distance-gradient [ctx trig board tower-board]
  (doseq [[x col] (map-indexed list board)
          [y cell] (map-indexed list col)]
    (fillhex ctx trig x y (if (get-in tower-board [x y])
                            "#333"
                            (icolor (and cell (/ cell 100)))))))

(defn draw-towers [ctx trig tower-board]
  (set! (.-fillStyle ctx) "#fff")
  (.fillRect ctx 0 0 1000 500)
  (doseq [[x col] (map-indexed list tower-board)
          [y cell] (map-indexed list col)]
    (fillhex ctx trig x y (if cell "#333" "#cec"))))

(defn draw-hex-cursor [ctx trig xy]
  (let [[mx my] (mouse-cell trig xy)]
    (fillhex ctx trig mx my "rgba(255,255,128,0.9)")))

(defn neighbors [x-limit y-limit [x y]]
  (filter (fn [[x y]]
            (and (< -1 x x-limit) (< -1 y y-limit)))
          (into [[x (- y 2)] [x (- y 1)] [x (+ y 1)] [x (+ y 2)]]
                (if (even? y)
                  [[(- x 1) (- y 1)] [(- x 1) (+ y 1)]]
                  [[(+ x 1) (- y 1)] [(+ x 1) (+ y 1)]]))))

(defn to-direction-board [distance-board]
  (vec (for [[x col] (map-indexed list distance-board)]
         (vec (for [[y cell] (map-indexed list col)]
                (let [nbrs (neighbors (count distance-board) (count col) [x y])
                      my-val (get-in distance-board [x y])
                      nmin (apply min my-val (map #(get-in distance-board %) nbrs))]
                  (when (not= nmin impossible)
                    (doall (filter #(== nmin (get-in distance-board %)) nbrs)))))))))

(defn walking-paths [ctx trig direction-board tower-board]
  (set! (.-strokeStyle ctx) "#000")
  (set! (.-fillStyle ctx) "#000")
  (doseq [[x col] (map-indexed list direction-board)
          [y targets] (map-indexed list col)]
    (when-not (get-in tower-board [x y])
      (let [[cx cy] (cell-center trig x y)]

        (.beginPath ctx)
        (.arc ctx cx cy (* 0.2 (:radius trig)) 0 (* Math/PI 2) true)
        (.closePath ctx)
        (.fill ctx)

        (when (seq targets)

          (.save ctx)
          (.beginPath ctx)
          (.arc ctx cx cy (* 1.2 (:radius trig)) 0 (* Math/PI 2) true)
          (.clip ctx)

          (doseq [[to-x to-y] targets]
            (.beginPath ctx)
            (.moveTo ctx cx cy)
            (let [[tcx tcy] (cell-center trig to-x to-y)]
              (.lineTo ctx tcx tcy))
            (.closePath ctx)
            (.stroke ctx))

          (.restore ctx))))))

(defn draw-baddie [ctx trig frame baddie]
  (.beginPath ctx)
  (let [[x y] (:xy baddie)]
    (.save ctx)
    (.translate ctx x y)
    (.rotate ctx (/ x 25))
    (hexpath ctx (update trig :radius * 0.7) 0 0)
    (.restore ctx)
    #_(.arc ctx x y (* 0.4 (:radius trig)) 0 (* Math/PI 2) true))
  (.closePath ctx)
  (.fill ctx)
  (.stroke ctx))

(defn empty-board [x-limit y-limit]
  (vec (repeatedly x-limit #(vec (repeatedly y-limit (fn [] (when (zero? (rand-int 5)) :x)))))))

(defn to-distance-board [tower-board]
  (let [x-limit (count tower-board)
        y-limit (count (first tower-board))
        board (-> (vec (repeat x-limit (vec (repeat y-limit impossible))))
                  (assoc-in [0 0] 0))
        work (into cljs.core/PersistentQueue.EMPTY
                   (neighbors x-limit y-limit [0 0]))]
    (loop [work-q work, board board, i 0]
      (if (empty? work-q)
        (do
          (prn :distance-board :iter i)
          board)
        (let [xy (peek work-q)
              xys (pop work-q)
              nbrs (neighbors x-limit y-limit xy)
              my-val (get-in board xy)
              nmin (apply min (keep #(get-in board %) nbrs))
              new-val (min (inc nmin) my-val)]
          (if (or (= new-val my-val) (get-in tower-board xy))
            (recur xys board (inc i))
            (recur (into xys nbrs)
                   (assoc-in board xy new-val)
                   (inc i))))))))

(defn busy [ctx]
  (let [trig (mk-trig 20 2 20 20)]
    (dotimes [_ 500000]
      (hexpath ctx trig 3 3))))

(defonce conn (repl/connect "http://localhost:9000/repl"))

(defn draw [app]
  (let [{:keys [ctx trig distance-board direction-board tower-board
                mouse baddies frame target-board]}
        (swap! app assoc :draw false)]
    ;;(time (busy ctx))
    (set! (.-fillStyle ctx) "#fff")
    (.fillRect ctx 0 0 1000 500)

    (draw-towers ctx trig tower-board)
    ;;(draw-distance-gradient ctx trig distance-board tower-board)
    ;;(walking-paths ctx trig direction-board tower-board)
    (set! (.-fillStyle ctx) "#a5a")
    (set! (.-strokeStyle ctx) "#fff")
    (set! (.-lineWidth ctx) "2")
    (doseq [[id baddie] baddies]
      (set! (.-fillStyle ctx) (baddie-color (/ (:hp baddie) 10000)))
      (draw-baddie ctx trig frame baddie))

    (set! (.-strokeStyle ctx) "#f00")
    (set! (.-lineWidth ctx) "5")
    (doseq [[x col] (map-indexed list target-board)
            [y id] (map-indexed list col)
            :when id]
      (.beginPath ctx)
      (let [[fx fy] (cell-center trig x y)
            [tx ty] (:xy (baddies id))]
        (.moveTo ctx fx fy)
        (.lineTo ctx tx ty))
      (.closePath ctx)
      (.stroke ctx))

    (draw-hex-cursor ctx trig mouse)))

(defn schedule-draw [app]
  (when-not (:draw @app)
    (swap! app assoc
           :draw (.requestAnimationFrame js/window (fn [] (draw app))))))

(declare animate)

(defn schedule-animate [app]
  (when-not (:animate @app)
    (swap! app assoc
           :animate (.setTimeout js/window (fn [] (animate app)) 30))))

(defn add-baddie [app]
  (schedule-animate app)
  (schedule-draw app)
  (let [x 14
        y (rand-int 26)
        id (rand-int 100000)
        baddie {:from-cell [x y],
                :xy (cell-center (:trig @app) x y),
                :hp 10000}]
    (swap! app update :baddies assoc id baddie))
  (.setTimeout js/window (fn [] (add-baddie app)) 500))

(defn distance**2 [x1 y1 x2 y2]
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (+ (* dx dx) (* dy dy))))

(defn animate [app]
  (let [{:keys [trig baddies direction-board]} (swap! app assoc :animate false)]
    (when (seq baddies)
      (schedule-animate app)
      (schedule-draw app))

    ;; warning -- time shear
    (swap! app update :frame inc)
    (swap! app update :baddies
           (fn [baddies]
             (into {}
              (for [[id {:as baddie [x y] :xy, :keys [hp to-cell from-cell]}] baddies
                    :when (and (< 0 x 1000)
                               (< 0 y 500)
                               (not= from-cell [0 0])
                               (< 0 hp))]
                (let [to-cell (or to-cell (rand-nth (get-in direction-board from-cell))
                                  (prn :problem-with id baddie (get-in direction-board from-cell))
                                  [0 0])
                      [cx cy] to-cell
                      [tx ty] (cell-center trig cx cy)
                      d (Math/sqrt (distance**2 x y tx ty))
                      scale (/ 1 d)
                      speed 4
                      nx (+ x (* speed scale (- tx x)))
                      ny (+ y (* speed scale (- ty y)))
                      [from-cell to-cell] (if (> d (* 2 speed))
                                            [from-cell to-cell]
                                            [to-cell nil])]
                  [id
                   (assoc baddie
                     :xy (array nx ny)
                     :from-cell from-cell
                     :to-cell to-cell)])))))
    (let [{:keys [baddies distance-board]} @app
          range**2 (* 4 (:radius trig) 4 (:radius trig))]
      (swap! app
             (fn [app]
               (let [tb (:target-board app)
                     badatom (atom (:baddies app))]
                 (assoc app
                   :target-board
                   (doall
                    (for [[x col] (map-indexed list (:tower-board app))]
                      (doall
                       (for [[y cell] (map-indexed list col)]
                         (when cell
                           (let [old-target-id (get-in tb [x y])
                                 [fx fy] (cell-center trig x y)
                                 ;;old-dist**2 (distance**2 fx fy ..)
                                 old-target (baddies old-target-id)
                                 target-id (or (when (and old-target
                                                          (< (apply distance**2 fx fy (:xy old-target)) range**2))
                                                 old-target-id)
                                               (first
                                                (for [[id {:as baddie [bx by] :xy}] baddies
                                                      :when (< (distance**2 fx fy bx by) range**2)]
                                                  id)))]
                             (when target-id
                               (swap! badatom update-in [target-id :hp] - 40))
                             target-id))))))
                   :baddies @badatom)))))))

(defn re*load []
  (when-let [canvas (dom/getElement "canvas")]
    (dom/removeNode canvas))

  (let [canvas (mk-canvas)
        tower-board (empty-board 15 26)
        distance-board (to-distance-board tower-board)
        app (atom
             {:ctx (.getContext canvas "2d")
              :trig (mk-trig 20 1 20 20)
              :tower-board tower-board
              :distance-board distance-board
              :direction-board (to-direction-board distance-board)
              :target-board nil
              :baddies {}
              :draw nil
              :frame 0
              :animate nil})]
    (events/listen
     canvas "mousemove"
     (fn [event]
       (swap! app assoc :mouse
              [(.-offsetX event)
               (.-offsetY event)])
       (schedule-draw app)))
    (events/listen
     canvas "click"
     (fn [event]
       (let [mxy (mouse-cell (:trig @app) [(.-offsetX event) (.-offsetY event)])]
         (swap! app (fn [app-val]
                      (let [tower-board (update-in (:tower-board app-val)
                                                   mxy #(if % nil :x))
                            distance-board (to-distance-board tower-board)]
                        (assoc app-val
                          :tower-board tower-board
                          :distance-board distance-board
                          :direction-board (to-direction-board distance-board)))))
         (schedule-draw app))))
    (add-baddie app)))

(re*load)
