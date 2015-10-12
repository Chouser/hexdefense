(ns n01se.hexdefense
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as dom]
            [goog.color :as color]))
(enable-console-print!)
(def ^:static impossible 9999)

(defn mk-canvas []
  (let [body (-> js/window .-document .-body)
        canvas (dom/createDom "canvas" (js-obj "width" 1000 "height" 500))]
    (dom/append body canvas)
    canvas))

(defprotocol ITrig
  (radius [this])
  (sin-cos [this i])
  (cell-center [this x y]))

(defn mk-trig [r pad pan-x pan-y]
  (let [scs (clj->js (for [t (range 0 (* 2 Math/PI) (/ Math/PI 3))]
                       [(Math/round (* r (Math/cos t)))
                        (Math/round (* r (Math/sin t)))]))
        sc1 (aget scs 1)]
    (reify ITrig
      (radius [this] r)
      (sin-cos [this i] (aget scs i))
      (cell-center [this x y]
        (let [xd (+ pad (* 2 (+ r (aget sc1 0))))
              yd (+ (/ pad 2) (aget sc1 1))]
          (array (+ pan-x (* x xd) (if (even? y) 0 (/ xd 2))) (+ pan-y (* y yd))))))))

(defn hexpath [ctx trig x y]
  (.beginPath ctx)
  (doseq [t (range 0 6)]
    (let [sc (sin-cos trig t)]
      (.lineTo ctx (+ x (aget sc 0)) (+ y (aget sc 1)))))
  (.closePath ctx))

(def red (array 255 0 0))
(def green (array 0 255 0))
(def blue (array 0 0 255))
(def white (array 255 255 255))
(def black (array 0 0 0))

(defn gradient [colors i]
  (if-not i
    "#aaa"
    (if (<= 0 i 1)
      (let [scaled (* i (dec (count colors)))
            segment (int scaled)]
        (color/rgbArrayToHex
         (color/blend (aget colors (inc segment))
                      (aget colors segment)
                      (rem scaled 1))))
      "#f0f")))

(defn icolor [i]
  (gradient (array white green blue red
                   white green blue red
                   white green blue red white) i))

(defn fillhex [ctx trig x y c]
  (let [[cx cy] (cell-center trig x y)]
    (set! (.-fillStyle ctx) c)
    (hexpath ctx trig cx cy)
    (.fill ctx)))

(defn grid [ctx trig board]
  (set! (.-fillStyle ctx) "#fff")
  (.fillRect ctx 0 0 1000 500)
  (doseq [[x col] (map-indexed list board)
          [y cell] (map-indexed list col)]
    (fillhex ctx trig x y (if cell "#ccf" "#333") #_(icolor (and cell (/ cell 40))))))

(defn neighbors [x-limit y-limit [x y]]
  (filter (fn [[x y]]
            (and (< -1 x x-limit) (< -1 y y-limit)))
          (into [[x (- y 2)] [x (- y 1)] [x (+ y 1)] [x (+ y 2)]]
                (if (even? y)
                  [[(- x 1) (- y 1)] [(- x 1) (+ y 1)]]
                  [[(+ x 1) (- y 1)] [(+ x 1) (+ y 1)]]))))

(defn walking-paths [ctx trig board]
  (set! (.-strokeStyle ctx) "#000")
  (set! (.-fillStyle ctx) "#000")
  (doseq [[x col] (map-indexed list board)
          [y cell] (map-indexed list col)]
    (when (get-in board [x y])
      (let [[cx cy] (cell-center trig x y)
            nbrs (neighbors (count board) (count col) [x y])
            nmin (apply min (keep #(get-in board %) nbrs))]

        (.beginPath ctx)
        (.arc ctx cx cy (* 0.2 (radius trig)) 0 (* Math/PI 2) true)
        (.closePath ctx)
        (.fill ctx)

        (when-not (== nmin impossible)

          (.save ctx)
          (.beginPath ctx)
          (.arc ctx cx cy (* 1.2 (radius trig)) 0 (* Math/PI 2) true)
          (.clip ctx)

          (doseq [[to-x to-y] (filter #(== nmin (get-in board %)) nbrs)]
            (.beginPath ctx)
            (.moveTo ctx cx cy)
            (let [[tcx tcy] (cell-center trig to-x to-y)]
              (.lineTo ctx tcx tcy))
            (.closePath ctx)
            (.stroke ctx))

          (.restore ctx))))))

(defn distance-board [x-limit y-limit]
  (let [empty-board (-> (vec (repeat x-limit (vec (repeat y-limit impossible))))
                        (assoc-in [7 10] nil)
                        (assoc-in [7 12] nil)
                        (assoc-in [7 14] nil)
                        (assoc-in [7 16] nil)
                        (assoc-in [7 18] nil)
                        (assoc-in [7 20] nil)
                        (assoc-in [7 22] nil)
                        (assoc-in [7 24] nil)

                        ;(assoc-in [7 13] nil)

                        (assoc-in [8 0] nil)
                        (assoc-in [8 2] nil)
                        (assoc-in [8 4] nil)
                        (assoc-in [8 6] nil)
                        (assoc-in [8 8] nil)
                        (assoc-in [8 10] nil)
                        (assoc-in [8 12] nil)
                        (assoc-in [8 14] nil)
                        (assoc-in [8 16] nil)
                        )]
    (loop [work-q (into cljs.core/PersistentQueue.EMPTY [[0 1]])
           board (assoc-in empty-board [0 0] 0)]
      (if (empty? work-q)
        board
        (let [xy (peek work-q)
              xys (pop work-q)
              nbrs (neighbors x-limit y-limit xy)
              my-val (get-in board xy)
              nmin (apply min (keep #(get-in board %) nbrs))
              new-val (min (inc nmin) my-val)]
          (if (or (= new-val my-val) (nil? nmin))
            (recur xys board)
            (recur (into xys nbrs)
                   (assoc-in board xy new-val))))))))

(defn busy [ctx]
  (let [trig (mk-trig 20 2 20 20)]
    (dotimes [_ 500000]
      (hexpath ctx trig 3 3))))

(defonce conn (repl/connect "http://localhost:9000/repl"))

(defonce canvas (mk-canvas))

(def app
  (atom
   {:ctx (.getContext canvas "2d")
    :trig (mk-trig 20 1.5 20 20)
    :distance-board (distance-board 15 26)
    :animate nil}))

(defn draw [app]
  (let [{:keys [ctx trig distance-board]} (swap! app assoc :animate false)]
    ;;(time (busy ctx))
    ;;(time (busy ctx))
    ;;(time (busy ctx))
    (grid ctx trig distance-board)
    (walking-paths ctx trig distance-board)))

(defn animate [app]
  (when-not (:animate @app)
    (swap! app assoc
           :animate (.requestAnimationFrame js/window (fn [] (draw app))))))

(animate app)

#_(set! (.-location js/window) (.-location js/window))
