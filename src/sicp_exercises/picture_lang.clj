(ns sicp_exercises.picture-lang
  (:import
    (java.awt Dimension Shape)
    (java.awt.geom AffineTransform Area Path2D$Double)
    (javax.swing JFrame JPanel)))

(defprotocol Paintable
  (paint [this]))

(defrecord Painter [^Shape shape w h]
  Paintable
  (paint [this]
    (let [frame (JFrame. "Picture0")
          panel (proxy [JPanel] []
                  (paintComponent [g]
                    (let [transform (AffineTransform/getScaleInstance
                                      (/ w (.getWidth (.getBounds shape)))
                                      (/ h (.getHeight (.getBounds shape))))]
                      (.draw g (.createTransformedShape transform shape)))
                    )
                  (getPreferredSize [] (Dimension. w h)))]
      (.add frame panel)
      (.pack frame)                                         ; Pack the frame to calculate its full size including title bar
      (let [titleBarHeight (- (.getHeight frame) (.getHeight panel))]
        (.setSize frame w (+ h titleBarHeight)))
      (.setVisible frame true))))

(defn beside [p1 p2]
  "将两个图形并排放置，p1在左半部分，p2在右半部分"
  (let [s1 (:shape p1)
        s2 (:shape p2)
        w1 (.getWidth (.getBounds s1))
        w2 (.getWidth (.getBounds s2))
        h1 (.getHeight (.getBounds s1))
        h2 (.getHeight (.getBounds s2))
        w (:w p1)
        h (:h p1)
        transformed-s1 (.createTransformedShape
                         (AffineTransform/getScaleInstance (/ w 2 w1) (/ h h1))
                         s1)
        transformed-s2 (.createTransformedShape
                         (AffineTransform/getScaleInstance (/ w 2 w2) (/ h h2))
                         s2)
        translated-s2 (.createTransformedShape
                        (AffineTransform/getTranslateInstance (/ w 2) 0)
                        transformed-s2)
        a (Area. transformed-s1)]
    (.add a (Area. translated-s2))
    (Painter. a w h)))

(defn below [p1 p2]
  "将两个图形上下放置，p1在下半部分，p2在上半部分"
  (let [s1 (:shape p1)
        s2 (:shape p2)
        w1 (.getWidth (.getBounds s1))
        w2 (.getWidth (.getBounds s2))
        h1 (.getHeight (.getBounds s1))
        h2 (.getHeight (.getBounds s2))
        w (:w p1)
        h (:h p1)
        transformed-s1 (.createTransformedShape
                         (AffineTransform/getScaleInstance (/ w w1) (/ h 2 h1))
                         s1)
        transformed-s2 (.createTransformedShape
                         (AffineTransform/getScaleInstance (/ w w2) (/ h 2 h2))
                         s2)
        translated-s1 (.createTransformedShape
                        (AffineTransform/getTranslateInstance 0 (/ h 2))
                        transformed-s1)
        a (Area. transformed-s2)]
    (.add a (Area. translated-s1))
    (Painter. a w h)))


(defn right-split [p n]
  (if (= n 0)
    p
    (let [smaller (right-split p (- n 1))]
      (beside p (below smaller smaller)))))

(defn up-split [p n]
  (if (= n 0)
    p
    (let [smaller (up-split p (- n 1))]
      (below p (beside smaller smaller)))))

(defn corner-split [p n]
  (if (= n 0)
    p
    (let [up (up-split p (- n 1))
          right (right-split p (- n 1))
          smaller (corner-split p (- n 1))]
      (beside (below p (beside up up)) (below (below right right) smaller)))))

(defn flip-vert [p]
  (let [s (:shape p)
        w (:w p)
        h (:h p)
        shape-height (.getHeight (.getBounds s))
        transform (AffineTransform.)]
    (.scale transform 1 -1)
    (.translate transform 0 (- shape-height))
    (Painter. (.createTransformedShape transform s) w h)))

(defn triangle [x1 x2 h]
  (let [s (Path2D$Double.)
        _ (.moveTo s x1 h)
        _ (.lineTo s (/ (+ x1 x2) 2) 0)
        _ (.lineTo s x2 h)
        _ (.closePath s)]
    s))

(comment
  (def p (Painter. (triangle 0 100 100) 400 400))
  (instance? java.awt.geom.Ellipse2D (:shape p))
  (:w p)
  (paint p)
  (paint (beside p p))
  (paint (below p p))
  (paint (below p (beside p p)))
  (paint (up-split p 4))
  (paint (flip-vert p))
  (paint (beside p (flip-vert p)))
  (paint (corner-split p 4))
  )
