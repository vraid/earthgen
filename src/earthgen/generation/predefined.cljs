(ns earthgen.generation.predefined
  (:require [earthgen.generation.operations :as ops]))

(defn supercontinents []
  (let
   [amplitude 4000
    sea-level 500
    glacial-sea-level (- sea-level 200)]
    (ops/terrain nil
                 sea-level
                 (ops/heightmap-let
                  [["landform" (ops/heightmap {:granularity 1 :irregularity 0.2 :amplitude amplitude})]
                   ["sea-level-adjusted" (ops/op- "landform" glacial-sea-level)]
                   ["flatten" (ops/op* (ops/op- "landform" sea-level)
                                       (ops/sigmoid-at [0 0.75] [0.2 sea-level] [0.8 (+ sea-level 200)] "landform"))]
                   ["land?" (ops/sigmoid-at [0 1] [0.2 (- glacial-sea-level 200)] [0.8 sea-level] "landform")]
                   ["ocean?" (ops/sigmoid-at [0 1] [0.1 -200] [0.9 -400] "sea-level-adjusted")]
                   ["highlands" (ops/abs (ops/heightmap {:granularity 1 :irregularity 0.3 :amplitude 1}))]
                   ["highlands?" (ops/sigmoid-at [0 1] [0.8 0.14] [0.1 0.16] "highlands")]
                   ["highland-elevation" (ops/sigmoid-at [600 1000] [0.1 0] [0.8 500] (ops/abs (ops/heightmap {:granularity 2 :irregularity 0.2 :amplitude 1000})))]
                   ["coastal?" (ops/sigmoid-at [0 1] [0.2 300] [0.8 150] (ops/abs "sea-level-adjusted"))]
                   ["coast-sign" (ops/sigmoid-at [-1 1] [0.2 -50] [0.8 50] (ops/max "sea-level-adjusted" (ops/min 0 (ops/heightmap {:granularity 2 :irregularity 0.2 :amplitude 1000}))))]
                   ["coastal-mountain-elevation" (ops/max 0 (ops/heightmap {:granularity 3 :irregularity 0.2 :amplitude 6000}))]

                   ["mountains" (ops/abs (ops/heightmap {:granularity 2 :irregularity 0.2 :amplitude 1}))]
                   ["mountains?" (ops/sigmoid-at [0 1] [0.8 0.02] [0.1 0.1] "mountains")]
                   ["mountain-elevation" (ops/abs (ops/heightmap {:granularity 2 :irregularity 0.2 :amplitude 5000}))]]

                  (ops/op+ (ops/op- "landform" "flatten")
                           (ops/op* 3 "ocean?" "sea-level-adjusted")
                           (ops/op* "land?" "highlands?" "highland-elevation")
                           (ops/op* (ops/op- 1 "coastal?") "land?" "mountains?" "mountain-elevation")
                           (ops/op* "coastal?" "coast-sign" "coastal-mountain-elevation"))))))

(defn continents []
  (let
   [amplitude 4000
    sea-level 800
    glacial-sea-level (- sea-level 200)]
    (ops/terrain nil
                 sea-level
                 (ops/heightmap-let
                  [["ocean" (ops/op+ 1000 (ops/heightmap {:granularity 1 :irregularity 0.2 :amplitude (* 2 amplitude)}))]
                   ["continents" (ops/heightmap {:granularity 2 :irregularity 0.1 :amplitude amplitude})]
                   ["smooth" (ops/sigmoid-at [0 1] [0.2 -200] [0.8 200] "ocean")]
                   ["landform" (ops/op+ (ops/op* "smooth" "continents")
                                        (ops/op* (ops/op- 1 "smooth") (ops/min "continents" "ocean")))]
                   
                   ["sea-level-adjusted" (ops/op- "landform" glacial-sea-level)]
                   ["flatten" (ops/op* (ops/op- "landform" sea-level)
                                       (ops/sigmoid-at [0 0.75] [0.2 sea-level] [0.8 (+ sea-level 200)] "landform"))]
                   ["land?" (ops/sigmoid-at [0 1] [0.2 (- glacial-sea-level 200)] [0.8 sea-level] "landform")]
                   ["ocean?" (ops/sigmoid-at [0 1] [0.1 -200] [0.9 -400] "sea-level-adjusted")]
                   ["highlands" (ops/abs (ops/heightmap {:granularity 2 :irregularity 0.2 :amplitude 1}))]
                   ["highlands?" (ops/sigmoid-at [0 1] [0.8 0.14] [0.1 0.16] "highlands")]
                   ["highland-elevation" (ops/sigmoid-at [600 1000] [0.1 0] [0.8 500] (ops/abs (ops/heightmap {:granularity 2 :irregularity 0.2 :amplitude 1000})))]
                   ["coastal?" (ops/sigmoid-at [0 1] [0.2 350] [0.8 150] (ops/abs "sea-level-adjusted"))]
                   ["coast-sign" (ops/sigmoid-at [-1 1] [0.2 -50] [0.8 50] (ops/max "sea-level-adjusted" (ops/min 0 (ops/heightmap {:granularity 2 :irregularity 0.2 :amplitude 1000}))))]
                   ["coastal-mountain-elevation" (ops/max 0 (ops/heightmap {:granularity 1 :irregularity 0.2 :amplitude 6000}))]

                   ["mountains" (ops/abs (ops/heightmap {:granularity 3 :irregularity 0.2 :amplitude 1}))]
                   ["mountains?" (ops/sigmoid-at [0 1] [0.8 0.02] [0.1 0.1] "mountains")]
                   ["mountain-elevation" (ops/abs (ops/heightmap {:granularity 2 :irregularity 0.2 :amplitude 5000}))]]

                  (ops/op+ (ops/op- "landform" "flatten")
                           (ops/op* 2 "ocean?" "sea-level-adjusted")
                           (ops/op* "land?" "highlands?" "highland-elevation")
                           (ops/op* (ops/op- 1 "coastal?") "land?" "mountains?" "mountain-elevation")
                           (ops/op* "coastal?" "coast-sign" "coastal-mountain-elevation"))))))

(defn archipelago []
  (let
   [amplitude 1500
    sea-level 1200]
    (ops/terrain nil
                 sea-level
                 (ops/heightmap-let
                  [["landform" (ops/heightmap {:granularity 0 :irregularity 0.25 :amplitude amplitude})]
                   ["deep?" (ops/sigmoid-at [0 4] [0.1 (- sea-level 1000)] [0.8 (- sea-level 1500)] "landform")]
                   ["extra-depth" (ops/op* "deep?" (ops/op- (ops/abs "landform")))]
                   ["shallow?" (ops/sigmoid-at [0 1] [0.1 (- sea-level 1200)] [0.8 (- sea-level 1000)] "landform")]
                   ["islands" (ops/abs (ops/heightmap {:granularity 2 :irregularity 0.15 :amplitude 1}))]
                   ["islands?" (ops/sigmoid-at [0 1] [0.8 0.02] [0.1 0.08] "islands")]
                   ["island-elevation" (ops/abs (ops/heightmap {:granularity 5 :irregularity 0.3 :amplitude 5000}))]]
                  (ops/op+ "landform" "extra-depth" (ops/op* "shallow?" "islands?" "island-elevation"))))))
