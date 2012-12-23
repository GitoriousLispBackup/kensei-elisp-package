(load-file "kensei.el")
(setq kensei-use-mock-backend t)

(ert-deftest kensei-test-addition ()
  "Adding should work"
  (should (= (+ 1 4) 5)))

(ert-deftest kensei-test-subtraction ()
  "Subtracting should work"
  (should (= (- 3 2) 1)))

;;(ert-deftest kensei-test-json-parsing ()
;;  "Example of how to parse incoming json"
