(load-file "kensei.el")

(ert-deftest kensei-test-addition ()
  "Adding should work"
  (should (= (+ 1 4) 5)))

(ert-deftest kensei-test-subtraction ()
  "Subtracting should work"
  (should (= (- 3 2) 1)))

(ert-deftest kensei-test-backend-rubygem-present ()
  "Should fetch kensei backend version matching the frontend, if backend present"
  (let ((expected (concat "\"" kensei-current-version "\""))
	(actual (kensei-backend-version)))
    (should (equal expected actual))))

(ert-deftest kensei-test-example-and-backend-data-match ()
  "Should have testdata that matches that of backend rubygem"
;; TODO check that local example json and remote fixture json matches in structure
  )

(ert-deftest kensei-test-construct-account-line ()
  "Should map account json data into clickable INBOX folder line"
)

(ert-deftest kensei-test-construct-folder-line ()
  "Should map folder json data into clickable folder line"
)

(ert-deftest kensei-test-construct-folder-line ()
  "Should map email json data into clickable email summary line"
)
