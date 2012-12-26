(load-file "kensei.el")


(ert-deftest kensei-test-backend-rubygem-present ()
  "Should fetch kensei backend version matching the frontend, if backend present
   If no rubygem present, install/update to newest with 'gem install kensei'"
  (let ((expected (concat "\"" kensei-current-version "\""))
	(actual (kensei-backend-version)))
    (should (equal expected actual))))


(ert-deftest kensei-test-construct-account-line ()
  "Should map account json data into clickable INBOX folder line"
  )

(ert-deftest kensei-test-construct-folder-line ()
  "Should map folder json data into clickable folder line"
  )

(ert-deftest kensei-test-construct-folder-line ()
  "Should map email json data into clickable email summary line"
  )
