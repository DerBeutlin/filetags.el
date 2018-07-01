;;; filetags-test.el --- Tests for filetags
(load-file "filetags.el")
(require 'filetags)

(ert-deftest extract-filetags-from-filename-with-tags-and-extension-test
    ()
  (should (equal (filetags-extract-filetags "test -- test1 test2 test3.pdf") '("test1" "test2" "test3"))))

(ert-deftest extract-filetags-from-filename-with-tags-and-without-extension-test
    ()
  (should (equal (filetags-extract-filetags "test -- test1 test2 test3") '("test1" "test2" "test3"))))

(ert-deftest extract-filetags-from-filename-without-tags-test
    ()
  (should (equal (filetags-extract-filetags "test.pdf") nil)))

(ert-deftest extracted-filetags-are-a-set-test
    ()
  (should (equal (filetags-extract-filetags "test -- test1 test1 test3.pdf") '("test1" "test3"))))

(ert-deftest extracted-filetags-are-a-sorted-test
    ()
  (should (equal (filetags-extract-filetags "test -- test4 test1 test3.pdf") '("test1" "test3" "test4"))))

(ert-deftest extracted-filetags-whitespace-is-ignored-test
    ()
  (should (equal (filetags-extract-filetags "test --    test1       test2.pdf") '("test1" "test2"))))


(ert-deftest filetags-update-tags-adds-tags-with-plus-test
    ()
  (let ((fullname "/home/max/test.txt")
        (expected-fullname "/home/max/test -- test1.txt"))
    (should (equal (filetags-update-tags fullname
                                         '("+test1")) expected-fullname))))

(ert-deftest filetags-update-tags-remove-tags-with-minus-test
    ()
  (let ((fullname "/home/max/test -- test1.txt")
        (expected-fullname "/home/max/test.txt"))
    (should (equal (filetags-update-tags fullname
                                         '("-test1")) expected-fullname))))

(ert-deftest filetags-update-tags-removes-and-adds-simoultaniously-test
    ()
  (let ((fullname "/home/max/test -- test1.txt")
        (expected-fullname "/home/max/test -- test2.txt"))
    (should (equal (filetags-update-tags fullname
                                         '("-test1" "+test2")) expected-fullname))))

(ert-deftest filetags-update-tags-ignores-non-action-tags-test
    ()
  (let ((fullname "/home/max/test -- test1.txt")
        (expected-fullname "/home/max/test.txt"))
    (should (equal (filetags-update-tags fullname
                                         '("-test1" "not-actionable")) expected-fullname))))

(ert-deftest filetags-filter-add-tags-test
    ()
  (let ((tags-with-prefix '("+test1" "-test2" "test3"))
        (expected-add-tags '("test1")))
    (should (equal (filetags-filter-add-tags tags-with-prefix) expected-add-tags))))
(ert-deftest filetags-filter-remove-tags-test
    ()
  (let ((tags-with-prefix '("+test1" "-test2" "test3"))
        (expected-remove-tags '("test2")))
    (should (equal (filetags-filter-remove-tags tags-with-prefix) expected-remove-tags))))

(ert-deftest filetags-accumulate-remove-tags-candidates-are-sorted-union-of-tags-test
    ()
  (let ((filenames '("/home/max/test -- test3.txt" "/home/max/test -- test2 test1.txt"
                     "/home/max/test -- test2.txt"))
        (expected-remove-candidates '("test1" "test2" "test3")))
    (should (equal (filetags-accumulate-remove-tags-candidates
                    filenames) expected-remove-candidates))))

(ert-deftest filetags-accumulate-add-tags-candidates-are-sorted-union-of-tags-minus-tags-which-are-in-every-file-test
    ()
  (setq filetags-controlled-vocabulary '())
  (let ((filenames '("/home/max/test -- test2 test3.txt" "/home/max/test -- test2 test1.txt"
                     "/home/max/test -- test2.txt"))
        (expected-add-candidates '("test1" "test3")))
    (should (equal (filetags-accumulate-add-tags-candidates filenames) expected-add-candidates))))

(ert-deftest filetags-accumulate-add-tags-candidates-includes-filetags-controlled-vocabulary
    ()
  (setq filetags-controlled-vocabulary '("controlled" "test2"))
  (let ((filenames '("/home/max/test -- test1.txt" "/home/max/test -- atest test1 test2.txt"))
        (expected-add-candidates '("atest" "controlled" "test2")))
    (should (equal (filetags-accumulate-add-tags-candidates filenames) expected-add-candidates))))

(ert-deftest filetags-accumulate-add-tags-candidates-does-not-include-controlled-vocabulary-tags-which-are-in-every-file-test
    ()
  (setq filetags-controlled-vocabulary '("controlled"))
  (let ((filenames '("/home/max/test -- controlled.txt" "/home/max/test -- controlled.txt"))
        (expected-add-candidates '()))
    (should (equal (filetags-accumulate-add-tags-candidates filenames) expected-add-candidates))))



(ert-deftest filetags-prepend-add-to-tag-test
    ()
  (let ((tags '("test1" "test2" "test3"))
        (expected-tags '("+test1" "+test2" "+test3")))
    (should (equal (filetags-prepend-list "+" tags) expected-tags))))
(ert-deftest filetags-prepend-remove-to-tag-test
    ()
  (let ((tags '("test1" "test2" "test3"))
        (expected-tags '("-test1" "-test2" "-test3")))
    (should (equal (filetags-prepend-list "-" tags) expected-tags))))

(ert-deftest filetags-update-tags-with-prefix-adds-tag-test
    ()
  (let ((tags-with-prefix '())
        (entered-tag "+test"))
    (should (equal (filetags-update-tags-with-prefix entered-tag
                                                     tags-with-prefix) '("+test")))))

(ert-deftest filetags-update-tags-with-prefix-removes-inverse-tag-test
    ()
  (let ((tags-with-prefix '("-test"))
        (entered-tag "+test"))
    (should (equal (filetags-update-tags-with-prefix entered-tag
                                                     tags-with-prefix) '()))))

(ert-deftest filetags-inverse-tag-inverses-tag-test
    ()
  (let ((tag "+test"))
    (should (equal (filetags-inverse-tag tag) "-test"))
    (should (equal (filetags-inverse-tag (filetags-inverse-tag tag)) tag))))

(ert-deftest filetags-construct-candidates-adds-add-candidates-remove-candidates-and-inverse-tags-with-prefix-test
    ()
  (let ((add-candidates '("+test"))
        (remove-candidates '("-test2"))
        (tags-with-prefix '("+test3")))
    (should (equal (filetags-construct-candidates add-candidates
                                                  remove-candidates tags-with-prefix) '("+test" "-test2" "-test3")))))

(ert-deftest filetags-construct-candidates-removes-duplicates-and-sort-test
    ()
  (let ((add-candidates '("+test"))
        (remove-candidates '("-test3"))
        (tags-with-prefix '("+test3" "-test4")))
    (should (equal (filetags-construct-candidates add-candidates
                                                  remove-candidates tags-with-prefix) '("+test" "+test4" "-test3")))))

(ert-deftest filetags-construct-candidates-remove-tags-already-chosen-test ()
  (let ((add-candidates '("+test"))
        (remove-candidates '("-test2"))
        (tags-with-prefix '("+test" "-test2")))
    (should (equal (filetags-construct-candidates add-candidates
                                                  remove-candidates tags-with-prefix) '("+test2" "-test" ))))
  )



;;; filetags-test.el ends here
