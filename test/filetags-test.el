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
    (should (equal expected-remove-candidates (filetags-accumulate-remove-tags-candidates
                                               filenames)))))

(ert-deftest filetags-accumulate-add-tags-candidates-are-sorted-union-of-tags-minus-tags-which-are-in-every-file-test
    ()
  (setq filetags-controlled-vocabulary '())
  (let ((filenames '("/home/max/test -- test2 test3.txt" "/home/max/test -- test2 test1.txt"
                     "/home/max/test -- test2.txt"))
        (expected-add-candidates '("test1" "test3")))
    (should (equal expected-add-candidates (filetags-accumulate-add-tags-candidates filenames)))))

(ert-deftest filetags-accumulate-add-tags-candidates-includes-filetags-controlled-vocabulary ()
  (setq filetags-controlled-vocabulary '("controlled" "test2"))
  (let ((filenames '("/home/max/test -- test1.txt" "/home/max/test -- atest test1 test2.txt"))
        (expected-add-candidates '("atest" "controlled" "test2") ))
    (should (equal expected-add-candidates (filetags-accumulate-add-tags-candidates filenames)))
    )
  )


(ert-deftest filetags-prepend-add-to-tag-test
    ()
  (let ((tags '("test1" "test2" "test3"))
        (expected-tags '("+test1" "+test2" "+test3")))
    (should (equal expected-tags (filetags-prepend-list "+" tags)))))
(ert-deftest filetags-prepend-remove-to-tag-test
    ()
  (let ((tags '("test1" "test2" "test3"))
        (expected-tags '("-test1" "-test2" "-test3")))
    (should (equal expected-tags (filetags-prepend-list "-" tags)))))









;;; filetags-test.el ends here
