(asdf:defsystem "cl-ganzhi"
  :description "Convert Gregorian calendar date time to Chinese GanZhi calendar (also known as Sexagenary Cycle Calendar) date time."
  :version "0.1.0"
  :author "Jin, ChunHe <crane@cranejin.com>"
  :license "3-Clause BSD"
  :serial t
  :depends-on ("local-time" "local-time-duration")
  :in-order-to ((asdf:test-op (test-op #:cl-ganzhi/test)))
  :components ((:module "src" :serial t
                :components ((:file "packages")
                             (:file "specials")
                             (:file "utility")
                             (:file "tables")
		             (:file "cl-ganzhi")))))

(asdf:defsystem "cl-ganzhi/test"
  :description "Unit tests for cl-ganzhi."
  :version "0.1.0"
  :author "Jin, ChunHe <crane@cranejin.com>"
  :license "3-Clause BSD"
  :serial t
  :depends-on ("cl-ganzhi" :1am)
  :components ((:module "test" :serial t
  		:components ((:file "packages")
			     (:file "tests"))))
  :perform (asdf:test-op (op system)
			 (funcall (read-from-string "cl-ganzhi.test:run-tests"))))

