;;; test-db.el --- Schema/CRUD tests for patchwel-db.el -*- lexical-binding: t; -*-
(require 'ert)

(ert-deftest patchwork-db-test-connection-memoized ()
  (patchwork-test-with-temp-db
    (let ((c1 (patchwork-db-connection))
          (c2 (patchwork-db-connection)))
      (should (eq c1 c2)))
    (patchwork-db-close)
    (should (null patchwork-db--connection))
    (should (sqlitep (patchwork-db-connection)))))

(ert-deftest patchwork-db-test-project-roundtrip ()
  (patchwork-test-with-temp-db
    (patchwork-db-insert-project "http://x" 1 "Proj" "proj")
    ;; upsert overwrites name/slug for the same (server-url, id)
    (patchwork-db-insert-project "http://x" 1 "Proj Renamed" "proj2")
    (let ((rows (sqlite-select (patchwork-db-connection)
                                "SELECT name, slug FROM projects WHERE server_url = ? AND id = ?"
                                '("http://x" 1))))
      (should (equal rows '(("Proj Renamed" "proj2")))))))

(ert-deftest patchwork-db-test-series-roundtrip ()
  (patchwork-test-with-temp-db
    (let ((series (list :server-url "http://x" :id 1 :project-id 1 :project-slug "proj"
                         :name "s" :submitter "Alice" :version 1 :total 2
                         :submitted-at "2026-01-01T00:00:00" :state "new"
                         :assignee "bob" :comment-count 1 :ack-count 0 :review-count 0
                         :test-count 0 :fixes-count 0 :check-success 1 :check-warning 0
                         :check-fail 0 :url "http://x/series/1" :updated-at "2026-01-01T00:00:00")))
      (patchwork-db-upsert-series series)
      (let ((got (patchwork-db-get-series "http://x" 1)))
        (should (equal (plist-get got :name) "s"))
        (should (equal (plist-get got :assignee) "bob"))
        (should (= (plist-get got :check-success) 1)))
      ;; upsert overwrites in place
      (patchwork-db-upsert-series (plist-put (copy-sequence series) :state "accepted"))
      (should (equal (plist-get (patchwork-db-get-series "http://x" 1) :state) "accepted"))
      (should (null (patchwork-db-get-series "http://x" 999))))
    (let ((rows (patchwork-db-query-series "http://x" "proj")))
      (should (= (length rows) 1)))
    (should (null (patchwork-db-query-series "http://x" "nope")))))

(ert-deftest patchwork-db-test-patch-roundtrip ()
  (patchwork-test-with-temp-db
    (patchwork-db-insert-patch
     (list :server-url "http://x" :id 100 :series-id 1 :project-id 1 :state "new"
           :submitter "Alice" :delegate nil :name "a patch" :date "2026-01-01"
           :series-position 1 :check-state "pending" :content "body" :diff "diff"
           :submitter-email "alice@example.com" :msgid "<m@x>" :to "to@x" :cc "cc@x"
           :references "<r@x>" :in-reply-to "<i@x>" :updated-at "2026-01-01"))
    (let ((got (patchwork-db-get-patch "http://x" 100)))
      (should (equal (plist-get got :name) "a patch"))
      (should (equal (plist-get got :content) "body"))
      (should (equal (plist-get got :msgid) "<m@x>")))
    (patchwork-db-insert-patch
     (list :server-url "http://x" :id 101 :series-id 1 :project-id 1 :state "new"
           :submitter "Alice" :name "second" :date "2026-01-02" :series-position 2))
    (let ((patches (patchwork-db-get-series-patches "http://x" 1)))
      (should (= (length patches) 2))
      (should (equal (mapcar (lambda (p) (plist-get p :id)) patches) '(100 101))))
    (should (null (patchwork-db-get-patch "http://x" 999)))))

(ert-deftest patchwork-db-test-comment-roundtrip ()
  (patchwork-test-with-temp-db
    (patchwork-db-insert-comment
     (list :server-url "http://x" :id 200 :patch-id 100 :author "Bob"
           :date "2026-01-02" :content "hi" :msgid "<c@x>" :subject "Re: x"
           :submitter-email "bob@x" :to "to@x" :cc "cc@x" :references "<r@x>"
           :in-reply-to "<i@x>"))
    (let ((got (patchwork-db-get-comment "http://x" 200)))
      (should (equal (plist-get got :content) "hi"))
      (should (equal (plist-get got :author) "Bob")))
    (let ((comments (patchwork-db-get-comments "http://x" 100)))
      (should (= (length comments) 1)))
    (should (null (patchwork-db-get-comment "http://x" 999)))))

(ert-deftest patchwork-db-test-check-roundtrip ()
  (patchwork-test-with-temp-db
    (patchwork-db-insert-check
     (list :server-url "http://x" :id 300 :patch-id 100 :reporter "ci"
           :state "success" :context "build" :description "ok"
           :target-url "http://ci/300" :date "2026-01-03"))
    (let ((checks (patchwork-db-get-checks "http://x" 100)))
      (should (= (length checks) 1))
      (should (equal (plist-get (car checks) :state) "success")))))

(ert-deftest patchwork-db-test-sync-meta-roundtrip ()
  (patchwork-test-with-temp-db
    (should (null (patchwork-db-get-sync-meta "k")))
    (patchwork-db-set-sync-meta "k" "v1")
    (should (equal (patchwork-db-get-sync-meta "k") "v1"))
    (patchwork-db-set-sync-meta "k" "v2")
    (should (equal (patchwork-db-get-sync-meta "k") "v2"))))

(ert-deftest patchwork-db-test-schema-migration-drops-stale-data ()
  (patchwork-test-with-temp-db
    (patchwork-db-insert-project "http://x" 1 "Proj" "proj")
    (patchwork-db-upsert-series
     (list :server-url "http://x" :id 1 :project-id 1 :project-slug "proj"
           :name "s" :submitter "Alice" :version 1 :total 1
           :submitted-at "2026-01-01" :state "new" :assignee nil
           :comment-count 0 :ack-count 0 :review-count 0 :test-count 0
           :fixes-count 0 :check-success 0 :check-warning 0 :check-fail 0
           :url "" :updated-at "2026-01-01"))
    (should (= (length (patchwork-db-query-series "http://x")) 1))
    ;; force an old schema version directly, then re-init: data should be
    ;; dropped, but the schema itself remains queryable afterward
    (sqlite-execute (patchwork-db-connection) "PRAGMA user_version = 0")
    (patchwork-db-init (patchwork-db-connection))
    (should (null (patchwork-db-query-series "http://x")))
    (should (= (caar (sqlite-select (patchwork-db-connection) "PRAGMA user_version"))
               patchwork-db-schema-version))
    ;; schema still usable after migration
    (patchwork-db-insert-project "http://x" 1 "Proj" "proj")
    (patchwork-db-upsert-series
     (list :server-url "http://x" :id 2 :project-id 1 :project-slug "proj"
           :name "s2" :submitter "Alice" :version 1 :total 1
           :submitted-at "2026-01-01" :state "new" :assignee nil
           :comment-count 0 :ack-count 0 :review-count 0 :test-count 0
           :fixes-count 0 :check-success 0 :check-warning 0 :check-fail 0
           :url "" :updated-at "2026-01-01"))
    (should (= (length (patchwork-db-query-series)) 1))))

(provide 'test-db)

;;; test-db.el ends here
