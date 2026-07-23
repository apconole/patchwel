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

(ert-deftest patchwork-db-test-pending-change-crud ()
  (patchwork-test-with-temp-db
    (should (null (patchwork-db-get-pending-change "http://x" 100 "state")))
    (patchwork-db-queue-pending-change "http://x" 100 "state" "accepted" "new")
    (let ((got (patchwork-db-get-pending-change "http://x" 100 "state")))
      (should (equal (plist-get got :desired-value) "accepted"))
      (should (equal (plist-get got :observed-value) "new"))
      (should (plist-get got :queued-at)))
    ;; a second queue on the same (server, patch, field) replaces the first
    (patchwork-db-queue-pending-change "http://x" 100 "state" "rejected" "accepted")
    (let ((got (patchwork-db-get-pending-change "http://x" 100 "state")))
      (should (equal (plist-get got :desired-value) "rejected"))
      (should (equal (plist-get got :observed-value) "accepted")))
    ;; a different field on the same patch is tracked independently
    (patchwork-db-queue-pending-change "http://x" 100 "delegate" "bob" nil)
    (should (= (length (patchwork-db-get-pending-changes "http://x")) 2))
    (patchwork-db-delete-pending-change "http://x" 100 "state")
    (should (null (patchwork-db-get-pending-change "http://x" 100 "state")))
    (should (patchwork-db-get-pending-change "http://x" 100 "delegate"))))

(ert-deftest patchwork-db-test-pending-changes-filtered-by-server ()
  (patchwork-test-with-temp-db
    (patchwork-db-queue-pending-change "http://x" 100 "state" "accepted" "new")
    (patchwork-db-queue-pending-change "http://y" 200 "state" "accepted" "new")
    (should (= (length (patchwork-db-get-pending-changes)) 2))
    (should (= (length (patchwork-db-get-pending-changes "http://x")) 1))
    (should (= (length (patchwork-db-get-pending-changes "http://y")) 1))))

(ert-deftest patchwork-db-test-pending-change-survives-schema-migration ()
  ;; pending_changes is deliberately excluded from patchwork-db-init's
  ;; drop-and-recreate list, since it holds state that can't be
  ;; re-fetched from the server -- unlike the rest of the cache.
  (patchwork-test-with-temp-db
    (patchwork-db-queue-pending-change "http://x" 100 "state" "accepted" "new")
    (sqlite-execute (patchwork-db-connection) "PRAGMA user_version = 0")
    (patchwork-db-init (patchwork-db-connection))
    (let ((got (patchwork-db-get-pending-change "http://x" 100 "state")))
      (should (equal (plist-get got :desired-value) "accepted")))))

(ert-deftest patchwork-db-test-purge-project-leaves-other-projects-intact ()
  (patchwork-test-with-temp-db
    (patchwork-db-insert-project "http://x" 1 "Good" "good")
    (patchwork-db-insert-project "http://x" 2 "Bad" "bad")
    (patchwork-db-upsert-series
     (list :server-url "http://x" :id 100 :project-id 1 :project-slug "good"
           :name "good series" :submitter "A" :version 1 :total 1 :submitted-at "2026-01-01"
           :state "new" :assignee nil :comment-count 0 :ack-count 0 :review-count 0
           :test-count 0 :fixes-count 0 :check-success 0 :check-warning 0 :check-fail 0
           :url "" :updated-at "2026-01-01"))
    (patchwork-db-upsert-series
     (list :server-url "http://x" :id 200 :project-id 2 :project-slug "bad"
           :name "bad series" :submitter "A" :version 1 :total 1 :submitted-at "2026-01-01"
           :state "new" :assignee nil :comment-count 0 :ack-count 0 :review-count 0
           :test-count 0 :fixes-count 0 :check-success 0 :check-warning 0 :check-fail 0
           :url "" :updated-at "2026-01-01"))
    (patchwork-db-insert-patch
     (list :server-url "http://x" :id 1000 :series-id 100 :project-id 1 :state "new"
           :submitter "A" :name "good patch" :date "2026-01-01" :series-position 1))
    (patchwork-db-insert-patch
     (list :server-url "http://x" :id 2000 :series-id 200 :project-id 2 :state "new"
           :submitter "A" :name "bad patch" :date "2026-01-01" :series-position 1))
    (patchwork-db-insert-comment
     (list :server-url "http://x" :id 5000 :patch-id 2000 :author "B" :date "2026-01-01" :content "c"))
    (patchwork-db-insert-check
     (list :server-url "http://x" :id 6000 :patch-id 2000 :reporter "ci" :state "success"
           :context "b" :description "d" :date "2026-01-01"))
    (patchwork-db-queue-pending-change "http://x" 2000 "state" "accepted" "new")
    (patchwork-db-set-note "http://x" "patch" 1000 "good note")
    (patchwork-db-set-note "http://x" "series" 100 "good series note")
    (patchwork-db-set-note "http://x" "patch" 2000 "bad note")
    (patchwork-db-set-note "http://x" "series" 200 "bad series note")
    (patchwork-db-purge-project "http://x" "bad")
    (should (patchwork-db-get-series "http://x" 100))
    (should (patchwork-db-get-patch "http://x" 1000))
    (should-not (patchwork-db-get-series "http://x" 200))
    (should-not (patchwork-db-get-patch "http://x" 2000))
    (should-not (patchwork-db-get-comment "http://x" 5000))
    (should (null (patchwork-db-get-checks "http://x" 2000)))
    (should-not (patchwork-db-get-pending-change "http://x" 2000 "state"))
    (should (null (sqlite-select (patchwork-db-connection)
                                  "SELECT 1 FROM projects WHERE server_url = ? AND slug = ?"
                                  '("http://x" "bad"))))
    ;; untouched project's notes survive; purged project's notes are gone
    (should (equal (patchwork-db-get-note "http://x" "patch" 1000) "good note"))
    (should (equal (patchwork-db-get-note "http://x" "series" 100) "good series note"))
    (should-not (patchwork-db-get-note "http://x" "patch" 2000))
    (should-not (patchwork-db-get-note "http://x" "series" 200))))

(ert-deftest patchwork-db-test-note-crud ()
  (patchwork-test-with-temp-db
    (should-not (patchwork-db-get-note "http://x" "patch" 100))
    (patchwork-db-set-note "http://x" "patch" 100 "line one\nline two")
    (should (equal (patchwork-db-get-note "http://x" "patch" 100) "line one\nline two"))
    ;; overwrite
    (patchwork-db-set-note "http://x" "patch" 100 "updated")
    (should (equal (patchwork-db-get-note "http://x" "patch" 100) "updated"))
    ;; a series note with the same target-id is tracked independently
    (patchwork-db-set-note "http://x" "series" 100 "series note")
    (should (equal (patchwork-db-get-note "http://x" "series" 100) "series note"))
    (should (equal (patchwork-db-get-note "http://x" "patch" 100) "updated"))
    (patchwork-db-delete-note "http://x" "patch" 100)
    (should-not (patchwork-db-get-note "http://x" "patch" 100))
    (should (patchwork-db-get-note "http://x" "series" 100))))

(ert-deftest patchwork-db-test-note-blank-content-deletes ()
  (patchwork-test-with-temp-db
    (patchwork-db-set-note "http://x" "patch" 100 "something")
    (should (patchwork-db-get-note "http://x" "patch" 100))
    (patchwork-db-set-note "http://x" "patch" 100 "   \n  ")
    (should-not (patchwork-db-get-note "http://x" "patch" 100))
    (patchwork-db-set-note "http://x" "patch" 100 "something else")
    (patchwork-db-set-note "http://x" "patch" 100 nil)
    (should-not (patchwork-db-get-note "http://x" "patch" 100))))

(ert-deftest patchwork-db-test-note-survives-schema-migration ()
  ;; notes is deliberately excluded from patchwork-db-init's
  ;; drop-and-recreate list, since it holds hand-authored content that
  ;; can't be re-fetched from the server.
  (patchwork-test-with-temp-db
    (patchwork-db-set-note "http://x" "patch" 100 "a note worth keeping")
    (sqlite-execute (patchwork-db-connection) "PRAGMA user_version = 0")
    (patchwork-db-init (patchwork-db-connection))
    (should (equal (patchwork-db-get-note "http://x" "patch" 100) "a note worth keeping"))))

(ert-deftest patchwork-db-test-purge-project-noop-when-nothing-cached ()
  (patchwork-test-with-temp-db
    ;; Just confirm it doesn't error out when there's nothing to purge.
    (patchwork-db-purge-project "http://x" "never-synced")
    (should-not (patchwork-db-query-series "http://x"))))

(provide 'test-db)

;;; test-db.el ends here
