;;; test-ui-highlight.el --- highlighting/filter pure-function tests -*- lexical-binding: t; -*-
(require 'ert)

(ert-deftest patchwork-ui-test-rule-mine ()
  (let ((patchwork-my-identities '("alice")))
    (should (eq (patchwork-series--rule-mine (list :assignee "Alice Dev <alice@x.com>"))
                'patchwork-series-mine-face))
    (should (null (patchwork-series--rule-mine (list :assignee "Bob"))))
    (should (null (patchwork-series--rule-mine (list :assignee nil))))))

(ert-deftest patchwork-ui-test-rule-stale ()
  (let ((patchwork-series-stale-days 14))
    (should (eq (patchwork-series--rule-stale
                 (list :submitted-at (patchwork-cache--iso-datetime
                                       (time-subtract (current-time) (days-to-time 20)))
                       :comment-count 0))
                'patchwork-series-stale-face))
    (should (null (patchwork-series--rule-stale
                   (list :submitted-at (patchwork-cache--iso-datetime
                                        (time-subtract (current-time) (days-to-time 20)))
                         :comment-count 1))))
    (should (null (patchwork-series--rule-stale
                   (list :submitted-at (patchwork-cache--iso-datetime (current-time))
                         :comment-count 0))))))

(ert-deftest patchwork-ui-test-rule-old ()
  (let ((patchwork-series-old-days 30))
    (should (eq (patchwork-series--rule-old
                 (list :submitted-at (patchwork-cache--iso-datetime
                                       (time-subtract (current-time) (days-to-time 40)))))
                'patchwork-series-old-face))
    (should (null (patchwork-series--rule-old
                   (list :submitted-at (patchwork-cache--iso-datetime (current-time))))))))

(ert-deftest patchwork-ui-test-age-days-missing-date-is-nil ()
  (should (null (patchwork-series--age-days (list :submitted-at nil)))))

(ert-deftest patchwork-ui-test-row-face-first-match-wins ()
  (let ((patchwork-my-identities '("alice"))
        (patchwork-series-stale-days 14)
        (patchwork-series-old-days 30))
    ;; assigned to "alice" AND old enough to also match the old-rule:
    ;; mine wins since it's checked first
    (should (eq (patchwork-series--row-face
                 (list :assignee "alice"
                       :submitted-at (patchwork-cache--iso-datetime
                                      (time-subtract (current-time) (days-to-time 40)))
                       :comment-count 5))
                'patchwork-series-mine-face))
    (should (null (patchwork-series--row-face
                   (list :assignee "bob"
                         :submitted-at (patchwork-cache--iso-datetime (current-time))
                         :comment-count 5))))))

(ert-deftest patchwork-ui-test-matches-filter-p ()
  (let ((series (list :state "new" :server-url "http://x" :project-slug "proj"
                       :submitter "Alice Dev")))
    (should (patchwork-series--matches-filter-p series nil))
    (should (patchwork-series--matches-filter-p series (list :states '("new" "assigned"))))
    (should-not (patchwork-series--matches-filter-p series (list :states '("accepted"))))
    (should (patchwork-series--matches-filter-p series (list :server "http://x")))
    (should-not (patchwork-series--matches-filter-p series (list :server "http://y")))
    (should (patchwork-series--matches-filter-p series (list :project "proj")))
    (should-not (patchwork-series--matches-filter-p series (list :project "other")))
    (should (patchwork-series--matches-filter-p series (list :author "alice")))
    (should-not (patchwork-series--matches-filter-p series (list :author "bob")))
    (should (patchwork-series--matches-filter-p
             series (list :states '("new") :server "http://x" :project "proj" :author "alice")))
    (should-not (patchwork-series--matches-filter-p
                 series (list :states '("new") :server "http://y")))))

(provide 'test-ui-highlight)

;;; test-ui-highlight.el ends here
