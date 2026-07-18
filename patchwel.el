;;; patchwel.el --- Emacs client for Patchwork API -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Aaron Conole <aconole@redhat.com>

;; Author: Aaron Conole <aconole@redhat.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Patchwork is a patch tracking and management interface for mailing list
;; project development. This package provides an Emacs interface to interact
;; with Patchwork APIs.
;;
;; Entry point: `M-x patchwork-show-series' opens the main listing buffer.
;;

;;; Code:

(require 'patchwel-config)
(require 'patchwel-db)
(require 'patchwel-api)
(require 'patchwel-cache)
(require 'patchwel-git)
(require 'patchwel-ui)

(provide 'patchwel)

;;; patchwel.el ends here
