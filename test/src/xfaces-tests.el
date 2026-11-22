;;; xfaces-tests.el --- tests for xfaces.c           -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)

(ert-deftest xfaces-color-distance ()
  ;; Check symmetry (bug#41544).
  (should (equal (color-distance "#222222" "#ffffff")
                 (color-distance "#ffffff" "#222222"))))

(ert-deftest xfaces-internal-color-values-from-color-spec ()
  (should (equal (color-values-from-color-spec "#f05")
                 '(#xffff #x0000 #x5555)))
  (should (equal (color-values-from-color-spec "#1fb0C5")
                 '(#x1f1f #xb0b0 #xc5c5)))
  (should (equal (color-values-from-color-spec "#1f8b0AC5e")
                 '(#x1f81 #xb0aa #xc5eb)))
  (should (equal (color-values-from-color-spec "#1f83b0ADC5e2")
                 '(#x1f83 #xb0ad #xc5e2)))
  (should (equal (color-values-from-color-spec "#1f83b0ADC5e2g") nil))
  (should (equal (color-values-from-color-spec "#1f83b0ADC5e20") nil))
  (should (equal (color-values-from-color-spec "#12345") nil))
  (should (equal (color-values-from-color-spec "rgb:f/23/28a")
                 '(#xffff #x2323 #x28a2)))
  (should (equal (color-values-from-color-spec "rgb:1234/5678/09ab")
                 '(#x1234 #x5678 #x09ab)))
  (should (equal (color-values-from-color-spec "rgb:0//0") nil))
  (should (equal (color-values-from-color-spec "rgbi:0/0.5/0.1")
                 '(0 32768 6554)))
  (should (equal (color-values-from-color-spec "rgbi:1e-3/1.0e-2/1e0")
                 '(66 655 65535)))
  (should (equal (color-values-from-color-spec "rgbi:0/0.5/10") nil))
  (should (equal (color-values-from-color-spec "rgbi:0/0/ 0") nil))
  (should (equal (color-values-from-color-spec "rgbi:0/0x0/0") nil))
  (should (equal (color-values-from-color-spec "rgbi:0/+0x1/0") nil)))

(ert-deftest xfaces-test-circular-inheritance ()
  "Test that bug#79672 remains solved."
  (let ((buf (get-buffer-create "xfaces-test")))
    (with-current-buffer buf
      (font-lock-mode -1)
      (set-face-attribute 'button nil :inherit 'link)
      (should (equal
               (should-error (set-face-attribute 'link nil :inherit 'button))
               (list 'error
                     "Face inheritance results in inheritance cycle"
                     'button))))
    (kill-buffer buf)))

(ert-deftest xfaces-test-face-equality ()
  "Test `internal-lisp-face-equal-p'."
  (should (internal-lisp-face-equal-p 'default 'default))
  (should-not (internal-lisp-face-equal-p 'default 'region))
  (should-not (internal-lisp-face-equal-p 'line-number 'default))
  (should-not (internal-lisp-face-equal-p 'line-number '
                                          line-number-current-line))
  (should (internal-lisp-face-equal-p 'line-number
                                      'line-number-current-line t)))
;; Index of the :font-features attribute in a Lisp face vector.
(defconst xfaces-tests--font-features-index 20)

(defun xfaces-tests--font-features (face-ref)
  (aref (face-attributes-as-vector face-ref)
        xfaces-tests--font-features-index))

(ert-deftest xfaces-font-features-validation ()
  (let ((face (make-face 'xfaces-tests--font-features)))
    (dolist (bad '(zero (zero) ((zero . "1")) (("zero" . 1))
                   ((zero . 1) . ss01) ((zero . 1) (zero . 0))))
      (should-error (set-face-attribute face nil :font-features bad)))
    (set-face-attribute face nil :font-features '((zero . 1) (ss01 . 0)))
    (should (equal (face-attribute face :font-features)
                   '((zero . 1) (ss01 . 0))))
    (set-face-attribute face nil :font-features nil)
    (should (null (face-attribute face :font-features)))))

(ert-deftest xfaces-font-features-merge ()
  ;; Features earlier in the list override the same features later in
  ;; the list, other features are kept, and the result is sorted.
  (should (equal (xfaces-tests--font-features
                  '((:font-features ((zero . 1)))
                    (:font-features ((ss01 . 1) (zero . 0)))))
                 '((ss01 . 1) (zero . 1))))
  ;; The same features in a different order give an 'equal' result.
  (should (equal (xfaces-tests--font-features
                  '(:font-features ((zero . 1) (calt . 0))))
                 (xfaces-tests--font-features
                  '(:font-features ((calt . 0) (zero . 1))))))
  ;; Invalid elements are ignored, and the first of duplicate tags
  ;; wins, rather than signaling an error during redisplay.
  (should (equal (xfaces-tests--font-features
                  '(:font-features ((zero . 1) bogus ("ss01" . 1)
                                    (zero . 0))))
                 '((zero . 1))))
  ;; Merging doesn't modify the merged lists.
  (let* ((from (list (cons 'zero 1)))
         (to (list (cons 'zero 0) (cons 'ss01 1)))
         (from-copy (copy-tree from))
         (to-copy (copy-tree to)))
    (xfaces-tests--font-features
     `((:font-features ,from) (:font-features ,to)))
    (should (equal from from-copy))
    (should (equal to to-copy))))

(ert-deftest xfaces-font-features-face-spec ()
  ;; `defface' and `face-spec-set' must not drop :font-features.
  (let ((face 'xfaces-tests--font-features-spec))
    (face-spec-set face '((t :font-features ((zero . 1)))))
    (should (equal (face-attribute face :font-features) '((zero . 1))))
    (face-spec-set face '((t :foreground "red")))
    (should (eq (face-attribute face :font-features) 'unspecified))))

(ert-deftest xfaces-font-features-inherit ()
  (let ((parent (make-face 'xfaces-tests--font-features-parent))
        (child (make-face 'xfaces-tests--font-features-child)))
    (set-face-attribute parent nil :font-features '((zero . 0) (ss01 . 1)))
    (set-face-attribute child nil :font-features '((zero . 1))
                        :inherit parent)
    (should (equal (xfaces-tests--font-features child)
                   '((ss01 . 1) (zero . 1))))
    ;; Inheritance doesn't modify the parent's features.
    (should (equal (face-attribute parent :font-features)
                   '((zero . 0) (ss01 . 1))))))

(provide 'xfaces-tests)

;;; xfaces-tests.el ends here
