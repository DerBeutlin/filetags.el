;;; filetags.el --- package to manage filetags in filename  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Max Beutelspacher

;; Author: Max Beutelspacher <max@MACS>
;; Keywords: convenience, files
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;


;;; Code:
(defvar filetags-delimiter " -- " "delimiter between filename and tags")

(defun filetags-extract-filetags (filename)
  "extract the tags from FILENAME remove duplicates and sort them"
  (if (string-match-p (regexp-quote filetags-delimiter) (file-name-sans-extension filename))
    (filetags-sort-and-uniq-tags 
      (split-string 
       (car (reverse (split-string (file-name-sans-extension filename) filetags-delimiter))) split-string-default-separators
        t split-string-default-separators)
      )
    '()
    )
)

(defun filetags-sort-and-uniq-tags (tags)
  "sort TAGS and remove duplicates"
  (sort
   (remove-duplicates tags :test 'string=)
   'string<)
)

(defun filetags-extract-filename-without-tags(filename)
  "return filename without tags or delimiter or extension"
  (car(split-string (file-name-sans-extension filename) filetags-delimiter))
  )

(defun filetags-generate-new-filename (old_filename tags)
  "generates filename with TAGS from OLD_FILENAME"
  (let ((new_filename (filetags-extract-filename-without-tags old_filename))
        (extension (file-name-extension old_filename)))
    (if tags
    (concat new_filename filetags-delimiter (mapconcat 'identity (filetags-sort-and-uniq-tags tags) " ")  (if extension ".") extension)
    (concat new_filename (if extension ".") extension)
    )
      )
    )

(defun filetags-add-tags(fullname tags)
    (let* ((old_tags (filetags-extract-filetags (file-name-nondirectory fullname)))
          (all_tags (filetags-sort-and-uniq-tags (append tags old_tags)))
          (new_file_name (filetags-generate-new-filename (file-name-nondirectory fullname) all_tags)))
      (if (not (string= new_file_name (file-name-nondirectory fullname)))
            (dired-rename-file fullname (concat (file-name-directory fullname) new_file_name) nil)
        )
     )
 )

(defun filetags-remove-tags(fullname tags)
  (let* ((old_tags (filetags-extract-filetags (file-name-nondirectory fullname)))
         (all_tags (filetags-sort-and-uniq-tags (cl-set-difference old_tags tags :test 'string=)))
         (new_file_name (filetags-generate-new-filename (file-name-nondirectory fullname) all_tags)))
    (if (not (string= new_file_name (file-name-nondirectory fullname)))
        (dired-rename-file fullname (concat (file-name-directory fullname) new_file_name) nil)
      )
    )
  )

(defun dired-add-tags(tagsstring)
  (interactive "sEnter Tags space seperated: " )
  (if (dired-get-marked-files)
      (let((filenames (dired-get-marked-files))
           (tags (split-string tagsstring split-string-default-separators t split-string-default-separators)))
        (dolist (filename filenames)
          (filetags-add-tags filename tags)
          )
      )
  (let((fullname (dired-get-filename))
        (tags (split-string tagsstring split-string-default-separators t split-string-default-separators)))
    (filetags-add-tags fullname tags)
    )
  )
  )

(defun dired-remove-tags(tagsstring)
  (interactive "sEnter Tags space seperated: " )
  (if (dired-get-marked-files)
      (let((filenames (dired-get-marked-files))
           (tags (split-string tagsstring split-string-default-separators t split-string-default-separators)))
        (dolist (filename filenames)
          (filetags-remove-tags filename tags)
          )
        )
    (let((fullname (dired-get-filename))
         (tags (split-string tagsstring split-string-default-separators t split-string-default-separators)))
      (filetags-remove-tags fullname tags)
      )
    )
  )

(provide 'filetags)
;;; filetags.el ends here
