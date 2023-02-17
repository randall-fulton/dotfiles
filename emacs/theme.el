(defun rf/mode-line-pad (n)
  "Generate padding of length N."
  (list (format (format "%%%ds" n) "")))

(defun rf/mode-line-render (left center right)
  "Render modeline with appropriate spacing."
  (let* ((half-width (/ (window-total-width) 2))
         (available-left
          (- half-width
             (length (format-mode-line left))))
         (available-right
          (- half-width
             (length (format-mode-line right))))
         (center-half-width
          (/ (length (format-mode-line center))
             2))
         (left-pad
          (- available-left
             center-half-width))
         (right-pad
          (- available-right
             center-half-width)))
    (append left
            (rf/mode-line-pad left-pad)
            center
            (rf/mode-line-pad right-pad)
            right)))

(setq-default
 mode-line-format
 '((:eval
    (rf/mode-line-render
     ;; left
     '(" beginning")
     
     ;; center
     '((:eval (propertize " %b " 'face 'mode-line-buffer-id)))
     
     ;; right
     '("end ")))))
