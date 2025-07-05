;;; claude-code-emacs-native-v2-test.el --- Test v2 implementation -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple test to see if we can use streaming JSON input correctly

;;; Code:

(defun test-streaming-json ()
  "Test streaming JSON input."
  (interactive)
  (let* ((process-buffer (get-buffer-create "*test-streaming*"))
         (first-message (json-encode
                         '((type . "user")
                           (message . ((role . "user")
                                       (content . [((type . "text")
                                                    (text . "Hello! This is the first message."))])))))))
    (with-current-buffer process-buffer
      (erase-buffer)
      (let ((process (make-process
                      :name "test-streaming"
                      :buffer process-buffer
                      :command (list "claude"
                                     "-p" "Starting conversation..."  ; Initial prompt
                                     "--output-format" "stream-json"
                                     "--input-format" "stream-json"
                                     "--verbose")
                      :filter (lambda (proc output)
                                (with-current-buffer (process-buffer proc)
                                  (goto-char (point-max))
                                  (insert output)))
                      :sentinel (lambda (proc event)
                                  (message "Process event: %s" event)))))
        ;; Wait a bit then send first real message
        (sit-for 1)
        (process-send-string process (concat first-message "\n"))

        ;; Wait and send second message
        (sit-for 3)
        (let ((second-message (json-encode
                               '((type . "user")
                                 (message . ((role . "user")
                                             (content . [((type . "text")
                                                          (text . "This is the second message. Are you still there?"))])))))))
          (process-send-string process (concat second-message "\n")))))
    (switch-to-buffer process-buffer)))

(provide 'claude-code-emacs-native-v2-test)
;;; claude-code-emacs-native-v2-test.el ends here
