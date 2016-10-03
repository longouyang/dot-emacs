
emacs -Q -l profile-dotemacs.el \
    --eval "(setq profile-dotemacs-file \
        (setq load-file-name \"init.el\"))" \
    -f profile-dotemacs
