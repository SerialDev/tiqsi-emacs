#!/bin/bash

# Test Emacs performance with large Python files

echo "=== Testing Emacs Performance with Django ==="
echo "Starting at: $(date)"

# Set up environment
export EMACS_TEST_DIR="$PWD"
cd test-projects

# Launch Emacs with init-lite.el and performance test
echo "Launching Emacs..."
time emacs -q -l ../init-lite.el \
    --eval "(setq initial-buffer-choice nil)" \
    --eval "(add-to-list 'load-path \"$EMACS_TEST_DIR\")" \
    -l ../test-performance.el \
    --eval "(message \"Test setup complete\")"