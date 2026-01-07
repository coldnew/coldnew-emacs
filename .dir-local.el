;;; Directory Local Variables
((git-commit-mode . ((magit-gptcommit-llm-provider . (lambda ()
						       (if (boundp 'magit-gptcommit-llm-provider--opencode)
							   magit-gptcommit-llm-provider--opencode
							 (make-llm-ollama
							  :chat-model "gpt-oss:20b"
							  :host "127.0.0.1"
							  :port 11434))
						       )
						   ))))

