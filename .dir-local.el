;;; Directory Local Variables
((git-commit-mode . ((magit-gptcommit-llm-provider . (lambda ()
						       (if (boundp 'llm-provider--opencode-bigpickle)
							   llm-provider--opencode-bigpickle
							 (make-llm-ollama
							  :chat-model "gpt-oss:20b"
							  :host "127.0.0.1"
							  :port 11434))
						       )
						   ))))

