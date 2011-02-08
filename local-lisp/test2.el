;;; test2.el ---                                                                

;; Copyright 2011 Yen-Chin,Lee                                           
;;                                                                                    
;; Author: coldnew coldnew.tw@gmail.com                                   
;; Keywords:                                                                        
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/test2.el                 
(defconst test2-version "0.1")                                          

;; This program is free software; you can redistribute it and/or modify               
;; it under the terms of the GNU General Public License as published by               
;; the Free Software Foundation; either version 2, or (at your option)                
;; any later version.                                                                 
;;                                                                                    
;; This program is distributed in the hope that it will be useful,                    
;; but WITHOUT ANY WARRANTY; without even the implied warranty of                     
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                      
;; GNU General Public License for more details.                                       
;;                                                                                    
;; You should have received a copy of the GNU General Public License                  
;; along with this program; if not, write to the Free Software                        
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.                          

;;; Commentary:                                                                       
;;                                                                                    
;;                                                                                  

;;; Bug Report:                                                                       
;;                                                                                    
;; If you have problems, send a bug report via M-x test2-send-bug-report. 
;; I implemented bug report feature because I want to know your current state.        
;; It helps me to solve problems easily.                                              
;; The step is:                                                                       
;;  0) Setup mail in Emacs, the easiest way is:                                       
;;       (setq user-mail-address "your@mail.address")                               
;;       (setq user-full-name "Your Full Name")                                     
;;       (setq smtpmail-smtp-server "your.smtp.server.tw")                          
;;       (setq mail-user-agent 'message-user-agent)                                   
;;       (setq message-send-mail-function 'message-smtpmail-send-it)                  
;;  1) Be sure to use the LATEST version of anything.el.                              
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)          
;;  3) Use Lisp version instead of compiled one: (load "test2.el")              
;;  4) Do it!                                                                         
;;  5) If you got an error, please do not close *Backtrace* buffer.                   
;;  6) M-x test2-send-bug-report (outside)                                
;;     then M-x insert-buffer *Backtrace* (if you got error)                          
;;  7) Describe the bug using a precise recipe.                                       
;;  8) Type C-c C-c to send.                                                          
;;  # If you are a Taiwanese, please write in Taiwanese :P                            

;;; Change Log:                                                                       
;;                                                                                    
;;                                                                                    

;;; Usage:                                                                            
;; Put this file into your load-path and the following into your ~/.emacs:            
;;   (require 'test2)                                                     

;;; Code:                                                                             

(provide 'test2)                                                          
(eval-when-compile (require 'cl))                                                     

;;;;##########################################################################        
;;;;  User Options, Variables                                                         
;;;;##########################################################################        





;; test2.el ends here.                                                            
