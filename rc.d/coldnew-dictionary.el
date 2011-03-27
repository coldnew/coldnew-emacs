;;
(eval-when-compile (require 'cl))


;; (setq sdcv-dictionary-simple-list       ;星際譯王屏幕取詞詞典, 簡單, 快速
;;       '("懶蟲簡明英漢詞典"
;;	"懶蟲簡明漢英詞典"
;;	"KDic11萬英漢詞典"))
;; (setq sdcv-dictionary-complete-list     ;星際譯王的詞典, 完全, 詳細
;;       '("KDic11萬英漢詞典"
;;	"懶蟲簡明英漢詞典"
;;	"朗道英漢字典5.0"
;;	"XDICT英漢辭典"
;;	"朗道漢英字典5.0"
;;	"XDICT漢英辭典"
;;	"懶蟲簡明漢英詞典"
;;	"牛津英漢雙解美化版"
;;	"stardict1.3英漢辭典"
;;	"英漢漢英專業詞典"
;;	"CDICT5英漢辭典"
;;	"Jargon"
;;	"FOLDOC"
;;	"WordNet"))

(when (require* 'sdcv)
  (setq sdcv-dictionary-simple-list
	'(
	  "朗道英漢字典5.0"
	  ))
  (setq sdcv-dictionary-complete-list
	'(
	  "牛津現代英漢雙解詞典"
	  ))

  )


(provide 'coldnew-dictionary)
;; coldnew-dictionary.el ends here.
