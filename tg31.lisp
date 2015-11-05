;;;; Projeto IA
;;;; 1 Sem 2015-2016
;;;; Grupo tg31


; replace with (load "utils.fas")
;(load (compile-file "utils.lisp"))





;;; 2.1 Tipos

;;; 2.1.1	Tipo Accao
(defstruct accao
	coluna
	peca)

;; cria-accao : inteiro x array -> accao
(defun cria-accao (c p)
	(make-accao :coluna c :peca p))


; definidos automaticamente:
;; accao-coluna : accao -> inteiro
;; accao-peca : accao -> array


;;; 2.1.2	Tipo tabuleiro
