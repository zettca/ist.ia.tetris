;;;; Projeto IA
;;;; 1 Sem 2015-2016
;;;; Grupo tg31
;;;;
;;;; 77944 Luis Silva
;;;; 78013 Bruno Henriques
;;;; 78040 Sofia Reis




;;; Funcoes auxiliares

;; AUX peca-largura : peca -> inteiro
(defun peca-largura (p)
	(array-dimension p 1))


;; AUX list-max : lista -> inteiro
(defun list-max (l)
	(let ((max (first l)))
		(dotimes (i (length l))
			(let ((el (nth i l)))
				(when	(> el max)
						(setf max el))))
		max))


;;; 2.1 Tipos

;;; 2.1.1	Tipo Accao

;; cria-accao : inteiro x array -> accao
(defun cria-accao (c p)
	(cons c p))


;; accao-coluna : accao -> inteiro
(defun accao-coluna (accao)
	(first accao))

;; accao-peca : accao -> array
(defun accao-peca (accao)
	(rest accao))


;;; 2.1.2	Tipo Tabuleiro

;; cria-tabuleiro : void -> tabuleiro
(defun cria-tabuleiro ()
	(make-array '(18 10)))

;; copia-tabuleiro : tabuleiro -> tabuleiro
(defun copia-tabuleiro (tab)
	(let ((novo (cria-tabuleiro)))
		(dotimes (i 18)
		(dotimes (j 10)
			(setf (aref novo i j) (aref tab i j))))
		novo))

;; tabuleiro-preenchido-p : tabuliro x inteiro x inteiro -> logico
(defun tabuleiro-preenchido-p (tab l c)
	(aref tab l c))

;; tabuleiro-altura-coluna : tabuleiro x inteiro -> inteiro
(defun tabuleiro-altura-coluna (tab c)
	(dotimes (i 18) ; each line
		(let ((i (- 17 i)))
			(when (tabuleiro-preenchido-p tab i c)
				(return-from tabuleiro-altura-coluna (+ 1 i)))))
	0)

;; tabuleiro-linha-completa-p : tabuleiro x inteiro -> logico
(defun tabuleiro-linha-completa-p (tab l)
	(dotimes (j 10) ; each column
		(unless (tabuleiro-preenchido-p tab l j)
			(return-from tabuleiro-linha-completa-p nil)))
	t)

;; tabuleiro-preenche! : tabuleiro x inteiro x inteiro -> {}
(defun tabuleiro-preenche! (tab l c)
	(if (and (>= l 0) (< l 18) (>= c 0) (< c 10))
		(setf (aref tab l c) T)))

;; tabuleiro-remove-linha! : tabuleiro x inteiro -> {}
(defun tabuleiro-remove-linha! (tab l)
	(let ((n (- 17 l))) ; numero de linhas para mover
		(dotimes (i n)
			(let ((ii (+ i l))) ; de baixo pra cima
				(dotimes (j 10) ; copia a linha de cima para baixo
					(setf (aref tab ii j) (aref tab (+ ii 1) j))))))
	; poe a linha de cima a nil
	(dotimes (j 10)
		(setf (aref tab 17 j) nil)))

;; tabuleiro-topo-preenchido-p : tabuleiro -> logico
(defun tabuleiro-topo-preenchido-p (tab)
	(dotimes (j 10) ; columns
		(when (aref tab 17 j)
			(return-from tabuleiro-topo-preenchido-p T)))
	nil)

;; AUX tabuleiro-remove-linhas-preenchidas : tabuleiro -> inteiro (nr)
(defun tabuleiro-remove-linhas-preenchidas (tab)
	(let ((count 0))
		(dotimes (l 0)
			(when	(tabuleiro-linha-completa-p tab l)
					(tabuleiro-remove-linha tab l)
					(incf count)))
		count))

;; not tested
;; tabuleiros-iguais-p : tabuleiro x tabuleiro -> logico
(defun tabuleiros-iguais-p (tab1 tab2)
	(dotimes (i 18)
	(dotimes (j 10)
		(unless (eql (aref tab1 i j) (aref tab2 i j))
			(return-from tabuleiros-iguais-p nil))))
	T)

;; tabuleiro->array : tabuleiro -> array
(defun tabuleiro->array (tab)
	(copia-tabuleiro tab)) ; a nossa representacao e' um array, so it's done

;; array->tabuleiro : array -> tabuleiro
(defun array->tabuleiro (array)
	(copia-tabuleiro array))


;;; 2.1.3	Tipo Estado

(defstruct estado
	(pontos 0)
	(pecas-por-colocar ())
	(pecas-colocadas ())
	(tabuleiro (cria-tabuleiro)))


;; copia-estado : estado -> estado
(defun copia-estado (e)
	(let ((novo))
		(setf novo (make-estado))
		; temos de copiar o tabuleiro e as listas,
		; otherwise fica uma ref para a mesma
		(setf (estado-pontos novo) (estado-pontos e))
		(setf (estado-pecas-por-colocar novo)
					(copy-list (estado-pecas-por-colocar e)))
		(setf (estado-pecas-colocadas novo)
					(copy-list (estado-pecas-colocadas e)))
		(setf (estado-tabuleiro novo)
			  (copia-tabuleiro (estado-tabuleiro e)))
		novo))

;; estados-iguais-p : estado x estado -> logico
(defun estados-iguais-p (e1 e2)
	; temos de comparar os campos todos, em especial o tabuleiro
	(and (equal (estado-pontos e1) (estado-pontos e2))
		 (equal (estado-pecas-por-colocar e1) (estado-pecas-por-colocar e2))
		 (equal (estado-pecas-colocadas e1) (estado-pecas-colocadas e2))
		 (tabuleiros-iguais-p (estado-tabuleiro e1) (estado-tabuleiro e2))))

;; estado-final-p : estado ->logico
(defun estado-final-p (estado)
	(or (not (estado-pecas-por-colocar estado)) ; if empty
		(tabuleiro-topo-preenchido-p (estado-tabuleiro estado))))

;;; 2.1.4	Tipo Problema

(defstruct problema
	estado-inicial			; estado
	(solucao 'solucao)		; funcao : estado -> logico
	(accoes 'accoes)		; funcao : estado -> lista de accoes
	(resultado 'resultado)	; funcao : estado x accao -> estado
	custo-caminho)			; funcao : estado -> inteiro

;;; 2.2 Funcoes

;;; 2.2.1 Funcoes do problema


;; solucao : estado -> logico
(defun solucao (estado)
	(and (not (estado-pecas-por-colocar estado)) ; se lista vazia
		 (not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)))))

;; accoes : estado -> lista
(defun accoes (estado)
	(let ((peca (first (estado-pecas-por-colocar estado))))
		(accoes-aux-peca-configuracoes (peca-configuracoes peca))))

;; AUX ;; devolve as accoes para as configuracoes da peca
(defun accoes-aux-peca-configuracoes (configs)
	(let ((lista-accoes (list)))
		(dotimes (i (list-length configs))
			(setf	lista-accoes
					(append	(accoes-aux-uma-configuracao (nth i configs))
							lista-accoes)))
	(reverse lista-accoes)))

;; AUX ;; devolve as accoes para uma configuracao
(defun accoes-aux-uma-configuracao (config)
	(let ((largura (peca-largura config))
		(lista-accoes (list)))
			(dotimes (i (- 11 largura)) ; for each horizontal fit
				(push (cria-accao i config) lista-accoes))
	lista-accoes))

;; AUX calcula-pontos : inteiro -> inteiro
(defun calcula-pontos (linhas-removidas-count)
	(nth linhas-removidas-count '(0 100 300 500 800)))

;; AUX calcula-espaco-base-peca-coluna : peca x inteiro -> inteiro
;; numero de posicoes vazias na base da peca, numa coluna
(defun calcula-espaco-base-peca-coluna (peca coluna)
	(dotimes (l (array-dimension peca 0)) ; percorre as linhas
		(when (aref peca l coluna)		  ; quando linha estiver preenchida
			(return-from calcula-espaco-base-peca-coluna (+ 1 l))))
	0)

;; calcula-linha : tabuleiro x accao -> {}
(defun calcula-linha (tab accao)
	(let ((max 0) (shift 0) (vazio-tab 0) (vazio-peca 0)
		(peca (accao-peca accao))
		(width (peca-largura (accao-peca accao)))
		(coluna (accao-coluna accao)))

		(dotimes (i width) ; calcula max
			(setf max (max max (tabuleiro-altura-coluna tab (+ coluna i)))))

		(dotimes (i width) ; calcula shift
			(setf vazio-tab (- max (tabuleiro-altura-coluna tab (+ coluna i))))
			(setf vazio-peca (calcula-espaco-base-peca-coluna peca i))
			(setf shift (min shift (+ vazio-tab vazio-peca))))

		(- max shift)))


;; AUX coloca-peca-no-tabuleiro! : tabuleiro x accao -> {}
(defun coloca-peca-no-tabuleiro! (tab accao)
	(let ((peca (accao-peca accao))
		(l (calcula-linha tab accao))
		(c (accao-coluna accao)))

		(dotimes (i (array-dimension peca 0)) ; lines
		(dotimes (j (array-dimension peca 1)) ; columns
			(when (aref peca i j)
				(tabuleiro-preenche! tab (+ i l) (+ j c)))))))

;; resultado : estado x accao -> estado
(defun resultado (estado accao)
	(let ((novo-estado (copia-estado estado))) ; novo estado a ser retornado

		; ** atualiza listas de pecas ** ;
		; adiciona peca as pecas colocadas
		(push (first (estado-pecas-por-colocar novo-estado)) (estado-pecas-colocadas novo-estado))
		; remove peca colocada
		(setf	(estado-pecas-por-colocar novo-estado)
				(rest (estado-pecas-por-colocar novo-estado)))


		(coloca-peca-no-tabuleiro! (estado-tabuleiro estado) accao)

		; verifica topo preenchido
		(unless	(tabuleiro-topo-preenchido-p (estado-tabuleiro estado))
			; remove linhas e calcula pontos
			(incf	(estado-pontos novo-estado)
					(calcula-pontos (tabuleiro-remove-linhas-preenchidas (estado-tabuleiro novo-estado)))))

	novo-estado))

;; qualidade : estado -> inteiro
(defun qualidade (estado)
	(- 0 (estado-pontos estado))) ; returns CUSTO do estado. should be negative

;; custo-oportunidade : estado -> inteiro
(defun custo-oportunidade (estado)
	(let ((max-pontos 0) (pecas-colocadas (estado-pecas-colocadas estado)))
		(dotimes (i (length pecas-colocadas))
			(let ((peca (nth i pecas-colocadas)))
				(incf max-pontos (calcula-pontos (peca-altura peca)))))
		(- max-pontos (estado-pontos estado))))



; entrega:
(load "utils.fas")

; testing:
; (load (compile-file "utils.lisp"))


;;; CONSTANTES

;; configuracoes de cada peca
(defconstant peca-o (list peca-o0))
(defconstant peca-i (list peca-i0 peca-i1))
(defconstant peca-s (list peca-s0 peca-s1))
(defconstant peca-z (list peca-z0 peca-z1))
(defconstant peca-l (list peca-l0 peca-l1 peca-l2 peca-l3))
(defconstant peca-j (list peca-j0 peca-j1 peca-j2 peca-j3))
(defconstant peca-t (list peca-t0 peca-t1 peca-t2 peca-t3))



;; AUX peca-configuracoes : peca -> lista de pecas
(defun peca-configuracoes (p)
	(cond
		((eq p 'o) peca-o)
		((eq p 'i) peca-i)
		((eq p 's) peca-s)
		((eq p 'z) peca-z)
		((eq p 'l) peca-l)
		((eq p 'j) peca-j)
		((eq p 't) peca-t)
		(t nil)))

;; AUX peca-altura : peca -> inteiro
(defun peca-altura (p)
	(cond
		((eq p 'o) 2)
		((eq p 'i) 4)
		((eq p 's) 2)
		((eq p 'z) 2)
		((eq p 'l) 3)
		((eq p 'j) 3)
		((eq p 't) 2)
		(t 0)))