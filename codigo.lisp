; Autor: Gustavo Silva Malvestiti
; Arquivo de código fonte

; Carrega o arquivo de inserção de eventos
(load "~/trabalho/agenda.lisp")

(defun getMonthName(n) "[getMonthName] => Converte o número de um mês para a sua respectiva string de nome."
    (if (integerp n)
        (if (and (> n 0) (< n 13))
            ; Caso n válido, retorne a posição n - 1 do array
            (return-from getMonthName (aref #("Janeiro" "Fevereiro" "Março" "Abril" "Maio" "Junho" "Julho" "Agosto" "Setembro" "Outubro" "Novembro" "Dezembro") (- n 1)))
            (return-from getMonthName nil)
        )
        (return-from getMonthName nil)
    )
)

(defun getMonthNumber(n) "[getMonthNumber] => Converte a string de nome de um mês para o seu respectivo número inteiro."
    ; Array de meses
    (defparameter meses #("janeiro" "fevereiro" "março" "abril" "maio" "junho" "julho" "agosto" "setembro" "outubro" "novembro" "dezembro"))
    (if (stringp n)
        (dotimes (i 12)
            ; Caso lowercase de n corresponder ao valor de índice i em meses, retorne i + 1
            (when (string-equal (string-downcase n) (aref meses i))
                (return-from getMonthNumber (+ i 1))
            )
        ) 
        (return-from getMonthNumber nil)
    )
    (return-from getMonthNumber nil)
)


(defun tnil(n) "[tnil] => Verifica e converte valores nil para -1."
    (if (eq n nil)
        (return-from tnil -1)
        (return-from tnil n)
    )
)

(defun tminus1(n) "[tminus1] => Verifica e converte valores -1 para nil."
    (if (= n -1)
        (return-from tminus1 nil)
        (return-from tminus1 n)
    )
)

(defun agendaTNil(entrada) "[agendaTNil] => Aplica a conversão de nil para -1 em uma lista de listas."
    (loop for i in entrada do
        ; Posição de indice n da lista i recebe o retorno de tnil aplicado sobre a n° posição da lista
        (setf (nth 0 i) (tnil (first i)))
        (setf (nth 1 i) (tnil (second i)))
        (setf (nth 2 i) (tnil (third i)))
        (setf (nth 3 i) (tnil (fourth i)))
        (setf (nth 4 i) (tnil (fifth i)))
        (setf (nth 5 i) (tnil (sixth i)))
        (setf (nth 6 i) (tnil (seventh i)))
        (setf (nth 7 i) (tnil (eighth i)))
    )
    (return-from agendaTNil entrada)
)

(defun comparaDatas(a b) "[comparaDatas] => Compara duas datas a e b, retornando a maior."
    ; Verifica se a ou b são vazias, e caso sim, retorna a outra
    (when (eq a nil)
        (return-from comparaDatas b)
    )
    (when (eq b nil)
        (return-from comparaDatas a)
    )
    ; Definição dos parâmetros para cada elemento das listas de data
    (defparameter a1 (tnil (first a)))(defparameter a2 (tnil (second a)))(defparameter a3 (tnil (third a)))(defparameter a4 (tnil (fourth a)))
    (defparameter a5 (tnil (fifth a)))(defparameter a6 (tnil (sixth a)))(defparameter a7 (tnil (seventh a)))(defparameter a8 (tnil (eighth a)))
    (defparameter b1 (tnil (first b)))(defparameter b2 (tnil (second b)))(defparameter b3 (tnil (third b)))(defparameter b4 (tnil (fourth b)))
    (defparameter b5 (tnil (fifth b)))(defparameter b6 (tnil (sixth b)))(defparameter b7 (tnil (seventh b)))(defparameter b8 (tnil (eighth b)))
    ; Início da comparação
    (if (< a1 b1)
        (return-from comparaDatas b)
        (if (and (= a1 b1) (< a2 b2))
            (return-from comparaDatas b)
            (if (and (= a1 b1) (and (= a2 b2) (< a3 b3)))
                (return-from comparaDatas b)
                (if (and (= a1 b1) (and (= a2 b2) (and (= a3 b3) (< a4 b4))))
                    (return-from comparaDatas b)
                    (if (and (= a1 b1) (and (= a2 b2) (and (= a3 b3) (and (= a4 b4) (< a5 b5)))))
                        (return-from comparaDatas b)
                        (if (and (= a1 b1) (and (= a2 b2) (and (= a3 b3) (and (= a4 b4) (and (= a5 b5) (< a6 b6))))))
                            (return-from comparaDatas b)
                            (if (and (= a1 b1) (and (= a2 b2) (and (= a3 b3) (and (= a4 b4) (and (= a5 b5) (and (= a6 b6) (< a7 b7)))))))
                                (return-from comparaDatas b)
                                (if (and (= a1 b1) (and (= a2 b2) (and (= a3 b3) (and (= a4 b4) (and (= a5 b5) (and (= a6 b6) (and (= a7 b7) (< a8 b8))))))))
                                    (return-from comparaDatas b)
                                    ; Caso a >= b em todas as comparações, retorne a, caso contrário, b
                                    (return-from comparaDatas a)
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

(defun comparaDatasIgual(a b) "[comparaDatasIgual] => Compara se duas datas a e b são iguais, retornando T ou nil."
    ; Verifica se é a última posição da lista de datas, o nome/título da data
    (if (= (length a) 1)
        ; Comparação para strings
        (if (string-equal (car a) (car b))
            (return-from comparaDatasIgual T)
            (return-from comparaDatasIgual nil)
        )
        ; Comparação para números
        (if (not (eq (car a) (car b)))
            (return-from comparaDatasIgual nil)
            (comparaDatasIgual (cdr a) (cdr b))
        )
    )
)

(defun getMaior(lista) "[getMaior] => Retorna a maior lista de data em uma lista de listas de data."
    (defun maior(lista m) "[maior] => Função auxiliar de recursão de getMaior."
        ; Comparação e definição de maior para cada iteração da lista
        (setf m (comparaDatas (first lista) m))
        ; Verifica se final da lista
        (if (or (endp lista) (eq lista '()))
            (return-from getMaior m)
            (maior (cdr lista) m)
        )
    )
    ; Início da recursão, com valor de maior inicialmente sendo o primeiro elemento da lista
    (maior (cdr lista) (car lista))
)

(defun getIndex(n eventos) "[getIndex] => Retorna o índice do elemento n em uma lista."
    ; Posição inicial
    (setq j 0)
    (loop for i in eventos do
        ; Compara se os elementos são iguais e, caso sim, retorna o índice
        (when (comparaDatasIgual i n)
            (return-from getIndex j)
        )
        (setq j (+ j 1))
    )
    ; Retorno de nil caso elemento não encontrado
    (return-from getIndex nil)
)

(defun ordenaAgenda() "[ordenaAgenda] => Realiza a ordenação da lista de eventos."
    ; Verifica se lista de eventos está vazia
    (when (eq agenda '())
        (return-from ordenaAgenda nil)
    )
    ; Definição de eventos que recebe a aplicação de agendaTnil da cópia de agenda
    (defparameter eventos (agendaTNil (copy-tree agenda)))
    (defparameter maior nil)
    ; Definição e inicialização da lista de retorno
    (defparameter final '())
    ; Posição inicial
    (setq i 0)
    (loop while (< i (length agenda)) do
        ; Variável maior recebe uma cópia do maior elemento da lista
        (setq maior (copy-list (getMaior eventos)))
        ; Lista de retorno recebe o maior no começo da lista
        (push maior final)
        ; Posição de maior na lista de eventos é alterado para um valor que o desconsidere em futuras iterações
        (setf (nth (getIndex maior eventos) eventos) '(-1 -1 -1 -1 -1 -1 -1 -1 "nil"))
        (setq i (+ i 1))
    )
    ; Agenda recebe final
    (setq agenda final)
)

(defun vHora(h) "[vHora] => Realiza a verificação de uma hora."
    (if (integerp h)
        (if (and (> h 0) (< h 24))
            ; Caso hora válida, retorne T, e caso contrário, nil
            (return-from vHora T)
            (return-from vHora nil)
        )
        (return-from vHora nil)
    )
)

(defun vDia(d) "[vDia] => Realiza a verificação de um dia."
    (if (integerp d)
        (if (and (> d 0) (< d 31))
            ; Caso dia válido, retorne T, e caso contrário, nil
            (return-from vDia T)
            (return-from vDia nil)
        )
        (return-from vDia nil)
    )
)

(defun vMes(m) "[vMes] => Realiza a verificação de um mês."
    (if (integerp m)
        (if (and (> m 0) (< m 13))
            ; Caso mês válido, retorne T, e caso contrário, nil
            (return-from vMes T)
            (return-from vMes nil)
        )
        (return-from vMes nil)
    )
)

(defun calendario(&optional &key ano mesi diai mesf diaf) "[calendario] => Função que mostra o calendário para uma data específica."
    (defun calendarioDias(a b) "[calendarioDias] => Função auxiliar para impressão de dias."
        ; a = limite de dia inicial e b = limite de dia final
        ; Início do loop
        (loop for j from 1 to 30 do
            ; Caso limites de dia inicial e final inexistentes
            (when (and (eq a nil) (eq b nil))
                (format t "~d " j)
            )
            ; Caso limites de dia inicial e final ambos existentes
            (when (and (not (eq a nil)) (not (eq b nil)))
                ; Aplica ambos os limites
                (when (and (>= j a) (<= j b))
                    (format t "~d " j)
                )
            )
            ; Caso apenas limite de dia inicial existente
            (when (and (not (eq a nil)) (eq b nil))
                ; Aplica apenas o limite de dia inicial
                (when (>= j a)
                    (format t "~d " j)
                )
            )
            ; Caso apenas limite de dia final existente
            (when (and (not (eq b nil)) (eq a nil))
                ; Aplica apenas o limite de dia final
                (when (<= j b)
                    (format t "~d " j)
                )
            )
        )
    )
    ; Verificação da entrada de ano
    ; Caso ano nulo
    (when (eq ano nil)
        (print "Erro: Ano necessário!")
        (return-from calendario nil)
    )
    ; Caso ano não inteiro
    (when (not (integerp ano))
        (print "Erro: Ano inválido!")
        (return-from calendario nil)
    )
    ; Verificação das entradas dos meses inicial e final
    (when (not (eq mesi nil))
        ; Caso mês for string, converte para número
        (when (stringp mesi)
            (setq mesi (getMonthNumber mesi))
        )
        ; Caso mês inicial inválido
        (when (not (vMes mesi))
            (print "Erro: Mês inválido!")
            (return-from calendario nil)
        )
    )
    (when (not (eq mesf nil))
        ; Caso mês for string, converte para número
        (when (stringp mesf)
            (setq mesf (getMonthNumber mesf))
        )
        ; Caso mês final inválido
        (when (not (vMes mesf))
            (print "Erro: Mês inválido!")
            (return-from calendario nil)
        )
    )
    ; Verificação das entradas dos dias inicial e final
    (when (not (eq diai nil))
        ; Caso dia inicial inválido
        (when (not (vDia diai))
            (print "Erro: Dia inválido!")
            (return-from calendario nil)
        )
    )
    (when (not (eq diaf nil))
        ; Caso dia final inválido
        (when (not (vDia diaf))
            (print "Erro: Dia inválido!")
            (return-from calendario nil)
        )
    )
    ; Impressões
    ; Título do calendário para o respectivo ano
    (format t "~&Calendário de ~d:" ano)
    ; Início do loop dos meses
    (loop for i from 1 to 12 do
        ; Caso limites de mês inicial e final inexistentes
        (when (and (eq mesi nil) (eq mesf nil))
            ; Impressão do mês em questão e os seus dias
            (format t "~&    [~d] Mês de ~[Janeiro~;Fevereiro~;Março~;Abril~;Maio~;Junho~;Julho~;Agosto~;Setembro~;Outubro~;Novembro~:;Dezembro~]:~&        " i (- i 1))
            (calendarioDias diai diaf)
        )
        ; Caso limites de mês inicial e final ambos existentes
        (when (and (not (eq mesi nil)) (not (eq mesf nil)))
            ; Aplica ambos os limites
            (when (and (>= i mesi) (<= i mesf))
                ; Impressão do mês em questão e os seus dias
                (format t "~&    [~d] Mês de ~[Janeiro~;Fevereiro~;Março~;Abril~;Maio~;Junho~;Julho~;Agosto~;Setembro~;Outubro~;Novembro~:;Dezembro~]:~&        " i (- i 1))
                (calendarioDias diai diaf)
            )
        )
        ; Caso apenas limite de mês inicial existente
        (when (and (not (eq mesi nil)) (eq mesf nil))
            ; Aplica apenas o limite de mês inicial
            (when (>= i mesi)
                ; Impressão do mês em questão e os seus dias
                (format t "~&    [~d] Mês de ~[Janeiro~;Fevereiro~;Março~;Abril~;Maio~;Junho~;Julho~;Agosto~;Setembro~;Outubro~;Novembro~:;Dezembro~]:~&        " i (- i 1))
                (calendarioDias diai diaf)
            )
        )
        ; Caso apenas limite de mês final existente
        (when (and (not (eq mesf nil)) (eq mesi nil))
            ; Aplica apenas o limite de mês final
            (when (<= i mesf)
                ; Impressão do mês em questão e os seus dias
                (format t "~&    [~d] Mês de ~[Janeiro~;Fevereiro~;Março~;Abril~;Maio~;Junho~;Julho~;Agosto~;Setembro~;Outubro~;Novembro~:;Dezembro~]:~&        " i (- i 1))
                (calendarioDias diai diaf)
            )
        )
    )
)

(defun eventos(&optional &key anoi mesi diai horai anof mesf diaf horaf nome) "[eventos] => Função que lista eventos para uma data específica."
    ; Ordenação da agenda
    (ordenaAgenda)
    ; Definição de eventos que recebe a aplicação de agendaTnil da cópia de agenda
    (defparameter eventos (agendaTNil (copy-tree agenda)))
    ; Verificação das entradas de anos
    ; Caso ano inicial nulo
    (when (eq anoi nil)
        (print "Erro: Ano necessário!")
        (return-from eventos nil)
    )
    ; Caso ano inicial inválido
    (when (not (integerp anoi))
        (print "Erro: Ano inválido!")
        (return-from eventos nil)
    )
    ; Caso ano final exitente e inválido
    (when (not (eq anof nil))
        (when (not (integerp anof))
            (print "Erro: Ano inválido!")
            (return-from eventos nil)
        )
    )
    ; Caso ano final existente e menor que o ano inicial
    (when (and (not (eq anoi nil)) (not (eq anof nil)))
        (when (> anoi anof)
            (print "Erro: Ano inválido!")
            (return-from eventos nil)
        )
    )
    ; Verificação das entradas dos meses inicial e final
    (when (not (eq mesi nil))
        ; Caso mês for string, converte para número
        (when (stringp mesi)
            (setq mesi (getMonthNumber mesi))
        )
        ; Caso mês inicial inválido
        (when (not (vMes mesi))
            (print "Erro: Mês inválido!")
            (return-from eventos nil)
        )
    )
    (when (not (eq mesf nil))
        ; Caso mês for string, converte para número
        (when (stringp mesf)
            (setq mesf (getMonthNumber mesf))
        )
        ; Caso mês final inválido
        (when (not (vMes mesf))
            (print "Erro: Mês inválido!")
            (return-from eventos nil)
        )
    )
    ; Caso mês inicial maior que mês final na situação em que o ano final é nulo
    (when (and (not (eq mesi nil)) (not (eq mesf nil)))
        (when (> mesi mesf)
            (when (eq anof nil)
                (print "Erro: Mês inválido!")
                (return-from eventos nil)
            )
        )
    )
    ; Verificação das entradas dos dias inicial e final
    ; Caso dia inicial inválido
    (when (not (eq diai nil))
        (when (not (vDia diai))
            (print "Erro: Dia inválido!")
            (return-from eventos nil)
        )
    )
    ; Caso dia final inválido
    (when (not (eq diaf nil))
        (when (not (vDia diaf))
            (print "Erro: Dia inválido!")
            (return-from eventos nil)
        )
    )
    ; Verificação das entradas das horas inicial e final
    ; Caso hora inicial inválida
    (when (not (eq horai nil))
        (when (not (vHora horai))
            (print "Erro: Hora inválida!")
            (return-from eventos nil)
        )
    )
    ; Caso hora final inválida
    (when (not (eq horaf nil))
        (when (not (vHora horaf))
            (print "Erro: Hora inválida!")
            (return-from eventos nil)
        )
    )
    ; Verificação da entrada de nome
    (when (not (eq nome nil))
        (when (not (stringp nome))
            (print "Erro: Nome inválido!")
            (return-from eventos nil)
        )
    )
    ; Verifica se lista de eventos está vazia
    (when (eq (length eventos) 0)
        (print "Agenda vazia!")
        (return-from eventos nil)
    )
    ; Impressões
    ; Início do loop dos elementos da lista de eventos
    (loop for e in eventos do
        ; Definição da flag de impressão
        (defparameter flag T)
        ; Aplicação dos filtros, caso existentes
        ; E caso elemento da lista diferente de seu filtro respectivo, flag de impressão recebe nil
        ; Filtro de ano inicial
        (when (not (eq (first e) anoi))
            (setq flag nil)
        )
        ; Filtro de mês inicial
        (when (not (eq mesi nil))
            (when (not (eq (second e) mesi))
                (setq flag nil)
            )
        )
        ; Filtro de dia inicial
        (when (not (eq diai nil))
            (when (not (eq (third e) diai))
                (setq flag nil)
            )
        )
        ; Filtro de hora inicial
        (when (not (eq horai nil))
            (when (not (eq (fourth e) horai))
                (setq flag nil)
            )
        )
        ; Filtro de ano final
        (when (not (eq anof nil))
            (when (not (eq (fifth e) anof))
                (setq flag nil)
            )
        )
        ; Filtro de mês final
        (when (not (eq mesf nil))
            (when (not (eq (sixth e) mesf))
                (setq flag nil)
            )
        )
        ; Filtro de dia final
        (when (not (eq diaf nil))
            (when (not (eq (seventh e) diaf))
                (setq flag nil)
            )
        )
        ; Filtro de hora final
        (when (not (eq horaf nil))
            (when (not (eq (eighth e) horaf))
                (setq flag nil)
            )
        )
        ; Filtro de nome/título do evento
        (when (not (eq nome nil))
            (when (not (string-equal (ninth e) nome))
                (setq flag nil)
            )
        )
        ; Verifica a flag de impressão, e caso T, imprime o evento
        (when (eq flag T)
            ; Definição dos parâmetros para cada elemento da lista de data
            (defparameter e1 (tminus1 (first e)))(defparameter e2 (tminus1 (second e)))(defparameter e3 (tminus1 (third e)))(defparameter e4 (tminus1 (fourth e)))
            (defparameter e5 (tminus1 (fifth e)))(defparameter e6 (tminus1 (sixth e)))(defparameter e7 (tminus1 (seventh e)))(defparameter e8 (tminus1 (eighth e)))
            ; Formatação e impressão do evento
            (format t "~&    Evento: ~s | Início: ~2,'0d/~2,'0d/~4,'0d às ~2,'0d:00 | Fim: ~2,'0d/~2,'0d/~4,'0d às ~2,'0d:00" (ninth e) e3 e2 e1 e4 e7 e6 e5 e8)
        )
    )
)