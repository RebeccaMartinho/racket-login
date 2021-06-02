#lang racket/gui
; ALUNOS: Rebecca Martinho e Luan Bruno

;banco
(require db)
(define pgc
   (mysql-connect #:user "root"
                  #:database "racket"
                  #:server "localhost"
                  #:password "root"
    )
 )

;(query-exec pgc
 ;"create table usuario (conta integer, senha integer, saldo integer)")

;temporario limpa o banco sempre que aaplicação inicia
;(query-exec pgc
   ;"create temporary table usuario (conta integer, senha integer, saldo integer)")

(define (select account password)
 (query-maybe-value pgc "SELECT conta FROM usuario WHERE conta=? AND senha=?" account password))

(define (login a b)
  (if (number? (select a b)) (message-box "Login" (format  "Os dados conferem!\n Conta: ~a" a)
                                             frame '(ok)) (error (message-box "error" "Dados não conferem"))
  )
 )

(define (select-dados account password)
 (query-maybe-value pgc "SELECT saldo FROM usuario WHERE conta=? AND senha=?" account password)
)


(define (create account password)
 (query-exec pgc
  "INSERT INTO usuario values (?, ?, 0)" account password
 )
)

(define (deposito saldo account)
 (query-exec pgc
  "UPDATE usuario SET saldo = saldo + ? WHERE conta = ?" saldo account
))

(define (saque saldo account)
 (query-exec pgc
  "UPDATE usuario SET saldo = saldo - ? WHERE conta = ?" saldo account
 )
)
; verifica se o saque é permitido
(define (verificaSaque conta saldo valor)
  (cond ( (>= saldo valor)
          (message-box "Saque" "Saque realizado com sucesso!")
          (saque valor conta)
          (send frame-saque show #f)
          (send frame-dados show #t)
          (send saldo-banco set-value (format "~a" (- saldo valor) ))
        )(else
          (message-box "Error" "Saque não autorizado! Saldo insuficiente!"))
 )
)
;verifica valor do deposito
(define (verificaDeposito conta senha valor)
   (cond ( (> valor 0)
          (message-box "Deposito" "Deposito realizado com sucesso!")
          (deposito valor conta)
          (send frame-deposito show #f)
          (send frame-dados show #t)
          (send saldo-banco set-value (format "~a" (select-dados conta senha) ))
          )(else
            (message-box "Error" "Depósito negativo impossível!"))
   )
 )
; validação da senha criada
(define (senhaNumerica conta senha)
  (cond ((number? (string->number senha))
                  (create conta senha)
                  (message-box "Criação de conta" (format  "Sua conta foi criada!\n Número: ~a"conta);
                   frame '(ok))
         )(else
           (message-box "error" "Senha inválida"))
 )
)
;fim banco

;Frame inicial
(define frame (new frame%
                [label "Sistema de banco"]
                [height 200]
                [width 400]))
(new message% [parent frame][label "Já possui uma conta? entre!"])

;Frame do "criar conta"
(define frame-criaconta (new frame%
                [label "Sistema de banco"]
                [height 200]
                [width 400]))
(new message% [parent frame-criaconta][label "Escolha uma senha numérica para a sua conta"])

;frame dos dados
(define frame-dados (new frame%
                [label "Sistema de banco"]
                [height 200]
                [width 400]))
(new message% [parent frame-dados][label "Dados do cliente"])

;frame de deposito
(define frame-deposito (new frame%
                [label "Depósito"]
                [height 200]
                [width 400]))
(new message% [parent frame-deposito][label "Digite o valor que irá depositar na conta"])

;frame de saque
(define frame-saque (new frame%
                [label "Depósito"]
                [height 200]
                [width 400]))
(new message% [parent frame-saque][label "Digite o valor que irá sacar da conta"])

;inputs de conta
(define conta
  (new text-field%
       [label "Conta corrente"] [parent frame] [min-width  32] ))

(define
  conta-criacao
  (new text-field%
       [label "Nova conta corrente"]
       [enabled #f]
       [font (make-object font% 10 'default 'normal 'bold)]
       [parent frame-criaconta]
       [min-width  32] ))

(define conta-banco
  (new text-field%
       [label "Conta corrente"]
       [enabled #f]
       [font (make-object font% 10 'default 'normal 'bold)]
       [parent frame-dados]
       [min-width  24] ))

(define saldo-banco
  (new text-field%
       [label "Saldo"]
       [enabled #f]
       [font (make-object font% 10 'default 'normal 'bold)]
       [parent frame-dados]
       [min-width  24] ))

;input de senhas
(define senha
  (new text-field%
       [label "Senha"]
       [parent frame]
       [min-width  32]))

(define senha-criacao
  (new text-field%
       [label "Senha"]
       [parent frame-criaconta]
       [min-width  32]))

;input valor do depósito e saque
(define deposito-valor (new text-field% [label "Valor"] [parent frame-deposito] [min-width  32]))
(define saque-valor (new text-field% [label "Valor"] [parent frame-saque] [min-width  32]))

; entrar
(new button% [parent frame]
             [label "Entrar"]
             [callback (lambda (button event)
                        
                         (~a "\nConta " (string->number (send conta get-value))
                             "\nSenha " (string->number (send senha get-value))
                             
                                           
                             (display (~a "\nVerificando credenciais... " ))                  
                             (display (login(send conta get-value) (send senha get-value)))
        
                             (send frame show #f)
                             (send saldo-banco set-value (format "~a" (select-dados (send conta get-value)(send senha get-value))))
                             (send conta-banco set-value (send conta get-value))
                             (send frame-dados show #t)                

                          ))])

; BOTÃO CRIAR CONTA
  (new button% [parent frame] [label "Criar conta"]
       
       [callback (lambda (button event)
                            (send conta-criacao set-value (~a (random 10000)))
                   (display (~a "\nCriação de conta... " ))

                   (send frame-criaconta show #t)
                   (send frame show #f)
                   (send conta set-value "")
                   (send senha set-value "")
                   (send senha-criacao set-value "")
                 )
       ]
)
;BOTÃO CRIAR 
(new button% [parent frame-criaconta]
             [label "Criar"]
             [callback (lambda (button event) 
                         (senhaNumerica (send conta-criacao get-value) (send senha-criacao get-value) )
                         
                          (display (~a "\n CONTA "))
                          (display(string->number (send conta-criacao get-value)))
                         
                          (display (~a "\n SENHA "))
                          (display(string->number (send senha-criacao get-value)))
                         )
             ]
)

;BOTÃO VOLTAR
(new button% [parent frame-criaconta][label "Voltar"]
             [callback (lambda (button event)
                         (display (~a "\n VOLTANDO... "))
                         (send frame-criaconta show #f)
                         (send frame show #t)
                         )
             ]
)

;deposito
(new button% [parent frame-dados][label "Depósito"]
    [callback (lambda (button event)
                            
                  (display (~a "DEPÓSITO... " ))  
                  (send frame-deposito show #t)
                  (send frame-dados show #f)
                  (send deposito-valor set-value "")
               )
    ]
)

;BOTÃO DEPOSITAR
(new button% [parent frame-deposito] [label "Depositar"]
       
    [callback (lambda (button event)
                    (display (~a "\n DEPOSITANDO... "))
                (verificaDeposito (send conta get-value) (send senha get-value) (string->number(send deposito-valor get-value)))
               )
    ]
)


;BOTÃO SAQUE
(new button% [parent frame-dados] [label "Saque"]
       
    [callback (lambda (button event)
                         
                  (display (~a "\n SAQUE...  "))                
                  (send frame-saque show #t)
                  (send frame-dados show #f)
                  (send saque-valor set-value "")
                )])

;BOTÃO SACAR
(new button% [parent frame-saque] [label "Sacar"]
       
    [callback (lambda (button event)
                (display (~a "\n SACANDO...  "))
                (verificaSaque (send conta get-value) (string->number(send saldo-banco get-value)) (string->number(send saque-valor get-value)))
               )
    ]
)
                               
;BOTÃO Sair
(new button% [parent frame-dados]
             [label "Sair"]
             [callback (lambda (button event)
                         (display (~a "\n SAINDO... "))
                         (send frame-dados show #f)
                         (send frame show #t)
                        )
             ]
)

;voltar
(new button% [parent frame-deposito] [label "Voltar"]
       
    [callback (lambda (button event)
                            
                   (display (~a "\n VOLTANDO... "))
                   (send frame-deposito show #f) 
                   (send frame-dados show #t))
    ]
)
(send frame show #t)

;voltar
(new button% [parent frame-saque] [label "Voltar"]
       
    [callback (lambda (button event)
                            
                   (display (~a "\n VOLTANDO... "))
                   (send frame-saque show #f) 
                   (send frame-dados show #t))
     ]
)

(send frame show #t)